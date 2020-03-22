{-# LANGUAGE LambdaCase #-}

-- | ShapeFile format
--
-- https://www.esri.com/library/whitepapers/pdfs/shapefile.pdf
--
module Haskus.Format.Shapefile
   ( Header (..)
   , readHeader
   , readRecord
   , readShapeFile
   , Record (..)
   , RecordHeader (..)
   , Shape (..)
   , Box (..)
   , Point (..)
   , PointM (..)
   , PointZ (..)
   , MultiPoint (..)
   , PolyLine (..)
   , PatchType (..)
   , Patch (..)
   , MRange
   , ZRange
   , IndexRecord (..)
   )
where

import Haskus.Number.Word
import Haskus.Number.Float
import Haskus.Binary.Serialize.Get
import Haskus.Utils.Flow

-- | Header of a SHP/SHX file
--
-- Header has fixed 100-byte size.
data Header = Header
   { headerFileLength  :: !Word32
   , headerShapeType   :: !ShapeType   -- ^ All the shapes in the file have the same type
   , headerBoundX      :: (Float64,Float64)
   , headerBoundY      :: (Float64,Float64)
   , headerBoundZ      :: (Float64,Float64)
   , headerBoundM      :: (Float64,Float64)
   }
   deriving (Show)

data ShapeType
   = TNull
   | TPoint
   | TPolyLine
   | TPolygon
   | TMultiPoint
   | TPointZ
   | TPolyLineZ
   | TPolygonZ
   | TMultiPointZ
   | TPointM
   | TPolyLineM
   | TPolygonM
   | TMultiPointM
   | TMultiPatch
   deriving (Show,Eq,Ord)

-- | Surface patch type
data PatchType
   = TTriangleStrip
   | TTriangleFan
   | TOuterRing
   | TInnerRing
   | TFirstRing
   | TRing
   deriving (Show,Eq,Ord)


fromShapeId :: Word32 -> ShapeType
fromShapeId = \case
   0  -> TNull
   1  -> TPoint
   3  -> TPolyLine
   5  -> TPolygon
   8  -> TMultiPoint
   11 -> TPointZ
   13 -> TPolyLineZ
   15 -> TPolygonZ
   18 -> TMultiPointZ
   21 -> TPointM
   23 -> TPolyLineM
   25 -> TPolygonM
   28 -> TMultiPointM
   31 -> TMultiPatch
   e  -> error $ "Unknown shape type: " ++ show e

fromPatchType :: Word32 -> PatchType
fromPatchType = \case
   0 -> TTriangleStrip
   1 -> TTriangleFan
   2 -> TOuterRing
   3 -> TInnerRing
   4 -> TFirstRing
   5 -> TRing
   e -> error $ "Unknown patch type: " ++ show e

-- | Read ShapeFile header
readHeader :: GetMonad m => m Header
readHeader = do
   -- file code is fixed to 9994
   fileCode <- getWord32BE
   when (fileCode /= 9994) $ error "Invalid file code"

   -- skip unused bytes
   getSkipBytes (5*4)

   fileLength <- getWord32BE
   version    <- getWord32LE
   when (version /= 1000) $ error "Invalid version"

   shapeType <- fromShapeId <$> getWord32LE
   
   boundX <- (,) <$> getFloat64LE <*> getFloat64LE
   boundY <- (,) <$> getFloat64LE <*> getFloat64LE
   boundZ <- (,) <$> getFloat64LE <*> getFloat64LE
   boundM <- (,) <$> getFloat64LE <*> getFloat64LE

   return $ Header
      { headerFileLength = fileLength
      , headerShapeType  = shapeType
      , headerBoundX     = boundX
      , headerBoundY     = boundY
      , headerBoundZ     = boundZ
      , headerBoundM     = boundM
      }

-- | Record header
data RecordHeader = RecordHeader
   { recordNumber        :: !Word32 -- ^ Record number (starting at 1)
   , recordContentLength :: !Word32 -- ^ Content size in 16-bit words
   }
   deriving (Show)

-- | Read a record header
readRecordHeader :: GetMonad m => m RecordHeader
readRecordHeader = RecordHeader <$> getWord32BE <*> getWord32BE

-- | (x,y)
data Point
   = Point Float64 Float64
   deriving (Show)

-- | (x,y,m)
data PointM
   = PointM Float64 Float64 Float64
   deriving (Show)

-- | (x,y,z,m)
data PointZ
   = PointZ Float64 Float64 Float64 Float64
   deriving (Show)

newtype MultiPoint p
   = MultiPoint [p]
   deriving (Show)

newtype PolyLine p
   = PolyLine [[p]]
   deriving (Show)

newtype Polygon p
   = Polygon [[p]]
   deriving (Show)

data Box
   = Box (Float64,Float64) (Float64,Float64)
   deriving (Show)

data Patch
   = Patch PatchType [PointZ]
   deriving (Show)

type MRange = (Float64,Float64)
type ZRange = (Float64,Float64)

data Shape
   = SNull
   | SPoint  Point
   | SPointM PointM
   | SPointZ PointZ
   | SMultiPoint  Box (MultiPoint Point)
   | SMultiPointM Box MRange (MultiPoint PointM)
   | SMultiPointZ Box ZRange MRange (MultiPoint PointZ)
   | SPolyLine    Box (PolyLine Point)
   | SPolyLineM   Box MRange (PolyLine PointM)
   | SPolyLineZ   Box ZRange MRange (PolyLine PointZ)
   | SPolygon     Box (Polygon Point)                -- ^ Counter-clockwise rings are holes. Rings are closed.
   | SPolygonM    Box MRange (Polygon PointM)        -- ^ Counter-clockwise rings are holes. Rings are closed.
   | SPolygonZ    Box ZRange MRange (Polygon PointZ) -- ^ Counter-clockwise rings are holes. Rings are closed.
   | SMultiPatch  Box ZRange MRange [Patch]
   deriving (Show)

-- | Read a 2D Point
readPoint :: GetMonad m => m Point
readPoint = Point <$> getFloat64LE <*> getFloat64LE

-- | Read a 2D Box
readBox :: GetMonad m => m Box
readBox = do
   x <- (,) <$> getFloat64LE <*> getFloat64LE
   y <- (,) <$> getFloat64LE <*> getFloat64LE
   return (Box x y)

data Record
   = Record !RecordHeader !Shape
   deriving (Show)

readRecord :: GetMonad m => m Record
readRecord = Record <$> readRecordHeader <*> readShape

readShape :: GetMonad m => m Shape
readShape = do
   shapeType <- fromShapeId <$> getWord32LE

   let
      -- split a list of points from a list of start indices
      splitPoints _  []       = []
      splitPoints ps [_]      = [ps]
      splitPoints ps (a:b:cs) = case splitAt (fromIntegral (b-a)) ps of
         (aps,ps') -> aps : splitPoints ps' (b:cs)

      readPointsM numPoints points = do
         mrange <- readRange
         ms <- replicateM (fromIntegral numPoints) $ getFloat64LE
         let ps = zipWith (\(Point x y) m -> PointM x y m) points ms
         return (mrange,ps)

      readPointsZ numPoints points = do
         zrange <- readRange
         zs <- replicateM (fromIntegral numPoints) $ getFloat64LE
         mrange <- readRange
         ms <- replicateM (fromIntegral numPoints) $ getFloat64LE
         let ps = zipWith3 (\(Point x y) z m -> PointZ x y z m) points zs ms
         return (zrange,mrange,ps)

      readRange = (,) <$> getFloat64LE <*> getFloat64LE

   case shapeType of
      TNull        -> return SNull

      TPoint       -> SPoint  <$> readPoint
      TPointM      -> SPointM <$> (PointM <$> getFloat64LE <*> getFloat64LE <*> getFloat64LE)
      TPointZ      -> SPointZ <$> (PointZ <$> getFloat64LE <*> getFloat64LE <*> getFloat64LE <*> getFloat64LE)

      TMultiPoint  -> do
         box <- readBox
         numPoints <- getWord32LE
         points <- replicateM (fromIntegral numPoints) $ readPoint
         return (SMultiPoint box (MultiPoint points))

      TMultiPointM -> do
         box <- readBox
         numPoints <- getWord32LE
         points <- replicateM (fromIntegral numPoints) $ readPoint
         (mrange,pointsM) <- readPointsM numPoints points
         return (SMultiPointM box mrange (MultiPoint pointsM))

      TMultiPointZ -> do
         box <- readBox
         numPoints <- getWord32LE
         points <- replicateM (fromIntegral numPoints) $ readPoint
         (zrange,mrange,pointsZ) <- readPointsZ numPoints points
         return (SMultiPointZ box zrange mrange (MultiPoint pointsZ))

      TPolyLine -> do
         box <- readBox
         numParts <- getWord32LE
         numPoints <- getWord32LE
         startIndices <- replicateM (fromIntegral numParts) getWord32LE
         points <- replicateM (fromIntegral numPoints) readPoint
         return (SPolyLine box (PolyLine (splitPoints points startIndices)))

      TPolyLineM -> do
         box <- readBox
         numParts <- getWord32LE
         numPoints <- getWord32LE
         startIndices <- replicateM (fromIntegral numParts) getWord32LE
         points <- replicateM (fromIntegral numPoints) readPoint
         (mrange,pointsM) <- readPointsM numPoints points
         return (SPolyLineM box mrange (PolyLine (splitPoints pointsM startIndices)))

      TPolyLineZ -> do
         box <- readBox
         numParts <- getWord32LE
         numPoints <- getWord32LE
         startIndices <- replicateM (fromIntegral numParts) getWord32LE
         points <- replicateM (fromIntegral numPoints) readPoint
         (zrange,mrange,pointsZ) <- readPointsZ numPoints points
         return (SPolyLineZ box zrange mrange (PolyLine (splitPoints pointsZ startIndices)))

      TPolygon -> do
         box <- readBox
         numParts <- getWord32LE
         numPoints <- getWord32LE
         startIndices <- replicateM (fromIntegral numParts) getWord32LE
         points <- replicateM (fromIntegral numPoints) readPoint
         return (SPolygon box (Polygon (splitPoints points startIndices)))

      TPolygonM -> do
         box <- readBox
         numParts <- getWord32LE
         numPoints <- getWord32LE
         startIndices <- replicateM (fromIntegral numParts) getWord32LE
         points <- replicateM (fromIntegral numPoints) readPoint
         (mrange,pointsM) <- readPointsM numPoints points
         return (SPolygonM box mrange (Polygon (splitPoints pointsM startIndices)))

      TPolygonZ -> do
         box <- readBox
         numParts <- getWord32LE
         numPoints <- getWord32LE
         startIndices <- replicateM (fromIntegral numParts) getWord32LE
         points <- replicateM (fromIntegral numPoints) readPoint
         (zrange,mrange,pointsZ) <- readPointsZ numPoints points
         return (SPolygonZ box zrange mrange (Polygon (splitPoints pointsZ startIndices)))

      TMultiPatch -> do
         box <- readBox
         numParts <- getWord32LE
         numPoints <- getWord32LE
         startIndices <- replicateM (fromIntegral numParts) getWord32LE
         partTypes <- fmap fromPatchType <$> replicateM (fromIntegral numParts) getWord32LE
         points <- replicateM (fromIntegral numPoints) readPoint
         (zrange,mrange,pointsZ) <- readPointsZ numPoints points
         let partPoints = splitPoints pointsZ startIndices
         let patchs = zipWith Patch partTypes partPoints
         return (SMultiPatch box zrange mrange patchs)


-- | Read a full shape fill
readShapeFile :: GetMonad m => m (Header,[Record])
readShapeFile = do
   header <- readHeader
   let go current rs
         | current >= headerFileLength header
         = return (header, reverse rs)

         | otherwise = do
            r@(Record h _) <- readRecord
            let recLength = recordContentLength h * 2 -- record length are in 16-bit words
            go (current+recLength) (r:rs)

   go 100 [] -- size of the header

-- | Index record in SHX file
data IndexRecord = IndexRecord
   { indexOffset        :: !Word32  -- ^ Offset in SHP file (in 16-bit words)
   , indexContentLength :: !Word32  -- ^ Length (in 16-bit words)
   }
