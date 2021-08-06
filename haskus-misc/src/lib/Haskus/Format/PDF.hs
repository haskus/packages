{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Haskus.Format.PDF where

import Haskus.Utils.Flow
import Data.Maybe

import ByteString.StrictBuilder
import qualified Data.ByteString as BS
import Data.Word
import Data.Char
import Data.Bits
import GHC.Natural
import Foreign.Storable
import qualified Data.List as List
import Numeric
import GHC.Exts

type PDF = Builder

newtype Offset
  = Offset Word
  deriving newtype (Show,Num,Eq,Ord)

renderPDF :: FilePath -> PDF -> IO ()
renderPDF fp pdf = BS.writeFile fp (builderBytes pdf)

ascii :: String -> PDF
ascii s = mconcat (fmap asciiChar s)

hexa_word8 :: Word8 -> PDF
hexa_word8 x = word8 (asc (x `shiftR` 4)) <> word8 (asc (x .&. 0xF))
  where
    asc v
      | v <= 9    = 0x30 + v
      | otherwise = 0x41 + v - 10

encodeUtf8 :: Char -> [Word8]
encodeUtf8 (ord -> x)
  | x <= 0x007f = [ fromIntegral x]
  | x <= 0x07ff = [ fromIntegral (0xC0 .|. ((x `shiftR` 6) .&. 0x1F))
                  , fromIntegral (0x80 .|. (x .&. 0x3F))
                  ]
  | x <= 0xffff = [ fromIntegral (0xE0 .|. (x `shiftR` 12) .&. 0x0F)
                  , fromIntegral (0x80 .|. (x `shiftR` 6) .&. 0x3F)
                  , fromIntegral (0x80 .|. (x .&. 0x3F))
                  ]
  | otherwise   = [ fromIntegral (0xF0 .|. (x `shiftR` 18))
                  , fromIntegral (0x80 .|. ((x `shiftR` 12) .&. 0x3F))
                  , fromIntegral (0x80 .|. ((x `shiftR` 6) .&. 0x3F))
                  , fromIntegral (0x80 .|. (x .&. 0x3F))
                  ]


------------------------------------
-- Character set
------------------------------------

-- There are 3 character classes: regular, delimiter, white-space

-- | Is the character a white-space
--
-- 1) White-space are equivalent except in comments, strings, and streams.
-- 2) In all other contexts, a sequence of consecutive white-space characters is
-- considered as one character.
isWhiteSpace :: Word8 -> Bool
isWhiteSpace = \case
  0x00 -> True -- null (NUL)
  0x09 -> True -- horizontal tab (HT)
  0x0A -> True -- line feed (LF)
  0x0C -> True -- form feed (FF)
  0x0D -> True -- carriage return (CR)
  0x20 -> True -- space (SP)
  _    -> False

space :: PDF
space = word8 0x20

-- | End-of-line can be CR, LF or CR-LF.
eol :: PDF
eol = word8 0x0A -- LF

isDelimiter :: Word8 -> Bool
isDelimiter = \case
  0x28 -> True -- left parenthesis
  0x29 -> True -- right parenthesis
  0x3C -> True -- less-than sign
  0x3E -> True -- greater-than sign
  0x5B -> True -- left square bracket
  0x5D -> True -- right square bracket
  0x7B -> True -- left curly brace
  0x7D -> True -- right curly brace
  0x2F -> True -- solidus ("slash")
  0x25 -> True -- percent sign
  _    -> False

leftParen, rightParen, leftAngle, rightAngle,
  leftSquare, rightSquare, leftCurly, rightCurly,
  solidus, percent :: PDF
leftParen   = word8 0x28
rightParen  = word8 0x29
leftAngle   = word8 0x3C
rightAngle  = word8 0x3E
leftSquare  = word8 0x5B
rightSquare = word8 0x5D
leftCurly   = word8 0x7B
rightCurly  = word8 0x7D
solidus     = word8 0x2F
percent     = word8 0x25

isRegular :: Word8 -> Bool
isRegular = \case
  -- white-spaces
  0x00 -> False -- null (NUL)
  0x09 -> False -- horizontal tab (HT)
  0x0A -> False -- line feed (LF)
  0x0C -> False -- form feed (FF)
  0x0D -> False -- carriage return (CR)
  0x20 -> False -- space (SP)
  -- delimiters
  0x28 -> False -- left parenthesis
  0x29 -> False -- right parenthesis
  0x3C -> False -- less-than sign
  0x3E -> False -- greater-than sign
  0x5B -> False -- left square bracket
  0x5D -> False -- right square bracket
  0x7B -> False -- left curly brace
  0x7D -> False -- right curly brace
  0x2F -> False -- solidus ("slash")
  0x25 -> False -- percent sign
  _    -> True

------------------------------------
-- Comment
------------------------------------

-- | Insert a comment into a PDF.
--
-- Note that characters are truncated to ASCII
comment :: String -> PDF
comment s = case filter is_invalid s of
  [] -> percent <> mconcat (fmap asciiChar s) <> eol
  xs -> error $ "Invalid character in comment: " ++ show (fmap ord xs)
  where
    is_invalid c = case fromIntegral (ord c) of
      0x20 -> False
      0x09 -> False
      x | isDelimiter x -> False
      x | isRegular x   -> False
      _ -> True

------------------------------------
-- Objects
------------------------------------

-- Booleans: true and false

bTrue :: PDF
bTrue = ascii "true"

bFalse :: PDF
bFalse = ascii "false"

-- Numeric: integer and real

int :: Integral a => a -> PDF
int a = mconcat (fmap asciiChar (show (toInteger a)))

float :: RealFloat a => a -> PDF
float a = mconcat (fmap asciiChar (showFFloat Nothing a ""))

-- Strings: literal or hexadecimal

litString :: [Word8] -> PDF
litString s = leftParen <> mconcat (fmap go s) <> rightParen
  where
    -- any character except (unbalanced) parentheses and reverse solidus.
    -- We escape every parentheses to avoid checking balancing.
    go = \case
      0x5C -> word16BE 0x5C5C -- reverse solidus:  \\
      0x28 -> word16BE 0x5C28 -- left parenthese:  \(
      0x29 -> word16BE 0x5C29 -- right parenthese: \)
      x    -> word8 x

litStringUtf8 :: String -> PDF
litStringUtf8 s = litString (concatMap encodeUtf8 s)

hexaString :: [Word8] -> PDF
hexaString s = leftAngle <> mconcat (fmap hexa_word8 s) <> rightAngle

-- Name

name :: [Word8] -> PDF
name s = solidus <> mconcat (fmap go s)
  where
    go = \case
      0x00 -> error "NULL character not valid in a PDF object name"
      0x23 -> ascii "#23"
      x | isRegular x -> word8 x
      x    -> word8 0x23 <> hexa_word8 x

nameUtf8 :: String -> PDF
nameUtf8 s = name (concatMap encodeUtf8 s)

-- Array

array :: [PDF] -> PDF
array xs = leftSquare <> mconcat (List.intersperse space xs) <> rightSquare

-- Dictionary

dict :: [(String,PDF)] -> PDF
dict xs = mconcat
  [ leftAngle, leftAngle, eol
  , mconcat (fmap go xs)
  , rightAngle, rightAngle, eol
  ]
  where
    go (key,val) = nameUtf8 key <> space <> val <> eol

makeDict :: [Maybe (String,PDF)] -> PDF
makeDict = dict . catMaybes

-- Stream

stream :: PDF -> [Word8] -> PDF
stream d xs = d <> ascii "stream" <> eol <> mconcat (fmap word8 xs) <> eol <> ascii "endstream" <> eol

streamStorable :: Storable a => PDF -> a -> PDF
streamStorable d xs = d <> ascii "stream" <> eol <> storable xs <> eol <> ascii "endstream" <> eol

-- Null

null :: PDF
null = ascii "null"

-- Indirect object

obj :: Word -> Word16 -> PDF -> PDF
obj i gen contents = int i <> space <> int gen <> space
                           <> ascii "obj" <> eol <> contents
                           <> ascii "endobj" <> eol

ref :: Word -> Word16 -> PDF
ref i gen = int i <> space <> int gen <> space <> ascii "R"

------------------------------------
-- XRef table
------------------------------------

data EntryUse
  = InUse
  | Free

data XRefEntry = XRefEntry
  { xeOffset :: {-# UNPACK #-} !Offset
  , xeGen    :: {-# UNPACK #-} !Word16
  , xeUsed   :: !EntryUse
  }


xrefTable :: Natural -> [XRefEntry] -> PDF
xrefTable first entries = mconcat
  [ ascii "xref", eol, int first, space, int (length entries), eol
  , mconcat (fmap entry entries)
  ]
  where
    showN n a = ascii (replicate (n-length s) '0' ++ s)
      where s = show a

    entry (XRefEntry o g t) = mconcat
      [ showN 10 o, space  -- 10-digit offset
      , showN 5 g, space   -- 5-digit generation number
      , case t of
          InUse -> asciiChar 'n'
          Free  -> asciiChar 'f'
      , space, eol
      ]

xrefNullEntry :: XRefEntry
xrefNullEntry = XRefEntry 0 65535 Free

xrefNullObjRef :: ObjRef
xrefNullObjRef = ObjRef 0 xrefNullEntry

------------------------------------
-- Trailer
------------------------------------

trailer :: Offset -> PDF -> PDF
trailer (Offset xref_offset) trail_dict = mconcat
  [ ascii "trailer", eol
  , trail_dict
  , ascii "startxref", eol
  , int xref_offset, eol
  , ascii "%%EOF"
  ]

------------------------------------
-- Document catalog (root object)
------------------------------------

data PageLayout
  = SinglePage     -- ^ 1 page at a time
  | OneColumn      -- ^ 1 column
  | TwoColumnLeft  -- ^ 2 columns, odd-pages on the left
  | TwoColumnRight -- ^ 2 columns, odd-pages on the right
  | TwoPageLeft    -- ^ 2 pages at a time, odd-pages on the left
  | TwoPageRight   -- ^ 2 pages at a time, odd-pages on the right
  deriving (Show,Eq,Ord)

data PageMode
  = DefaultMode
  | PaneOutlines
  | PaneThumbs
  | PaneOptionalContent
  | PaneAttachments
  | FullScreen
  deriving (Show,Eq,Ord)

data Doc = Doc
  { docVersion            :: !PDF
  , docExtensions         :: !(Maybe PDF)
  , docPages              :: !PDF
  , docPageLabels         :: !(Maybe PDF)
  , docNames              :: !(Maybe PDF)
  , docDests              :: !(Maybe PDF)
  , docViewerPrefs        :: !(Maybe PDF)
  , docPageLayout         :: !(Maybe PageLayout)
  , docPageMode           :: !(Maybe PageMode)
  , docOutlines           :: !(Maybe PDF)
  , docThreads            :: !(Maybe PDF)
  , docOpenAction         :: !(Maybe PDF)
  , docActions            :: !(Maybe PDF)
  , docURI                :: !(Maybe PDF)
  , docAcroForm           :: !(Maybe PDF)
  , docMetaData           :: !(Maybe PDF)
  , docTreeRoot           :: !(Maybe PDF)        -- ^ StructTreeRoot
  , docTags               :: !(Maybe PDF)        -- ^ MarkInfo
  , docLang               :: !(Maybe PDF)
  , docWebCapture         :: !(Maybe PDF)
  , docOutputIntents      :: !(Maybe PDF)
  , docPagePiece          :: !(Maybe PDF)
  , docOptionalContents   :: !(Maybe PDF)
  , docPerms              :: !(Maybe PDF)
  , docLegal              :: !(Maybe PDF)
  , docRequirements       :: !(Maybe PDF)
  , docCollection         :: !(Maybe PDF)
  , docFormsNeedRendering :: !(Maybe Bool)
  }

defaultDoc :: Doc
defaultDoc = Doc
  { docVersion            = nameUtf8 "1.7"
  , docExtensions         = Nothing
  , docPages              = renderRef xrefNullObjRef
  , docPageLabels         = Nothing
  , docNames              = Nothing
  , docDests              = Nothing
  , docViewerPrefs        = Nothing
  , docPageLayout         = Nothing
  , docPageMode           = Nothing
  , docOutlines           = Nothing
  , docThreads            = Nothing
  , docOpenAction         = Nothing
  , docActions            = Nothing
  , docURI                = Nothing
  , docAcroForm           = Nothing
  , docMetaData           = Nothing
  , docTreeRoot           = Nothing
  , docTags               = Nothing
  , docLang               = Nothing
  , docWebCapture         = Nothing
  , docOutputIntents      = Nothing
  , docPagePiece          = Nothing
  , docOptionalContents   = Nothing
  , docPerms              = Nothing
  , docLegal              = Nothing
  , docRequirements       = Nothing
  , docCollection         = Nothing
  , docFormsNeedRendering = Nothing
  }

-- | Mandatory entry
mentry :: String -> PDF -> Maybe (String,PDF)
mentry n c = Just (n,c)

-- | Optional entry
oentry :: String -> Maybe PDF -> Maybe (String,PDF)
oentry n mc = fmap (n,) mc

-- | Optional entry with custom filter
centry :: String -> Maybe a -> (a -> PDF) -> Maybe (String,PDF)
centry n mc f = fmap (\x -> (n,f x)) mc

renderDoc :: Doc -> PDF
renderDoc Doc{..} = makeDict
  [ mentry "Type"              (nameUtf8 "Catalog")
  , mentry "Version"           docVersion
  , mentry "Pages"             docPages
  , oentry "PageLabels"        docPageLabels
  , oentry "Names"             docNames
  , oentry "Dests"             docDests
  , oentry "ViewerPreferences" docViewerPrefs
  , centry "PageLayout"        docPageLayout \case
      SinglePage     -> nameUtf8 "SinglePage"
      OneColumn      -> nameUtf8 "OneColumn"
      TwoColumnLeft  -> nameUtf8 "TwoColumnLeft"
      TwoColumnRight -> nameUtf8 "TwoColumnRight"
      TwoPageLeft    -> nameUtf8 "TwoPageLeft"
      TwoPageRight   -> nameUtf8 "TwoPageRight"
  , centry "PageMode"          docPageMode \case
      DefaultMode         -> nameUtf8 "UseNone"
      PaneOutlines        -> nameUtf8 "UseOutlines"
      PaneThumbs          -> nameUtf8 "UseThumbs"
      PaneOptionalContent -> nameUtf8 "UseOC"
      PaneAttachments     -> nameUtf8 "UseAttachments"
      FullScreen          -> nameUtf8 "FullScreen"
  ]

------------------------------------
-- Page tree
------------------------------------

data PageTree
  = PageTreeNode [PageTree]
  | PageTreeLeaf Page

pageTreeNode :: Maybe Word -> [PDF] -> Word -> PDF
pageTreeNode mparent cs count = makeDict
  [ mentry "Type"   (nameUtf8 "Pages")
  , centry "Parent" mparent (\x -> ref x 0)
  , mentry "Kids"   (array cs)
  , mentry "Count"  (int count)
  ]

------------------------------------
-- Page
------------------------------------

data Inheritable a
  = Inherited
  | NotInherited a
  deriving (Show,Eq,Ord,Functor)

type Rectangle = PDF

data Rotate
  = Rotate0
  | Rotate90
  | Rotate180
  | Rotate270
  deriving (Show,Eq,Ord)

data Page = Page
  { pageTreeParent     :: PDF
  , pageLastModified   :: Maybe PDF
  , pageResources      :: Maybe PDF
  , pageMediaBox       :: Inheritable Rectangle
  , pageCropBox        :: Inheritable Rectangle
  , pageBleedBox       :: Maybe Rectangle
  , pageTrimBox        :: Maybe Rectangle
  , pageArtBox         :: Maybe Rectangle
  , pageBoxColorInfo   :: Maybe PDF
  , pageContents       :: Maybe PDF
  , pageRotate         :: Inheritable Rotate
  , pageGroup          :: Maybe PDF
  , pageThumb          :: Maybe PDF
  , pageArticleBreads  :: Maybe PDF
  , pageDuration       :: Maybe Word            -- ^ Duration in seconds
  , pageTransition     :: Maybe PDF
  , pageAnnotations    :: Maybe PDF
  , pageActions        :: Maybe PDF
  , pageMetaData       :: Maybe PDF
  , pagePieceInfo      :: Maybe PDF
  , pageStructParents  :: Maybe PDF
  , pageWebCaptureID   :: Maybe PDF
  , pagePreferredZoom  :: Maybe PDF
  , pageSeparationInfo :: Maybe PDF
  , pageTabs           :: Maybe PDF
  , pageTemplate       :: Maybe PDF
  , pageNavigation     :: Maybe PDF             -- ^ PresSteps
  , pageUserUnit       :: Maybe PDF
  , pageViewPorts      :: Maybe PDF
  }

defaultPage :: Page
defaultPage = Page
  { pageTreeParent     = renderRef xrefNullObjRef
  , pageLastModified   = Nothing
  , pageResources      = Nothing
  , pageMediaBox       = Inherited
  , pageCropBox        = Inherited
  , pageBleedBox       = Nothing
  , pageTrimBox        = Nothing
  , pageArtBox         = Nothing
  , pageBoxColorInfo   = Nothing
  , pageContents       = Nothing
  , pageRotate         = Inherited
  , pageGroup          = Nothing
  , pageThumb          = Nothing
  , pageArticleBreads  = Nothing
  , pageDuration       = Nothing
  , pageTransition     = Nothing
  , pageAnnotations    = Nothing
  , pageActions        = Nothing
  , pageMetaData       = Nothing
  , pagePieceInfo      = Nothing
  , pageStructParents  = Nothing
  , pageWebCaptureID   = Nothing
  , pagePreferredZoom  = Nothing
  , pageSeparationInfo = Nothing
  , pageTabs           = Nothing
  , pageTemplate       = Nothing
  , pageNavigation     = Nothing
  , pageUserUnit       = Nothing
  , pageViewPorts      = Nothing
  }

-- | Inherited (optional) entry
ientry :: String -> Inheritable PDF -> Maybe (String,PDF)
ientry _ Inherited        = Nothing
ientry n (NotInherited c) = Just (n,c)

renderPage :: Page -> PDF
renderPage Page{..} = makeDict
  [ mentry "Type"           (nameUtf8 "Page")
  , mentry "Parent"         pageTreeParent
  , oentry "LastModified"   pageLastModified
  , oentry "Resources"      pageResources
  , ientry "MediaBox"       pageMediaBox
  , ientry "CropBox"        pageCropBox
  , oentry "BleedBox"       pageBleedBox
  , oentry "TrimBox"        pageTrimBox
  , oentry "ArtBox"         pageArtBox
  , oentry "BoxColorInfo"   pageBoxColorInfo
  , oentry "Contents"       pageContents
  , ientry "Rotate"         (pageRotate ||> \case
                              Rotate0   -> int (0   :: Int)
                              Rotate90  -> int (90  :: Int)
                              Rotate180 -> int (180 :: Int)
                              Rotate270 -> int (270 :: Int))
  , oentry "Group"          pageGroup
  , oentry "Thumb"          pageThumb
  , oentry "B"              pageArticleBreads
  , centry "Dur"            pageDuration int
  , oentry "Trans"          pageTransition
  , oentry "Annots"         pageAnnotations
  , oentry "AA"             pageActions
  , oentry "MetaData"       pageMetaData
  , oentry "PieceInfo"      pagePieceInfo
  , oentry "StructParents"  pageStructParents
  , oentry "ID"             pageWebCaptureID
  , oentry "PZ"             pagePreferredZoom
  , oentry "SeparationInfo" pageSeparationInfo
  , oentry "Tabs"           pageTabs
  , oentry "TemplateInstantiated" pageTemplate
  , oentry "PresSteps"      pageNavigation
  , oentry "UserUnit"       pageUserUnit
  , oentry "VP"             pageViewPorts
  ]



------------------------------------
-- Monad
------------------------------------

data ObjRef = ObjRef
  { objNum    :: Word   -- ^ Object number
  , objEntry  :: XRefEntry
  }

data PdfState = PdfState
  { st_pdf      :: !PDF
  , st_obj_num  :: {-# UNPACK #-} !Word
  , st_obj_refs :: [ObjRef] -- ^ Objects (reverse order)
  }

emptyPdfState :: PdfState
emptyPdfState = PdfState
  { st_pdf      = mempty
  , st_obj_num  = 0
  , st_obj_refs = [xrefNullObjRef]
  }

newtype PdfM a
  = PdfM' (PdfState -> (PdfState,a))

{-# COMPLETE PdfM #-}
pattern PdfM :: (PdfState -> (PdfState, a)) -> PdfM a
pattern PdfM a <- PdfM' a
  where
    PdfM a = PdfM' (oneShot a)

unPdfM :: PdfM a -> (PdfState -> (PdfState,a))
unPdfM (PdfM a) = a

instance Functor PdfM where
  fmap f (PdfM g) = PdfM \p0 -> case g p0 of
    (p1,a) -> (p1, f a)

instance Applicative PdfM where
  pure a = PdfM \p -> (p, a)
  PdfM f <*> PdfM g = PdfM \p0 -> case f p0 of
    (p1, fa) -> case g p1 of
      (p2, a) -> (p2, fa a)

instance Monad PdfM where
  PdfM f >>= g = PdfM \p0 -> case f p0 of
    (p1, a) -> unPdfM (g a) p1

getOffset :: PdfM Offset
getOffset = PdfM \s -> (s, Offset (fromIntegral (builderLength (st_pdf s))))

getNextObjNum :: PdfM Word
getNextObjNum = PdfM \s ->
  let
     n  = st_obj_num s + 1
     s' = s { st_obj_num = n }
  in (s', n)

runPdfM :: PdfM a -> (PdfState, a)
runPdfM p = unPdfM p emptyPdfState

runPdf :: PdfM a -> PDF
runPdf p = st_pdf (fst (runPdfM p))

getState :: PdfM PdfState
getState = PdfM \s -> (s, s)

modifyState :: (PdfState -> PdfState) -> PdfM ()
modifyState f = PdfM \p -> (f p, ())

modifyPdf :: (PDF -> PDF) -> PdfM ()
modifyPdf f = modifyState \s -> s { st_pdf = f (st_pdf s) }

append :: PDF -> PdfM Offset
append p = do
  o <- getOffset
  modifyPdf (<> p)
  return o

append_ :: PDF -> PdfM ()
append_ p = modifyPdf (<> p)

-- | Add an object with generation 0
addObject :: PDF -> PdfM ObjRef
addObject contents = do
  i <- getNextObjNum
  addObject' i 0 contents

-- | Add an object
addObject' :: Word -> Word16 -> PDF -> PdfM ObjRef
addObject' i gen contents = do
  offset <- append (obj i gen contents)
  let obj_ref = ObjRef
        { objNum   = i
        , objEntry = XRefEntry
            { xeGen    = gen
            , xeUsed   = InUse
            , xeOffset = offset
            }
        }
  modifyState \s -> s { st_obj_refs = obj_ref : st_obj_refs s }
  return obj_ref

genXRefTable :: PdfM Offset
genXRefTable = do
  refs <- st_obj_refs <$> getState
  -- FIXME: we don't handle subsections (holes in the object list) and we assume
  -- that we start from the NULL entry
  append $ xrefTable 0 (reverse (fmap objEntry refs))

renderRef :: ObjRef -> PDF
renderRef r = ref (objNum r) (xeGen (objEntry r))

genTrailer :: Offset -> ObjRef -> PdfM ()
genTrailer xref_offset root_ref = do
  n <- st_obj_num <$> getState
  append_ $ trailer xref_offset $ makeDict
    [ mentry "Size" (int (n+1))
    , mentry "Root" (renderRef root_ref)
    -- , (nameUtf8 "Prev", ...)
    -- , (nameUtf8 "Encrypt", ...)
    -- , (nameUtf8 "Info", ...)
    -- , (nameUtf8 "ID", ...)
    ]

renderPageTree :: PageTree -> PdfM PDF
renderPageTree tree = fst <$> go_first tree
  where
    go_first = \case
      PageTreeLeaf _  -> error "Can't render a single page that is not in a tree"
      PageTreeNode cs -> do
        cid <- getNextObjNum
        (as,ns) <- unzip <$> forM cs (go cid)
        let count = sum ns
        r <- addObject' cid 0 (pageTreeNode Nothing as count)
        pure (renderRef r, count)

    go :: Word -> PageTree -> PdfM (PDF,Word)
    go parent = \case
      PageTreeLeaf p  -> do
        page_ref <- addObject (renderPage p)
        pure (renderRef page_ref,1)
      PageTreeNode cs -> do
        cid <- getNextObjNum
        (as,ns) <- unzip <$> forM cs (go cid)
        let count = sum ns
        r <- addObject' cid 0 (pageTreeNode (Just parent) as count)
        pure (renderRef r, count)



-- TODO: object streams
-- TODO: xref streams
-- TODO: encryption

------------------------------------
-- Example
------------------------------------

toAscii :: String -> [Word8]
toAscii = fmap (fromIntegral . ord)

example :: IO ()
example = do
  let hdr = ascii "%PDF-1.7" <> eol
            <> asciiChar '%'
            <> word32BE 0xd0d4c5d8 -- binary values
            <> eol
  renderPDF "test.pdf" $ runPdf do
    append_ $ mconcat
      [ hdr
      , litString (toAscii "this is a test")
      , hexaString (toAscii "this is a test")
      , name (toAscii "il était une fois 133###") <> eol
      , nameUtf8 "il était une fois 133###"
      , array
          [ nameUtf8 "il était une fois 133###"
          , int (18 :: Int)
          , float (1.2 :: Double)
          ]
      , dict [ ("Type", nameUtf8 "Example")
             , ("SubType", nameUtf8 "DictionaryExample")
             , ("Version", float (0.01 :: Float))
             , ("IntegerItem", int (12 :: Int))
             , ("StringItem", litStringUtf8 "a string")
             , ("Subdictionary", dict
                  [ ("Item1", float (0.4 :: Double))
                  , ("Item2", bTrue)
                  ])
             ]
      , obj 7 0 $ dict [("Length", ref 8 0)]
      , obj 8 0 (int (77 :: Int))
      ]
    _obj_ref <- addObject $ dict
      [ ("Some", nameUtf8 "Custom")
      , ("Object", int (42 :: Int))
      ]
    _obj_ref2 <- addObject $ dict
      [ ("Some", nameUtf8 "Other")
      , ("Object", int (33 :: Int))
      ]
    page_tree <- renderPageTree $ PageTreeNode
      [PageTreeLeaf defaultPage, PageTreeLeaf defaultPage]
    let doc = defaultDoc
                { docPages = page_tree
                }
    root_ref <- addObject (renderDoc doc)
    xref_table <- genXRefTable
    genTrailer xref_table root_ref
