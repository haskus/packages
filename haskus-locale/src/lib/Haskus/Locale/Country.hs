{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | Country codes
module Haskus.Locale.Country
   ( Country(..)
   , countries
   )
where

import Data.Csv
import Data.Text
import Data.Vector (Vector)
import GHC.Generics
import qualified Data.ByteString.Lazy as LBS
import Haskus.Utils.Embed.ByteString

-- | Raw country data taken from https://github.com/datasets/country-codes/
-- (see https://datahub.io/core/country-codes)
countryData :: LBS.ByteString
countryData = LBS.fromStrict $(embedBSFile "src/data/country-codes.csv")

-- | Country data
countries :: Vector Country
countries = do
   case decode HasHeader countryData :: Either String (Vector Country) of
      Left err -> error ("Invalid country data: " ++ show err)
      Right vs -> vs

-- | Country information
data Country = Country
   { countryOfficialNameArabic         :: !Text -- ^ Country or Area official Arabic short name from UN Statistics Division
   , countryOfficialNameChinese        :: !Text -- ^ Country or Area official Chinese short name from UN Statistics Division
   , countryOfficialNameEnglish        :: !Text -- ^ Country or Area official English short name from UN Statistics Division
   , countryOfficialNameSpanish        :: !Text -- ^ Country or Area official Spanish short name from UN Statistics Division
   , countryOfficialNameFrench         :: !Text -- ^ Country or Area official French short name from UN Statistics Division
   , countryOfficialNameRussian        :: !Text -- ^ Country or Area official Russian short name from UN Statistics Division
   , countryISO3166CodeAlpha2          :: !Text -- ^ Alpha-2 codes from ISO 3166-1
   , countryISO3166CodeAlpha3          :: !Text -- ^ Alpha-3 codes from ISO 3166-1 (synonymous with World Bank Codes)
   , countryISO3166CodeNumeric         :: !Word -- ^ Numeric codes from ISO 3166-1
   , countryISO4217CurrencyAlphaCode   :: !Text -- ^ ISO 4217 currency alphabetic code
   , countryISO4217CurrencyCountryName :: !Text -- ^ ISO 4217 country name
   , countryISO4217CurrencyMinorUnit   :: !Text -- ^ ISO 4217 currency number of minor units
   , countryISO4217CurrencyName        :: !Text -- ^ ISO 4217 currency name
   , countryISO4217CurrencyNumericCode :: !Text -- ^ ISO 4217 currency numeric code
   , countryM49                        :: !Text -- ^ UN Statistics M49 numeric codes (nearly synonymous with ISO 3166-1 numeric codes,
                                                --   which are based on UN M49. ISO 3166-1 does not include Channel Islands or Sark, for example)

   , countryUntermArabicFormal         :: !Text -- ^ Country's formal Arabic name from UN Protocol and Liaison Service
   , countryUntermArabicShort          :: !Text -- ^ Country's short Arabic name from UN Protocol and Liaison Service
   , countryUntermChineseFormal        :: !Text -- ^ Country's formal Chinese name from UN Protocol and Liaison Service
   , countryUntermChineseShort         :: !Text -- ^ Country's short Chinese name from UN Protocol and Liaison Service
   , countryUntermEnglishFormal        :: !Text -- ^ Country's formal English name from UN Protocol and Liaison Service
   , countryUntermEnglishShort         :: !Text -- ^ Country's short English name from UN Protocol and Liaison Service
   , countryUntermFrenchFormal         :: !Text -- ^ Country's formal French name from UN Protocol and Liaison Service
   , countryUntermFrenchShort          :: !Text -- ^ Country's short French name from UN Protocol and Liaison Service
   , countryUntermRussianFormal        :: !Text -- ^ Country's formal Russian name from UN Protocol and Liaison Service
   , countryUntermRussianShort         :: !Text -- ^ Country's short Russian name from UN Protocol and Liaison Service
   , countryUntermSpanishFormal        :: !Text -- ^ Country's formal Spanish name from UN Protocol and Liaison Service
   , countryUntermSpanishShort         :: !Text -- ^ Country's short Spanish name from UN Protocol and Liaison Service

   , countryCLDR                       :: !Text -- ^ Country's customary English short name (CLDR)
   , countryCapital                    :: !Text -- ^ Capital city from Geonames
   , countryContinent                  :: !Text -- ^ Continent from Geonames
   , countryVehicleDS                  :: !Text -- ^ Distinguishing signs of vehicles in international traffic
   , countryDevelopment                :: !Text -- ^ Country classification from United Nations Statistics Division
   , countryDial                       :: !Text -- ^ Country code from ITU-T recommendation E.164, sometimes followed by area code
   , countryEDGAR                      :: !Text -- ^ EDGAR country code from SEC
   , countryFIFA                       :: !Text -- ^ Codes assigned by the Fédération Internationale de Football Association
   , countryFIPS                       :: !Text -- ^ Codes from the U.S. standard FIPS PUB 10-4
   , countryGAUL                       :: !Text -- ^ Global Administrative Unit Layers from the Food and Agriculture Organization
   , countryGeonameID                  :: !Text -- ^ Geoname ID
   , countryGlobalCode                 :: !Text -- ^ Country classification from United Nations Statistics Division
   , countryGlobalName                 :: !Text -- ^ Country classification from United Nations Statistics Division
   , countryOlympics                   :: !Text -- ^ Codes assigned by the International Olympics Committee (IOC)
   , countryITU                        :: !Text -- ^ Codes assigned by the International Telecommunications Union
   , countryIntermediateRegionCode     :: !Text -- ^ Country classification from United Nations Statistics Division
   , countryIntermediateRegionName     :: !Text -- ^ Country classification from United Nations Statistics Division
   , countryLLDC                       :: !Text -- ^ Land locked developing countries. Country classification from United Nations Statistics Division
   , countryLanguages                  :: !Text -- ^ Languages from Geonames
   , countryLeastDeveloped             :: !Text -- ^ Country classification from United Nations Statistics Division
   , countryMARC                       :: !Text -- ^ MAchine-Readable Cataloging codes from the Library of Congress
   , countryRegionCode                 :: !Text -- ^ Country classification from United Nations Statistics Division
   , countryRegionName                 :: !Text -- ^ Country classification from United Nations Statistics Division
   , countrySIDS                       :: !Text -- ^ Small Island Developing States. Country classification from United Nations Statistics Division
   , countrySubRegionCode              :: !Text -- ^ Country classification from United Nations Statistics Division
   , countrySubRegionName              :: !Text -- ^ Country classification from United Nations Statistics Division
   , countryTLD                        :: !Text -- ^ Top level domain from Geonames
   , countryWMO                        :: !Text -- ^ Country abbreviations by the World Meteorological Organization
   , countryCIAIsIndependent           :: !Text -- ^ Country status, based on the CIA World Factbook
   }
   deriving (Generic,Show)

instance ToRecord   Country
instance FromRecord Country
