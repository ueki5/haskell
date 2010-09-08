module Ch14.Carrier where
import qualified Data.Map as M
type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bubs_Phone_NetWork
                   | Morrisas_Narvelous_Mobiles
                   | Petes_Plutocratic_Phones
                     deriving (Eq, Ord, Show)
findCarrierBillingAddress :: PersonName
                             -> M.Map PersonName PhoneNumber
                             -> M.Map PhoneNumber MobileCarrier
                             -> M.Map MobileCarrier BillingAddress
                             -> Maybe BillingAddress
-- findCarrierBillingAddress = undefined
findCarrierBillingAddress person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap 
  carrier <- M.lookup number carrierMap
  address <-M.lookup carrier addressMap
  return address
map1 = M.singleton ("ueki1"::PersonName) ("090-9999-0001"::PhoneNumber)
map2 = M.singleton ("090-9999-0001"::PhoneNumber) (Morrisas_Narvelous_Mobiles)
map3 = M.singleton (Morrisas_Narvelous_Mobiles) ("090-9999-0003"::BillingAddress)
