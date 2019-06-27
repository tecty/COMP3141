import Prelude hiding (Word)
import Test.QuickCheck 
import Test.QuickCheck.Modifiers
-- import qualified Prelude as P 

-- lookuup :: [(a, b)] -> a -> maybe b
type Word = String
type Definition = String

sorted:: (Ord a ) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x: y: xs) =  x <= y && sorted(y : xs)


data DictEntry = Entry {
        word :: Word, 
        defn :: Definition
    } deriving (Eq, Show)


newtype Dict = D [DictEntry]
    deriving (Show, Eq)


wellformed :: Dict -> Bool
wellformed (D es) = True

prop_insert_wf dict w defn 
    = wellformed dict ==> wellformed (insertWord w defn dict) 
    

emptyDict :: Dict 
emptyDict = D []

insertWord :: Word -> Definition -> Dict -> Dict
insertWord w d (D es) = let entry  = Entry w d 
    in D (entry: es)

lookUp :: Word -> Dict -> Maybe Definition
lookUp  w (D  es) = search w es 
    where 
        search w [] = Nothing
        search w (e:es) = case compare w (word e) of 
            LT -> Nothing
            EQ -> Just (defn e)
            GT -> search w es

    -- P.lookup w (map (\(Entry w d) -> (w,d)) es)


instance Arbitrary DictEntry where
    arbitrary = Entry <$> arbitrary <*> arbitrary

instance Arbitrary Dict where 
    arbitrary = do 
        Ordered ds <- arbitrary 
        pure (D ds)
