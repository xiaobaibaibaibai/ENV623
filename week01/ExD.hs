import Data.Char (toLower)
import Prelude hiding (Word,span)
import qualified Prelude as P

-- words :: [Char] -> [[Char]]
-- "Thelma and Louise" -> ["Thelma", "and", "Louise"]

type Word = [Char]

apple :: Word
apple = ['A', 'p', 'P', 'L', 'e']

main = do
    print (map toLower apple)
    print (map toLower "Hello WorLD")
    print (toLower 'A')