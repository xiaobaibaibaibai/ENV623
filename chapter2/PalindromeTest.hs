import Data.Char (toLower,isAlpha)
palindrome :: IO()
palindrome
= do {putStrLn "Enter a string:";
xs <- getLine;
if isPalindrome xs then putStrLn "Yes!"
else putStrLn "No!"}
isPalindrome :: String -> Bool
isPalindrome xs = (ys == reverse ys)
where ys = map toLower (filter isAlpha xs)