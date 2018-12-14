
hangman :: IO()
hangman = do {xs <- readFile "/home/wentan/SUcode/cis623/ENV623/chapter10/words.txt";
              play (words xs)}

play (w:ws) = do {putStrLn "I am thinking of a word:";
      putStrLn (replicate (length w) '-');
      putStrLn "Try and guess it."
      guess w ws}


guess w ws= do {
    putStr "guess : ";
    w' <- getLine;
    if length w' /= length w
    then do {
        putStrLn "Wrong number of letter!";
        guess w ws
    }
    else if w' == w
        then do {
            putStrLn "You got it!";
            putStrLn "Play again? (yes or no)";
            ans <- getLine;
            if ans == "yes"
            then play ws
            else putStrLn "Bye!"
        }
    else do {
        putStrLn match w' w;
        guess w ws
    }
}

match w' w = map check w
             where check x = if x `elem` w then x else '-' 


main = do
    hangman
