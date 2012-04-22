{- Challenge #1 Easy
 - Create a program that will ask the users name, age, and reddit username.
 - Have it tell them the information back, in the format:
 -
 - Your name is ____, you are ____ years old, and your username is ____.
 -
 - For extra credit, have the program log this information in a file to be
 - accessed later.
 -}

main = do
    putStrLn "Name: "
    name <- getLine
    putStrLn "Age: "
    age <- getLine
    putStrLn "Reddit Username: "
    reddit <- getLine

    -- Need to refactor data to one single string.
    print (concat ["Your name is ", name, ", you are ", age, " years old, and your username is ", reddit, "."])
    
    writeFile "challenge1_e.txt" (concat ["Your name is ", name, ", you are ", age, " years old, and your username is ", reddit, "."])
    
