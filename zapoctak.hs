import Data.Char ( isDigit )
import System.IO ()
import Data.List ()

data Player = Player | Ai deriving (Show, Eq)

-- kontrola jestli je suma mensi nez jedna a tedy zda hra skoncila
end :: Int -> Bool 
end sum = sum < 1

-- dalsi hrac
next :: Player -> Player
next Player = Ai
next Ai = Player

-- nacte cislo od uzivatele, pokud cislo neni v poradku, ohlasi chybu a zkusi nacist znovu
userPick :: IO Int
userPick = do 
        num <- getLine
        if num /= [] && all isDigit num then
                return (read num)
        else do 
                putStrLn "Invalid input! Pick a number!"
                userPick 

-- zkontroluje, ze vybrane cislo je uvnitr seznamu moznych odberu
validMove :: [Int] -> Int -> Bool 
validMove odbery odber = elem odber odbery

-- vezme seznam moznych odberu, odber a sumu kaminku, zkontroluje zda je odber validni a vrati novou sumu v seznamu, coz je dulezite kvuli vytvareni herniho stromu
move :: [Int] -> Int -> Int -> [Int] 
move odbery odber sum = if validMove odbery odber then [sum-odber] else []

-- struktura pro vytvoreni stromu hry - v kazdem vrcholu mam seznam moznych odberu, hrace, ktery je na tahu a sumu kaminku, ze ktere odebira
data Tree a = Node [a] Player a [Tree a] deriving Show

-- viz bestPick
winners :: [Tree Int] -> [Player]
winners [] = []
winners ((Node _ p _ _):xs) = [p] ++ winners xs

winners' :: [Tree Int] -> Int
winners' ((Node odbery p sum _):[]) = sum
winners' ((Node _ p sum _):xs) = if p == Ai then sum else winners' xs

-- vezme seznam moznych odberu, sumu a hrace, ktery je na tahu a vytvori strom hry - vsechny moznosti, jak hra bude probihat
wholeGame :: [Int] -> Int -> Player -> Tree Int
wholeGame odbery sum p = Node odbery p sum [wholeGame odbery newSum (next p) | newSum <- possiblePicks odbery sum p]

-- vrati seznam vsech moznych novych sum po odebrani povoleneho poctu
possiblePicks :: [Int] -> Int -> Player -> [Int]
possiblePicks odbery sum p | end sum = []
                           | otherwise = concat [move odbery i sum | i <- odbery]

-- ze stromu hry vytvori minimaxovy strom
-- rozdil bude v tom, ze Player v jednotlivych vrcholech nerika, ktery hrac je na tahu, ale ktery hrac v dane sitauci vyhraje, pokud se bude 
-- drzet minimaxove strategie
-- minimaxovy hry se stavi od spodu - na posledni hladine se ve vrcholu kdo je na tahu a kdo tedy vyhral - protoze uz nema co odebirat
-- z toho pak vytvorim node nad tim a tak rekurzivne stavim 
minimax :: Tree Int -> Tree Int 
minimax (Node odbery p suma []) = Node odbery p suma []
minimax (Node odbery Ai suma tahy) = Node odbery winner suma tahy1 
                                where 
                                        tahy1 = [minimax (tahy !! i) | i <- [0..(length tahy) -1]]
                                        winner = if elem Ai (winners tahy1) then Ai else Player


minimax (Node odbery Player suma tahy) = Node odbery winner suma tahy1 
                                where 
                                        tahy1 = [minimax (tahy !! i) | i <- [0..(length tahy) -1]]
                                        winner = if elem Player (winners tahy1) then Player else Ai       

-- funkce dostane minimaxovy strom, podiva se na nasledniky a pokud tam je AI, tedy ze AI v danem bode vyhrava, tak si vybere tuto cestu
-- v opacnem pripade vybere prvni prvek ze seznamu a bude doufat, ze protihrac udela chybu
-- to, zda nejaky z nasledujicich vrcholu stromu ma hodnotu Ai implementuje funkce winners, ktera vraci seznam [Player] a me tedy zajima
-- zda se ve vysledku teto funkce objevi hodnota AI
-- funkce winners' mi pak zajisti, ze si vyberu takovou cestu vyberu
--  implementuje to tak, ze mi vrati sumu, na kterou se chci dostat, tudiz odebrane cislo ziskam tak, ze proste odectu starou sumu a tuto novou sumu,
-- na kterou se chci dostat
bestPick :: Tree Int -> Int 
bestPick (Node odbery _ suma tahy) = if elem Ai (winners tahy) then (suma - winners' tahy)
                                     else odbery !! 0
-- nasledne mame funkci play, ktera proste vypisuje info o hre, nacita hracuv vyber a vola funkci turnAi s novou sumou
play :: Int -> [Int] -> IO ()
play sum odbery = do 
                    putStrLn ("Your turn!\nPossible picks are: " ++ (show odbery) ++ "\nNumber of stones remaining: " ++ (show sum) ++ "\nNumber of stones to remove:")
                    intpicked <- userPick
                    if validMove odbery intpicked 
                        then do
                            putStrLn ("Pick ok!")
                            if (sum - intpicked > 0) 
                                then turnAi (sum - intpicked) odbery 
                                else putStrLn "You lost!"
                    else do 
                            putStrLn "Pick not ok, pick again!"
                            play sum odbery

-- nejdulezitejsi je vybrani hodnoty – postavim strom hry, z toho minimaxovy strom, z toho vyberu nejlepsi hodnotu pomoci popsanych funkci,
-- zkontroluji konec a necham hrat hrace
turnAi :: Int -> [Int] -> IO ()
turnAi sum odbery = do
                    putStrLn ("\n\nAI is thinking...")
                    let intpicked = bestPick (minimax (wholeGame odbery sum Ai))
                    putStrLn ("Ai removed " ++ (show intpicked) ++ " stones.\n\n")
                    if (sum - intpicked > 0) 
                        then play (sum - intpicked) odbery 
                        else putStrLn "You won!"
