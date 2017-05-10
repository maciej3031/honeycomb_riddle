-- ALGORYTM ROZWIĄZUJĄCY ZAGADKI TYPU PLASTER MIODU: http://www.lamiglowkimix.pl/modules/honeycomb/honeycomb.php
-- OPIS ALGORYTMU DZIAŁANIA
-- 0. Dane wejściowe w postaci pliku .txt z przykładową zawartością: Plaster ['BD..', '.GA.D', '.FEG', 'ABDCF', 'E..B']
-- 1. Sparsuj dane wejściowe do postaci listy list
-- 2. Sprawdź czy format danych jest poprawny
-- 3. Sprawdź czy konfiguracja początkowa liter jest poprawna
-- 4. Rozpocznij od końca ostatniego wiersza wstawiając kolejno litery 'ABCDEFG'
-- 5. Wstawiaj litery i sprawdzaj czy wstawiona litera wraz z jej otoczeniem n+1 i n+2 są poprawne, tzn. litery w
--    swoim otoczeniu nie powtarzają się oraz czy aktualna kombinacja nie znajduje się na liście błędnych kombinacji:
--      5.1. Jeżeli wszystko jest OK: to postępuj tak dalej iterując po kolejnych polach w więrszu od tyłu,
--      5.2. Jeżeli NIE: to próbuj każdą następną literę w kolejności 'ABCDEFG'
--      5.3. Jeżeli ŻADNA z liter nie pasuje wówczas zapisz aktualną kombinację do tego momentu jako błędną, zamień
--           ostatnio wstawioną literę na '.' i cofnij się do poprzedniego pola powtarzając krok 5 od początku
-- 6. Po wypełnieniu całej tablicy zwróć rozwiązanie zagadki w postaci listy list


import Data.Char

-- Zamiana wszystkich znaków w Stringu na duże
upperCase :: [Char] -> [Char]
upperCase s = map toUpper s


-- Zamiana wszystkich znaków w Stringu na małe
lowerCase :: [Char] -> [Char]
lowerCase s = map toLower s


-- Z wejściowego Stringa tworzy listę Stringów, gdzie każdy element to jeden wiersz w zagadce
getRawData :: [Char] -> [[Char]]
getRawData text = [drop 1 (init (init x)) | x <- linesArray]  where
        linesArray = words (drop 9 text)


-- Pobiera element z listy na danym miejscu, początek numerowania od 1
takeAt :: [t] -> Int -> [t]
takeAt [] _ = []
takeAt (x:xs) k | k == 1 = [x]
                | otherwise = takeAt xs (k-1)


-- Pobiera element z listy list w danym wierszu i kolumnie, zwraca listę z tym elementem, początek numerowania od 1
takeAt2 :: [[Char]] -> Int -> Int -> [Char]
takeAt2 [] _ _ = []
takeAt2 [[]] _ _ = []
takeAt2 ((x:xs):ys) row col | row == 1 && col == 1 = [x]
                            | row == 1 = takeAt2 [xs] row (col-1)
                            | otherwise = takeAt2 ys (row-1) col


-- Wstawia element a do listy w danym miejscu, zwraca tą listę, początek numerowania od 1,
insertAt :: [Char] -> Int -> Char -> [Char]
insertAt [] _ a = error "Za duze n "
insertAt (x:xs) n a | n == 1 = [a] ++ [x] ++ xs
                    | otherwise = x : insertAt xs (n-1) a


-- Wstawia element a do listy list w danym wierszu i kolumnie, zwraca tą listę list, początek numerowania od 1
insertAt2 :: [[Char]] -> Int -> Int -> Char -> [[Char]]
insertAt2 [] _ _ a = error "Za duze n "
insertAt2 [[]] _ _ a = error "Za duze n "
insertAt2 ((x:xs):ys) row col a | row == 1 && col == 1 = (a : x : xs) : ys
                                | row == 1 = (x : (insertAt xs (col-1) a)) : ys
                                | otherwise = (x:xs) : (insertAt2 ys (row-1) col a)


-- Usuwa element z listy w danym miejscu, zwraca tą listę, początek numerowania od 1
removeAt :: [Char] -> Int -> [Char]
removeAt [] n = []
removeAt (x:xs) n | n == 1 = removeAt xs (n-1)
                  | otherwise = [x] ++ removeAt xs (n-1)


-- Usuwa element z listy list w danym wierszu i kolumnie, zwraca tą listę list, początek numerowania od 1
removeAt2 :: [[Char]] -> Int -> Int -> [[Char]]
removeAt2 [] _ _ = error "Za duze n "
removeAt2 [[]] _ _ = error "Za duze n "
removeAt2 ((x:xs):ys) row col | row == 1 && col == 1 = xs : ys
                              | row == 1 = (x : (removeAt xs (col-1))) : ys
                              | otherwise = (x:xs) : (removeAt2 ys (row-1) col)


-- Zamienia element w liście list w danym wierszu i kolumnie na element a, zwraca tą listę list, początek numerowania od 1
replaceAt2 :: [[Char]] -> Int -> Int -> Char -> [[Char]]
replaceAt2 rawList row col a = removeAt2 tempList row (col + 1) where
                                tempList = insertAt2 rawList row col a


-- Zwraca długość wiersza row w podanej liście list rawList
getRowLen :: [[Char]] -> Int -> Int
getRowLen rawList row = length (head (takeAt rawList row))


-- Sprawdza poprawność wprowadzonych danych, tzn czy każdy następny wiersz różni się dlugością od poprzedniego o 1,
-- oraz czy pierwszy i ostatni wiersz są krótsze, zwraca prawdę lub fałsz
checkData :: [[Char]] -> Int -> Bool
checkData rawList 1 = True
checkData rawList 2 = True
checkData rawList totLen =
     if len1 == getRowLen rawList totLen && len2 == getRowLen rawList (totLen - 1) && len1 /= len2 then
        checkData rawList (totLen - 2)
     else
        False
    where
        len1 = getRowLen rawList 1
        len2 = getRowLen rawList 2


-- Usuwa wszystkie wystąpienia elementu w liście
deleteAllInstances :: Eq a => a -> [a] -> [a]
deleteAllInstances a xs = filter (/= a) xs


-- Zwraca listę elementów w sąsiedztwie znaku w wierszu row i kolumnie col, dla wiersza dłuższego
getNearbiesLongLine :: [[Char]] -> Int -> Int -> [Char]
getNearbiesLongLine rawList row col = upperCase (deleteAllInstances '.' (e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6 ++ e7 ++ e8 ++ e9 ++ e10 ++ e11
                                                    ++ e12 ++ e13 ++ e14 ++ e15 ++ e16 ++ e17 ++ e18))
        where
            e1 = takeAt2 rawList row (col + 1)
            e2 = takeAt2 rawList row (col + 2)
            e3 = takeAt2 rawList row (col - 1)
            e4 = takeAt2 rawList row (col - 2)
            e5 = takeAt2 rawList (row + 1) (col)
            e6 = takeAt2 rawList (row + 1) (col + 1)
            e7 = takeAt2 rawList (row + 1) (col - 1)
            e8 = takeAt2 rawList (row + 1) (col - 2)
            e9 = takeAt2 rawList (row + 2) (col)
            e10 = takeAt2 rawList (row + 2) (col - 1)
            e11 = takeAt2 rawList (row + 2) (col + 1)
            e12 = takeAt2 rawList (row - 1) (col)
            e13 = takeAt2 rawList (row - 1) (col + 1)
            e14 = takeAt2 rawList (row - 1) (col - 1)
            e15 = takeAt2 rawList (row - 1) (col - 2)
            e16 = takeAt2 rawList (row - 2) (col)
            e17 = takeAt2 rawList (row - 2) (col - 1)
            e18 = takeAt2 rawList (row - 2) (col + 1)


-- Zwraca listę elementów w sąsiedztwie znaku w wierszu row i kolumnie col, dla wiersza krótszego
getNearbiesShortLine :: [[Char]] -> Int -> Int -> [Char]
getNearbiesShortLine rawList row col = upperCase (deleteAllInstances '.' (e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6 ++ e7 ++ e8 ++ e9 ++ e10 ++ e11
                                                     ++ e12 ++ e13 ++ e14 ++ e15 ++ e16 ++ e17 ++ e18))
        where
            e1 = takeAt2 rawList row (col + 1)
            e2 = takeAt2 rawList row (col + 2)
            e3 = takeAt2 rawList row (col - 1)
            e4 = takeAt2 rawList row (col - 2)
            e5 = takeAt2 rawList (row + 1) (col)
            e6 = takeAt2 rawList (row + 1) (col + 1)
            e7 = takeAt2 rawList (row + 1) (col - 1)
            e8 = takeAt2 rawList (row + 1) (col + 2)
            e9 = takeAt2 rawList (row + 2) (col)
            e10 = takeAt2 rawList (row + 2) (col - 1)
            e11 = takeAt2 rawList (row + 2) (col + 1)
            e12 = takeAt2 rawList (row - 1) (col)
            e13 = takeAt2 rawList (row - 1) (col + 1)
            e14 = takeAt2 rawList (row - 1) (col - 1)
            e15 = takeAt2 rawList (row - 1) (col + 2)
            e16 = takeAt2 rawList (row - 2) (col)
            e17 = takeAt2 rawList (row - 2) (col - 1)
            e18 = takeAt2 rawList (row - 2) (col + 1)


-- Sprawdza poprawność sąsiedztwa dla elementu w wierszu row i w kolumnie col, dla wiersza dłuższego,
-- zwraca prawdę lub fałsz
checkCorrectnessLongLine :: [[Char]] -> Int -> Int -> Bool
checkCorrectnessLongLine rawList row col =
         if elem letter nearbies then
            False
         else
            True
         where letter = toUpper (head (takeAt2 rawList row col))
               nearbies = getNearbiesLongLine rawList row col


-- Sprawdza poprawność sąsiedztwa dla elementu w wierszu row i w kolumnie col, dla wiersza krótszego,
-- zwraca prawdę lub fałsz
checkCorrectnessShortLine :: [[Char]] -> Int -> Int -> Bool
checkCorrectnessShortLine rawList row col =
         if elem letter nearbies then
            False
         else
            True
         where letter = toUpper (head (takeAt2 rawList row col))
               nearbies = getNearbiesShortLine rawList row col


-- Sprawdza czy podany wiersz jest krótszy czy dłuższy, zwraca odpowiednio Stringi "Short" lub "Long"
checkRow :: [[Char]] -> Int -> [Char]
checkRow rawList row =
    if row == 1 then
        if getRowLen rawList row > getRowLen rawList (row + 1) then
            "Long"
        else
            "Short"
    else if getRowLen rawList row > getRowLen rawList (row - 1) then
            "Long"
         else
            "Short"

-- Sprawdza poprawność sąsiedztwa dla elementu w wierszu row i w kolumnie col, zwraca prawdę lub fałsz
checkCorrectness :: [[Char]] -> Int -> Int -> Bool
checkCorrectness rawList row col =
    if checkRow rawList row == "Long" then
        checkCorrectnessLongLine rawList row col
    else
        checkCorrectnessShortLine rawList row col


-- Sprawdza poprawność sąsiedztwa dla całego plastra po podaniu ilości wiersz row i kolumn col, zwraca prawdę lub fałsz
checkWholePlaster :: [[Char]] -> Int -> Int -> Bool
checkWholePlaster rawList 1 0 = True
checkWholePlaster rawList row 0 = checkWholePlaster rawList (row - 1) (getRowLen rawList (row - 1))
checkWholePlaster rawList row col =
    if checkCorrectness rawList row col then
        checkWholePlaster rawList row (col - 1)
    else False


-- Sprawdza czy element w wierszu row i kolumnie col jest '.', zwraca prawdę lub fałsz
checkIsDot :: [[Char]] -> Int -> Int -> Bool
checkIsDot rawList row col =
    if takeAt2 rawList row col == ['.'] then
        True
    else
        False


-- Zamienia wszystkie małe litery w całym plastrze na duże, zwraca nową listę list reprezentującą plaster
getUpperResult :: [[Char]] -> Int -> [[Char]]
getUpperResult solution 1 = [(upperCase (head solution))]
getUpperResult solution totLen = getUpperResult solution (totLen - 1) ++ [(upperCase (head (takeAt solution totLen)))]



-- Bierze listę list reprezentującą plaster i zamienia na Stringa reprezentującego aktualną kombinację w kolejności: od
-- końca ostatniego wiersza, a potem od końca każdego następnego
getCurrentComb :: [[Char]] -> [Char]
getCurrentComb [] = []
getCurrentComb (x:xs) = (getCurrentComb xs) ++ (reverse x)


-- Realizuje obliczenia, dotyczące znalezienia rozwiązania, zwraca rozwiązanie w postaci listy list
-- Bierze aktualny numer literki do wstawienia n, listę list reprezentującą plaster, aktualny numer wiersza,
-- aktualny numer kolumny, oraz listę list reprezentującą listę niepoprawnych kombinacji do aktualnego miejsca obliczeń.
solve :: Int -> [[Char]] -> Int -> Int -> [[Char]] -> [[Char]]
-- Ta część realizuje algorytm w przypadku braku możliowści wstawienia jakiejkolwiek litery
solve 8 rawList row col badCombs =
    if col > (getRowLen rawList row) && row == length rawList then
        error "BRAK ROZWIAZANIA!"
    else
        if col > (getRowLen rawList row) then
            solve 8 rawList (row + 1) 1 badCombs
        else
            if isLower (head (takeAt2 rawList row col)) then do
                let newBadComb = badCombs ++ [takeWhile (/='.') (upperCase (getCurrentComb rawList))]
                let tempList = replaceAt2 rawList row col '.'
                solve 1 tempList row col newBadComb
            else
                solve 8 rawList row (col + 1) badCombs

-- Ta część realizuje algorytm w normalnym pryzpadku wstawiania liter
solve n rawList row col badCombs =
    if (row == 1 && col == 0) then
        rawList
    else
        if col == 0 then
            solve n rawList (row - 1) (getRowLen rawList (row - 1)) badCombs
        else
            if checkIsDot rawList row col then do
                let newLetter = toLower(head (takeAt "ABCDEFG" n))
                let tempList = replaceAt2 rawList row col newLetter
                let currComb = takeWhile (/='.') (upperCase (getCurrentComb tempList))
                if any (==currComb) badCombs then
                    solve (n + 1) rawList row col badCombs
                else
                    if checkCorrectness tempList row col then
                        solve 1 tempList row (col - 1) badCombs
                    else
                        solve (n + 1) rawList row col badCombs
            else
                solve 1 rawList row (col - 1) badCombs


-- Główna funkcja programu
main = do
    putStrLn "Podaj nazwę pliku z zawartością w formacie: \nPlaster ['BD..', '.GA.D', '.FEG', 'ABDCF', 'E..B']"
--  Wczytanie z klawiatury nazwy pliku
    fileName <- getLine
    text <- readFile fileName
    let rawList = getRawData text
    putStrLn "\nWprowadzone dane:"
    print rawList

--  Sprawdzenie czy format plastra we wprowadzanych danych jest odpowiedni
    let totLen = length rawList
    let plasterFormat = checkData rawList totLen
    if plasterFormat then
        putStrLn "\nFormat danych w pliku OK"
    else
        putStrLn "\nFormat danych w pliku niepoprawny. Prawdopodobnie sąsiednie wiersze nie różnią się długością o 1\n lub wiersze pierwszy i ostatni nie są wierszami krótkimi"

--  Sprawdzenie czy początkowa konfiguracja jest poprawna
    let rowLen = getRowLen rawList totLen
    let startConfig = checkWholePlaster rawList totLen rowLen
    if startConfig && plasterFormat then
        putStrLn "\nPoczątkowa konfiguracja OK"
    else
        putStrLn "\nPoczątkowa konfiguracja jest niepoprawna"

-- Sprawdzenie czy można wykonać obliczenia, oraz wybór przypadku (krótszy czy dłuższy wiersz jest ostatnim)
    if startConfig && plasterFormat then do
        let solution = solve 1 rawList totLen rowLen [[]]
        putStrLn "\nRozwiązanie to: "
        print (getUpperResult solution totLen)
    else
        putStrLn "\nObliczenia nie zostały wykonane ze względu na błędne dane"
