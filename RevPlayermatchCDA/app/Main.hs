import Data.List (sortBy, transpose)
import Data.Function (on)
import Control.Monad.Trans.State.Lazy (modify, get, StateT, execStateT)
import Control.Monad.Trans (liftIO)
import System.Directory (removeFile, renameFile)
import System.IO
    ( hClose,
      openFile,
      hGetContents,
      hPutStr,
      openTempFile,
      IOMode(ReadMode) )
import Data.List (delete)
import Text.Read (readMaybe)
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)


-----------------------------------------------------------------------------------------------------------MAIN MODULE----------------------------------------------------------------------------------------


type PlayerPair = (String, Int) --(player & rating data tuple)

main :: IO()
main = gameStart []

gameStart :: [PlayerPair] -> IO() --(starting block with options of major functionalities)
gameStart oldPlayer = do
                     putStrLn " RB's TMBApp! Please opt and kindly type the indexing letter of functionality you want to use."
                     putStrLn "(a) Add new player name & rating (d) Delete last player input (z) Clear all player data entry and restart appm! (m) Make teams and balance (v) View the list of logged teams (r) Remove logged team (q) Quit"
                     stepChoice <- getLine
                     case stepChoice of
                        "a"  -> addPlayer oldPlayer
                        "d"  -> deletePlayer oldPlayer
                        "z"  -> main
                        "m"  -> matchmakeTeambalance oldPlayer
                        "v"  -> viewTeams oldPlayer
                        "r"  -> deleteTeamsCheck oldPlayer
                        "q"  -> quitApp
                        _    -> catchAll oldPlayer


------------------------------------------------------------------------------------------------------------ADD MODULE-------------------------------------------------------------------------------------------------


getLineInt :: IO Int --(non-numberproof readLn)
getLineInt = do
  line <- getLine
  case readMaybe line of
    Just x -> return x
    Nothing -> putStrLn "Invalid character entered! Please try again witn integers only!" >> getLineInt

addPlayer :: [PlayerPair] -> IO () --(creating an entry of player & rating data tuple to main list)
addPlayer oldPlayer = do
                                   putStrLn "Player Name ? "
                                   playerName <- getLine
                                   putStrLn "Player Rating ? (0-100) "
                                   playerRating <- getLineInt
                                   let newPlayerList = oldPlayer ++ [(playerName, playerRating)]
                                   putStrLn "Current registered players: "
                                   print newPlayerList
                                   putStr "Total player(s) : "
                                   print (length newPlayerList)
                                   gameStart  newPlayerList



------------------------------------------------------------------------------------------------------------DELETE MODULE---------------------------------------------------------------------------------------------------------------


deletePlayer :: [PlayerPair] -> IO () --(deleting the last data entry of in the main list)
deletePlayer [] = do putStrLn "There is no player in the entry yet!!" >> main
deletePlayer oldPlayer = do
    putStrLn "Delete last player? (Y/N) "
    yn <- getLine
    let prevPlayer = init oldPlayer
    case yn of
        "Y" -> do putStrLn "Current registered players: "
                  print prevPlayer
                  putStr "Total player(s) : "
                  print $length prevPlayer
                  gameStart prevPlayer
        "N" -> stepBack oldPlayer
        _ -> putStrLn "Please choose from available option\n" >> deletePlayer oldPlayer

stepBack :: [PlayerPair] -> IO () --(for double-checking purpose with feature to look at current player data entry state)
stepBack oldPlayer = do
    putStrLn "Current registered players: "
    print oldPlayer
    putStr "Total player(s) : "
    print (length oldPlayer)
    gameStart oldPlayer


-------------------------------------------------------------------------------------------------TEAM MAKING AND BALANCING MODULE---------------------------------------------------------------------------------------------------------------


reverseEvenPostn :: [[PlayerPair]] -> [[PlayerPair]] --(helper function to reverse the elements of even-positioned list element inside the main list)
reverseEvenPostn  [] = []
reverseEvenPostn  [[]] = [[]]
reverseEvenPostn  [x] = [x]
reverseEvenPostn (x:y:xs) = x : reverse y : reverseEvenPostn xs

group :: Int -> [PlayerPair] -> [[PlayerPair]] --(helper function to group into number of specified in variable)
group _ [] = []
group n l
 | n > 0 = take n l : group n (drop n l)
 | otherwise = error "Negative or zero n"

teamBalanceMonadEng :: StateT [[PlayerPair]] IO () --(final subroutine State Monad to produce desired end result)
teamBalanceMonadEng = do
  maBalTeams <- initFeederMT
  modify transpose
  liftIO $ putStrLn "-----------------------------------------------------------------------------------------NEW TEAM HAS BEEN CREATED!---------------------------------------------------------"

initFeederMT :: StateT [[PlayerPair]] IO [[PlayerPair]] --(initial subroutine to apply helper function to State Monad)
initFeederMT = do
 modify reverseEvenPostn
 get

matchmakeTeambalance :: [PlayerPair] -> IO () --(main team making and balancing batch algorithm along enhanced logging and timestamping features)
matchmakeTeambalance oldPlayer = do
                        putStrLn "Make how many teams? "
                        nTeams <- getLineInt
                        if nTeams < 1 then do
                            putStrLn "There must be at least one (1) team !!!\n"
                            matchmakeTeambalance oldPlayer else do
                                let oldPlayer' = group nTeams $ sortBy (flip compare `on` snd) oldPlayer
                                maBalTeams <- execStateT teamBalanceMonadEng oldPlayer'
                                putStrLn " Balanced teams are as below: Nth List -> Team N"
                                putStrLn " Rating of each respective teams are : "
                                let totalRating = map (sum . map snd) maBalTeams
                                print totalRating
                                putStrLn " Teams had been logged! Please check logTeam.txt for history. Thank you!"
                                appendFile "logTeam.txt" "\nTeams: "
                                appendFile "logTeam.txt" (show maBalTeams)
                                appendFile "logTeam.txt" " ; Respective relative cumulative strength :"
                                appendFile "logTeam.txt" (show totalRating)
                                appendFile "logTeam.txt" " ; Total number of teams :"
                                appendFile "logTeam.txt" (show $length maBalTeams)
                                appendFile "logTeam.txt" " ; Timestamp :"
                                now <- getCurrentTime
                                let nowX = formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S %Z" now
                                print nowX
                                appendFile "logTeam.txt" nowX
                                gameStart oldPlayer


-------------------------------------------------------------------------------------------------VIEW LOG ENTRY MODULE---------------------------------------------------------------------------------------------------------------


viewTeams :: [PlayerPair] -> IO () --(checking the latest state of saved teams log)
viewTeams oldPlayer = do
    teamlog <- readFile "logTeam.txt"
    let teams = lines teamlog
        numTeam = zipWith (\i l -> show i ++ " - " ++ l) [0..] teams
    putStr $ unlines numTeam
    gameStart oldPlayer


-------------------------------------------------------------------------------------------------DELETE LOG ENTRY MODULE---------------------------------------------------------------------------------------------------------------


deleteTeamsCheck :: [PlayerPair] -> IO () --(removing certain logged team and bumping up all logged teams below the one deleted an indexing number higher)
deleteTeamsCheck oldPlayer = do
    teamlog <- readFile "logTeam.txt"
    let teams = lines teamlog
        indexor n line = show n ++ " - " ++ line
        numTeam = zipWith indexor [0..] teams
    putStr $ unlines numTeam
    putStrLn "Please type the indexing number of the team you wish to delete. "
    indxTeam  <- getLineInt
    if indxTeam > read (take 2 (last numTeam)) || indxTeam < 0 then putStrLn "Indexing number input must be equal to existing log indexes and cannot be negative!!!\n" >> deleteTeamsCheck oldPlayer else
     do
      handle <- openFile "logTeam.txt" ReadMode
      (tempName, tempHandle) <- openTempFile "." "temp"
      contents <- hGetContents handle
      let number  = indxTeam
          todoTasks = lines contents
          newTodoItems = (Data.List.delete) (todoTasks !! number) todoTasks
      hPutStr tempHandle $ unlines newTodoItems
      hClose handle
      hClose tempHandle
      removeFile "logTeam.txt"
      renameFile tempName "logTeam.txt"
      putStrLn $ "Log entry no. " ++ show indxTeam ++ " has been successfully deleted!"
      gameStart oldPlayer


-------------------------------------------------------------------------------------------------COMPLETE PATTERN MODULE---------------------------------------------------------------------------------------------------------------


catchAll :: [PlayerPair] -> IO() --(wildcard, also can be used as the current state of player data entries lookup)
catchAll oldPlayer = do
     putStrLn "Please choose from indexing letters in the menu!!"
     stepBack oldPlayer


-------------------------------------------------------------------------------------------------TERMINATE APP MODULE---------------------------------------------------------------------------------------------------------------


quitApp :: IO () --(as is and terminating program operation from main menu)
quitApp = do
     putStrLn "Thank you for using our service :)"
     putStrLn "-----------------------------------------------------------------------------------------END OF SESSION----------------------------------------------------------------------------------------"
