import Mancala
import System.Console.ANSI


play :: State -> IO State
play state = case (status state) of
    Turn player -> let
        prompt = case player of
            P1 -> "P1: "
            P2 -> "P2: "
        in do
            putStr prompt
            input <- getLine
            case (reads input :: [(Int, String)]) of
                [] -> do
                    putStrLn $ "[!] input not recognized: '" ++ input ++ "'"
                    play state
                ((action_int,_):_) -> update (Action action_int ) state


loop :: State -> IO ()
loop state = do
    clearScreen
    putStrLn $ take 20 $ repeat '='
    putStrLn $ show state
    putStrLn $ take 20 $ repeat '='
    state_new <- play state
    case (status state_new) of
        Turn _   -> loop state_new
        Finished -> do
            putStrLn $ show state_new
            putStrLn "GAME FINISHED"
            putStrLn $ "P1's score: " ++ (show $ score1 state_new)
            putStrLn $ "P2's score: " ++ (show $ score2 state_new)


main :: IO ()
main = do
    loop state_init

