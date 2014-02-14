import Data.Char
import System.IO

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Read)

isMonday :: Day -> Bool
isMonday Monday = True
isMonday _ = False

main :: IO ()
main = do
    putStrLn "enter the name of a day"

    (x : xs) <- getLine
    let dayString = toUpper x : xs
    let day = read dayString :: Day

    putStrLn ("isMonday = " ++ show (isMonday day))
