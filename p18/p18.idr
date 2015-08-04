import System

data path = Nil | MkPath Int path;

weight : path -> Int;
weight Nil = 0;
weight (MkPath n p) = n + weight p;

instance Show path where
	show Nil = ""
	show (MkPath n Nil) = show n
	show (MkPath n p) = show n ++ " " ++ show p

-- It'd be nice to use Data.Vect here, but it needs compile-time-known sizes 
read : String -> List Int
read = map cast . words

toPaths : List Int -> List path
toPaths = map (\n => MkPath n Nil)

step : List path -> List Int -> List path
step [] _ = []
step _ [] = []
step (p1::p2::ps) (n::ns) = pg :: step (p2 :: ps) ns where
	pg : path
	pg = if weight p1 < weight p2 then MkPath n p2 else MkPath n p1

steps : List path -> List (List Int) -> List path
steps paths [] = paths
steps paths (line :: lines) = steps (step paths line) lines

findPath : List (List Int) -> Maybe path
findPath (line :: lines) = head' $ steps (toPaths line) lines

load : File -> List (List Int) -> IO (List (List Int))
load fh lines = do {
	line <- fread fh
	eof <- feof fh
	if eof then do
		return lines 
	else do
		load fh $ (read line) :: lines
}

main : IO ()
main = do {
	-- TODO: redo this with Maybe
	[prog, fname] <- getArgs
	fh <- fopen fname "r"
	lines <- load fh []
	case findPath lines of
		Just path => do {
			putStrLn $ show path
			putStrLn $ show $ weight path
		}
		Nothing => putStrLn "No path found"
}
