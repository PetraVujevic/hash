module Language.Exec (
   ScriptState (..)
 , exec
 , command
 , checkTable
 , printOut
 , find
 , pwd
 , ls
 , cd
 , remove
 , move
 , copy
 , create
 , createDir
 , removeDir
 , moveDir
 , cat
 , printLines 
) where
 
import Parsing.HashParser
import Language.Expressions
import System.Directory
import System.IO

type VarTable = [(String, String)]

data ScriptState = ScriptState { 
     wd :: FilePath
   , vartable :: VarTable
   } deriving Show
     
exec :: Cmd -> ScriptState -> IO ScriptState 
exec cmd ScriptState {wd = swd, vartable = svartable} = 
   case cmd of 
     Assign {var = Var a, val = Str b} -> do
       if elem a (map fst svartable)
	   then return $ ScriptState {wd = swd, vartable = (filter (\(x,y) -> x /= a) svartable) ++ [(a,b)]} 
	   else return $ ScriptState {wd = swd, vartable = svartable ++ [(a,b)]}   
     Cmd {name = cname, args = cargs} -> do 
       command cname cargs svartable swd
       return $ ScriptState {wd = swd, vartable = svartable}

command :: Expr -> [Expr] -> [(String,String)] -> t -> IO ()	   
command (Var n) args table wd = case n of
   "#" -> return ()
   "ls" -> ls arg
   "cd" -> cd arg 
   "rm" -> remove arg
   "mv" -> move arg
   "cp" -> copy arg
   "touch" -> create arg
   "mkdir" -> createDir arg  
   "rmdir" -> removeDir arg
   "mvdir" -> moveDir arg
   "echo" -> printOut arg 
   "cat" -> cat arg
   "pwd" -> pwd 
   where arg = map (checkTable table) args  

checkTable :: [(String,String)] -> Expr -> Expr 
checkTable table (Str v)
   | v!!0 == '$' = find (tail v) table
   | otherwise = Str v

find :: String -> [(String,String)] -> Expr
find x = Str . snd . head . filter (\(a,b) -> a == x)
  
printOut :: [Expr] -> IO ()  
printOut [Str c] = putStrLn c 
printOut ((Str c):args) = putStrLn c >> printOut args
 
pwd :: IO ()
pwd = do 
  dir <- getCurrentDirectory
  putStrLn dir
 
ls :: [Expr] -> IO ()  
ls [] = do
   cnt <- getDirectoryContents "."
   print cnt
   
ls [Str wd] = do
   cnt <- getDirectoryContents wd
   print cnt
   
cd :: [Expr] -> IO ()    
cd [] = do
   home <- getHomeDirectory 
   setCurrentDirectory home

cd [Str dir] = setCurrentDirectory dir    

remove :: [Expr] -> IO ()
remove [Str f] = removeFile f
remove ((Str f):files) = removeFile f >> remove files

move :: [Expr] -> IO ()
move [Str f1, Str f2] = renameFile f1 f2

copy :: [Expr] -> IO ()
copy [Str f1, Str f2] = copyFile f1 f2
copy ((Str f1):args) = copyFile f1 dest >> copy args 
   where (Str dest) = last args 

create :: [Expr] -> IO ()
create [Str str] = do
   h <- openFile str WriteMode 
   hClose h   
   return ()

create ((Str str):args) = do
   h <- openFile str WriteMode 
   hClose h   
   create args
   return ()
   
createDir :: [Expr] -> IO ()   
createDir [Str dir] = createDirectory dir
createDir ((Str dir):args) = createDirectory dir >> createDir args

removeDir :: [Expr] -> IO ()
removeDir [Str dir] = removeDirectory dir
removeDir ((Str dir):args) = removeDirectory dir >> removeDir args

moveDir :: [Expr] -> IO ()
moveDir [Str dir1, Str dir2] = renameDirectory dir1 dir2

cat :: [Expr] -> IO ()
cat [Str f] = do
   h <- openFile f ReadMode
   printLines h
   hClose h 
   
cat ((Str f):args) = do
   h <- openFile f ReadMode
   printLines h
   hClose h 
   cat args

printLines :: Handle -> IO () 
printLines h = do 
   l <- hGetLine h
   putStrLn l
   eof <- hIsEOF h
   if eof then return () else printLines h   
   
   