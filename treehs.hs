import System.Directory
import System.Environment
import Data.List
import Data.Tree
import Control.Monad

-- Implementation of tree in haskell

data FileSystemObject = IsDirectory | IsFile | IsSymLink
    deriving Show

data FileTree = File FilePath | Directory FilePath [FileTree] | SymLink FilePath FileTree
    deriving Show

{-  
    Example FileTree:

    dira/                                       <- directory
        |_ dirb/
        |_ filea                                <- file
        |_ fileb                                   
        |_ dirc/
            |_ filec
            |_ filed
            |_ filee -> fileb                   <- symbolic link
            |_ dird/

-}

-- This function does not follow symbolic links
prettyPrintFileTree :: FileTree -> String
prettyPrintFileTree (File filePath) = filePath
prettyPrintFileTree (SymLink filePath destination) = case destination of
    File f -> filePath ++ " -> " ++ f
    Directory d _ -> filePath ++ " -> " ++ d
prettyPrintFileTree (Directory filePath contents) = filePath ++ "/\n" ++ prettyPrintFileTreeHelper 4 contents

prettyPrintFileTreeHelper :: Int -> [FileTree] -> String
prettyPrintFileTreeHelper _ [] = ""
prettyPrintFileTreeHelper indentation (t:ts) = case t of
    File f                    -> (replicate indentation ' ') ++ "|_ " ++ f ++ "\n"  ++ (prettyPrintFileTreeHelper indentation ts)
    SymLink s (File f)        -> (replicate indentation ' ') ++ "|_ " ++ s ++ " -> " ++ f
    SymLink s (Directory d _) -> (replicate indentation ' ') ++ "|_ " ++ s ++ " -> " ++ d ++ "/"
    Directory d []            -> (replicate indentation ' ') ++ "|_ " ++ d ++ "/\n" ++ (prettyPrintFileTreeHelper indentation ts)
    Directory d xs            -> (replicate indentation ' ') ++ "|_ " ++ d ++ "/\n" ++ (prettyPrintFileTreeHelper (indentation * 2) xs) ++ (prettyPrintFileTreeHelper indentation ts)

detectType :: FilePath ->  IO (FileSystemObject)
detectType f = do
    isFile <- doesFileExist f
    isDirectory <- doesDirectoryExist f
    isSymLink <- pathIsSymbolicLink f
    let filePathType = if isFile then IsFile else if isDirectory then IsDirectory else if isSymLink then IsSymLink else error "could not detect type of file system object"
    pure (filePathType)

-- Takes a path and generates a FileTree using it as the root
toFileTree :: FilePath -> IO FileTree
toFileTree filePath = do
    object <- detectType filePath
    case object of
        -- IsDirectory -> (\l -> (\l2 -> pure (Directory filePath l2)) =<< (traverse toFileTree l)) =<< (listDirectory filePath)
        IsDirectory -> do
            contents <- listDirectory filePath
            subcontents <- traverse (\f -> withCurrentDirectory filePath (toFileTree f)) contents
            pure (Directory filePath subcontents)
        IsFile      -> pure (File filePath)
        IsSymLink   -> pure (SymLink filePath (File ""))

--Random comment
printFileTree :: FilePath -> IO ()
printFileTree filePath = (putStrLn . prettyPrintFileTree) =<< (toFileTree filePath)
