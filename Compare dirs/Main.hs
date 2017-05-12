--некоторые модули предварительно установить через cabal install
import System.Directory
import Control.Monad
import Data.List
import Data.List.Split
import System.FilePath.Posix
import System.PosixCompat.Files

--рекурсивно получаем все файлы из папки(включая её подпапки)
getAllFiles :: FilePath -> IO [String]
getAllFiles nameDirectory = do
    names <- getDirectoryContents nameDirectory
    let content = names \\ [".", ".."]
    paths <- forM content $ \name -> do
      let path = nameDirectory ++ "\\" ++ name
      isDirectory <- doesDirectoryExist path
      if isDirectory
          then getAllFiles path 
          else return [path]
    return (concat paths)

--сравнение файлов только по именам
compareFilesByName :: [FilePath] -> [FilePath] -> String -> String -> IO()
compareFilesByName files1 files2 dir1 dir2 = do
        putStrLn ""
        putStrLn $ "Unique files by name in " ++ dir1 ++ ":"
        putStrLn $ intercalate ", " $ getUniqueFiles (getNames files1) (getNames files2)
        putStrLn ""
        putStrLn $ "Unique files by name in " ++ dir2 ++ ":"
        putStrLn $ intercalate ", " $ getUniqueFiles (getNames files2) (getNames files1)
        putStrLn ""
        putStrLn "Identical files by name"
        putStrLn $ intercalate ", " $ getIdenticalFilesByName (getNames files2) (getNames files1)
        putStrLn ""

--сравним каждый файл с каждым из другой папки
compareFiles :: [FilePath] -> [FilePath] -> IO()
compareFiles [] files2 = return()
compareFiles (file1:files1) files2 = do 
                                compare1 file1 files2
                                compareFiles files1 files2

compare1 :: FilePath -> [FilePath] -> IO()
compare1 file1 [] = return()
compare1 file1 (file2:files2) = do 
                                  --если у них совпадают имена, то сравним их по параметрам
                                  if (getName file1) == (getName file2) 
                                  then compareFilesByParams file1 file2
                                  else return()
                                  compare1 file1 files2

--сравнение по различным параметрам: содержимому, размеру, времени последней модификации
compareFilesByParams :: FilePath -> FilePath -> IO()
compareFilesByParams file1 file2 = do
                     putStrLn ("Compare files with name " ++ (getName file1) ++ "\n")
                     file1Contents <- readFile file1
                     file2Contents <- readFile file2
                     if (file1Contents == file2Contents) 
                     then putStrLn("Contents identical in \n" ++ file1 ++ "\n" ++ file2 ++ "\n")
                     else putStrLn("Contents different in \n" ++ file1 ++ "\n" ++ file2 ++ "\n")
                     file1Status <- getFileStatus file1
                     file2Status <- getFileStatus file2
                     if (fileSize(file1Status) == fileSize(file2Status))
                     then do putStrLn("File size identical in \n" ++ file1 ++ "\n" ++ file2 ++ "\n")
                     else putStrLn("File size different in \n" ++ file1 ++ "\n" ++ file2 ++ "\n")
                     if (modificationTime(file1Status) == modificationTime(file2Status))
                     then putStrLn("Modification time identical in \n" ++ file1 ++ "\n" ++ file2 ++ "\n")
                     else putStrLn("Modification time different in \n" ++ file1 ++ "\n" ++ file2 ++ "\n")
                     putStrLn("##################################################################\n")


--Получим имена файлов по списку путей
getNames :: [FilePath] -> [String]
getNames paths = map (\file -> (getName file)) paths

--Получим имя файла зная путь к файлу
getName :: FilePath -> String
getName path = last (splitOn "\\" path)

--Получим уникальные файлы по имени
getUniqueFiles :: [String] -> [String] -> [String]
getUniqueFiles filesFirst filesSecond = filesFirst \\ filesSecond

--Получим файлы совпадающие по именам
getIdenticalFilesByName :: [String] -> [String] -> [String]
getIdenticalFilesByName filesFirst filesSecond = intersect filesFirst filesSecond
 

main = do
        currentDirectory <- getCurrentDirectory
        putStrLn "Write first directory:"
        dir1 <- getLine
        putStrLn "Write second directory:"
        dir2 <- getLine
        --получим все файлы из каждой папки
        files1 <- getAllFiles (currentDirectory ++ "\\" ++ dir1)
        files2 <- getAllFiles (currentDirectory ++ "\\" ++ dir2)
        --сравним по именам и ещё чему-нибудь
        compareFilesByName files1 files2 dir1 dir2
        compareFiles files1 files2