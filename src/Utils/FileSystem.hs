module Utils.FileSystem where


import System.Directory (doesFileExist)


-- A function to get the file extension from a file path.
getFileExtension :: FilePath -> Maybe String
getFileExtension filePath =
  let ext = reverse $ takeWhile (/= '.') $ reverse filePath
  in if ext == filePath || null ext then Nothing else Just ext


-- A function to check if a file exists.
fileExists :: FilePath -> IO Bool
fileExists = doesFileExist 