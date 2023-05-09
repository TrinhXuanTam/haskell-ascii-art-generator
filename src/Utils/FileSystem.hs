module Utils.FileSystem where


import System.Directory (doesFileExist)


-- A function to get the file suffix from a file path.
getFileSuffix :: FilePath -> Maybe String
getFileSuffix filePath =
  let suffix = reverse $ takeWhile (/= '.') $ reverse filePath
  in if suffix == filePath || null suffix then Nothing else Just suffix


-- A function to check if a file exists.
fileExists :: FilePath -> IO Bool
fileExists filePath = doesFileExist filePath