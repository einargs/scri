module Example (locCommand) where

locCommand :: IO Int
locCommand = do
  putStrLn "Can run a local command"
  return 2

main :: IO ()
main = do
  putStrLn "Hello world!"
