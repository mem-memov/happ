
data Environment = Environment
data Log = Log
data Error = Fine | Error
data Input = Input
data Output = Output
data App = App Environment Input Output Error Log

main :: IO ()
main = do
  return ()