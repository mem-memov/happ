data AppState = AppState
data AppConfiguration = AppConfiguration
data AppError = AppNoError | AppError

data App a = App {
  getAppState :: AppState
  , getAppConfiguration :: AppConfiguration
  , getAppError :: AppError
} | AppApplication a

app :: App ()
app = App ()

instance Functor App where
  fmap f (App x) = 
    let 
      appConfiguration = getAppConfiguration x
      appError = getAppError x
      y = case appError of
        AppNoError -> App (f x)
        AppError -> App x
      newAppConfiguration = getAppConfiguration y
      result = if newAppConfiguration == appConfiguration then y else x { getAppError = AppError }
    in
      result

instance Applicative App where
  pure ((->) x y) = AppApplication ()
  pure x = App x
  (AppApplication f) <*> (App x) = 
    let 
      appConfiguration = getAppConfiguration x
      appError = getAppError x
      y = case appError of
        AppNoError -> pure (f x)
        AppError -> App x
      newAppConfiguration = getAppConfiguration y
      result = if newAppConfiguration == appConfiguration then y else x { getAppError = AppError }
    in
      result

instance Monad App where
  (App x) >>= f =
    let 
      appConfiguration = getAppConfiguration x
      appError = getAppError x
      y = case appError of
        AppNoError -> f x
        AppError -> App x
      newAppConfiguration = getAppConfiguration y
      result = if newAppConfiguration == appConfiguration then y else x { getAppError = AppError }
    in
      result

main :: IO ()
main = do
  return ()