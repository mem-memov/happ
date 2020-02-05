
data App a = App a

instance Functor App where
  fmap f (App x) = App (f x)

instance Applicative App where
  pure x = App x
  (App f) (<*>) (App x) = App (f x) 

instance Monad App where
  (App x) (>>=) f = f x

main :: IO ()
main = do
  return ()