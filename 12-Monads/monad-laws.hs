(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)  
f <=< g = (\x -> g x >>= f) 

f x = [x, -x]
g x = [x*3, x*2]
h = f <=< g