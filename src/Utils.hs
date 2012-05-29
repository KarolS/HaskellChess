module Utils where

obj |> f = f $! obj

infixl 0 |>

negIf :: Num a =>  Bool -> a -> a
negIf b x = if b then -x else x
negUnless :: Num a =>  Bool -> a -> a
negUnless b x = if b then x else -x

(^/^) :: (a->a)->Int->(a->a)
f ^/^ 0 = id
f ^/^ n = f . (f ^/^ (n-1))

avg xs = avg' xs (0.0,0.0) |> (\tuple -> fst tuple / snd tuple)
   where
      avg' (x:xs) (sum, count) = avg' xs (sum+x, count+1.0)
      avg' [] z = z

maybeMaxBy f [] = Nothing
maybeMaxBy f a@(x:xs) = Just $ maxBy f a

maxBy :: Ord v => (o->v)->[o]->o
maxBy f (x:xs) = fst $ maxBy' f xs (x, f x)
   where
      maxBy' f (x:xs) (y, fy) = if fx>fy then maxBy' f xs (x,fx) else maxBy' f xs (y,fy)
         where fx = f x
      maxBy' _ [] z = z
