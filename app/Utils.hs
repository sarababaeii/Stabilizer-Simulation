module Utils
(extractValue
, filterSecondByFirst
) where

----------------------
-- General utilities
----------------------
extractValue :: Maybe a -> a
extractValue (Just x) = x

----------------------
-- List utilities
----------------------
filterSecondByFirst :: (a -> Bool) -> [a] -> [b] -> [b]
filterSecondByFirst _ [] _ = []
filterSecondByFirst _ _ [] = []
filterSecondByFirst p (x:xs) (y:ys)
    | p x       = y:rest
    | otherwise = rest
    where
        rest = filterSecondByFirst p xs ys
