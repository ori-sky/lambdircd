module LeftApplication
( ($>)
) where

($>) :: (a -> b) -> a -> b
f $> x = f x
infixl 0 $>
