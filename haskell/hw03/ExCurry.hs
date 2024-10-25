module ExCurry where

import Prelude hiding (curry, uncurry)

-- use your mind to infer the types, don't cheat!

-- curry gets a "traditional" binary function
-- and returns its currified version
curry :: ((α, β) -> γ) -> (α -> β -> γ)
curry f a b = f (a, b)

-- uncurry gets a currified function
-- and returns its "traditional" binary version
uncurry :: (α -> β -> γ) -> ((α, β) -> γ)
uncurry f (a, b) = f a b
