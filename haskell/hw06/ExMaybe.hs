module ExMaybe where

-- Do not alter this import!

import Data.Maybe qualified as M
import Prelude hiding (Maybe (..), maybe)

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes l =
    case l of
        [] -> []
        (m : ms) ->
            case m of
                Just e  -> e : catMaybes ms
                _       -> catMaybes ms

fromJust :: Maybe a -> a
fromJust m = case m of
    Just e  -> e
    _       -> error "Cannot convert"

fromMaybe :: a -> Maybe a -> a
fromMaybe d m = case m of
    Just a -> a
    _      -> d

isJust :: Maybe a -> Bool
isJust = not . isNothing

isNothing :: Maybe a -> Bool
isNothing m = case m of
    Nothing -> True
    _       -> False

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe f m = case m of
                 Just a  -> Just (f a)
                 _       -> Nothing

justMap :: (a -> Maybe b) -> [a] -> [b]
justMap _ [] =  []
justMap f (a:as) = case (f a) of
                     Just b  -> b:(justMap f as)
                     _       -> justMap f as

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b f (Just x) = f x
maybe b _ _ = d

maybeToList :: Maybe a -> [a]
maybeToList m = case m of
                  Just x  -> [x]
                  _       -> []

listToMaybe :: [a] -> Maybe a
listToMaybe l = case l of
                  []    -> Nothing
                  (x:_) -> Just x

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith [] _ = []
tryToModifyWith _ [] = []
tryToModifyWith (mf:mfs) (x:xs) = case mf of
                                    Just f -> (f x):(tryToModifyWith mfs xs)
                                    _      -> x:(tryToModifyWith mfs xs)
