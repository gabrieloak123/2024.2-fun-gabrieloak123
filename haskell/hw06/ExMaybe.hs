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
                Nothing -> catMaybes ms
                Just e -> e : catMaybes ms

fromJust :: Maybe a -> a
fromJust m = case m of
    Nothing -> error "Cannot convert"
    Just e -> e

fromMaybe :: a -> Maybe a -> a
fromMaybe d m = case m of
    Just a -> a
    _ -> d

isJust :: Maybe a -> Bool
isJust = not . isNothing

isNothing :: Maybe a -> Bool
isNothing m = case m of
    Nothing -> True
    _ -> False
mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe = undefined

justMap :: (a -> Maybe b) -> [a] -> [b]
justMap = undefined

maybe :: b -> (a -> b) -> Maybe a -> b
maybe = undefined

maybeToList :: Maybe a -> [a]
maybeToList = undefined

listToMaybe :: [a] -> Maybe a
listToMaybe = undefined

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith = undefined
