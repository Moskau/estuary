> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2
> import qualified Data.Maybe

> import           GHCJS.Types as GHCJS
> import qualified GHCJS.DOM.Event  as GHCJS (IsEvent)
> import qualified GHCJS.DOM.Element as GHCJS
> import           GHCJS.DOM.EventM as GHCJS (preventDefault, stopPropagation, EventM)

> data Simple = One | Two | Three deriving (Show)
> type Multiple = [Simple]
> data SimpleWidgetRequest = Set Simple | Flash
> data SimpleWidgetEvent k = DeleteMe k deriving (Show)

> data Misc = Add deriving (Show)
> type Hetero = Either Simple Misc
> data MiscRequest = Resize | Disable deriving (Eq)
> data MiscEvent = IGotClicked

> listWithChildEvents :: (Ord k, MonadWidget t m)
>    => Map k v                        -- an ordered map of initial values
>    -> Event t (Map k (Maybe v))      -- construction events (add/replace/delete)
>    -> Event t (Map k (Either SimpleWidgetRequest MiscRequest ))              -- events delivered to child widgets
>    -> (k -> v -> Event t (Either SimpleWidgetRequest MiscRequest) -> m a)   -- function to make a widget given key, value and request event
>    -> m (Dynamic t (Map k a))
>
> listWithChildEvents initial cEvents rEvents mkChild = do
>   let selector = fanMap $ rEvents               -- EventSelector (Const2 k r)
>   let mkChild' k v = mkChild k v $ select selector $ Const2 k
>   let changesNoValues = fmap (fmap (() <$)) cEvents       -- Event (Map k (Maybe () ))
>   let initialNoValues = () <$ initial                     -- Map k ()
>   stillExisting <- foldDyn (flip applyMap) initialNoValues changesNoValues -- m (Dynamic (Map k ()))  -- map of still-existing values
>   let relevantDiff diff _ = case diff of
>         Nothing -> Just Nothing -- Even if we let a Nothing through when the element doesn't already exist, this doesn't cause a problem because it is ignored
>         Just _ -> Nothing -- We don't want to let spurious re-creations of items through
>   let f = flip (differenceWith relevantDiff)
>   let beh = current stillExisting                           -- Behaviour (Map k ())
>   let evt = cEvents                                         -- Event (Map k (Maybe v))
>   let c = attachWith f beh evt                              -- Event
>   listHoldWithKey initial c mkChild'

> requestableSimpleWidget :: (Ord k, MonadWidget t m) => k -> Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple, Event t (SimpleWidgetEvent k)))
> requestableSimpleWidget key initialValue signal = do
>   let flashEvent = fforMaybe signal g
>   flashToggle <- toggle True flashEvent
>   attr <- forDyn flashToggle h
>   let buttons = forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   buttons' <- elDynAttr "div" attr buttons
>   deleteButton <- liftM (DeleteMe key <$) $ button "-"
>   let setEvent = fforMaybe signal f
>   value <- holdDyn initialValue (leftmost (buttons'++[setEvent]))
>   display value
>   forDyn value (\a-> (a,deleteButton))
>   where f (Set x) = Just x
>         f _ = Nothing
>         g (Flash) = Just ()
>         g _ = Nothing
>         h True = singleton "style" "background-color: red; border: 3px solid black"
>         h False = singleton "style" "background-color: green; border: 3px solid black"


> builder :: (MonadWidget t m, Ord k)=> k -> Hetero -> Event t (Either SimpleWidgetRequest MiscRequest) -> m (Dynamic t (Maybe Simple,Event t (SimpleWidgetEvent k)))
> builder k (Left s) e = do
>   let event = coincidence $ fmap (either (<$ e) (return $ (Set Three) <$ never)) e -- if e=Event Left, double wraps in same event and calls coincidence, otherwise  gets: coincidence Event (Never ...)
>   requestableSimpleWidget k s event >>= mapDyn (\(a,b)-> (Just a, b))
> builder k (Right m) e = do
>   let event = coincidence $ fmap (either (return $ Disable <$ never) (<$ e)) e -- if e=Event Right, double wraps in same event and calls coincidence, otherwise  gets: coincidence Event (Never ...)
>   miscWidget k event >>= mapDyn (\(a,b)-> (Nothing, b))

> -- @Probably don't want this tupled with 'SimpleWidgetRequest' -> maybe want new data type and re-wire things to take Either simple/miscWidgetRequest
> miscWidget:: (Ord k, MonadWidget t m )=> k -> Event t MiscRequest -> m (Dynamic t (Simple, Event t(SimpleWidgetEvent k)))
> miscWidget key e = do
>   let event = fmap (\a->if a==Disable then "disabled"=:"" else empty) e
>   attrs <- holdDyn empty event
>   el "div" $ do
>     buttonWithAttrs <- elDynAttr "button" attrs $ text "    +    "
>     deleteButton <- liftM (DeleteMe key <$) $ button "-"
>     return $ constDyn (One,deleteButton)

> growAndShrinkWidget' :: MonadWidget t m => m (Dynamic t [Maybe Simple])
> growAndShrinkWidget' = el "div" $ mdo
>   let initialMap = empty :: Map Int Hetero
>   makeSimpleWidget <- liftM (fmap (=:(Just(Left One))) . (tagDyn maxKey)) $ button "Add SimpleWidget"
>   makeMiscWidget <- liftM (fmap (=:(Just(Right Add))) . (tagDyn maxKey)) $ button "Add MiscWidget"
>   let growEvents = mergeWith makeMap [makeMiscWidget, makeSimpleWidget]
>   let updateEvent = mergeWith union [growEvents, deleteEvents']
>   setTwoEvent <- liftM (attachWith (\a _-> fromList (zip a (repeat $ Left (Set Two)))) (current activeKeys) ) $ button "Set Two"
>   disableEvent <- liftM (attachWith (\a _-> fromList (zip a (repeat $ Right Disable))) (current activeKeys) ) $ button "Disable '+' "
>   let parentEvents = leftmost [setTwoEvent,disableEvent]
>   widgets <- liftM (joinDynThroughMap) $ listWithChildEvents initialMap updateEvent parentEvents builder --MonadWidget t m => m (Dynamic t( Map k (Maybe Simple,Event t(SimpleWidgetEvent k))))
>   (values,events) <- forDyn widgets (unzip . elems) >>=splitDyn
>   deleteEvents <- forDyn events (fmap (fmap (\(DeleteMe k)-> singleton k Nothing))) -- m (Dynamic t [Event t (Map k Nothing...)])
>   let deleteEvents' = switch $ fmap (mergeWith (union)) $ current deleteEvents -- Behaviour [Event Map ...]
>   activeKeys <- forDyn widgets keys
>   maxKey <-  forDyn activeKeys (\k-> if k==[] then 0 else (maximum k)+1)
>   el "div" $ do
>     text "keys "
>     el "div" $ return values

makeMap should assign unique keys to two widgets when they're made at the same time, giving parameter 'a' the lower key

> makeMap::Map Int (Maybe Hetero) -> Map Int (Maybe Hetero) -> Map Int (Maybe Hetero)
> makeMap a b = union a $ fromList [(bKey+1,bVal)]
>   where (bKey,bVal) = elemAt 0 b

> main = mainWidget $ growAndShrinkWidget'>>=display
