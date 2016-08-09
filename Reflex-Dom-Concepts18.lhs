> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map as M
> import Data.Either
> import Data.Functor.Misc -- For Const2
> import qualified Data.Maybe

> import           GHCJS.Types as GHCJS
> import qualified GHCJS.DOM.Event  as GHCJS (IsEvent)
> import qualified GHCJS.DOM.Element as GHCJS
> import           GHCJS.DOM.EventM as GHCJS (preventDefault, stopPropagation, EventM)

> import Estuary.Reflex.Container

> data Simple = One | Two | Three deriving (Show,Eq)
> type Multiple = [Simple]
> data SimpleWidgetRequest = Set Simple | Flash
> data WidgetEvent = Drop | DragEnd | DeleteMe | MakeSimple | Idle deriving (Show,Eq)
> data Misc = Add deriving (Show,Eq)

> requestableSimpleWidget :: MonadWidget t m => Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple, Event t (WidgetEvent)))
> requestableSimpleWidget key initialValue signal = mdo
>   (wid,value) <- elAttr' "div" attr $ do
>     buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>     value <-  holdDyn initialValue (leftmost buttons)
>     return value
>   x <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Dragend)  (void $ GHCJS.preventDefault)
>   y <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Dragover) (void $ GHCJS.preventDefault)
>   _ <- performEvent_ $ return () <$ y
>   let widgetEvent = (Main.DragEnd key <$) $ x
>   display value
>   forDyn value (\a -> (a,widgetEvent))
>   where
>         attr = singleton "draggable" "true"

> miscWidget :: MonadWidget t m => m (Dynamic t (Misc, Event t WidgetEvent))
> miscWidget = do
>   (wid,deleteButton) <- elAttr' "div" attr $ do
>     liftM (DeleteMe key <$) $ button "-"
>   x <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Drop)     (void $ GHCJS.preventDefault)
>   y <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Dragover) (void $ GHCJS.preventDefault)
>   _ <- performEvent_ $ return () <$ y
>   let miscEvents = leftmost [(Main.Drop key <$) $ x, deleteButton]
>   return $ constDyn (Add,miscEvents)
>   where
>         attr = singleton "style" "background-color: red; border: 3px solid black"

> builder :: (MonadWidget t m, Ord k)=> Either Simple Misc -> Event t SimpleWidgetRequest -> m (Dynamic t (Either Simple Misc,Event t WidgetEvent))
> builder (Left simp) e = do
>   x <- requestableSimpleWidget simp e
>   mapDyn (\(a,b) -> (Left a,b)) x
> builder (Right misc) _ = do
>   x <- miscWidget
>   mapDyn (\(a,b) -> (Right a,b)) x

> dragAndDropWidget :: MonadWidget t m => m (Dynamic t [Simple])
> dragAndDropWidget = el "div" $ mdo
>   let initialMap = fromList [(0::Int,Right Add),(1::Int,Left One),(2::Int,Right Add),(3::Int,Left Two)]
>   let cEvents = mergeWith union [deleteMap, insertMap]
>   let rEvents = never
(values,events) :: ( Dynamic t (Map k Either Simple Misc), Event t (Map k WidgetEvent) )
values :: Dynamic t (Map k Either Simple Misc)
events :: Event t (Map k WidgetEvent)
>   (values,events) <- container initialMap cEvents rEvents builder
eventsOld :: Behavior t (Map k WidgetEvent)
>   eventsOld <- hold M.empty events -- Map of old events
valuesOld :: Behavior t (Map k Either Simple Misc)
>   let valuesOld = (current values) -- Map of old values
dragKeys :: Behaviour t [k]
>   let dragKeys = fmap (keys . M.filter (==DragEnd)) eventsOld
dropKeys :: Event t [k]
>   let dropKeys = fmap (keys . M.filter (==Main.Drop)) events
deleteList :: Event t [(k,Construction)]
>   let deleteList = attachWith (\dg dp -> if(dp\=null)then (fmap(k -> [(k,Delete)])dg) else [])) dragKeys dropKeys
deleteMap :: Event t (Map k Construction)
>   let deleteMap = fmap (fromList) deleteList

Fix this part
-----------------------------------------------------
dragKeys :: [k]
>   dragKeys' <- sample dragKeys
valuesOld' :: Map k Either Simple Misc
>   valuesOld' <- sample valuesOld
valuesOld'' :: Map k Either Simple Misc
>   let valuesOld'' =
valuesOld''' :: Behavior t (Map k Either Simple Misc)
>   let valuesOld''' = pull (liftM valuesOld'')
-----------------------------------------------------

valuesList :: Behavior t [Either Simple Misc]
>   let valuesOld'''' = fmap (elems) valuesOld'''
insertList :: Event t [(k,Contruction Either Simple Misc)]
>   let insertList = attachWith (fmap(\v k -> [ (k,Insert (Right Add)),(k+1,Insert(Left v))])) valuesOld'''' dropKeys
insertMap :: Event t (Map k (Construction Either Simple Misc))
>   let insertMap = fmap (fromList) insertList
>   mapDyn (elems . mapMaybe (id) . fmap (either (Just) (const Nothing))) values

> main = mainWidget $ dragAndDropWidget>>=display

To Do:

- Fix Highlighted portion
- Simplify miscWidget
- Simplify requestableSimpleWidget
