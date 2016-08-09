> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map as M
> import Data.Either
> import Data.Functor.Misc -- For Const2
> import qualified Data.Maybe

> import qualified GHCJS.DOM.Element
> import           GHCJS.DOM.EventM as G (preventDefault, stopPropagation, EventM)

> import Container

> data Simple = One | Two | Three deriving (Show,Eq)
> type Multiple = [Simple]
> data SimpleWidgetRequest = Set Simple | Flash
> data WidgetEvent = Drop | DragEnd | DeleteMe | MakeSimple | Idle deriving (Show,Eq)
> data Misc = Add deriving (Show,Eq)

> requestableSimpleWidget :: MonadWidget t m => Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple, Event t (WidgetEvent)))
> requestableSimpleWidget initialValue signal = mdo
>   (wid,value) <- elAttr' "div" attr $ do
>     buttons <- forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>     value <-  holdDyn initialValue (leftmost buttons)
>     return value
>   x <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Dragend) (void $ G.preventDefault)
>   y <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Dragover) (void $ G.preventDefault)
>   _ <- performEvent_ $ return () <$ y
>   let widgetEvent = (Main.DragEnd <$ x)
>   display value
>   forDyn value (\a -> (a,widgetEvent))
>   where
>         attr = singleton "draggable" "true"

> miscWidget :: MonadWidget t m => m (Dynamic t (Misc, Event t WidgetEvent))
> miscWidget = do
>   (wid,deleteButton) <- elAttr' "div" attr $ do
>     deleteButton <- liftM (DeleteMe <$) $ button "-"
>     return $ deleteButton
>   x <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Drop) (void $ G.preventDefault)
>   y <- Reflex.Dom.wrapDomEvent (Reflex.Dom._el_element wid) (Reflex.Dom.onEventName Reflex.Dom.Dragover) (void $ G.preventDefault)
>   _ <- performEvent_ $ return () <$ y
>   let miscEvents = leftmost [Main.Drop <$ x, deleteButton]
>   return $ constDyn (Add,miscEvents)
>   where
>         attr = singleton "style" "background-color: red; border: 3px solid black"

> builder :: MonadWidget t m => Either Simple Misc -> Event t SimpleWidgetRequest -> m (Dynamic t (Either Simple Misc,Event t WidgetEvent))
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
>   (values,events) <- container initialMap cEvents rEvents builder -- (values,events) :: ( Dynamic t (Map k Either Simple Misc), Event t (Map k WidgetEvent) )
>   eventsOld <- hold M.empty events -- Map of old events eventsOld :: Behavior t (Map k WidgetEvent)
>   let valuesOld = (current values) -- Map of old values valuesOld :: Behavior t (Map k Either Simple Misc)
>   let dragKeys = fmap (keys . M.filter (==DragEnd)) eventsOld -- dragKeys :: Behaviour t [k]
>   let dropKeys = fmap (keys . M.filter (==Main.Drop)) events -- dropKeys :: Event t [k]
>   let deleteList = attachWith (\dg dp -> if(not (Prelude.null dp)) then ((concat . Prelude.map (\k ->[(k,Delete)])) dg) else []) dragKeys dropKeys
>   let deleteMap = fmap (fromList) deleteList -- deleteMap :: Event t (Map k Construction)
>   dragKeys' <- sample dragKeys -- dragKeys :: [k]
>   valuesOld' <- sample valuesOld -- valuesOld' :: Map k Either Simple Misc
>   let valuesOld'' = findWithDefault (Left One) (head dragKeys') valuesOld'
>   let valuesOld''' = (constant valuesOld'') -- valuesOld''' :: Behavior t (Either Simple Misc)
>   let insertList = attachWith (concat . Prelude.map (\v k -> ([ (k,Insert (Right Add)) , (k+1,Insert (Left v)) ]))) valuesOld''' dropKeys
>   let insertMap = fmap (fromList) insertList -- insertMap :: Event t (Map k (Construction Either Simple Misc))
>   mapDyn (elems . mapMaybe (id) . fmap (either (Just) (const Nothing))) values

> main = mainWidget $ dragAndDropWidget>>=display
