module Ifui.HtmlViews

import public Ifui.Changes
import public Ifui.Views
import Ifui.Dom
import Data.IORef
import Data.List

public export
HtmlView : Type -> Type -> Type
HtmlView = XmlView

record UpdateHandler a where
    constructor MkUpdateHandler
    treeChanges : Changes a a -> Changes (Tree (XmlNode ((a -> Changes a a)))) (Tree (XmlNode ((a -> Changes a a))))
    state : IORef (a, Tree (XmlNode ((a -> Changes a a))))
    root : DomNode


buildEvent : DomEvent -> IO Event
buildEvent x =
    pure $ MkEvent
            !(targetValue x)

addListener : (b -> IO ()) -> DomNode -> EventListener b -> IO ()
addListener procEvent x (evt, f) =
    addEventListener evt (\e => buildEvent e >>= (procEvent . f)) x

createRootNode : XmlTree b -> IO DomNode
createRootNode (Node (MkXmlNode tag _ _ _) _) =
    createElement tag

mutual
    initHtmlChild : (b -> IO ()) -> DomNode -> Tree (XmlNode b) -> IO ()
    initHtmlChild procEvent r x =
        do
            n <- createRootNode x
            initHtmlTree n procEvent x
            appendChild r n


    initHtmlTree : DomNode -> (b -> IO ()) -> XmlTree b -> IO ()
    initHtmlTree n procEvent (Node (MkXmlNode tag attrs listeners txt) xs) =
        do
            txtNode <- createTextNode txt
            appendChild n txtNode

            traverse_ (\(k,v) => setAttribute n k v) attrs
            traverse_ (addListener procEvent n) listeners

            traverse_ (initHtmlChild procEvent n) xs

updateTree : (b -> IO ()) -> DomNode -> Tree (XmlNode b) ->
              Changes (Tree (XmlNode b)) (Tree (XmlNode b)) -> IO ()
updateTree procEvent n oldTree (set x) =
    do
        n' <- createRootNode x
        initHtmlTree n' procEvent x
        replaceWith n n'
updateTree procEvent n oldTree (updateNodeValue (set (MkXmlNode tag attrs listeners txt))) =
    do
        n' <- createElement tag
        txtNode <- createTextNode txt
        appendChild n' txtNode
        traverse_ (\(k,v) => setAttribute n' k v) attrs
        traverse_ (addListener procEvent n') listeners
        childs <- getChildNodes n
        traverse_ (\c => appendChild n' c) childs
        replaceWith n n'
updateTree procEvent n (Node (MkXmlNode _ _ _ oldTxt) _) (updateNodeValue (updateXmlNodeText x)) =
    do
        txtNode <- createTextNode (applyChanges x oldTxt)
        replaceWith !(firstChild n) txtNode
updateTree procEvent n (Node _ oldChilds) (changeChilds c) =
    do
        children <- getChildren n
        sequence_ $ zipWith3 (updateTree procEvent) children oldChilds c


updateView : UpdateHandler a -> (a -> Changes a a) -> IO ()
updateView uh@(MkUpdateHandler treeChanges state root) f =
    do
        (s, tree) <- readIORef state
        let changes = f s
        let changesT = treeChanges changes
        updateTree (updateView uh) root tree changesT
        writeIORef state (applyChanges changes s, applyChanges changesT tree)


export
startView : a -> HtmlView a (a -> Changes a a) -> IO ()
startView s0 (MkView f g) =  do
    let startTree = f s0
    n <- createRootNode startTree
    let updateHandler = MkUpdateHandler g !(newIORef (s0, startTree)) n
    initHtmlTree n (updateView updateHandler) startTree
    b <- body
    appendChild b n


------------------------------------------

export
displayText : XmlView String b
displayText = textView "span" [] []

export
onChangeTextInput : (String -> b) -> XmlView a b
onChangeTextInput f = nodeView "input" [] [("change", \e => f e.targetValue)] []

export
div : List (XmlView a b) -> XmlView a b
div xs = nodeView "div" [] [] xs



--
-- export
-- button : ((x:a) -> h x) -> String -> View a f g h
-- button calcEvent label =
--   MkView init update
--   where
--     init : DomNode -> (x : a) -> g x -> UpdateHandler a h -> IO ()
--     init r _ _ uh =
--       do
--         n <- createElement "button"
--         setTextContent label n
--         addEventListener "click" (\e => processDomEvent (\z,()=>calcEvent z) uh ()) n
--         appendChild r n
--     update : DomNode -> (x : a) -> (y : a) -> f x y -> UpdateHandler a h -> IO ()
--     update _ _ _ _ uh = pure ()
--
-- export
-- container : String -> List (View a f g h) -> View a f g h
-- container tag views =
--   MkView init update
--   where
--     init : DomNode -> (x : a) -> g x -> UpdateHandler a h -> IO ()
--     init r x s0 uh =
--       do
--         n <- createElement tag
--         traverse_ (\(MkView i _) => i n x s0 uh) views
--         appendChild r n
--     update : DomNode -> (x : a) -> (y : a) -> f x y -> UpdateHandler a h -> IO ()
--     update r x y change uh =
--       do
--         childNodes <- getChildren r
--         traverse_ (\(n, (MkView _ u)) => u n x y change uh) (zip childNodes views)
--
-- export
-- div : List (View a f g h) -> View a f g h
-- div = container "div"
