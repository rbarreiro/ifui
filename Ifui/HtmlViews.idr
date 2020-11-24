module Ifui.HtmlViews

import public Ifui.Changes
import Ifui.Views
import Ifui.Dom
import Data.IORef
import Data.List
import Data.HVect
import Decidable.Equality

public export
data HtmlView a b = MkHtmlView (XmlView a b)

public export
HtmlWidget : Type -> Type
HtmlWidget a = HtmlView a (a -> Changes a)

xView : HtmlView a b -> XmlView a b
xView (MkHtmlView x) = x

public export
data HtmlDView : (a:Type) -> (a->Type) -> (a->Type) -> Type where
    MkHtmlDView : XmlDView a f g -> HtmlDView a f g

public export
HtmlDWidget : (a:Type) -> (a->Type) -> Type
HtmlDWidget a f = HtmlDView a f (\x => f x -> (y:a ** DChanges a f x y))

record UpdateHandler a where
    constructor MkUpdateHandler
    treeChanges : Changes a -> Changes (Tree (XmlNode ((a -> Changes a))))
    state : IORef (a, Tree (XmlNode ((a -> Changes a))))
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
              Changes (Tree (XmlNode b)) -> IO ()
updateTree _ _ _ NoChanges =
    pure ()
updateTree procEvent n oldTree (set x) =
    do
        n' <- createRootNode x
        initHtmlTree n' procEvent x
        replaceWith n n'
updateTree _ _ _ (updateNodeValue NoChanges) =
    pure ()
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
        let Right newTxt = applyChanges x oldTxt | Left e => consoleLog e
        txtNode <- createTextNode newTxt
        replaceWith !(firstChild n) txtNode
updateTree procEvent n (Node _ oldChilds) (changeChilds c) =
    do
        children <- getChildren n
        sequence_ $ zipWith3 (updateTree procEvent) children oldChilds c


updateView : UpdateHandler a -> (a -> Changes a) -> IO ()
updateView uh@(MkUpdateHandler treeChanges state root) f =
    do
        (s, tree) <- readIORef state
        let changes = f s
        let changesT = treeChanges changes
        updateTree (updateView uh) root tree changesT
        let Right newS = applyChanges changes s | Left e => consoleLog e
        let Right newT = applyChanges changesT tree | Left e => consoleLog e
        writeIORef state (newS, newT)


export
startView : a -> HtmlWidget a -> IO ()
startView s0 (MkHtmlView (MkView f g)) =  do
    let startTree = f s0
    n <- createRootNode startTree
    let updateHandler = MkUpdateHandler g !(newIORef (s0, startTree)) n
    initHtmlTree n (updateView updateHandler) startTree
    b <- body
    appendChild b n


------------------------------------------

export
displayText : HtmlView String b
displayText = MkHtmlView $ textView "span" [] []

export
onChangeTextInput : (String -> b) -> HtmlView a b
onChangeTextInput f = MkHtmlView $ nodeView "input" [] [("change", \e => f e.targetValue)] []

export
div : List (HtmlView a b) -> HtmlView a b
div xs = MkHtmlView $ nodeView "div" [] [] (map xView xs)


export
fieldView : (j : Fin k) -> HtmlView (index j ts) b -> HtmlView (HVect ts) b
fieldView j (MkHtmlView x) = MkHtmlView $ fieldView j x

export
mapEvent : (x -> y) -> HtmlView a x -> HtmlView a y
mapEvent f (MkHtmlView x) =
    MkHtmlView $ mapEvent f x

export
fieldWidget : (j : Fin k) -> HtmlWidget (index j ts) -> HtmlWidget (HVect ts)
fieldWidget j (MkHtmlView x) =  MkHtmlView $ fieldWidget j x

------------------------------------------

export
displayTextD : HtmlDView a (const String) g
displayTextD = MkHtmlDView $ textViewD "span" [] []

export
vectUlLiD : HtmlDView Nat (const a) g -> HtmlDView Nat (\n=>Vect n a) g
vectUlLiD (MkHtmlDView x) = MkHtmlDView $ vectViewD "ul" [] [] (nodeViewD "li" [] [] [x])

export
dWidget : DecEq a => HtmlDWidget a f -> HtmlWidget (DPair a f)
dWidget (MkHtmlDView x) = MkHtmlView $ dXmlWidget x
