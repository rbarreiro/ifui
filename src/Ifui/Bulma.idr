module Ifui.Bulma

import Data.Vect
import Ifui.Widget
import Ifui.Patterns
import Data.List
import Ifui.JSUtils
import Decidable.Equality

class__ : String -> WidgetAttribute b
class__ x = WidgetSimpleAttribute $ CSSClassAttr x

type__ : String -> WidgetAttribute b
type__ x = WidgetSimpleAttribute $ StringAttr "type" x

onChange_ : WidgetAttribute String
onChange_ = WidgetEventListener "change" (\e => target e >>= getValue)

onClick_ : a -> WidgetAttribute a
onClick_ x = WidgetEventListener "click" (\e => pure x)

value_ : String -> WidgetAttribute b
value_ x = WidgetSimpleAttribute $ ValueAttr x

multiple_ : Bool -> WidgetAttribute a
multiple_ x = WidgetSimpleAttribute $ BoolAttr "multiple" x

selected_ : Bool -> WidgetAttribute a
selected_ x = WidgetSimpleAttribute $ SelectedAttr x

export
data Column a = MkColumn (Widget a)  

export
data BulmaStyleOption = MkBulmaStyleOption String

public export
data DivSpan = Div | Span

export
Show DivSpan where
  show Div = "div"
  show Span = "span"

export 
ml : Fin 7 -> BulmaStyleOption
ml z = MkBulmaStyleOption $ "ml-" ++ show z

export
hasTextSuccess : BulmaStyleOption
hasTextSuccess = MkBulmaStyleOption "has-text-success"

export
hasTextWarning : BulmaStyleOption
hasTextWarning = MkBulmaStyleOption "has-text-warning"

optionsToAttributes : List BulmaStyleOption -> List (WidgetAttribute a)
optionsToAttributes xs = (\(MkBulmaStyleOption z) => class__ z)  <$> xs

export
div : {default Nothing onclick : Maybe a} -> {default [] styleOptions : List BulmaStyleOption} -> List (Widget a) -> Widget a
div xs = node "div" ((optionsToAttributes styleOptions) ++ (catMaybes [onClick_ <$> onclick])) xs

export
span : {default Nothing onclick : Maybe a} -> {default [] styleOptions : List BulmaStyleOption} -> List (Widget a) -> Widget a
span xs = node "span" ((optionsToAttributes styleOptions) ++ (catMaybes [onClick_ <$> onclick])) xs


export
data BulmaButtonStyleOption = MkBulmaButtonStyleOption String

buttonOptionsToAttributes : List BulmaButtonStyleOption -> List (WidgetAttribute a)
buttonOptionsToAttributes xs = (\(MkBulmaButtonStyleOption z) => class__ z)  <$> xs

export
button : {default [] styleOptions : List BulmaButtonStyleOption} -> String -> a -> Widget a
button label x = node "button" (([class__ "button", onClick_ x] ++ buttonOptionsToAttributes styleOptions)) [text label]

export
container : List (Widget a) -> Widget a
container x = node "div" [class__ "container"] x

export
navbar : Widget a -> List (Widget a) -> List (Widget a) -> Widget a
navbar navbarBrand navbarStart navbarEnd = 
  node "nav" [class__ "navbar"] [
    node "div" [class__ "navbar-brand"] [
      nitem navbarBrand
    ],
    node "div" [class__ "navbar-menu"] [
      node "div" [class__ "navbar-start"] (nitem <$> navbarStart),
      node "div" [class__ "navbar-end"] (nitem <$> navbarEnd)
    ]
  ]
  where
    nitem : Widget a -> Widget a
    nitem x = node "a" [class__ "navbar-item"] [x]

export
column : Widget a -> Column a
column w = 
  MkColumn $ node "div" [class__ "column"] [w]

export
columns : List (Column a) -> Widget a
columns xs = 
  node "div" [class__ "columns"] ((\(MkColumn z) => z) <$> xs)

export
textInputBulma : {default Nothing label : Maybe String} ->
                    String -> Widget String
textInputBulma {label = Nothing} value  = 
  node "div" [class__ "control"] [node "input" [type__ "text", class__ "input", value_ value, onChange_] []]
textInputBulma {label = (Just x)} value = 
 node "div" [class__ "field"]
  [ node "label" [class__ "label"] [text x]
  , textInputBulma {label = Nothing} value
  ]

export
numberInputBulma : {default Nothing label : Maybe String} ->
                    Maybe Double -> Widget (Maybe Double)
numberInputBulma {label = Nothing} value  = 
  node "div" [ class__ "control"] 
             [node 
               "input" 
               [type__ "number"
               , class__ "input"
               , value_ $ fromMaybe "" $  cast <$> value
               , (\x => if x == "" then Nothing else Just (cast x)) <$> onChange_
               ] 
               []
             ]
numberInputBulma {label = (Just x)} value = 
 node "div" [class__ "field"]
  [ node "label" [class__ "label"] [text x]
  , numberInputBulma {label = Nothing} value
  ]

export
selectBulma : Vect n String -> Maybe (Fin n) -> Widget (Fin n)
selectBulma xs z = 
  node "div" [class__ "select"]
    [ node "select" [value_ $ fromMaybe "" $ (\w => index w xs) <$> z, WidgetEventListener "change" change]
        (toList $ (\x => node "option" [WidgetSimpleAttribute (ValueAttr x )] [text x] ) <$> xs)
    ]
  where
    change : DomEvent -> IO (Fin n)
    change e =
      do 
        v <- target e >>= getValue 
        case elemIndex v xs of
             Nothing => throw "selectBulma internal error"
             Just x => pure x

multiSelectOnChange : WidgetAttribute (List Bool)
multiSelectOnChange = 
  WidgetEventListener "change" $ \e =>
    do
      t <- target e
      opts <- getOptions t
      traverse getSelected opts
                    
listToNVect : (n : Nat) -> List a -> Maybe (Vect n a)
listToNVect n xs = 
  case decEq n (length xs) of
       Yes prf => Just $ rewrite prf in fromList xs
       No _ => Nothing

export
multiSelectBulma : {n : Nat} -> Vect n String -> Maybe (Vect n Bool) -> Widget (Vect n Bool)
multiSelectBulma xs Nothing = 
  node "div" [class__ "select", class__ "is-multiple"]
    [ node "select" [multiple_ True, (fromMaybe (replicate n False) . listToNVect n) <$> multiSelectOnChange]
        (toList $ (\x => node "option" [WidgetSimpleAttribute (ValueAttr x )] [text x] ) <$> xs)
    ]
multiSelectBulma xs (Just vs) = 
  node "div" [class__ "select", class__ "is-multiple"]
    [ node "select" [multiple_ True, (fromMaybe (replicate n False) . listToNVect n) <$> multiSelectOnChange]
        (toList $ zipWith (\x, v => node "option" [WidgetSimpleAttribute (ValueAttr x ), selected_ v] [text x] ) xs vs)
    ]

export
fieldsSection : String -> List (Widget a) -> Widget a
fieldsSection x xs = 
  node "div" [] [
    node "label" [class__ "label"] [text x],
    node "div" [class__ "ml-6"] xs
  ]

export
labeled : String -> Widget a -> Widget a
labeled str x = 
  node "div" [] [
    node "label" [class__ "label"] [text str],
    x
  ]

export
formBulma : (a -> Widget a) -> a -> Widget (Maybe a)
formBulma f v0 = 
  loopState
    v0
    (\x => node "form" 
             [WidgetEventListener "submit" (\e =>do preventDefault e; pure $ Right $ Just x)]
             [Left <$> f x, buttons x] 
    )
  where
    buttons  : a -> Widget (Either a (Maybe a))  
    buttons x  = node "div" [class__ "field", class__ "is-grouped"]
                  [ node "div" [class__ "control"] 
                       [node "button" [ type__ "submit" 
                                      , class__ "button"
                                      , class__ "is-link"
                                      ]
                                      [text "Submit"]] 
                  , node "div" [class__ "control"] 
                       [node "button" [ type__ "button"
                                      , class__ "button"
                                      , class__ "is-link"
                                      , class__ "is-light"
                                      , onClick_ (Right Nothing)
                                      ] 
                                      [text "Cancel"]] 
                  ]

export
box : List (Widget a) -> Widget a
box xs = node "div" [class__ "box"] xs

export
card : {default Nothing headerTitle : Maybe String} -> {default [] footerItems: List (Widget a)} -> List (Widget a) -> Widget a
card xs = 
  let header = case headerTitle of 
                    Nothing => []
                    Just str => [node "header" [class__ "card-header"] [node "p" [class__ "card-header-title"] [text str]]]
      content = case xs of
                     [] => []
                     o => [node "div" [class__ "card-content"] o]
      footer = case footerItems of
                    [] => []
                    o => [node "footer" [class__ "card-footer"] ((\x => node "span" [class__ "card-footer-item"] [x]) <$> footerItems) ]
  in node "div" [class__ "card"] (header ++ content ++ footer)

export
fasIconText : {default [] styleOptions : List BulmaStyleOption} -> {default Nothing onclick : Maybe a} -> String -> String -> Widget a
fasIconText icon str =
  let attrs = case onclick of
                   Nothing => []
                   Just x => [onClick_ x]
  in node "span" (class__ "icon-text" :: (attrs ++ optionsToAttributes styleOptions)) [
       node "span" [class__ "icon"] [node "i" [class__ "fas", class__ "fa-\{icon}"] []],
       node "span" [] [text str]
     ]


export
fasIcon : {default [] styleOptions : List BulmaStyleOption} -> {default Nothing onclick : Maybe a} -> String -> Widget a
fasIcon icon =
  let attrs = case onclick of
                   Nothing => []
                   Just x => [onClick_ x]
  in node "span" (class__ "icon" :: (attrs ++ optionsToAttributes styleOptions)) [node "i" [class__ "fas", class__ "fa-\{icon}"] []]
