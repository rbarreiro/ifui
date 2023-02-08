module Ifui.Bulma

import Data.Vect
import Ifui.Widget
import Ifui.Patterns
import Data.List

class__ : String -> WidgetAttribute b
class__ x = WidgetSimpleAttribute $ CSSClassAttr x

type__ : String -> WidgetAttribute b
type__ x = WidgetSimpleAttribute $ StringAttr "type" x

onChange_ : WidgetAttribute String
onChange_ = WidgetEventListener "change" (\e => targetValue e)

onClick_ : a -> WidgetAttribute a
onClick_ x = WidgetEventListener "click" (\e => pure x)

value_ : String -> WidgetAttribute b
value_ x = WidgetSimpleAttribute $ ValueAttr x

export
data Column a = MkColumn (Widget a)  

export
data BulmaStyleOption = MkBulmaStyleOption String

export 
ml : Fin 7 -> BulmaStyleOption
ml z = MkBulmaStyleOption $ "ml-" ++ show z

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
    nitem x = node "div" [class__ "navbar-item"] [x]

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
  node "div" [class__ "control", value_ value, onChange_] [node "input" [type__ "text", class__ "input"] []]
textInputBulma {label = (Just x)} value = 
 node "div" [class__ "field"]
  [ node "label" [class__ "label"] [text x]
  , textInputBulma {label = Nothing} value
  ]

export
numberInputBulma : {default Nothing label : Maybe String} ->
                    Maybe Double -> Widget (Maybe Double)
numberInputBulma {label = Nothing} value  = 
  node "div" [ class__ "control"
             , value_ $ fromMaybe "" $  cast <$> value
             , (\x => if x == "" then Nothing else Just (cast x)) <$> onChange_
             ] 
             [node "input" [type__ "number", class__ "input"] []]
numberInputBulma {label = (Just x)} value = 
 node "div" [class__ "field"]
  [ node "label" [class__ "label"] [text x]
  , numberInputBulma {label = Nothing} value
  ]

export
selectBulma : Vect n String -> Maybe (Fin n) -> Widget (Fin n)
selectBulma xs z = 
  node "div" [class__ "select", value_ $ fromMaybe "" $ (\w => index w xs) <$> z, WidgetEventListener "change" change]
    [ node "select" []
        (toList $ (\x => node "option" [WidgetSimpleAttribute (StringAttr "value" x )] [text x] ) <$> xs)
    ]
  where
    change : DomEvent -> IO (Fin n)
    change e =
      do 
        v <- targetValue e
        case elemIndex v xs of
             Nothing => throw "selectBulma internal error"
             Just x => pure x

export
fieldsSection : String -> List (Widget a) -> Widget a
fieldsSection x xs = 
  node "div" [] [
    node "label" [class__ "label"] [text x],
    node "div" [class__ "ml-3"] xs
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
                       [node "button" [ class__ "button"
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
fasIconText : {default Nothing onclick : Maybe a} -> String -> String -> Widget a
fasIconText icon str =
  let attrs = case onclick of
                   Nothing => []
                   Just x => [onClick_ x]
  in node "span" (class__ "icon-text" :: attrs) [
       node "span" [class__ "icon"] [node "i" [class__ "fas", class__ icon] []],
       node "span" [] [text str]
     ]
