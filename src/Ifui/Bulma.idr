module Ifui.Bulma

import Data.Vect
import Ifui.Widget
import Ifui.Patterns

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
div : {default [] styleOptions : List BulmaStyleOption} -> List (Widget a) -> Widget a
div xs = node "div" (optionsToAttributes styleOptions) xs

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
selectBulma : Vect (S n) String -> Fin (S n) -> Widget (Fin (S n))
selectBulma xs z = 
  node "div" [class__ "select", value_ $ index z xs, WidgetEventListener "change" change]
    [ node "select" []
        (toList $ (\x => node "option" [WidgetSimpleAttribute (StringAttr "value" x )] [text x] ) <$> xs)
    ]
  where
    change : DomEvent -> IO (Fin (S n))
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

