module Ifui.Bulma

import Data.Vect
import Ifui.Widget

class__ : String -> WidgetAttribute b
class__ x = WidgetSimpleAttribute $ CSSClassAttr x

type__ : String -> WidgetAttribute b
type__ x = WidgetSimpleAttribute $ StringAttr "type" x

onChange_ : WidgetAttribute String
onChange_ = WidgetEventListener "change" (\e => targetValue e)

value_ : String -> WidgetAttribute b
value_ x = WidgetSimpleAttribute $ ValueAttr x

export
data Column a = MkColumn (Widget a)  

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
selectBulma : Vect n String -> Maybe (Fin n) -> Widget (Fin n)
selectBulma xs z = 
  node "div" [class__ "select", value_ (fromMaybe "" $ (\w => index w xs) <$> z), WidgetEventListener "change" change]
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
