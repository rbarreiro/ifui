module Ifui.ReadWidgetBulma

import public Ifui.ReadWidgetBulmaPureExp 

test : Widget (Maybe (Tree [("Record", \w => List (String, w)), ("String", const ()), ("HowMany", \w => (String, w))]))
test = getFormBulma

test2 : Widget (Maybe (Record [("id", String),("spec", PTy)]))
test2 = getFormBulma


