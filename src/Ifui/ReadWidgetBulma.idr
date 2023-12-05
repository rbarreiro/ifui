module Ifui.ReadWidgetBulma

import public Ifui.ReadWidgetBulmaPureExp 

export
readerForm : {default Nothing startVal : Maybe a} -> (Maybe a -> Reader a)  -> Widget (Maybe a)
readerForm reader = 
  loopState 
    (False, reader startVal)
    (\(check, x) => do
             res <- formBulma (\z => getWidget z check) x
             case res of
                  Nothing => 
                     pure $ Right Nothing
                  Just r =>
                     case getValue r of
                          Nothing => pure $ Left (True ,r)
                          Just v =>  pure $ Right $ Just v
    )

export
getFormBulma : ReadWidgetBulma a => {default Nothing startVal : Maybe a} -> Widget (Maybe a)
getFormBulma = 
  readerForm getReaderBulma 

test : Widget (Maybe (Tree [("Record", \w => List (String, w)), ("String", const ()), ("HowMany", \w => (String, w))]))
test = getFormBulma

test2 : Widget (Maybe (Record [("id", String),("spec", PTy)]))
test2 = getFormBulma


