module Schema

import IfuiServer.RethinkDB

public export
Schema : ServerSchema 
Schema = [MkTableSchema "todoApp" "todoItem" String  [("desc", String)]]

