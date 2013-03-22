-module(list_to_record).
-compile(export_all).
-record(item,{id,name,owner}).

list_to_record([X1|[X2|[X3]]]) -> {#item{id=X1,name=X2,owner=X3}}.
