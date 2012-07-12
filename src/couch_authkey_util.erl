-module(couch_authkey_util).

-export([get_value/2, get_value/3]).

-include_lib("couch/include/couch_db.hrl").

get_value(Key, List) ->
    get_value(Key, List, undefined).

get_value(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
    {value, {Key,Value}} ->
        Value;
    false ->
        Default
    end.
