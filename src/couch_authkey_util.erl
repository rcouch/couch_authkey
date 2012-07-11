-module(couch_authkey_util).

-export([ensure_exists/1,
         get_value/2, get_value/3]).

-include_lib("couch/include/couch_db.hrl").

ensure_exists(DbName) when is_list(DbName) ->
    ensure_exists(list_to_binary(DbName));
ensure_exists(DbName) ->
    Options = [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}],
    case couch_db:open_int(DbName, Options) of
    {ok, Db} ->
        {ok, Db};
    _ ->
        couch_server:create(DbName, Options)
    end.

get_value(Key, List) ->
    get_value(Key, List, undefined).

get_value(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
    {value, {Key,Value}} ->
        Value;
    false ->
        Default
    end.
