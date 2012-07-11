-module(couch_authkey_handler).

-export([key_authentification_handler/1]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_httpd/include/couch_httpd.hrl").


key_authentification_handler(Req) ->
    AutKeyParam = couch_config:get("couch_authkey", "auth_key_param",
                                   "auth_key"),
    % priority to the query string
    case couch_httpd:qs_value(Req, AutKeyParam) of
        undefined ->
            XKey = couch_config:get("couch_authkey", "x_auth_key",
                                    "X-Auth-Key"),
            case couch_httpd:header_value(Req, XKey) of
                undefined ->
                    Req;
                HdrKey ->
                    validate_key(Req, HdrKey)

            end;
        Key ->
            validate_key(Req, Key)
    end.

%% TODO: add validation against IP & Domain.
validate_key(Req, Key) ->
    case couch_authkey_api:get_key(Key) of
        nil ->
            throw({unauthorized, <<"invalid key">>});
        KeyProps ->
            ok = validate_expires(KeyProps),
            Roles = couch_util:get_value(<<"roles">>, KeyProps, []),
            Req#httpd{user_ctx=#user_ctx{name=?l2b(Key), roles=Roles}}
    end.

validate_expires(KeyProps) ->
    case couch_util:get_value(<<"expires">>, KeyProps) of
        undefined ->
            ok;
        Expires ->
            CurrentTime = get_unix_timestamp(erlang:now()),
            if CurrentTime > Expires ->
                    throw({unauthorized, <<"key expired">>});
                    true -> ok
            end
    end.


get_unix_timestamp({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs*1000000+Secs.
