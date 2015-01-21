%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(camel_request).

-export([get/1]).
-export([post/2]).
-export([put/2]).
-export([delete/1]).

-include_lib("camel/include/camel.hrl").

-type http_verb() :: 'put' | 'post' | 'get' | 'delete'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a get request to camel's system
%% @end
%%--------------------------------------------------------------------
-spec get/1 :: (nonempty_string()) -> camel_json().
get(Path) ->
    do_request(get, Path, <<>>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a post request to camel's system
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (nonempty_string(), binary()) -> camel_json().
post(Path, Request) ->
    do_request(post, Path, Request).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a put request to camel's system
%% @end
%%--------------------------------------------------------------------
-spec put/2 :: (nonempty_string(), binary()) -> camel_json().
put(Path, Request) ->
    do_request(put, Path, Request).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a delete request to camel's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (nonempty_string()) -> camel_json().
delete(Path) ->
    do_request(delete, Path, <<>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Preform a request to the camel service
%% @end
%%--------------------------------------------------------------------
-spec do_request/3 :: (http_verb(), nonempty_string(), binary()) -> camel_json().
do_request(Method, Path, Body) ->
    StartTime = erlang:now(),
    lager:debug("making ~s request to camel ~s", [Method, Path]),
    Url = lists:flatten(["http://"
                         ,?CAMEL_SERVER_URL
                         ,"/"
                         ,?CAMEL_SERVER_VERSION
                         ,Path
                        ]),
    Headers = [{"Accept", "application/json"}
               ,{"User-Agent", "camel Erlang Library 1"}
               ,{"Content-Type", "application/json"}],
    HTTPOptions = [{ssl,[{verify,0}]}
                   ,{basic_auth, {whapps_config:get_string(<<"camel">>, <<"default_public_key">>, <<>>)
                                  ,whapps_config:get_string(<<"camel">>, <<"default_private_key">>, <<>>)}}
                  ],
    verbose_debug("Request:~n~s ~s~n~s~n", [Method, Url, Body]),
    case ibrowse:send_req(Url, Headers, Method, Body, HTTPOptions) of
        {ok, "401", _, _Response} ->
            verbose_debug("Response:~n401~n~s~n", [_Response]),
            lager:debug("camel error response(~pms): 401 Unauthenticated", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            camel_util:error_authentication();
        {ok, "403", _, _Response} ->
            verbose_debug("Response:~n403~n~s~n", [_Response]),
            lager:debug("camel error response(~pms): 403 Unauthorized", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            camel_util:error_authorization();
        {ok, "404", _, _Response} ->
            verbose_debug("Response:~n404~n~s~n", [_Response]),
            lager:debug("camel error response(~pms): 404 Not Found", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            camel_util:error_not_found(<<>>);
        {ok, "426", _, _Response} ->
            verbose_debug("Response:~n426~n~s~n", [_Response]),
            lager:debug("camel error response(~pms): 426 Upgrade Required", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            camel_util:error_upgrade_required();
        {ok, "500", _, _Response} ->
            verbose_debug("Response:~n500~n~s~n", [_Response]),
            lager:debug("camel error response(~pms): 500 Server Error", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            camel_util:error_server_error();
        {ok, "503", _, _Response} ->
            verbose_debug("Response:~n503~n~s~n", [_Response]),
            lager:debug("camel error response(~pms): 503 Maintenance", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            camel_util:error_maintenance();
        {ok, Code, _, Response} ->
            verbose_debug("Response:~n~p~n~s~n", [Code, Response]),
            lager:debug("braintree JSON response(~pms)", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            Response;
        {error, _R} ->
            verbose_debug("Response:~nerror~n~p~n", [_R]),
            lager:debug("camel request error(~pms): ~p", [timer:now_diff(erlang:now(), StartTime) div 1000, _R]),
            camel_util:error_io_fault()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If camel verbose debuging is enabled write the log line to the file
%% @end
%%--------------------------------------------------------------------
-spec verbose_debug/2 :: (string(), [term()]) -> 'ok'.
verbose_debug(Format, Args) ->
    case ?CAMEL_DEBUG of
        false -> ok;
        true -> 
            _ = file:write_file("/tmp/camel.xml", io_lib:format(Format, Args), [append]),
            ok
    end.
