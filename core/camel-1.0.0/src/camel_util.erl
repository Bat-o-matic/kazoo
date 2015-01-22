%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(camel_util).

-export([response_to_json/1]).
-export([response_from_json/1]).

-export([error_authentication/0]).
-export([error_authorization/0]).
-export([error_not_found/1]).
-export([error_upgrade_required/0]).
-export([error_server_error/0]).
-export([error_maintenance/0]).
-export([error_api/1]).
-export([error_io_fault/0]).

-include_lib("camel/include/camel.hrl").

%% from stdlib/src/unicode.erl
-type char_to_bin_res() :: binary() |
                           {'error', binary(), unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata()} |
                           {'incomplete', binary(), binary()}.
-export_type([char_to_bin_res/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec response_to_json/1 :: (#camel_response{}) -> wh_json:object().
response_to_json(#camel_response{}=CamelResp) ->
    Props = [{<<"code">>, CamelResp#camel_response.code}
             ,{<<"message">>, CamelResp#camel_response.message}
             ,{<<"data">>, CamelResp#camel_response.data}
            ],
    wh_json:from_list([KV || {_, V}=KV <- Props, V =/= undefined]).

-spec response_from_json/1 :: (#camel_response{}) -> wh_json:object().
response_from_json(Resp) ->
    #camel_response{code = wh_json:get_value(<<"code">>, Resp)
                    ,message = wh_json:get_value(<<"code">>, Resp)
                    ,data = wh_json:get_value(<<"data">>, Resp)
                   }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec error_authentication/0 :: () -> no_return().
error_authentication() ->
    Error = <<"Failed to authenticate with the card processor">>,
    lager:debug("~s", [Error]),
    throw({authentication, wh_json:from_list([{<<"authentication">>, Error}])}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec error_authorization/0 :: () -> no_return().
error_authorization() ->
    Error = <<"Failed to authorize with the card processor">>,
    lager:debug("~s", [Error]),
    throw({authorization, wh_json:from_list([{<<"authorization">>, Error}])}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec error_not_found/1 :: (ne_binary()) -> no_return().
error_not_found(Object) ->
    Error = <<Object/binary, " not found">>,
    lager:debug("~s", [Error]),
    throw({not_found, wh_json:from_list([{<<"not_found">>, Error}])}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec error_upgrade_required/0 :: () -> no_return().
error_upgrade_required() ->
    Error = <<"Card processor requires API library upgrade">>,
    lager:debug("~s", [Error]),
    throw({upgrade_required, wh_json:from_list([{<<"upgrade_required">>, Error}])}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec error_server_error/0 :: () -> no_return().
error_server_error() ->
    Error = <<"Card processor server error">>,
    lager:debug("~s", [Error]),
    throw({server_error, wh_json:from_list([{<<"server_error">>, Error}])}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec error_maintenance/0 :: () -> no_return().
error_maintenance() ->
    Error = <<"Card processor currently down for maintenance">>,
    lager:debug("~s", [Error]),
    throw({maintenance, wh_json:from_list([{<<"maintenance">>, Error}])}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec error_io_fault/0 :: () -> no_return().
error_io_fault() ->
    Error = <<"Unable to establish communication with card processor">>,
    lager:debug("~s", [Error]),
    throw({io_fault, wh_json:from_list([{<<"io_fault">>, Error}])}).

-spec response_error/1 :: (camel_response()) -> no_return().
response_error(Resp) ->
    Error = Resp#camel_response.message,
    lager:debug("Camel response error ~s", [Error]),
    throw({response_error, wh_json:from_list([{<<"response_error">>, Resp#camel_response.message}])}).
