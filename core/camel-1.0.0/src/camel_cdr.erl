%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(camel_cdr).

-export([url/0]).
-export([new/1]).
-export([get_id/1]).
-export([search/1]).
-export([is_camel_cdr/1]).
-export([json_to_record/1]).
-export([record_to_json/1, record_to_json/2]).

-export([transform_cdr/1]).

-include_lib("camel/include/camel.hrl").

-type cdr() :: #camel_cdr{}.
-type cdrs() :: [cdr(),...] | [].
-export_type([cdr/0
              ,cdrs/0
             ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create the partial url for this module
%% @end
%%--------------------------------------------------------------------
-spec url() -> string().

url() ->
    "/callrecord/".

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new cdr record
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary()) -> cdr().
new(CdrId) ->
    #camel_cdr{call_id=CdrId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the cdr id
%% @end
%%--------------------------------------------------------------------
-spec get_id(cdr()) -> api_binary().
get_id(CdrId) ->
    CdrId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a cdr by id
%% @end
%%--------------------------------------------------------------------
-spec search(ne_binaries()) -> cdrs().
search(CdrIds) ->
    Url = url() ++ "search",
    Ids = wh_json:encode({[{"ids", CdrIds}]}),
    case camel_request:post(Url, Ids) of
        #camel_response{code = 200, data = Data} ->
            [json_to_record(transform_cdr(JObj)) || JObj <- Data];
        #camel_response{} = Resp ->
            camel_util:response_error(Resp)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given json object into a record
%% @end
%%--------------------------------------------------------------------
is_camel_cdr(#camel_cdr{}=Cdr)->'true';
is_camel_cdr(_)->'false'.

%% Need to convert parameter names
-spec json_to_record(api_object()) -> cdr().
json_to_record('undefined') -> #camel_cdr{};
json_to_record(JObj) ->
    #camel_cdr{call_id = wh_json:get_binary_value(<<"call_id">>, JObj)
               ,call_direction = wh_json:get_binary_value(<<"call_direction">>, JObj)
               ,billing_party = wh_json:get_binary_value(<<"billing_party">>, JObj)
               ,other_party = wh_json:get_binary_value(<<"other_party">>, JObj)
               ,date = wh_json:get_binary_value(<<"date">>, JObj)
               ,time = wh_json:get_binary_value(<<"time">>, JObj)
               ,duration = wh_json:get_binary_value(<<"duration">>, JObj)
               ,price = wh_json:get_binary_value(<<"price">>, JObj)
              }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json(cdr()) -> wh_json:object().
-spec record_to_json(cdr(), boolean()) -> wh_json:object() | text().
record_to_json(Cdr) -> record_to_json(Cdr, 'false').

record_to_json(Cdr, ToString) ->
    Props = [{<<"call_id">>, Cdr#camel_cdr.call_id}
             ,{<<"call_direction">>, Cdr#camel_cdr.call_direction}
             ,{<<"billing_party">>, Cdr#camel_cdr.billing_party}
             ,{<<"other_party">>, Cdr#camel_cdr.other_party}
             ,{<<"start_time">>, Cdr#camel_cdr.time}
             ,{<<"stop_time">>, Cdr#camel_cdr.time}
             ,{<<"duration">>, Cdr#camel_cdr.duration}
             ,{<<"price">>, Cdr#camel_cdr.price}
            ],
    Json = wh_json:from_list(props:filter_undefined(Props)),
    case ToString of
        'true' ->
            wh_json:encode(Json);
        'false' ->
            Json
    end.

-spec transform_cdr(wh_json:object()) -> wh_json:object().
transform_cdr(Cdr) ->
    [Called|_] = re:split(wh_json:get_value(<<"CalledStationId">>, Cdr, <<"anonymous">>), "@"),
    [Calling|_] = re:split(wh_json:get_value(<<"CallingStationId">>, Cdr, <<"anonymous">>), "@"),
    Props = [{<<"billing_party">>, Calling}
             ,{<<"other_party">>, Called}
            ],
    New = lists:foldl(fun({OldK, NewK}, Acc) ->
                    wh_json:set_value(NewK, wh_json:get_value(OldK, Cdr), Acc)
                end, wh_json:from_list(Props), ?CAMEL_CDR_TRANSFORM),
    case re:split(wh_json:get_value(<<"Realm">>, Cdr, <<"conversant.co.nz">>), <<"\.">>) of
        [<<"inbound">>|_] -> wh_json:set_value(<<"call_direction">>, <<"inbound">>, New);
        _ -> wh_json:set_value(<<"call_direction">>, <<"outbound">>, New)
    end.
