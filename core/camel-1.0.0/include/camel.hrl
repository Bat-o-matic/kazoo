-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-type camel_result() :: {'ok', term()}.
-type camel_json() :: term().

-type camel_failures() :: authentication |
                              authorization |
                              not_found |
                              upgrade_required |
                              server_error |
                              maintenance |
                              io_fault |
                              api_error.

-define(CAMEL_DEBUG, whapps_config:get_is_true(<<"camel">>, <<"debug">>, 'false')).

-define(CAMEL_SERVER_URL, whapps_config:get_binary(<<"camel">>, <<"url">>, <<"localhost:9091/chump">>)).
-define(CAMEL_SERVER_VERSION, whapps_config:get_binary(<<"camel">>, <<"version">>, <<"v1">>)).

-record(camel_cdr, {call_id :: api_binary()
                    ,call_direction :: api_binary() %%% ?
                    ,billing_party :: api_binary()
                    ,other_party :: api_binary()
                    ,date :: api_binary()
                    ,time :: api_binary()
                    ,duration :: api_binary()
                    ,price :: api_binary() %%% ?
                   }).
-type camel_cdr() :: #camel_cdr{}.
-type camel_cdrs() :: [camel_cdr(),...] | [].

%% Transforms Camel parameter names to Kazoo names.
-define(CAMEL_CDR_TRANSFORM, [{<<"AcctSessionId">>, <<"call_id">>}
                              ,{<<"AcctStartTime">>, <<"start_time">>}
                              ,{<<"AcctStopTime">>, <<"stop_time">>}
                              ,{<<"AcctSessionTime">>, <<"duration">>}
                              ,{<<"Price">>, <<"price">>}
                             ]).

%% @todo Get subscription field names.
-record(camel_subscription, {id :: api_binary()
                            }).
-type camel_subscription() :: #camel_subscription{}.
-type camel_subscriptions() :: [camel_subscription(),...] | [].

-record(camel_response, {code :: non_neg_integer()
                         ,message :: api_binary()
                         ,data :: term()
                        }).
-type camel_response() :: #camel_response{}.
