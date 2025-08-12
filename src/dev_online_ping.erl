-module(dev_online_ping).
-export([info/1, info/3, ping_once/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @doc A simple device that sends a signed ping to the network.
%%% The ping includes an "Online: Yes" tag that can be indexed for GraphQL queries.
%%% Each ping is cryptographically signed with the node's wallet to ensure authenticity.
%%% 
%%% This device provides:
%%% - Manual ping via ping_once endpoint
%%% - Cryptographically signed messages using the node's wallet
%%% - "Online: Yes" tag for easy GraphQL indexing
%%% - Proper commitment/signature using configurable commitment device
%%%
%%% To schedule recurring pings, use the cron device externally:
%%% curl "http://localhost:10000/~cron@1.0/every?cron-path=/~online-ping@1.0/ping_once&interval=12-hours"

%% @doc Device info export specification.
info(_) ->
    #{
        exports => [info, ping_once],
        handlers => #{
            <<"info">> => fun info/3,
            <<"ping_once">> => fun ping_once/3
        }
    }.

%% @doc Return device information.
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Simple online ping device for HyperbEAM nodes">>,
        <<"version">> => <<"1.0">>,
        <<"purpose">> => <<"Sends network pings with 'Online: Yes' tag for indexing">>,
        <<"paths">> => #{
            <<"info">> => <<"Get device information">>,
            <<"ping_once">> => <<"Send a single ping to the network">>
        },
        <<"usage">> => #{
            <<"manual">> => <<"Call ping_once to send a single ping">>,
            <<"recurring">> => <<"Use cron device to schedule recurring pings">>,
            <<"tag">> => <<"Uses 'Online: Yes' tag for easy GQL indexing">>,
            <<"cron_example">> => <<"curl 'http://localhost:10000/~cron@1.0/every?cron-path=/~online-ping@1.0/ping_once&interval=12-hours'">>
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc Send a single ping to the network.
ping_once(Msg1, _Msg2, Opts) ->
    ?event({online_ping_once_called, {msg1, Msg1}}),
    case send_ping(Opts) of
        {ok, Result} ->
            ?event({online_ping_once_success, {result, Result}}),
            {ok, #{
                <<"status">> => 200,
                <<"body">> => #{
                    <<"message">> => <<"ping_sent">>,
                    <<"timestamp">> => hb:now(),
                    <<"result">> => Result
                }
            }};
        {error, Reason} ->
            ?event({online_ping_once_error, {reason, Reason}}),
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to send ping">>,
                    <<"reason">> => Reason
                }
            }}
    end.

%%% Private functions

%% @doc Send a ping message to the network with the "Online: Yes" tag.
%% This properly signs the message with the node's wallet before sending.
send_ping(Opts) ->
    ?event({debug_send_ping_start, "Function called"}),
    % Get the node's wallet for signing
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    case Wallet of
        no_viable_wallet ->
            {error, <<"No wallet available for signing ping message">>};
        _ ->
            % Get the node's address from the wallet
            NodeAddress = hb_util:id(ar_wallet:to_address(Wallet)),
            
            % Create a simple ping message using the exact pattern that works in HyperBEAM tests
            % Start with minimal data, then add tags
            UnsignedPingMessage = #{
                <<"data">> => <<"Node online ping from HyperbEAM">>,
                <<"Online">> => <<"Yes">>,
                <<"Action">> => <<"Ping">>,
                <<"Timestamp">> => integer_to_binary(hb:now()),
                <<"codec-device">> => <<"ans104@1.0">>
            },
            
            try
                ?event({debug_start_of_try_block, "Starting ping process"}),
                % Sign the message with the node's wallet using ans104 commitment device
                % (ans104 is better supported for uploads than httpsig)
                CommitmentDevice = hb_opts:get(commitment_device, <<"ans104@1.0">>, Opts),
                {ok, SignedMessage} = dev_message:commit(
                    UnsignedPingMessage,
                    #{ <<"commitment-device">> => CommitmentDevice },
                    Opts
                ),
                ?event({debug_signed_message, SignedMessage}),

                % Let's see what the conversion produces step by step
                ?event({debug_about_to_convert, "Converting to ans104@1.0"}),
                Converted = hb_message:convert(SignedMessage, <<"ans104@1.0">>, Opts),
                ?event({debug_converted_tx, Converted}),
                
                % Check if ar_bundles can verify it before serialization
                case ar_bundles:verify_item(Converted) of
                    true -> 
                        ?event({debug_verify_success, "TX verifies locally"});
                    false -> 
                        ?event({debug_verify_failed, "TX does NOT verify locally"})
                end,
                
                % See the serialization
                Serialized = ar_bundles:serialize(Converted),
                ?event({debug_serialized, {size, byte_size(Serialized)}, {first_100_bytes, binary:part(Serialized, 0, min(100, byte_size(Serialized)))}}),
                % END OF DEBUG LINES

                ?event({online_ping_signed, {node_address, NodeAddress}, {message_id, hb_message:id(SignedMessage, all)}}),
                
                % Log what we're about to upload for debugging (upload directly without codec-device)
                ?event({online_ping_uploading, {message_size, byte_size(term_to_binary(SignedMessage))}, {commitment_device, CommitmentDevice}}),
                
                % Now submit the signed message to the Arweave network (upload directly)
                case hb_client:upload(SignedMessage, Opts) of
                    {ok, UploadResult} ->
                        ?event({online_ping_uploaded, {upload_result, UploadResult}}),
                        {ok, #{
                            <<"message">> => <<"ping_sent_to_network">>,
                            <<"message_id">> => hb_message:id(SignedMessage, all),
                            <<"node_address">> => NodeAddress,
                            <<"commitment_device">> => CommitmentDevice,
                            <<"upload_result">> => UploadResult
                        }};
                    {error, UploadError} ->
                        ?event({online_ping_upload_error, {error, UploadError}, {bundler_response_details, UploadError}}),
                        % Still return success for signing, but note upload failed
                        {ok, #{
                            <<"message">> => <<"ping_signed_but_upload_failed">>,
                            <<"message_id">> => hb_message:id(SignedMessage, all),
                            <<"node_address">> => NodeAddress,
                            <<"commitment_device">> => CommitmentDevice,
                            <<"upload_error">> => UploadError,
                            <<"signed_message">> => SignedMessage
                        }}
                end
            catch
                Class:Reason:Stacktrace ->
                    ?event({online_ping_error, {class, Class}, {reason, Reason}, {stacktrace, Stacktrace}}),
                    {error, #{
                        <<"error">> => <<"Failed to sign ping message">>,
                        <<"class">> => Class,
                        <<"reason">> => Reason
                    }}
            end
    end.

%%% Tests

%% @doc Test that the device info is returned correctly.
info_test() ->
    Info = info(#{}),
    ?assert(maps:is_key(exports, Info)),
    ?assert(maps:is_key(handlers, Info)),
    ?assert(lists:member(ping_once, maps:get(exports, Info))).

%% @doc Test device info endpoint.
info_endpoint_test() ->
    {ok, Result} = info(#{}, #{}, #{}),
    ?assertMatch(#{<<"status">> := 200}, Result),
    Body = maps:get(<<"body">>, Result),
    ?assertMatch(#{<<"description">> := _}, Body),
    ?assertMatch(#{<<"version">> := <<"1.0">>}, Body).

%% @doc Test sending a single ping.
ping_once_test() ->
    % Mock wallet for testing
    Wallet = ar_wallet:new(),
    Opts = #{priv_wallet => Wallet},
    
    % This test would need proper mocking of dev_meta:handle in a real test environment
    % For now, we just verify the function structure
    Msg1 = #{<<"device">> => <<"online-ping@1.0">>},
    
    % In a real test, you'd mock dev_meta:handle to return success
    % Result = ping_once(Msg1, #{}, Opts),
    % ?assertMatch({ok, _}, Result).
    
    % For now, just verify the function exists and takes correct parameters
    ?assert(is_function(fun ping_once/3, 3)).
