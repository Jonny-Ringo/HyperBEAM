%%% @doc The green zone device, which provides secure communication and identity
%%% management between trusted nodes. It handles node initialization, joining
%%% existing green zones, key exchange, and node identity cloning. All operations
%%% are protected by hardware attestation and encryption.
-module(dev_green_zone).
-export([join/3, init/3, become/3, key/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% @doc Initialize the green zone.
%% Sets up the node's cryptographic identity by ensuring that a wallet (keypair)
%% exists and generating a shared AES key for secure communication. The wallet,
%% AES key, and an empty trusted nodes list are stored in the node's configuration.
%% @param M1 Ignored parameter.
%% @param M2 Ignored parameter.
%% @param Opts A map containing configuration options. If the wallet is not already
%%             provided (under key `priv_wallet'), a new one will be created.
%% @returns {ok, Msg} where Msg is a binary confirmation message.
-spec init(M1 :: term(), M2 :: term(), Opts :: map()) -> {ok, binary()}.
init(_M1, _M2, Opts) ->
    ?event(green_zone, {init, start}),
    % Check if a wallet exists; create one if absent.
    NodeWallet = case hb_opts:get(priv_wallet, undefined, Opts) of
        undefined -> 
            ?event(green_zone, {init, wallet, missing}),
            hb:wallet();
        ExistingWallet ->
            ?event(green_zone, {init, wallet, found}),
            ExistingWallet
    end,
    % Generate a new 256-bit AES key if we have not already joined
	% a green zone.
    GreenZoneAES =
		case hb_opts:get(priv_green_zone_aes, undefined, Opts) of
			undefined ->
				?event(green_zone, {init, aes_key, generated}),
				crypto:strong_rand_bytes(32);
			ExistingAES ->
				?event(green_zone, {init, aes_key, found}),
				ExistingAES
		end,
    ?event(green_zone, {init, aes_key, generated}),
    % Store the wallet, AES key, and an empty trusted nodes map.
    ok = hb_http_server:set_opts(Opts#{
        priv_wallet => NodeWallet,
        priv_green_zone_aes => GreenZoneAES,
        trusted_nodes => #{}
    }),
    ?event(green_zone, {init, complete}),
    {ok, <<"Green zone initialized successfully.">>}.

%% @doc Join an existing green zone.
%% Processes a join request by:
%%   1. Extracting the attestation report, node address, and public key.
%%   2. Verifying the attestation report.
%%   3. Generating (or reusing) the shared AES key.
%%   4. Updating the trusted nodes list with the joining node's details.
%%   5. Encrypting the shared AES key with the joining node's public key.
%%
%% @param M1 The join request message containing attestation details.
%% @param M2 Ignored parameter.
%% @param Opts A map of configuration options.
%% @returns {ok, Map} on success, where Map includes keys: status, message,
%%          node_address, encrypted_payload, and public_key.
%%          Returns {error, Reason} if the attestation fails.
-spec join(M1 :: term(), M2 :: term(), Opts :: map()) -> {ok, map()} | {error, binary()}.
join(M1, M2, Opts) ->
    ?event(green_zone, {join, start}),
	Peer = hb_converge:get(<<"node">>, M1, undefined, Opts),
	?event(green_zone, {join, peer, Peer}),
	case Peer of
		undefined ->
			validate_join(M1, M2, Opts);
		Peer_ ->
			join_peer(Peer_, M1, M2, Opts)
	end.

join_peer(Peer, M1, _M2, Opts) ->
	% TODO: Guard for:
	% 1. Joining a zone after the node has already joined.
	% 2. That `physical-operator' is the caller.
	% Generate an attestation report and send it to the peer.
	{ok, Report} = dev_snp:generate(#{}, #{}, Opts),
	MergedReq = maps:merge(Report, #{
		<<"address2">> => hb_converge:get(<<"address">>, M1, Opts),
		<<"attestors">> => <<"none">>,
		<<"public-key">> => base64:encode(term_to_binary(element(2, hb_opts:get(priv_wallet, undefined, Opts))))
	}),
	Req = hb_message:attest(MergedReq, hb_opts:get(priv_wallet, undefined, Opts)),
	?event(green_zone, {join, sending_attestation_report, Peer, Req}),
	case hb_http:post(Peer, <<"/~greenzone@1.0/join">>, Req, Opts) of
		{ok, Resp} ->
			?event(green_zone, {join, join_response, Peer, Resp}),
			ZoneKey = hb_converge:get(<<"zone-key">>, Resp, Opts),
			PubKey = hb_converge:get(<<"public-key">>, Resp, Opts),
			{_LocalPubKey, LocalPrivKey} = hb_opts:get(priv_wallet, undefined, Opts),
			% Decrypt the zone key using the local node's private key.
			{ok, AESKey} = decrypt_zone_key(ZoneKey, Opts),
			hb_http_server:set_opts(Opts#{
				priv_green_zone_aes => AESKey
			}),
			{ok, <<"Node joined green zone successfully.">>};
		{error, Reason} ->
			{error, #{ status => 400, <<"reason">> => Reason }}
	end.

%% @doc Validate a join request.

validate_join(_M1, Req, Opts) ->
    % Extract the attestation report, node address, and node message.
    Report = hb_converge:get(<<"report">>, Req, Opts),
    NodeAddr = hb_converge:get(<<"address">>, Req, Opts),
    NodeMsg = hb_converge:get(<<"node-message">>, Req, Opts),
    ?event(green_zone, {join, extract, {node_addr, NodeAddr}}),
    % Extract and decode the public key.
    EncodedPubKey = hb_converge:get(<<"public-key">>, Req, Opts),
    RequesterPubKey = case EncodedPubKey of
        not_found -> not_found;
        Encoded -> binary_to_term(base64:decode(Encoded))
    end,
    ?event(green_zone, {join, public_key, ok}),
    % Verify the attestation report.
    case dev_snp:verify(Req, #{<<"target">> => <<"self">>}, Opts) of
        {ok, true} ->
            ?event(green_zone, {join, attestation, verified}),
            % Get or generate the shared AES key.
            GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
            % Retrieve the local node's wallet.
            {PubKey, _PrivKey} = hb_opts:get(priv_wallet, undefined, Opts),
			% Add the joining node's details to the trusted nodes.
			add_trusted_node(NodeAddr, Report, RequesterPubKey, Opts),
            ?event(green_zone, {join, update, trusted_nodes, ok}),
            % Encrypt the AES key with the requester's public key.
            EncryptedPayload = encrypt_payload(GreenZoneAES, RequesterPubKey),
            ?event(green_zone, {join, encrypt, aes_key, complete}),
            {ok, #{
                <<"status">> => <<"success">>,
                <<"message">> => <<"Node joined green zone successfully">>,
                <<"node-address">> => NodeAddr,
                <<"zone-key">> => base64:encode(EncryptedPayload),
                <<"public-key">> => PubKey
            }};
        {ok, false} ->
            ?event(green_zone, {join, attestation, failed}),
            {error, <<"Invalid attestation report">>};
        Error ->
            ?event(green_zone, {join, attestation, error, Error}),
            Error
    end.

add_trusted_node(NodeAddr, Report, RequesterPubKey, Opts) ->
	% Retrieve the current trusted nodes map.
	TrustedNodes = hb_opts:get(trusted_nodes, #{}, Opts),
	% Add the joining node's details to the trusted nodes.
	UpdatedTrustedNodes = maps:put(NodeAddr, #{
		report => Report,
		public_key => RequesterPubKey
	}, TrustedNodes),
	% Update configuration with the new trusted nodes and AES key.
	ok = hb_http_server:set_opts(Opts#{
		trusted_nodes => UpdatedTrustedNodes
	}).

%% @doc Encrypt the shared AES key with the requester's public RSA key.
%% This function encrypts the shared AES key using the RSA public key provided
%% by the joining node. The RSA public key is extracted from a tuple and converted
%% into a record suitable for the public_key module.
%% @param AESKey The shared AES key (256-bit binary).
%% @param RequesterPubKey The public RSA key of the requester.
%% @returns The AES key encrypted with the RSA public key.
-spec encrypt_payload(AESKey :: binary(), RequesterPubKey :: term()) -> binary().
encrypt_payload(AESKey, RequesterPubKey) ->
    ?event(green_zone, {encrypt_payload, start}),
    case RequesterPubKey of
        {_KeyType = {rsa, E}, Pub} ->
            % Convert modulus to integer and build the RSA public key record
            RsaPubKey = #'RSAPublicKey'{
                publicExponent = E,
                modulus = crypto:bytes_to_integer(Pub)
            },
            Encrypted = public_key:encrypt_public(AESKey, RsaPubKey),
            ?event(green_zone, {encrypt_payload, complete}),
            Encrypted
    end.

%% @doc Decrypt the zone AES key using the local node's private key.
%% @param EncZoneKey The encrypted zone AES key.
%% @param Opts The node message.
%% @returns {ok, Key} on success, where Key is the decrypted zone AES key.
decrypt_zone_key(EncZoneKey, Opts) ->
    % Decode if necessary
    RawEncKey = case is_binary(EncZoneKey) of
        true -> base64:decode(EncZoneKey);
        false -> EncZoneKey
    end,
    % Get wallet and extract key components
    {{_KeyType = {rsa, E}, Priv, Pub}, _PubKey} = hb_opts:get(priv_wallet, undefined, Opts),
    % Create RSA private key record
    RSAPrivKey = #'RSAPrivateKey'{
        publicExponent = E,
        modulus = crypto:bytes_to_integer(Pub),
        privateExponent = crypto:bytes_to_integer(Priv)
    },
    DecryptedKey = public_key:decrypt_private(RawEncKey, RSAPrivKey),
    ?event(green_zone, {decrypt_zone_key, complete}),
    {ok, DecryptedKey}.

%% @doc Retrieve and encrypt the node's private key.
%% This function encrypts the node's private key using the shared AES key
%% in AES-256-GCM mode. It returns the encrypted key along with the
%% initialization vector (IV) needed for decryption.
%% @param M1 Ignored parameter.
%% @param M2 Ignored parameter.
%% @param Opts A map of configuration options. Must include keys `priv_wallet'
%%             and `priv_green_zone_aes'.
%% @returns {ok, Map} on success, where Map contains:
%%           - status: "success"
%%           - encrypted_key: the encrypted private key (Base64 encoded)
%%           - iv: the initialization vector (Base64 encoded)
%%          Returns {error, Reason} if the node is not part of the green zone.
-spec key(M1 :: term(), M2 :: term(), Opts :: map()) -> {ok, map()} | {error, binary()}.
key(_M1, _M2, Opts) ->
    ?event(green_zone, {get_key, start}),
    % Retrieve the shared AES key and the node's wallet.
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    {{KeyType, Priv, Pub}, _PubKey} = hb_opts:get(priv_wallet, undefined, Opts),
    case GreenZoneAES of
        undefined ->
            ?event(green_zone, {get_key, error, "no aes key"}),
            {error, <<"Node not part of green zone">>};
        _ ->
            % Generate an IV and encrypt the private key in AES-256-GCM mode.
            IV = crypto:strong_rand_bytes(16),
            {EncryptedKey, Tag} = crypto:crypto_one_time_aead(
                aes_256_gcm,
                GreenZoneAES,
                IV,
                term_to_binary({KeyType, Priv, Pub}),
                <<>>,
                true
            ),
            ?event(green_zone, {get_key, encrypt, complete}),
            {ok, #{
                <<"status">> => <<"success">>,
                <<"encrypted_key">> => base64:encode(<<EncryptedKey/binary, Tag/binary>>),
                <<"iv">> => base64:encode(IV)
            }}
    end.

%% @doc Clone the identity of a target node.
%% Allows a node to adopt the identity of a target node by:
%%   1. Receiving the target node's encrypted private key and IV.
%%   2. Decrypting the private key using the shared AES key.
%%   3. Updating the local node's wallet with the target node's keypair.
%% @param M1 The message containing the target node's encrypted private key and IV.
%% @param M2 Ignored parameter.
%% @param Opts A map of configuration options. Must include `priv_green_zone_aes'.
%% @returns {ok, Map} on success, where Map includes:
%%           - status: "success"
%%           - message: confirmation text
%%           - target_address: the target node's address
%%          Returns {error, Reason} if decryption fails or the node is not part of the green zone.
-spec become(M1 :: term(), M2 :: term(), Opts :: map()) -> {ok, map()} | {error, binary()}.
become(M1, _M2, Opts) ->
	% TODO: Guard that the node is not already part of the green zone and that
	% the caller is the physical operator.
    ?event(green_zone, {become, start}),
    % Extract the target address and the encrypted key and IV.
    Node = hb_converge:get(<<"node">>, M1, Opts),
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    case GreenZoneAES of
        undefined ->
            ?event(green_zone, {become, error, "no aes key"}),
            {error, <<"Node not part of green zone">>};
        _ ->
			?event(green_zone, {become, getting_key, Node}),
			{ok, KeyResp} = hb_http:get(Node, <<"/~greenzone@1.0/key">>, Opts),
			?event(green_zone, {become, got_key, KeyResp}),
			Combined = base64:decode(hb_converge:get(<<"encrypted_key">>, KeyResp, Opts)),
			IV = base64:decode(hb_converge:get(<<"iv">>, KeyResp, Opts)),
            % Separate the ciphertext and authentication tag, then decrypt.
            CipherLen = byte_size(Combined) - 16,
            <<Ciphertext:CipherLen/binary, Tag:16/binary>> = Combined,
            DecryptedBin = crypto:crypto_one_time_aead(
                aes_256_gcm,
                GreenZoneAES,
                IV,
                Ciphertext,
                <<>>,
                Tag,
                false
            ),
            {KeyType, Priv, Pub} = binary_to_term(DecryptedBin),
            % Update the local wallet with the target node's keypair.
            ok = hb_http_server:set_opts(Opts#{
                priv_wallet => {{KeyType, Priv, Pub}, {KeyType, Pub}}
            }),
            ?event(green_zone, {become, update_wallet, complete}),
            {ok, #{
                <<"status">> => 200,
                <<"message">> => <<"Successfully adopted target node identity">>,
                <<"node">> => Node
            }}
    end.

%%--------------------------------------------------------------------
%% Define trusted software properties used in tests.
%% This map contains various attributes (like vcpus, firmware, kernel, etc.)
%% that represent the expected configuration for trusted nodes.
%%--------------------------------------------------------------------
-define(TEST_TRUSTED_SOFTWARE, #{
    vcpus => 1,
    vcpu_type => 5, 
    vmm_type => 1,
    guest_features => 1,
    firmware => <<"b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510">>,
    kernel => <<"69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576">>,
    initrd => <<"6cb35bc55047d7e02a0c8e3f5c61b5913186310ba6b8cf202cfd42b290af6d8b">>,
    append => <<"8c8e78297694a44dfe61479e208263c8228937a6cb0bfb43203700c3388ceffe">>
}).

%% @doc Test Setup Helpers.
%% These functions create a controlled environment for testing by setting up
%% mocks for external modules and hardware-specific functions.
setup_test_env() ->
    % Set up mocks for the HTTP server module.
    meck:new(hb_http_server, [passthrough]),
    meck:expect(hb_http_server, set_opts, fun(_) -> ok end),
    meck:expect(hb_http_server, start_node, fun(_) -> <<"http://localhost:8888">> end),
    % Set up mocks for hardware-specific parts.
    meck:new(dev_snp_nif, [passthrough]),
    meck:expect(dev_snp_nif, compute_launch_digest, fun(_) ->
        {ok, [94,87,4,197,20,11,255,129,179,197,146,104,8,212,152,248,110,11,60,246,82,254,24,55,201,47,157,229,163,82,108,66]}
    end),
    meck:expect(dev_snp_nif, verify_measurement, fun(_, _) -> {ok, true} end),
    meck:expect(dev_snp_nif, verify_signature, fun(_) -> {ok, true} end),
    ?event(green_zone, {test_setup, complete}),
    ok.

%% @doc Clean up the test environment.
%% Unload the mocks for external modules.
cleanup_test_env() ->
    meck:unload(hb_http_server),
    meck:unload(dev_snp_nif),
    ?event(green_zone, {test_cleanup, complete}),
    ok.

%% @doc Create a test node.
%% Generates a new wallet and computes a human-readable node address from it.
%% @returns {Wallet, NodeAddr}
create_test_node() ->
    Wallet = ar_wallet:new(),
    NodeAddr = hb_util:human_id(ar_wallet:to_address(Wallet)),
    ?event(green_zone, {create_test_node, complete}),
    {Wallet, NodeAddr}.

%% @doc Test for green zone initialization.
init_green_zone_test() ->
    setup_test_env(),
    try
        {Wallet, _NodeAddr} = create_test_node(),
        Opts = #{priv_wallet => Wallet},
        {ok, <<"Green zone initialized successfully.">>} = init(#{}, #{}, Opts),
        History = meck:history(hb_http_server),
        ?assertMatch([{_, {hb_http_server, set_opts, [_]}, ok}], History),
        [{_, {hb_http_server, set_opts, [SetOpts]}, _}] = History,
        ?assertNotEqual(undefined, maps:get(priv_green_zone_aes, SetOpts, undefined)),
        ?assertEqual(#{}, maps:get(trusted_nodes, SetOpts, undefined)),
        ?assertEqual(Wallet, maps:get(priv_wallet, SetOpts, undefined))
    after
        cleanup_test_env()
    end.

%% @doc Test for joining a green zone.
join_green_zone_test() ->
    setup_test_env(),
    try
        {Wallet, NodeAddr} = create_test_node(),
        {ok, Report} = file:read_file("test/green_zone_report.json"),
        % Create a test node message containing attestation and public key.
        NodeMsg = #{
            <<"attestors">> => <<"none">>,
            <<"public-key">> => base64:encode(term_to_binary(element(2, Wallet)))
        },
        {ok, NodeMsgId} = dev_message:id(NodeMsg, #{}, #{}),
        % Generate nonce.
        RawAddr = hb_util:native_id(NodeAddr),
        RawMsgId = hb_util:native_id(NodeMsgId),
        Nonce = <<RawAddr/binary, RawMsgId/binary>>,
        M1 = hb_message:attest(#{
            <<"report">> => Report,
            <<"address">> => NodeAddr,
            <<"node-message">> => NodeMsg,
            <<"nonce">> => hb_util:encode(Nonce),
            <<"vcpus">> => 1,
            <<"vcpu_type">> => 5,
            <<"vmm_type">> => 1,
            <<"guest_features">> => 1,
            <<"firmware">> => <<"b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510">>,
            <<"kernel">> => <<"69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576">>,
            <<"initrd">> => <<"6cb35bc55047d7e02a0c8e3f5c61b5913186310ba6b8cf202cfd42b290af6d8b">>,
            <<"append">> => <<"8c8e78297694a44dfe61479e208263c8228937a6cb0bfb43203700c3388ceffe">>
        }, Wallet),
        Opts = #{
            priv_wallet => Wallet,
            trusted => ?TEST_TRUSTED_SOFTWARE,
            http_server => NodeAddr
        },
        {ok, Result} = join(M1, #{<<"target">> => <<"self">>}, Opts),
        ?assertEqual(<<"success">>, maps:get(<<"status">>, Result)),
        ?assertEqual(NodeAddr, maps:get(<<"node-address">>, Result)),
        ?assertNotEqual(undefined, maps:get(<<"encrypted-payload">>, Result))
    after
        cleanup_test_env()
    end.

%% @doc Test for retrieving the encrypted private key.
get_key_test() ->
    setup_test_env(),
    try
        {Wallet, NodeAddr} = create_test_node(),
        GreenZoneAES = crypto:strong_rand_bytes(32),
        Opts = #{
            priv_wallet => Wallet,
            priv_green_zone_aes => GreenZoneAES,
            http_server => NodeAddr
        },
        {ok, Result} = key(#{<<"address">> => NodeAddr}, #{}, Opts),
        ?assertEqual(<<"success">>, maps:get(<<"status">>, Result)),
        ?assertNotEqual(undefined, maps:get(<<"encrypted_key">>, Result)),
        ?assertNotEqual(undefined, maps:get(<<"iv">>, Result))
    after
        cleanup_test_env()
    end.

%% @doc Test for cloning a node's identity.
become_node_test() ->
    setup_test_env(),
    try
        % Create two test nodes.
        {Node1Wallet, Node1Addr} = create_test_node(),
        {Node2Wallet, _Node2Addr} = create_test_node(),
        GreenZoneAES = crypto:strong_rand_bytes(32),
        % Node1 retrieves its encrypted key.
        Node1Opts = #{
            priv_wallet => Node1Wallet,
            priv_green_zone_aes => GreenZoneAES
        },
        {ok, KeyResult} = key(#{<<"address">> => Node1Addr}, #{}, Node1Opts),
        % Node2 adopts Node1's identity.
        Node2Opts = #{
            priv_wallet => Node2Wallet,
            priv_green_zone_aes => GreenZoneAES
        },
        BecomeM1 = #{
            <<"target-address">> => Node1Addr,
            <<"encrypted_key">> => maps:get(<<"encrypted_key">>, KeyResult),
            <<"iv">> => maps:get(<<"iv">>, KeyResult)
        },
        {ok, Result} = become(BecomeM1, #{}, Node2Opts),
        ?assertEqual(<<"success">>, maps:get(<<"status">>, Result)),
        % Verify signatures match.
        TestMsg = <<"test message">>,
        Sig1 = ar_wallet:sign(element(1, Node1Wallet), TestMsg),
        History = meck:history(hb_http_server),
        ?assertMatch([{_, {hb_http_server, set_opts, [_]}, ok}], History),
        [{_, {hb_http_server, set_opts, [SetOpts]}, _}] = History,
        Node2UpdatedWallet = maps:get(priv_wallet, SetOpts),
        Sig2 = ar_wallet:sign(element(1, Node2UpdatedWallet), TestMsg),
        ?assert(ar_wallet:verify(element(2, Node1Wallet), TestMsg, Sig1)),
        ?assert(ar_wallet:verify(element(2, Node1Wallet), TestMsg, Sig2))
    after
        cleanup_test_env()
    end.


%% @doc Test RSA operations with existing wallet structure
rsa_wallet_integration_test() ->
    % Create a new wallet using ar_wallet
    Wallet = ar_wallet:new(),
    {{KeyType, Priv, Pub}, {KeyType, Pub}} = Wallet,
    
    % Create test message
    PlainText = <<"HyperBEAM integration test message">>,
    
    % Create RSA public key record for encryption
    RsaPubKey = #'RSAPublicKey'{
        publicExponent = 65537,
        modulus = crypto:bytes_to_integer(Pub)
    },
    
    % Encrypt using public key
    Encrypted = public_key:encrypt_public(PlainText, RsaPubKey),
    
    % Create RSA private key record for decryption
    RSAPrivKey = #'RSAPrivateKey'{
        publicExponent = 65537,
        modulus = crypto:bytes_to_integer(Pub),
        privateExponent = crypto:bytes_to_integer(Priv)
    },
    
    % Verify decryption works
    Decrypted = public_key:decrypt_private(Encrypted, RSAPrivKey),
    
    % Verify roundtrip
    ?assertEqual(PlainText, Decrypted),
    
    % Verify wallet structure
    ?assertEqual(KeyType, {rsa, 65537}).