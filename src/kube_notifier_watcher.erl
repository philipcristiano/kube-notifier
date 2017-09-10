-module(kube_notifier_watcher).

-behaviour(gen_server).

-compile({parse_transform, lager_transform}).

%% API functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {api,
                kubecallback,
                deployment_info,
                slack_channel,
                slack_token}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Options) ->
    S1 = #state{
        slack_channel = proplists:get_value(slack_channel, Options),
        slack_token = proplists:get_value(slack_token, Options)
    },
    lager:info("Kube Watcher starting"),
    init_defined_token(S1).


init_defined_token(#state{slack_token=undefined}) ->
    {stop, undefined_slack_token};
init_defined_token(State) ->
    gen_server:cast(self(), get_deployments),
    API = kuberlnetes:load(),
    CBFunc = swaggerl:async_op(API, "watchAppsV1beta1DeploymentListForAllNamespaces", []),
    lager:info("Connected to kubernetes"),
    {ok, State#state{api=API, kubecallback=CBFunc, deployment_info=#{}}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    lager:debug("Unhandled Call"),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(get_deployments, State=#state{api=API, deployment_info=DI}) ->
    lager:debug("Get deployments "),
    Data = swaggerl:op(API, "listAppsV1beta1DeploymentForAllNamespaces", []),
    lager:debug("Deployments ~p", [Data]),
    Deployments = maps:get(<<"items">>, Data),
    DI1 = set_deployment_generations(DI, Deployments),

    lager:debug("DI ~p", [DI1]),
    {noreply, State#state{deployment_info=DI1}};

handle_cast(_Msg, State) ->
    lager:debug("Unhandled Cast "),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State=#state{kubecallback=KCB}) ->
    Val = KCB(Info),
    % lager:debug("Unhandled info ~p", [Val]),
    handle_swaggerl(Val, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_swaggerl(unknown, State) ->
    {noreply, State};
handle_swaggerl(ok, State) ->
    {noreply, State};
handle_swaggerl(Val, State=#state{deployment_info=DI}) ->
    Obj = maps:get(<<"object">>, Val),
    Metadata = maps:get(<<"metadata">>, Obj),
    Namespace = maps:get(<<"namespace">>, Metadata),
    Name = maps:get(<<"name">>, Metadata),
    Status = maps:get(<<"status">>, Obj),
    Conditions = maps:get(<<"conditions">>, Status),
    [NewestCondition|_] = Conditions,
    Msgs = [maps:get(<<"message">>, Cond) || Cond <- Conditions],
    _ConditionMsg = maps:get(<<"message">>, NewestCondition),

    lager:debug("DI ~p", [DI]),

    CurrentGen = maps:get({Namespace, Name}, DI, 0),
    NewGen = get_generation_from_deployment(Obj),

    DI1 = if
        NewGen > CurrentGen -> NewDI = maps:put({Namespace, Name}, NewGen, DI),
                               send_new_gen_message(Obj, NewGen, State),
                               NewDI;
        true -> lager:debug("Not a worthy update"),
                DI
    end,

    lager:debug("Unhandled deployment ~p", [{Name, CurrentGen, NewGen, Msgs}]),
    {noreply, State#state{deployment_info=DI1}}.

set_deployment_generations(Map, []) ->
    Map;
set_deployment_generations(Map, [D|Rest]) ->
    Metadata = maps:get(<<"metadata">>, D),
    lager:debug("Deployment meta ~p", [Metadata]),
    Namespace = maps:get(<<"namespace">>, Metadata),
    Name = maps:get(<<"name">>, Metadata),

    Generation = get_generation_from_deployment(D),

    NewMap = maps:put({Namespace, Name}, Generation, Map),

    set_deployment_generations(NewMap, Rest).

get_generation_from_deployment(Deployment) ->
    Status = maps:get(<<"status">>, Deployment),
    Generation = maps:get(<<"observedGeneration">>, Status),
    Generation.

send_new_gen_message(Deployment, Gen, #state{slack_token=SlackToken, slack_channel=SlackChannel}) ->
    lager:debug("Sending slack message ~p", [SlackToken]),
    Metadata = maps:get(<<"metadata">>, Deployment),
    Name = maps:get(<<"name">>, Metadata),
    % Status = maps:get(<<"status">>, Deployment),
    % AvailableReplicas = maps:get(<<"availableReplicas">>, Status),
    % AvailableReplicasBin = list_to_binary(integer_to_list(AvailableReplicas)),
    % Replicas = maps:get(<<"replicas">>, Status, 0),
    % ReplicasBin = list_to_binary(integer_to_list(Replicas)),
    % UpdatedReplicas = maps:get(<<"updatedReplicas">>, Status),
    % UpdatedReplicasBin = list_to_binary(integer_to_list(UpdatedReplicas)),
    % ReadyReplicas = maps:get(<<"readyReplicas">>, Status),
    % ReadyReplicasBin = list_to_binary(integer_to_list(ReadyReplicas)),

    % ReplicaMsg = << <<"Updated Replicas: ">>/binary, UpdatedReplicasBin/binary,
    %                <<" Available Replicas: ">>/binary, AvailableReplicasBin/binary,
    %                <<" Ready Replicas: ">>/binary, ReadyReplicasBin/binary,
    %                <<" Replicas: ">>/binary, ReplicasBin/binary
    %              >>,

    GenBin = list_to_binary(integer_to_list(Gen)),

    Title = << <<"Deployment: ">>/binary, Name/binary, " .Generation ", GenBin/binary, <<" deploying.">>/binary >>,

    {ok, 200, _Headers, Resp} = slacker_chat:post_message(SlackToken, SlackChannel, Title, []),
    lager:debug("Slack message: 200 OK ~p", [Resp]).
