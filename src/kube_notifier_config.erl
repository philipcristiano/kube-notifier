%%%-------------------------------------------------------------------
%%% @author Philip Cristiano
%%% @copyright 2017 Philip Cristiano
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(kube_notifier_config).

-behaviour(gen_server).

-compile({parse_transform, lager_transform}).

%% API functions
-export([read_config_dirs/1,
         start_link/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

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
start_link(ConfigDirs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ConfigDirs, []).

read_config_dirs([]) ->
    ok;
read_config_dirs([H|T]) ->
    {ok, Filenames} = file:list_dir(H),
    ok = lager:info("Filenames for config ~p", [Filenames]),
    ok = update_config_for_files(H, Filenames),

    read_config_dirs(T).

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
init(ConfigDirs) ->
    subscribe_to_dirs(ConfigDirs, 0),
    {ok, #state{}}.

subscribe_to_dirs([], _Count) ->
    ok;
subscribe_to_dirs([Dir|Dirs], Count) ->
    Name = "kube_notifier_config_" ++ erlang:integer_to_list(Count),
    AName = erlang:list_to_atom(Name),
    fs:start_link(AName, Dir),
    fs:subscribe(AName),
    subscribe_to_dirs(Dirs, Count + 1).

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
handle_cast(_Msg, State) ->
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
handle_info({_Pid, {fs, file_event}, {Path, _Props}}, State) ->
    ok = lager:info("File modified ~p", [Path]),
    ok = update_config_for_file(Path),
    {noreply, State};
handle_info(Info, State) ->
    lager:info("Unhandled Info ~p", [Info]),
    {noreply, State}.

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

update_config_for_files(_Prefix, []) ->
    ok;
update_config_for_files(Prefix, [H|T]) ->
    Path = filename:join(Prefix, H),
    ok = update_config_for_file(Path),
    update_config_for_files(Prefix, T).

update_config_for_file(Path) ->
    ok = lager:info("Updating config from file ~p", [Path]),
    Name = filename:basename(Path),
    AName = erlang:list_to_atom(Name),
    case file:read_file(Path) of
          {ok, Data} -> ok = lager:info("Setting ~p to ~p", [Name, Data]),
                        ok = application:set_env(kube_notifier, AName, Data, [{persistent, true}] ),
                        ok;
          {error, Error} -> lager:info("Not update file because ~p", [Error]),
                            ok
    end.
