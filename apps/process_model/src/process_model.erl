%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%% @author Lukas Larsson <garazdawi@gmail.com>
%% @doc Model for process balls

-module(process_model).
-behaviour(gen_server).

-include_lib("process_model.hrl").

%% Interface exports
-export([start_link/1,add_node/2,step/2,batch/2]).

%% Behaviour exports
-export([init/1,handle_call/3,handle_cast/2,terminate/2,
	 code_change/3,handle_info/2]).


%% -----------------------------------------------------------------------------
%% Interface functions
%% -----------------------------------------------------------------------------
start_link(Opts) ->
    gen_server:start_link(?MODULE,Opts,[]).

step(Pid,Args) ->
    gen_server:call(Pid,{batch,step_fun(Args)}).

batch(Pid,Fun) ->
    gen_server:call(Pid,{batch,Fun}).

add_node(Pid, Node) ->
    gen_server:cast(Pid,{add_node,Node}).

%% -----------------------------------------------------------------------------
%% Behaviour functions
%% -----------------------------------------------------------------------------
init(_Opts) ->
    {ok, pm:new()}.

handle_cast({add_node,#pm_node{id = Id} = NewGoal},State) ->
    case pm:get(Id,State) of
	undefined ->
	    {noreply,pm:add(Id,{#pm_node{ id = Id },NewGoal},State)};
	{Curr,_OldGoal} ->
	    {noreply,pm:add(Id,{Curr,NewGoal},State)}
    end.

handle_call({batch,Fun},_From,State) ->
    {Reply,NewState} =  Fun(State),
    {reply,Reply,NewState}.

terminate(_Reason,_State) ->
    ok.

code_change(_From,_To,State) ->
    State.

handle_info(_Msg,State) ->
    {noreply,State}.

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------
step_fun(StepOpts) ->
    fun(State) ->
	    do_step(StepOpts,State)
    end.

do_step(StepOpts,State) ->
    {ok,State}.
