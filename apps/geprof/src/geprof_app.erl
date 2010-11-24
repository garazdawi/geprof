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
%% @doc Application file for Graphic Erlang Performance Tool

-module(geprof_app).
-behaviour(application).

%% Interface exports
-export([start/0,start/1]).
%% Behaviour callback exports
-export([start/2, stop/1]).

%% -----------------------------------------------------------------------------
%% Interface functions
%% -----------------------------------------------------------------------------
start() ->
	start([]).

start(Options) ->
	application:load(geprof),
	[application:set_env(geprof,Key,Value) || {Key,Value} <- Options],
	application:start(geprof_app).

%% -----------------------------------------------------------------------------
%%  Callback functions
%% -----------------------------------------------------------------------------
start(_StartType,_StartArgs) ->
	{ok,self()}.

stop(_State) ->
	ok.