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
%% @doc Skeleton file

-module(pm).

%% Interface exports
-export([new/0,
	 
	 add/3,
	 set/3,
	 del/2,
	 
	 is_defined/2,
	 get/2,
	 size/1,
	 
	 foreach/2,
	 map/2,
	 foldl/3]).

%% Behaviour callback exports
-export([]).

%% -----------------------------------------------------------------------------
%% Interface functions
%% -----------------------------------------------------------------------------
size(S)          -> dict:size(S).
new()            -> dict:new().
add(K, V, S)     -> dict:store(K, {K,V}, S).
set(K, V, S)     -> dict:store(K, {K,V}, S).
del(K, S)        -> dict:erase(K, S).
get(K, S)        -> case catch dict:fetch(K, S) of 
			{'EXIT', _} -> undefined;
			{_, V} -> V
		    end.
is_defined(K, S) -> dict:is_key(K, S).

map(F, S)        -> dict:map(fun(_,V) -> F(V) end, S).
foldl(F, I, S)   -> dict:fold(fun(_, V, O) -> F(V,O) end, I, S).

foreach(F, S)    -> dict:map(fun(_,V) -> F(V) end, S), S.
%% -----------------------------------------------------------------------------
%%  Callback functions
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Internal Functions
%% -----------------------------------------------------------------------------
