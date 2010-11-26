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
%% @doc An first attempt at 3D OpenGL modeling with wx and process_model

-module(gp_gl_test).

%% Interface exports
-export([start/0]).
-export([start/1]).
%% Callback exports
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl"). 
-include_lib("wx/include/glu.hrl").
-include("../process_model/include/process_model.hrl").

-record(state, {frame, gl_canvas, pm}).

%% -----------------------------------------------------------------------------
%% Interface functions
%% -----------------------------------------------------------------------------
start() ->
    start([{size,{200,200}}]).
start(Opts) ->
    wx_object:start(?MODULE,Opts,[]).

%% -----------------------------------------------------------------------------
%% Callback Functions
%% -----------------------------------------------------------------------------
init(Opts) ->
%    wx:new([{debug,trace}]),
    wx:new(),
    process_flag(trap_exit, true),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "geprof",Opts),
    Panel = wxPanel:new(Frame,[]),
    wxFrame:show(Frame),
    GLOpts = Opts,
    GLAttrib = [{attribList, [?WX_GL_RGBA,
			      ?WX_GL_DOUBLEBUFFER,
			      ?WX_GL_MIN_RED,8,
			      ?WX_GL_MIN_GREEN,8,
			      ?WX_GL_MIN_BLUE,8,
			      ?WX_GL_DEPTH_SIZE,24,0]}],
    Canvas = wxGLCanvas:new(Panel,Opts ++ GLAttrib),
    wxGLCanvas:connect(Canvas, size),
    wxGLCanvas:setCurrent(Canvas),
    setup_gl(Canvas),
    timer:send_interval(100, self(), update),

    {ok,PM} = process_model:start_link(whatever),
    Env = wx:get_env(),
    %ok = process_model:batch(Pid, fun(State) ->
%					  wx:set_env(Env),
%					  {ok,State}
%				  end),
    {_,NewPM} = process_model:add_node(
		   PM,#pm_node{ id = 1,
				 x = 0.0,
				 y = 0.0,
				 z = -6.0,
				 size = 0.8}),
    
    {Frame, #state{ frame = Frame,
		    gl_canvas = Canvas,
		    pm = NewPM}}.

handle_event(Event, State) ->
    io:format("Got event ~p~n",[Event]),
    {noreply,State}.

handle_call({add_node,Node},_From,#state{ pm = PM } = State) ->
    {ok,NewPM} = process_model:add_node(PM,Node),
    {reply,ok,State#state{pm=NewPM}};
handle_call(Msg, _From, State) ->
    io:format("Got call ~p~n",[Msg]),
    {reply,ok,State}.

handle_info(update,#state{ pm = PM } =State) ->
    wx:batch(fun() -> redraw(State) end),
    {_,NewPM} = process_model:step(PM,#pm_step_opts{ distance = 0.05}),
    {noreply,State#state{pm = NewPM}};
handle_info(Msg, State) ->
    io:format("Got message ~p~n",[Msg]),
    {noreply,State}.

terminate(Reason,State) ->
    ok.

%% -----------------------------------------------------------------------------
%% Internal Functions
%% -----------------------------------------------------------------------------
setup_gl(Win) ->
    {W,H} = wxWindow:getClientSize(Win),
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),

    glu:perspective(45.0,W/H,0.1,100.0),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    
    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(1.0,1.0,1.0,1.0),
    gl:clearDepth(1.0),
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:hint(?GL_PERSPECTIVE_CORRECTION_HINT, ?GL_NICEST),

%    gl:lightfv(?GL_LIGHT1,?GL_AMBIENT,{1.0,1.0,1.0,1.0}),
    gl:lightfv(?GL_LIGHT1,?GL_DIFFUSE,{0.95,0.95,0.95,1.0}),
    gl:lightfv(?GL_LIGHT1,?GL_POSITION,{-3.0,1.0,-2.0,1.0}),
    gl:enable(?GL_LIGHT1),
    gl:enable(?GL_LIGHTING),
    
    put(rot,0.0).


redraw(#state{ gl_canvas = GL, pm = PM }) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:loadIdentity(),

%    gl:rotatef(get(rot),0.0,1.0,0.0),
%    put(rot,get(rot) + 0.4),

    gl:translatef(0.0,0.0,-6.0),
    Sphere = glu:newQuadric(),

    gl:color3f(0.0,1.0,1.0),
    {Nodes,_} = process_model:batch(
		  PM,fun(Nodes) ->
			     pm:foreach(fun({Node,_}) -> 
						draw_node(Sphere,Node)
					
					end,Nodes),
			     {Nodes,Nodes}
		     end),
    
    
%    gl:'begin'(?GL_TRIANGLES),
%    gl:vertex3f(0.0,1.0,0.0),
%    gl:vertex3f(-1.0,-1.0,0.0),
%    gl:vertex3f(1.0,-1.0,0.0),
%    gl:'end'(),


%    gl:translatef(1.5,0.0,6.0),
%    gl:translatef(1.5,0.0,-6.0),

%    glu:sphere(Sphere1, 0.8, 50, 40),
    
    gl:translatef(0.0,0,6.0),
    
%    gl:loadIdentity(),
    wxGLCanvas:swapBuffers(GL).


draw_node(Sphere,#pm_node{ x = X, y = Y, z = Z, size = S} ) ->

    gl:translatef(X,Y,Z),
    glu:sphere(Sphere, S, 50, 40),
    gl:translatef(-X,-Y,-Z).

