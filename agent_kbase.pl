:- dynamic ([
            current/3,
            state_confundus/3,
            state_glitter/3,
            state_safe/3,
            state_stench/3,
            state_tingle/3,
            state_visited/3,
            state_wumpus/3,
            agent_arrow/1,
            action_bump/1, 
            action_scream/1
	    ]).


reborn :-
    retractall(current(_,_,_)),
    assert(current(0,0,rnorth)),
    reset_state_memory.
    reset_questitem. 

reposition(L):-
    L is [[confunded,yes],[stench,no],[tingle,no],[glitter,no],[bump,no],[scream,no]],
    retractall(current(_,_,_)),
    assert(current(0,0,rnorth)),
    reset_state_memory.

end:-
retractall(current(_,_,_)).


/* move(action, indicator after action) function*/

/*shoot action*/
move(shoot,[L]):-
    /* shoot the arrow (drop the arrow) */
    use_arrow,
    /* check the for scream */
    member([scream,A],[L]),
    process(scream,A).

/* moveforward action*/
move(moveforward,[[confounded,yes]| S]) :-
    /*if movefoward has made */
    member([confounded,A],[L]),
    process(confunded,A),
    member([stench,B],[L]),
    process(stench,B).
    member([tingle,C],[L]),
    process(tingle,C).

move(moveforward,[[confounded,no]|S]) :-
    /* check the for scream */
    member([bump,A],[L]),
    process(bump,A).

    
/* turnleft action */
move(turnleft,[L]) :-
    /* do the turn */
    turnleft. 

/* turnright action */
move(turnright,[L]) :-
    /* do the turn */
    turnright. 

/* pickup action */
move(pickup,[L]) :-
    member([glitter,A],[L]),
    process(glitter,A,[L]).

     
/*check for moveforward (confunded,yes)*/
process(confunded,yes) :-
    reborn.

/* check if moveforward hit the wall */
process(bump,no) :-
    current(X,Y,_),
    assert(state_visited(yes,X,Y)),
    moveforward.

process(bump,yes) :-
    current(X,Y,D),
    set_wall(X,Y,D).

/* for move forward action logic */
process(stench,yes) :-
    current(X,Y,_), 
    assert(state_stench(yes,X,Y)).

/* for move forward action logic */
process(tingle,yes) :-
    current(X,Y,_),
    assert(state_tingle(yes,X,Y)).

/* for pickup action only */
process(glitter,yes) :-
    pickup_coin.

/* for shoot action only */
process(scream,yes) :-
    clear_wumpus.
    

/* Movement Logic */
moveforward:-
    current(X,Y,D),
    retractall(current(_,_,_)), 
    move(X,Y,D).

move(X,Y,rnorth):-
    N is Y + 1,
    assert(current(X,N,rnorth)).

move(X,Y,reast):-
    N is X + 1,
    assert(current(N,Y,reast)).

move(X,Y,rsouth):-
    N is Y - 1,
    assert(current(X,N,rsouth)).

move(X,Y,rwest):-
    N is X - 1,
    assert(current(N,Y,rwest)).

/* Movement Logic */
/*turn left*/
turnleft:-
    current(X,Y,D),
    retractall(current(_,_,_)), 
    turnl(X,Y,D).

turnl(X,Y,rnorth) :-
    assert(current(X,Y,rwest)).

turnl(X,Y,reast) :-
    assert(current(X,Y,rnorth)).

turnl(X,Y,rsouth) :-
    assert(current(X,Y,reast)).

turnl(X,Y,rwest) :-
    assert(current(X,Y,rsouth)).

/*turn right*/
turnright:-
    current(X,Y,D),
    retractall(current(_,_,_)), 
    turnr(X,Y,D).

turnr(X,Y,rnorth) :-
    assert(current(X,Y,reast)).

turnr(X,Y,reast) :-
    assert(current(X,Y,rsouth)).

turnr(X,Y,rsouth) :-
    assert(current(X,Y,rwest)).

turnr(X,Y,rwest) :-
    assert(current(X,Y,rnorth)).



/*Localisation and mapping requirement*/
/* state attribute checker */

/* assert the state_xxx(yes no,X,Y) */
/* call clear_xxx to reset all state_xxx */


reset_state_memory :-

    clear_visited,
    clear_wumpus,
    clear_confoundus, 
    clear_tingle, 
    clear_glitter,
    clear_stench,
    clear_wall.


visited(X,Y) :-
    state_visited(yes,X,Y).

set_visited(X,Y) :-
    retractall(state_visited(_,X,Y)),
    assert(state_visited(yes,X,Y)).

clear_visited :-
    retractall(state_visited).

wumpus(X,Y) :-
    state_wumpus(yes,X,Y).

set_wumpus(X,Y) :-
    retractall(state_wumpus(_,X,Y)),
    assert(state_wumpus(yes,X,Y)).

clear_wumpus :-
    retractall(state_wumpus).

confundus(X,Y) :-
    state_confundus(yes,X,Y).

set_confoundus(X,Y) :-
    retractall(state_confundus(_,X,Y)),
    assert(state_confoundus(yes,X,Y)).

clear_confoundus :-
    retractall(state_confundus).

tingle(X,Y) :-
    state_tingle(yes,X,Y).

set_tiggle(X,Y) :-
    retractall(state_tiggle(_,X,Y)),
    assert(state_tiggle(yes,X,Y)).

clear_tingle :-
    retractall(state_tingle).

glitter(X,Y) :-
    state_glitter(yes,X,Y).

set_glitter(X,Y) :-
    retractall(state_glitter(_,X,Y)),
    assert(state_glitter(yes,X,Y)).

clear_glitter :-
    retractall(state_glitter).

stench(X,Y) :-
    state_stench(yes,X,Y).

set_stench(X,Y) :-
    retractall(state_stench(_,X,Y)),
    assert(state_stench(yes,X,Y)).

clear_stench() :-
    retractall(state_stench).


safe(X,Y) :-
    state_safe(yes,X,Y).

set_safe(X,Y) :-
    retractall(state_safe(_,X,Y)),
    assert(state_safe(yes,X,Y)).

clear_safe :-
    retractall(state_safe).    


/* custom wall kb*/
set_wall(X,Y,rnorth) :- 
    N is  Y + 1,
    assert(wall(N,y)).

set_wall(X,Y,rsouth) :- 
    N is  Y - 1,
    assert(wall(N,y)).

set_wall(X,Y,reast) :- 
    N is  X + 1,
    assert(wall(N,x)).

set_wall(X,Y,rwest) :- 
    N is  X - 1,
    assert(wall(N,x)).    

clear_wall :- 
    retract_all(wall(_,_)).


/* agent attribute checker */

reset_questitem:-
    assert(agent_arrow(yes)).

hasarrow :-
    agent_arrow(yes).

use_arrow :-
    retractall(agent_arrow(_)),


hascoin :-
    agent_coin(yes).

pickup_coin :-
    assert(agent_coin(yes)).