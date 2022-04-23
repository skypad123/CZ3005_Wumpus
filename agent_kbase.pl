:- dynamic ([
            current/3,
            state_confundus/2,
            state_glitter/2,
            state_safe/2,
            state_stench/2,
            state_tingle/2,
            state_visited/2,
            state_wumpus/2,
            state_realwumpus/2, 
            state_realconfundus/2,
            agent_arrow/1,
            agent_coin/1
	    ]).


reborn :-
    reset_state_memory,
    reset_questitem,
    set_current(0,0,rnorth).

/* [[confunded,yes],[stench,no],[tingle,no],[glitter,no],[bump,no],[scream,no]] */
reposition(L):-
    reset_state_memory,
    set_current(0,0,rnorth), 
    /*process_confunded(L),*/
    process_stench(L), 
    process_tingle(L), 
    process_safety(L).

end:-
    retractall(current(_,_,_)).

/* move(action, indicator after action) function*/

/*shoot action*/
move(shoot,L):-
    /* shoot the arrow (drop the arrow) */
    use_arrow,
    /* check the for scream */
    process_scream(L).

/* moveforward action*/
move(moveforward,L) :-
    /* actions if confunded */
    /* process_confunded(L), */
    /* action done regardless */
    process_bump(L).

/* turnleft action */
move(turnleft,L) :-
    /* do the turn */
    turnleft.

/* turnright action */
move(turnright,L) :-
    /* do the turn */
    turnright.

/* pickup action */
move(pickup,L) :-
   pickup_coin.


/*processing confunded indicator*/
/*process_confunded(L):-
    nth0(0,L,A),
    subprocess(confunded,A). */

/*processing stench indicator*/
process_stench(L):-
    nth0(1,L,B),
    subprocess(stench,B).

/*processing tingle indicator*/
process_tingle(L):-
    nth0(2,L,C),
    subprocess(tingle,C).


/*processing glitter indicator*/
process_glitter(L):-
    nth0(3,L,D),
    subprocess(glitter,D).

/*processing bump indicator*/
process_bump(L):-
    nth0(4,L,E),
    subprocess(bump,E),
    ( E = no -> process_stench(L);true),
    ( E = no -> process_tingle(L);true),
    ( E = no -> process_safety(L);true).

process_safety(L):-
    nth0(2,L,B),
    nth0(3,L,C),
    ( B= no -> (C = no -> mapout_safe;true);true).

/*processing scream indicator*/
process_scream(L):-
    nth0(5,L,F),
    subprocess(scream,F).

/*check for moveforward (confunded,yes)*/
 /*subprocess(confunded,yes) :-
    reposition([yes,no,no,no,no,no]). */

/*placeholder*/
/*subprocess(confunded,no). */


/* for move forward action logic */
subprocess(stench,yes) :-
    current(X,Y,_),
    set_stench(X,Y),
    blockout_wumpus.

subprocess(stench,no) :-
    cleanout_wumpus.

/* for move forward action logic */
/*tbw*/
subprocess(tingle,yes) :-
    current(X,Y,_),
    set_tingle(X,Y),
    blockout_confundus.

subprocess(tingle,no) :-
    cleanout_confundus.

/* for pickup action only */
subprocess(glitter,yes) :-
    current(X,Y,_),
    set_glitter(X,Y).
    

/*placeholder*/
subprocess(glitter,no).


/* check if moveforward hit the wall */
subprocess(bump,no) :-
    moveforward.

subprocess(bump,yes) :-
    current(X,Y,D),
    set_wall(X,Y,D).
    
/* for shoot action only */
subprocess(scream,yes) :-
    clear_realwumpus.

/*placeholder*/
subprocess(scream,no).

/* Movement Logic */
moveforward:-
    current(X,Y,D),
    retractall(current(_,_,_)),
    move(X,Y,D).

move(X,Y,rnorth):-
    N is Y + 1,
    set_current(X,N,rnorth).

move(X,Y,reast):-
    N is X + 1,
    set_current(N,Y,reast).

move(X,Y,rsouth):-
    N is Y - 1,
    set_current(X,N,rsouth).

move(X,Y,rwest):-
    N is X - 1,
    set_current(N,Y,rwest).

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
    clear_realwumpus,
    clear_realconfundus,
    clear_safe,
    clear_wall.

/* visited */
visited(X,Y) :-
    state_visited(X,Y).

set_visited(X,Y) :-
    retractall(state_visited(X,Y)),
    assert(state_visited(X,Y)).

clear_visited :-
    retractall(state_visited).


/*wumpus*/
wumpus(X,Y) :-
    state_wumpus(X,Y).
wumpus(X,Y) :-
    state_realwumpus(X,Y).

set_wumpus(X,Y) :-
    retractall(state_wumpus(X,Y)),
    assert(state_wumpus(X,Y)).

clear_wumpus :-
    retractall(state_wumpus(_,_)).

/*confundus*/
confundus(X,Y) :-
    state_confundus(X,Y).
confundus(X,Y) :-
    state_realconfundus(X,Y).

set_confoundus(X,Y) :-
    retractall(state_confundus(X,Y)),
    assert(state_confundus(X,Y)).

clear_confoundus :-
    retractall(state_confundus(_,_)).

/*tingle*/
tingle(X,Y) :-
    state_tingle(X,Y).

set_tingle(X,Y) :-
    retractall(state_tingle(X,Y)),
    assert(state_tingle(X,Y)).

clear_tingle :-
    retractall(state_tingle(_,_)).

/*glitter*/
glitter(X,Y) :-
    state_glitter(X,Y).

set_glitter(X,Y) :-
    retractall(state_glitter(X,Y)),
    assert(state_glitter(X,Y)).

clear_glitter :-
    retractall(state_glitter(_,_)).

/*stench*/
stench(X,Y) :-
    state_stench(X,Y).

set_stench(X,Y) :-
    retractall(state_stench(X,Y)),
    assert(state_stench(X,Y)).

clear_stench() :-
    retractall(state_stench(_,_)).
/*safe*/
safe(X,Y) :-
    state_safe(X,Y).

set_safe(X,Y) :-
    retractall(state_safe(X,Y)),
    assert(state_safe(X,Y)).

clear_safe() :-
    retractall(state_safe(_,_)).

/*wall*/
wall(X,Y) :-
    state_wall(X,Y).

set_wall(X,Y,rnorth) :-
    clear_assumption(X,Y+1),
    assert(state_wall(X,Y+1)).
set_wall(X,Y,rsouth) :-
    clear_assumption(X,Y-1),
    assert(state_wall(X,Y-1)).
set_wall(X,Y,reast) :-
    clear_assumption(X+1,Y),
    assert(state_wall(X+1,Y)).
set_wall(X,Y,rwest) :-
    clear_assumption(X-1,Y),
    assert(state_wall(X-1,Y)).

clear_wall :-
    retractall(state_wall(_,_)).

clear_assumption(X,Y) :-
    retractall(state_tingle(X,Y)), 
    retractall(state_stench(X,Y)),
    retractall(state_safe(X,Y)).

non-assumable(X,Y) :- wall(X,Y). 
non-assumable(X,Y) :- safe(X,Y). 
/*custom realwumpus*/
/*for the confirmed wumpus*/
realwumpus(X,Y) :-
    state_realwumpus(X,Y).
set_realwumpus(X,Y) :-
    retractall(state_realwumpus(_,_)),
    assert(state_realwumpus(X,Y)).

clear_realwumpus:-
    retractall(state_realwumpus(_,_)).

/*custom real confoundus*/
/*for the confirmed confoundus*/
realconfundus(X,Y) :-
    state_realconfundus(X,Y).
set_realconfoundus(X,Y) :-
    assert(state_realconfundus(X,Y)).
clear_realconfundus :-
    retractall(state_realconfundus(_,_)).
count_realconfundus(Count) :-
    count(state_realconfundus(_,_),Count).

count(P,Count) :-
        findall(1,P,L),
        length(L,Count).




/*custom set_current*/
set_current(X,Y,D) :-
    retractall(current(_,_,_)),
    assert(current(X,Y,D)),
    set_visited(X,Y).

/*explore([L]):-*/

/* to be implemented */



/*call after all the blockout*/
mapout_safe :-
    current(X,Y,_),
    find_safe(X+1,Y), 
    find_safe(X-1,Y),
    find_safe(X,Y+1), 
    find_safe(X,Y-1).
    
find_safe(X,Y) :-
    (non-assumable(X,Y) -> true; set_safe(X,Y)).


/*blockout_Wumpus*/
blockout_wumpus :-
    aggregate_all(count, realwumpus(_,_), C),
    ( C = 0 -> do_guess_wumpus; true).

do_guess_wumpus :-
    current(X,Y,D),
    guess_surround_wumpus(X,Y,D),
    aggregate_all(count, realwumpus(_,_), C),
    ( C > 0 -> clear_wumpus;true).

guess_surround_wumpus(X,Y,rnorth):-
    find_wumpus(X-1,Y),
    find_wumpus(X,Y+1),
    find_wumpus(X+1,Y).

guess_surround_wumpus(X,Y,rsouth):-
    find_wumpus(X+1,Y),
    find_wumpus(X,Y-1),
    find_wumpus(X-1,Y).

guess_surround_wumpus(X,Y,reast):-
    find_wumpus(X,Y+1),
    find_wumpus(X+1,Y),
    find_wumpus(X,Y-1).

guess_surround_wumpus(X,Y,rwest):-
    find_wumpus(X,Y-1),
    find_wumpus(X-1,Y),
    find_wumpus(X,Y+1).

find_wumpus(X,Y):-
    ( wumpus(X,Y) -> set_realwumpus(X,Y); ( non-assumable(X,Y) -> true ; set_wumpus(X,Y) ) ).

/*cleanout_wumpus*/
cleanout_wumpus :-
    aggregate_all(count, realwumpus(_,_), C),
    ( C = 0 -> do_clear_wumpus; true).

do_clear_wumpus :-
    current(X,Y,D),
    clear_surround_wumpus(X,Y,D).

clear_surround_wumpus(X,Y,rnorth) :-
    retractall(state_wumpus(X-1,Y)),
    retractall(state_wumpus(X,Y+1)),
    retractall(state_wumpus(X+1,Y)).

clear_surround_wumpus(X,Y,rsouth) :-
    retractall(state_wumpus(X+1,Y)),
    retractall(state_wumpus(X,Y-1)),
    retractall(state_wumpus(X-1,Y)).

clear_surround_wumpus(X,Y,reast) :-
    retractall(state_wumpus(X,Y+1)),
    retractall(state_wumpus(X+1,Y)),
    retractall(state_wumpus(X,Y-1)).

clear_surround_wumpus(X,Y,rwest) :-
    retractall(state_wumpus(X,Y-1)),
    retractall(state_wumpus(X-1,Y)),
    retractall(state_wumpus(X,Y+1)).

/*blockout confundus*/
blockout_confundus :-
    count_realconfundus(C),
    ( C < 3 -> do_guess_confundus; true).

do_guess_confundus :-
    current(X,Y,D),
    guess_surround_confundus(X,Y,D),
    count_realconfundus(C),
    ( C > 3 -> clear_confoundus; true).

guess_surround_confundus(X,Y,rnorth):-
    find_confundus(X-1,Y),
    find_confundus(X,Y+1),
    find_confundus(X+1,Y).

guess_surround_confundus(X,Y,rsouth):-
    find_confundus(X+1,Y),
    find_confundus(X,Y-1),
    find_confundus(X-1,Y).

guess_surround_confundus(X,Y,reast):-
    find_confundus(X,Y+1),
    find_wumpus(X+1,Y),
    find_wumpus(X,Y-1).

guess_surround_confundus(X,Y,rwest):-
    find_confundus(X,Y-1),
    find_confundus(X-1,Y),
    find_confundus(X,Y+1).

find_confundus(X,Y):-
    (confundus(X,Y) -> set_realconfundus(X,Y); ( non-assumable(X,Y) -> true ; set_confoundus(X,Y) ) ).


/*cleanout confoundus*/
cleanout_confundus :-    
    count_realconfundus(C),
    ( C < 3 -> do_clear_confundus; true).

do_clear_confundus :-
    current(X,Y,D), 
    clear_surround_confoundus(X,Y,D).

clear_surround_confoundus(X,Y,rnorth) :-
    retractall(state_confoundus(X-1,Y)),
    retractall(state_confoundus(X,Y+1)),
    retractall(state_confoundus(X+1,Y)).

clear_surround_confoundus(X,Y,rsouth) :-
    retractall(state_confoundus(X+1,Y)),
    retractall(state_confoundus(X,Y-1)),
    retractall(state_confoundus(X-1,Y)).

clear_surround_confoundus(X,Y,reast) :-
    retractall(state_confoundus(X,Y+1)),
    retractall(state_confoundus(X+1,Y)),
    retractall(state_confoundus(X,Y-1)).

clear_surround_confoundus(X,Y,rwest) :-
    retractall(state_confoundus(X,Y-1)),
    retractall(state_confoundus(X-1,Y)),
    retractall(state_confoundus(X,Y+1)).



/* agent attribute checker */

reset_questitem:-
    retractall(agent_arrow(_)),
    retractall(agent_coins(_)),
    assert(agent_arrow(1)),
    assert(agent_coins(0)).

use_arrow :-
    agent_arrow(X), 
    retractall(agent_arrow(_)), 
    assert(agent_arrow(X+1)).

pickup_coin :-
    agent_coins(X), 
    retractall(agent_coins(_)), 
    assert(agent_coins(X+1)).

agent_arrow(1).
agent_coins(0).