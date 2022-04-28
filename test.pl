a(1,2).
a(1,3).
a(1,4).
a(1,5).

b(1,3).
b(1,4).
b(1,5).

c(X,Y):- 
    a(X,Y),
    not(b(X,Y)).



init :-
    current(X,Y,D), 
    assert(temp(X,Y,D)),
    newsteps(X,Y)

nextstep :-
    nextstep(X+1,Y), 
    nextstep(X+1,Y), 
    nextstep(X+1,Y), 
    nextstep(X+1,Y), 
