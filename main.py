from pyswip import Prolog, Query

if __name__ == "__main__":

    prolog = Prolog()
    prolog.consult("agent_kbase.pl")

    #li = list(prolog.query("dog(X)"))
    #print(li)

    isGameEnd = False
   
    testing = list(prolog.query("faster(What, dog)"))
    print(testing)
    
    while not isGameEnd:
        print("Feedback")
        break

    TestingWorld = [[1,2,3,4,5,6,7],
                    [2,2,3,4,5,6,7],
                    [3,2,3,4,5,6,7],
                    [4,2,3,4,5,6,7],
                    [5,2,3,4,5,6,7],
                    [6,2,3,4,5,6,7]]

    for row in TestingWorld:
        for col in row:
            print(col, end=" ")
        print()

    Symbol_Confounded = ["%", "."]
    Symbol_Stench = ["=", "."]
    Symbol_Tingle = ["T", "."]
    Symbol_ContainAgentNPC = ["-", " "]
    Symbol_WumpusConfundus = ["W", "O", "U"]
    Symbol_AgentDirection = ["^","<",">","v"]
    Symbol_Visited = ["s", "S"]
    Symbol_Non = "?"
    Symbol_Glitter = ["*", "."]
    Symbol_Bump = ["B", "."]
    Symbol_Scream = ["@", "."]
    
    cells = [1,2,3,4,5,6,7,8,9]

class WumpusWorld(): 

    #map of size 7*6 outer cells is wall inner cells got at least 1 coin, 1 agent, 1 wumpus, 3 confundus portals 
    def __init__(self,Xsize,Ysize):
        self.Xsize = Xsize
        self.Ysize = Ysize


    def moveSingle(self):
        #return relative position from prolog
        #translate to absolute mapping
        #get state property python side
        #assert
        print("move")

    def reset():
        print("reset call reborn and feedback loop")
    
    def explore():
        path = list(prolog.query("explore([L])"))
        print("repeating call this to get the explore(L) to confirm generated path correctness")
        
    def memory(actions):
        print("ask from prolog whther is in state confundus")
        
    def main():
        prolog = Prolog()
        prolog.consult("agent_kbase.pl")
