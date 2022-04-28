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

    x = 7
    y = 6
    #TestingWorld = [[0]*x]*y

    #this is absolute map
    TestingWorld =  [["#","#","#","#","#","#","#"],
                    ["#","T","O","W","=","O","#"],
                    ["#","s","T","=","*","T","#"],
                    ["#","s","s","T","O","T","#"],
                    ["#","^","s","s","T","s","#"],
                    ["#","#","#","#","#","#","#"]]

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
    
    cells = [["%", "."],["=", "."],["T", "."],["-", " "],["W", "O", "U","^","<",">","v","s", "S","?"],["-", " "],["*", ","],["*", "."],["@", "."]]

    for count, cellNo in enumerate(cells, start=1):
        print(count, cellNo)

class WumpusWorld(): 

    #map of size 7*6 outer cells is wall inner cells got at least 1 coin, 1 agent, 1 wumpus, 3 confundus portals 
    def __init__(self,Xsize = 7,Ysize = 6):
        self.Xsize = Xsize
        self.Ysize = Ysize
        #self.reset =
        self.AbsMap = []

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
