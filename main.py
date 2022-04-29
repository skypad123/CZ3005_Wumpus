from pyswip import Prolog, Query
import enum

class WumpusWorld(): 

    def set_wall(self):
        self.wall = "on"
        self.format = [["#", "#", "#"],["#", "#", "#"],["#", "#", "#"]]

    
    #map of size 7*6 outer cells is wall inner cells got at least 1 coin, 1 agent, 1 wumpus, 3 confundus portals 
    def __init__(self,Xsize = 7,Ysize = 6):
        self.Xsize = Xsize
        self.Ysize = Ysize

        self.confounded = "off"
        self.glitter = False
        self.stench = False
        self.tingle = False
        self.visited = False
        self.wumpus = False
        self.portal = False
        self.scream = False
        self.wall = "off"
        self.bump = False
        self.agent_abs_pos = [5, 2, 'rnorth']
        
        self.agent_state_dir = {
        "north" : "^",
        "east": ">",
        "south": "v",
        "west": "<",
        "rnorth" : "^",
        "reast": ">",
        "rsouth": "v",
        "rwest": "<",
        "containsWumpus": "W",
        "containsPortal": "O",
        "wumpusAndPortal": "U",#impossible
        "nonVisitedSafe": "s",
        "visitedSafe": "S",
        "none": "?",
        "abs_none": ".",
        "glitter": "*"
        }

        cell_symbol = [["%", "."],["=", "."],["T", "."],
                       ["-", " "],["W", "O", "U","^","<",">","v","s", "S","?"],["-", " "],
                       ["*", ","],["*", "."],["@", "."]]
        #####################
        self.symbols = {
            1: {"confounded": "%", "notConfounded": "."},
            2: {"stench": "=", "notStench": "."},
            3: {"tingle": "T", "notTingle": "."},
            4: {"agent": "-", "noAgent": " "},
            5: self.agent_state_dir,
            6: {"agent": "-", "noAgent": " "},
            7: {"glitter": "*", "noGlitter": "."},
            8: {"bumpOn": "B", "bumpOff": "."},
            9: {"screamOn": "@", "screamOff": "."}
        }

        self.abs_map_symbols = {
            1: {"confounded": "%", "notConfounded": "."},
            2: {"stench": "=", "notStench": "."},
            3: {"tingle": "T", "notTingle": "."},
            4: {"agent": "-", "noAgent": "."},
            5: self.agent_state_dir,
            6: {"agent": "-", "noAgent": "."},
            7: {"glitter": "*", "noGlitter": "."},
            8: {"bumpOn": "B", "bumpOff": "."},
            9: {"screamOn": "@", "screamOff": "."}
        }
        self.symbols_no = {
            1: {"confounded": 1, "notConfounded": 1, "stench": 2, "notStench": 2,
            "tingle": 3, "notTingle": 3,"agent": 4, "noAgent": 4, "center": 5, 
            "glitter": 7, "noGlitter": 7, "bumpOn": 8, "bumpOff": 8,
            "screamOn": 9, "screamOff": 9}
        }
        #####################
        self.abs_map = [["#","#","#","#","#","#"],
                       ["#",self.set_abs_map_symbol("abs_none"), self.set_abs_map_symbol("containsPortal",_2 = "tingle",_4 = "tingle",_6 = "tingle",_8 = "tingle"), self.set_abs_map_symbol("containsWumpus", _2 = "stench",_4 = "stench",_6 = "stench",_8 = "stench"), self.set_abs_map_symbol("abs_none"),"#"],
                       ["#",self.set_abs_map_symbol("containsPortal",_2 = "tingle",_4 = "tingle",_6 = "tingle",_8 = "tingle"), self.set_abs_map_symbol("abs_none"), self.set_abs_map_symbol("abs_none"), self.set_abs_map_symbol("abs_none"),"#"],
                       ["#",self.set_abs_map_symbol("abs_none"), self.set_abs_map_symbol("abs_none"), self.set_abs_map_symbol("abs_none"), self.set_abs_map_symbol("abs_none"),"#"],
                       ["#",self.set_abs_map_symbol("abs_none"), self.set_abs_map_symbol("glitter"), self.set_abs_map_symbol("containsPortal",_2 = "tingle",_4 = "tingle",_6 = "tingle",_8 = "tingle"), self.set_abs_map_symbol("abs_none"),"#"],
                       ["#",self.set_abs_map_symbol("abs_none"), self.set_abs_map_symbol("abs_none"), self.set_abs_map_symbol("abs_none"), self.set_abs_map_symbol("containsPortal",_2 = "tingle",_4 = "tingle",_6 = "tingle",_8 = "tingle"),"#"],
                       ["#","#","#","#","#","#"]]

        self.wall = [["#", "#", "#"],
                     ["#", "#", "#"],
                     ["#", "#", "#"]
                     ]  

        for i in range(self.Ysize):
            self.abs_map[0][i] = self.wall
            self.abs_map[-1][i] = self.wall
        for j in range(self.Xsize):
            self.abs_map[j][0] = self.wall
            self.abs_map[j][-1] = self.wall

        
        samplewall = "#"  
        #self.reset =
        

    def worldCreate(self):
        self.wall = [["#", "#", "#"],
                     ["#", "#", "#"],
                     ["#", "#", "#"]
                     ]
        
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

    def set_symbol(self, X, _1="notConfounded", _2="notStench", _3="notTingle", _4="noAgent", _6="noAgent", _7="noGlitter", _8="bumpOff", _9="screamOff"):
        cell_symbols = [[self.symbols[1][_1], self.symbols[2][_2], self.symbols[3][_3]],
                        [self.symbols[4][_4], self.symbols[5][X], self.symbols[6][_6]],
                        [self.symbols[7][_7], self.symbols[8][_8], self.symbols[9][_9]]]
        return cell_symbols

    def set_abs_map_symbol(self, X, _1="notConfounded", _2="notStench", _3="notTingle", _4="noAgent", _6="noAgent", _7="noGlitter", _8="bumpOff", _9="screamOff"):
        cell_symbols = [[self.abs_map_symbols[self.symbols_no[1][_1]][_1], self.abs_map_symbols[self.symbols_no[1][_2]][_2], self.abs_map_symbols[self.symbols_no[1][_3]][_3]],
                        [self.abs_map_symbols[self.symbols_no[1][_4]][_4], self.abs_map_symbols[self.symbols_no[1]["center"]][X], self.abs_map_symbols[self.symbols_no[1][_6]][_6]],
                        [self.abs_map_symbols[self.symbols_no[1][_7]][_7], self.abs_map_symbols[self.symbols_no[1][_8]][_8], self.abs_map_symbols[self.symbols_no[1][_9]][_9]]]
        return cell_symbols

    def print_map(self, abs_map):
        print()
        for i in range(len(abs_map)):
            for k in range(3):
                for cell in abs_map[i]:
                    for j in range(len(cell)):
                        print(cell[k][j], end=" ")
                    print(end="  ")
                print()
            print()

    def actions(action, abs_map):
        action_tuple = ("shoot", "moveforward", "turnleft", "turnright", "pickup")
        
        
    def moveSingle(self, src, dst, new_x, new_y, abs_map):
        #return relative position from prolog
        #translate to absolute mapping
        #get state property python side
        #assert
        print("move")

    def reset():
        print("reset call reborn and feedback loop")

    def get_sensory_state(self, x, y):
        sensory_arr = []
        sensory = ""
        if self.bump:
            cell = self.wall
        else:
            cell = self.abs_map[x][y]
        if cell[0][0] == "%":
            sensory += "confounded-"
            sensory_arr.append("on")
        else:
            sensory += "C-"
            sensory_arr.append("off")

        if cell[0][1] == "=":
            sensory += "Stench-"
            sensory_arr.append("on")
        else:
            sensory += "S-"
            sensory_arr.append("off")

        if cell[0][2] == "T":
            sensory += "Tingle-"
            sensory_arr.append("on")
        else:
            sensory += "T-"
            sensory_arr.append("off")

        if cell[2][0] == "*":
            sensory += "Glitter-"
            sensory_arr.append("on")
            cell[2][0] == "."  # remove glitter from map
        else:
            sensory += "G-"
            sensory_arr.append("off")

        if self.bump:
            sensory += "Bump-"
            sensory_arr.append("on")
        else:
            sensory += "B-"
            sensory_arr.append("off")

        if self.scream:
            sensory += "Scream-"
            sensory_arr.append("on")
        else:
            sensory += "S"
            sensory_arr.append("off")

        return sensory, sensory_arr
    def move_agent(self, agent_location, action, abs=True):

        if action == 'shoot':
            self.scream = True

        if action != 'shoot':
            self.scream = False

        if action != 'moveforward':
            self.bump = False

        if action == 'moveforward':
            if agent_location[2] == 'rsouth':
                if agent_location[0] + 1 < len(self.map) and abs and self.map[agent_location[0] + 1][agent_location[1]] == self.wall:
                    self.bump = True
                else:
                    agent_location[0] += 1

            elif agent_location[2] == 'rnorth':
                if (agent_location[0] - 1 >= 0 and abs and self.map[agent_location[0] - 1][agent_location[1]] == self.wall):
                    self.bump = True
                else:
                    agent_location[0] -= 1

            elif agent_location[2] == 'reast':
                if (agent_location[1]+1 < len(self.map[0]) and abs and self.map[agent_location[0]][agent_location[1]+1] == self.wall):
                    self.bump = True
                else:
                    agent_location[1] += 1

            elif agent_location[2] == 'rwest':
                if (agent_location[1]-1 >= 0 and abs and self.map[agent_location[0]][agent_location[1] - 1] == self.wall):
                    self.bump = True
                else:
                    agent_location[1] -= 1

        elif action == 'turnright':
            if agent_location[2] == 'rnorth':
                agent_location[2] = 'reast'
            elif agent_location[2] == 'reast':
                agent_location[2] = 'rsouth'
            elif agent_location[2] == 'rsouth':
                agent_location[2] = 'rwest'
            elif agent_location[2] == 'rwest':
                agent_location[2] = 'rnorth'

        elif action == 'turnleft':
            if agent_location[2] == 'rnorth':
                agent_location[2] = 'rwest'
            elif agent_location[2] == 'reast':
                agent_location[2] = 'rnorth'
            elif agent_location[2] == 'rsouth':
                agent_location[2] = 'reast'
            elif agent_location[2] == 'rwest':
                agent_location[2] = 'rsouth'

        return agent_location
    
    def start_agent(self, action_seq):
        prolog = Prolog()
        prolog.consult("agent_kbase.pl")
        
        list(prolog.query("reborn"))

        sensory, sensory_arr = self.get_sensory_state(self.agent_abs_pos[0], self.agent_abs_pos[1])
        sensory_arr[0] = "on"

        print("Action sequence: ", action_seq)
        print(sensory_arr)
        list(prolog.query(f"reposition({sensory_arr})"))
        
        list(prolog.query("retractall(current(_,_,_))"))
        prolog.asserta("current(5,2,rnorth)")
        
        currXYD = list(prolog.query("current(X,Y,D)"))
        
        if bool(prolog.query("explore(L)")):
            print("safe")
        
        self.reposition(currXYD)
        self.print_map(self.relative_map)
        # returned = False
        # start_explore = True
        
        # while(not returned and start_explore):
            
        #     if not bool(prolog.query("explore(L)")):
        #         explore = False
        #         break

    def reposition(self, curr):
        self.bump = "off"
        self.scream = "off"
        self.confounded = "off"
        self.agent_abs_pos = [5, 2, "rnorth"]  # make it random
        self.agent_relative = [
            (curr[0]["X"], curr[0]["Y"]), curr[0]["D"]]
        if self.agent_relative[1] == "rnorth":
            dir = "north"
        elif self.agent_relative[1] == "reast":
            dir = "east"
        elif self.agent_relative[1] == "rsouth":
            dir = "south"
        elif self.agent_relative[1] == "rwest":
            dir = "west"
        self.relative_map = [
            [self.set_symbol("none"), self.set_symbol("none"), self.set_symbol("none")],
            [self.set_symbol("none"), self.set_symbol(dir, _1="confounded", _4="agent",_6="agent"), self.set_symbol("none")],
            [self.set_symbol("none"), self.set_symbol("none"), self.set_symbol("none")]]
    
    def explore():
        list(prolog.query("reborn"))
        path = list(prolog.query("explore([L])"))
        print("repeating call this to get the explore(L) to confirm generated path correctness")
        
    def memory(actions):
        print("ask from prolog whther is in state confundus")
        
    def main():
        prolog = Prolog()
        prolog.consult("agent_kbase.pl")



if __name__ == "__main__":

    prolog = Prolog()
    prolog.consult("agent_kbase.pl")

    #li = list(prolog.query("dog(X)"))
    #print(li)

    isGameEnd = False
   
    #testing = list(prolog.query("faster(What, dog)"))
    #print(testing)

    
    
    while not isGameEnd:
        #print("Feedback")
        break

    x = 7
    y = 6
    
    #TestingWorld = [[0]*x]*y

    WW = WumpusWorld()
    WW.print_map(WW.abs_map)
    WW.start_agent(['turnright', 'moveforward', 'moveforward', 'moveforward', 'moveforward', 'moveforward', 'turnleft', 'moveforward', 'moveforward', 'pickup'])


    
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
    
    

    #for count, cellNo in enumerate(cells, start=1):
        #print(count, cellNo)


