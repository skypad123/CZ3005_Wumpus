from pyswip import Prolog, Query
import

if __name__ == "__main__":

    prolog = Prolog()
    prolog.consult("agent_kbase.pl")

    li = list(prolog.query("dog(X)"))
    print(li)




class WumpusWorld(): 


    def __init__(self,Xsize,Ysize):
        self.Xsize = Xsize
        self.Ysize = Ysize


    def moveSingle(self):
        #return relative position from prolog
        #translate to absolute mapping
        #get state property python side
        #assert 