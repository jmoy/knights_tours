import sys
from collections import namedtuple

Graph = namedtuple("Graph",["nverts","neighs","names"])

def main():
    m = int(sys.argv[1])
    n = int(sys.argv[2])
    g = chess_graph(m,n)
    print(g)
    count = 0
    for path in enum_ham_cycles(g):
        print("".join(g.names[v] for v in path))
        count += 1
    print("\nTotal no. of cycles = ",count)

def enum_ham_cycles(g):
    path = [0]
    visited = {0}
    for p in complete_ham_cycle(g,g.nverts-1,path,visited):
        yield p

def complete_ham_cycle(g,remain,path,visited):
    last = path[-1]

    if remain==0:
        if 0 in g.neighs[last]:
            yield path
        return
    for n in g.neighs[last]:
        if n in visited:
            continue
        path.append(n)
        visited.add(n)
        for p in complete_ham_cycle(g,remain-1,path,visited):
            yield p
        del path[-1]
        visited.remove(n)

def chess_graph(m,n):
    def idx(i,j):
        return i*n+j
    def findneighs(i,j):
        valid_moves = [(1,2),(1,-2),(-1,2),(-1,-2),
                       (2,1),(2,-1),(-2,1),(-2,-1)]
        for x,y in valid_moves:
            p = i+x
            q = j+y
            if p>=0 and p<m and q>=0 and q<n:
                yield p,q
                
    nv = m*n
    neighs = [[idx(p,q) for p,q in findneighs(i,j)]
              for i in range(m)
              for j in range(n)]
    names = [chr(ord('A')+i)+str(j)
             for i in range(m)
             for j in range(n)]
    return Graph(nverts=nv,neighs=neighs,names=names)

if __name__=="__main__":
    main()
