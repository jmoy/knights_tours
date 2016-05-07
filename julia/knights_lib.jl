"""
    knights(m,n)

Enumerate all closed knight's tours on an 
m × n chesboard.
"""
function knights(m,n)
  g = mkChessGraph(m,n)
  n = 0
  enumHamCycle(g) do p
    println(join(map(k->g.names[k],p)))
    n+=1
  end
  println("Total no of cycles = $n")
end

"General graph type"
type Graph
  nverts::Int64
  names::Dict{Int,ASCIIString}
  neighs::Dict{Int,Array{Int64,1}}
end

"Enumerate all closed Hamiltonian cycles"
function enumHamCycle(callback::Function,g)
  visited = Set(0)
  path = Array(Int64,g.nverts)
  path[1] = 0
  completeHamCycle(g,g.nverts-1,path,visited,callback)
end

"Find all ways to extend a path to a closed Hamiltonian cycle"
function completeHamCycle(g,remain,path,visited,callback)
  depth = g.nverts-remain
  term = path[depth]
  if (remain==0)
    if in(0,g.neighs[term])
      callback(path)
    end
    return
  end
  for n in g.neighs[term]
    if in(n,visited)
      continue
    end
    path[depth+1] = n
    push!(visited,n)
    completeHamCycle(g,remain-1,path,visited,callback)
    delete!(visited,n)
  end
end

"Construct graph corresponding to Knights tours"
function mkChessGraph(m,n)
  nv = m*n
  g = Graph(nv,Dict(),Dict())
  for i in 0:m
    for j in 0:n
      k = i*n+j
      neighs = Int[]
      for t in [(1,2),(1,-2),(-1,2),(-1,-2),
                     (2,1),(2,-1),(-2,1),(-2,-1)]
          nx = i+t[1]
          ny = j+t[2]
          if nx>=0 && nx<m && ny>=0 && ny<n
            push!(neighs,nx*n+ny)
          end
      end
      g.neighs[k] = neighs
      g.names[k]="$(Char(i+Int('A')))$j"
    end
  end
  g
end

