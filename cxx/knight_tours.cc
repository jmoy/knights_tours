/*
A multi-threaded brute-force search for closed Knight's tours (used OpenMP)

Usage: knights_tours [m] [n]
 to search for tour on a m x n board
 rows are denoted by letters A.., columns by numbers 0..

Compile with
  g++ -O3 -Wall -std=c++11 -fopenmp    knight_tours.cc   -o knight_tours


Jyotirmoy Bhattacharya, 2015-04-22
*/


#include <vector>
#include <iostream>
#include <iterator>
#include <cstdlib>
#include <string>
#include <sstream>
#include <cassert>
#include <algorithm>

using namespace std;

const int PAR_THRESHOLD=10;

struct Graph{
  int nvertices;
  vector<string> vert_names;
  vector<vector<int> > neighbours;
};

void print_path(ostream &os,const Graph &g,const vector<int> path)
{
  for (auto k:path)
    os<<g.vert_names[k];
}

template <class CB>
void continue_tour_seq(const Graph &g,
                                vector<int> &path,
                                vector<char> &visited,
                                int remain,
                                CB &cb)
{
  const auto &neighs = g.neighbours[path.back()];

  if (remain==0){
    auto res = find(neighs.begin(),neighs.end(),0);
    if (res!=neighs.end()){ 
#pragma omp critical
      {   
        cb(path);
      }
    }
    return;
  }

  for (auto n: neighs){
   if (visited[n])
      continue;
      
    visited[n]=1;
    path.emplace_back(n);
    continue_tour_seq(g,path, visited,
                               remain-1,
                               cb);
    visited[n]=0;
    path.pop_back();
  }
}

template <class CB>
void continue_tour_par(const Graph &g,
                       const vector<int> &path,
                       const vector<char> &visited,
                       int depth,int remain,
                       CB &cb)
{
  assert(remain>0);
  for (auto n: g.neighbours[path.back()]){

    if (visited[n])
      continue;
    
#pragma omp task default(shared) firstprivate(n)
    {
      auto nvisited=visited;
      auto npath=path;
      nvisited[n]=1;
      npath.emplace_back(n);

      if (depth<PAR_THRESHOLD){
        continue_tour_par(g,npath, nvisited,
                          depth+1,remain-1,
                          cb);
      }else{
        continue_tour_seq(g,npath, nvisited,
                          remain-1,
                          cb);
      }
    }
  }
#pragma omp taskwait
}
  
template <class CB>
void enumerate_tours(const Graph &g,CB &cb)
{
  auto visited = vector<char>(g.nvertices,0);
  vector<int> path;
  visited[0] = true;
  path.emplace_back(0);
#pragma omp parallel
  {
#pragma omp single
    {
      continue_tour_par(g,path,visited,0,g.nvertices-1,cb);
    }
  }
}
             

struct Knight_Moves {
  int x;
  int y;
};

vector<Knight_Moves> valid_moves =
  { {2,1},
    {2,-1},
    {-2,1},
    {-2,-1},
    {1,2},
    {1,-2},
    {-1,2},
    {-1,-2}};

Graph make_chess_graph(int m,int n)
{
  Graph g;
  g.nvertices = m*n;
  g.vert_names.resize(m*n);
  g.neighbours.resize(m*n);
  for (auto i=0;i<m;i++){
    for (auto j=0;j<n;j++){
      auto k = i*n+j;
      ostringstream s;
      s<<char(i+'A')<<j;
      g.vert_names.at(k) = s.str();
      for (auto &mv: valid_moves){
        auto nx = i+mv.x;
        auto ny = j+mv.y;
        if (nx>=0 && nx<m && ny>=0 && ny<n)
          g.neighbours.at(k).emplace_back(nx*n+ny);
      }
    }
  }
  return g;
}

int main(int argc,char *argv[])
{
  if (argc!=3){
    cerr << "Usage: knight_tours [m] [n]";
    return 1;
  }

  int m = atoi(argv[1]);
  int n = atoi(argv[2]);

  if (m<1 || n<1){
    cerr << "Dimensions must be greater than 0";
    return 2;
  }

  auto g = make_chess_graph(m,n);
  for (auto i=0;i<g.nvertices;i++){
    cout<<g.vert_names[i]<<": ";
    for (auto k: g.neighbours[i]){
      cout<<g.vert_names[k]<<",";
    }
    cout<<'\n';
  }
  unsigned long count;
  auto printer =
    [&g,&count](const vector<int> &p){
    print_path(cout,g,p);cout<<'\n';
    count++;
  };
  
  enumerate_tours(g,printer);
  cout<<"\n\n Total tours = "<<count<<'\n';

  return 0;
}
    

