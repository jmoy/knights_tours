use std::env;

#[derive(Debug)]
struct Graph {
  nvertices: usize,
  vert_names: Vec<String>,
  neighbours: Vec<Vec<usize>>
}

fn main()
{
  let args: Vec<String> = env::args().collect();
  let m = arg_to_dim(&args,1);
  let n = arg_to_dim(&args,2);
  let g = make_chess_graph(m,n);
  let mut count = 0;
  {
    let mut printer = |path:&Vec<usize>| {
      for k in path {
        print!("{}",g.vert_names[*k]);
      } 
      println!("");
      count += 1;
    };
    enumerate_tours(&g,&mut printer);
  }
  println!("\nTotal no of paths = {}",count);
}
    
fn enumerate_tours<F>(g: &Graph, cb: &mut F)
where F:FnMut(&Vec<usize>)
{
  let mut visited:Vec<bool> = vec![false;g.nvertices];
  let mut path:Vec<usize> = vec![0;g.nvertices];
  visited[0] = true;
  continue_tour(&g,&mut path,&mut visited,g.nvertices-1,cb);
}
             
fn continue_tour<F>(g: &Graph,
                    path: &mut Vec<usize>,
                    visited: &mut Vec<bool>,
                    remain: usize,
                    cb: &mut F
                  )
where F:FnMut(&Vec<usize>)
{
  let depth = g.nvertices - remain;
  let neighs = &g.neighbours[path[depth - 1]];

  if remain==0 {
    if neighs.iter().find(|&&x| x==0).is_some() {
      cb(path);
    }
    return;
  }
  
  for n in neighs {
    if visited[*n] {
      continue;
    }
      
    visited[*n]=true;
    path[depth]=*n;
    continue_tour(g,path,visited,remain-1,cb);
    visited[*n]=false;
  }
}

 
fn make_chess_graph(m:usize,n:usize) -> Graph
{
  let valid_moves:Vec<(i32,i32)> = vec! [ 
                                        (2,1),
                                        (2,-1),
                                        (-2,1),
                                        (-2,-1),
                                        (1,2),
                                        (1,-2),
                                        (-1,2),
                                        (-1,-2)
                                        ];

  let nvertices = m*n;
  let mut vert_names = vec!["".to_string();m*n];
  let mut neighbours = vec![vec![];m*n];
  for i in 0..m {
    for j in 0..n{
      let k = i*n+j;
      vert_names[k] = format!("{}{}",(i as u8+'A' as u8) as char,j);
      for mv in &valid_moves{
        let nx = (i as i32)+mv.0;
        let ny = (j as i32)+mv.1;
        if nx>=0 && ny>=0 {
          let nx = nx as usize;
          let ny = ny as usize;
          if nx < m && ny < n {
            neighbours[k].push(nx*n+ny);
          }
        }
      }
    }
  }
  return Graph{ nvertices:nvertices,
                vert_names:vert_names,
                neighbours:neighbours};
}

fn arg_to_dim(args: &[String], n: usize) -> usize
{
  return args.get(n)
          .expect("missing argument")
          .parse()
          .expect("non-integral");
}


