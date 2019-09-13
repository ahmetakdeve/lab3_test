#' Dijkstra's shortest path algoritm
#'
#' Dijkstra's algorithm to find the shortest path in a graph
#'
#' @param v1 Start node
#' @param v2 End node
#' @param w Weighted path between start node and end node
#'
#' @return A vector contains the shortest paths from specific start node to other nodes
#'
#' @export


dijkstra = function(graph, init_node, mpath = FALSE) {
  A<-graph;source<-init_node
  
  if(colnames(A)[1]!="v1"|colnames(A)[2]!="v2"|colnames(A)[3]!="w") stop("Wrong colnames")
  
  # A: matrix
  # source: start node
  # MPATH: multi paths
  
  b <- matrix(0,nrow = 6,ncol = 6)
  for (i in 1:length(A$v1)) {
    b[A$v1[i],A$v2[i]] <- A$w[i]
  }
  
  temp.A <- b
  temp.A[which(temp.A == 0)] <- Inf 
  diag(temp.A)<-0  
  n <- dim(temp.A)[1]  
  
  result.dis <- t(temp.A[source, ])  
  if (source == 1) {    
    open = (source + 1):n  
  } else if (source == n) {    
    open = 1:(source - 1)  
  } else {    
    open = c(1:(source - 1), (source + 1):n)  
  }  
  close<-c(source)  
   path = matrix(0,n, n)  
  path[, 1] <- source  
  if (mpath == TRUE) {    
    p1 = matrix(c(source, rep(0, n - 1)), , n)    
    result.path = c()    
    for (i in 1:n) {      
      result.path[[i]] = p1    
    }  
  } else {    
    path = matrix(0, n, n)    
    path[, 1] = source  
  }  
  
  while (length(open) > 0) {    
   
    id = which.min(result.dis[open])    
   
    new.id = open[id]    
    if (mpath) {      
      temp.path <- result.path[[new.id]]      
      temp.path <- matrix(temp.path, ,n)      
      nn <- nrow(temp.path)      
      mm <- ncol(temp.path)      
      temp.id.vec <- apply(temp.path, 1, which.min)      
      id.vec = nn * (temp.id.vec - 1) + matrix(1:nn, nn, 1)      
      temp.path[id.vec] = new.id      
      result.path[[new.id]] = temp.path    
    } else {      
      temp.id = which.min(path[new.id, ])      
      path[new.id, temp.id] = new.id    
    }    
    open = open[-id]    
    if (length(open)>0)    {      
      update.node = open[temp.A[new.id, open] != Inf]      
      if (length(update.node) > 0) {        
        for (i in 1:length(update.node)) {          
          temp.dis = result.dis[new.id] + temp.A[new.id, update.node[i]]          
          if (temp.dis < result.dis[update.node[i]]) {            
            result.dis[update.node[i]] = temp.dis            
            if (mpath) {              
              result.path[[update.node[i]]] = result.path[[new.id]]           
            } else {              
              path[update.node[i], ] = path[new.id, ]            
            }          
          } else if ((temp.dis == result.dis[update.node[i]]) && mpath) {            
            temp.mat = result.path[[update.node[i]]]            
            add.row = result.path[[new.id]]            
            temp.mat = rbind(temp.mat, add.row)            
            result.path[[update.node[i]]] = temp.mat          
          }       
        }     
      }    
    }  
  }  
  if (mpath) {    
    path = result.path  
  }  
  output<-list(dis=result.dis, path=path)  
  return(c(result.dis))
}  

