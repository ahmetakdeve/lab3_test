wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
wiki_graph

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
  ## 初始化临街矩阵，不可达置为Inf  
  temp.A[which(temp.A == 0)] <- Inf 
  # 对角线置为0 去自连接  
  diag(temp.A)<-0  
  # 节点总数n  
  n <- dim(temp.A)[1]  
  # 记录源节点到其他节点的距离,默认取邻居节点距离 一行n列  
  
  result.dis <- t(temp.A[source, ])  
  # open: 待遍历节点 
  # close：已计算节点
  if (source == 1) {    
    open = (source + 1):n  
  } else if (source == n) {    
    open = 1:(source - 1)  
  } else {    
    open = c(1:(source - 1), (source + 1):n)  
  }  
  close<-c(source)  
  # 路径储存  
  path = matrix(0,n, n)  
  # 第一个为初始节点 
  path[, 1] <- source  
  # 计算多条最短路径  
  if (mpath == TRUE) {    
    # 初始化最短路径初始节点为source节点    
    p1 = matrix(c(source, rep(0, n - 1)), , n)    
    # 路径列表    
    result.path = c()    
    for (i in 1:n) {      
      result.path[[i]] = p1    
    }  
  } else {    
    # 只保留一条最短路径    
    path = matrix(0, n, n)    
    path[, 1] = source  
  }  
  
  while (length(open) > 0) {    
    # 从open（T）集合中选出待遍历节点中路径最短的节点，该节点是open（T）集合中到close(S)集合距离最小的节点 id从open取出然后放入close集合    
    # id = which(result.dis[open] == min(result.dis[open]))[1]    
    # id为最小距离节点的在open表中的下标    
    id = which.min(result.dis[open])    
    # 将id 加入到close   
    # 省略最小距离节点的下标对应的节点    
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
      # new.id节点的最短路径 第一个元素应该为0      
      temp.id = which.min(path[new.id, ])      
      path[new.id, temp.id] = new.id    
    }    
    # 从open表删除当前节点    
    open = open[-id]    
    ## 更新最短路径   
    if (length(open)>0)    {      
      # 更新节点new.id 的邻居节点的距离,update.node待更新节点，即new.id 的邻居节点中可达的节点      
      update.node = open[temp.A[new.id, open] != Inf]      
      if (length(update.node) > 0) {        
        #遍历要更新的节点        
        for (i in 1:length(update.node)) {          
          # source节点到new.id节点距离+new.id和待更新节点距离之和          
          temp.dis = result.dis[new.id] + temp.A[new.id, update.node[i]]          
          # 如果距离之和小于  直接距离source到待更新节点的直接距离          
          if (temp.dis < result.dis[update.node[i]]) {            
            # 更新最短路径距离            
            result.dis[update.node[i]] = temp.dis            
            # 更新最短路径            
            # path[update.node[i],which.min(path[update.node[i],])[1]]=new.id            
            # 如果保存多路径            
            if (mpath) {              
              result.path[[update.node[i]]] = result.path[[new.id]]           
            } else {              
              path[update.node[i], ] = path[new.id, ]            
            }          
          } else if ((temp.dis == result.dis[update.node[i]]) && mpath) {            
            # 当最短距离与已有最短距离相同时，加入最短路径集合            
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

?dijkstra
