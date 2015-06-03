get_graph_adj<-function (adj_matrix.or.csv_file, min.edge.weight=1)
{
  #Input: symmetric adjacency matrix
  library(igraph)
  if(length(dim(adj_matrix.or.csv_file))==0)
    m<-as.matrix(read.csv(file = adj_matrix.or.csv_file)[,-1])
  else
    m<-adj_matrix.or.csv_file
    
  if(min.edge.weight>1){
    m[m<min.edge.weight]<-0
    rc<-which(rowSums(m)==0)
    m<-m[-rc,-rc]
  }
    
  g<-graph.adjacency(adjmatrix = m, mode = "undirected",weighted = T)
}

# gr1<-get_graph_adj(adj_matrix.or.csv_file = "test_ICP.csv",min.edge.weight = 5)
# plot_graph(gr1,"gr1")
# gr2<-get_graph_adj(adj_matrix.or.csv_file = icp,min.edge.weight = 5)
# plot_graph(gr2,"gr2")
