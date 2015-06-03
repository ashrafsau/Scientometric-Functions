plot_graph<-function (graph_,title_="Not Titled")
{
  if(class(graph_)!="igraph")stop("graph_ must be igraph object!")
  
  library(igraph)
  ew<-(E(graph_)$weight/max(E(graph_)$weight))*5
  width <- 1200 #as.numeric(tkcget(canvas, "-width"))
  height <- 800 #as.numeric(tkcget(canvas, "-height"))
  padding<-50  
  
  id<-tkplot(graph = graph_,canvas.width = width, canvas.height = height, vertex.label=V(graph_)$name,
             layout=layout.fruchterman.reingold,
             edge.label=E(graph_)$weight,edge.color="gray",edge.width=ew,
             edge.label.color="black",vertex.label.color="black")  
  canvas <- tkplot.canvas(id)  
  coords <- layout.norm(layout = layout.fruchterman.reingold(graph_), xmin = padding, xmax = width-padding,
                        ymin= padding, height-padding)
  tkplot.setcoords(id, coords)  
  # plot the graph
  tkcreate(canvas, "text", width/2, 15,text= sprintf("Graph for %s",title_), justify="center",
           font=tkfont.create(family="helvetica", size=10,weight="bold"))
  id
}