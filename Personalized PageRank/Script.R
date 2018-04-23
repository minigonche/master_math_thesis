
library(igraph)
#Script for viewing clusters over the colombian political map
#Required Libraries
library('maptools')
library('rgdal')
library('classInt')
library('RColorBrewer')
library('spdep')
library('pracma')
library('raster')    
library('prettymapr')
library('gridExtra')
library('grid')
library('gtable')
library('ggplot2')
library("extrafont")# for arial font
library('xtable')



#Defines a function that given a starting vertex,
#Calculates its surrounding cluster

#' @param adj The adjacency matrix of the current graph
#' @param vertex The index of the initial vertex
#' @param alpha Float between 0 and 1, corresponding to the alpha value in PPR
#' @param eps Float larger than zero. The epsylon value indicating when to stop squeeshing.
#'
#' @return A list, where each coordiante contains the set of vertex index in that cluster

find_cluster_ppr = function(adj, vertex, alpha = 0.5, eps = 0.001){
  
  #declares the number of vertices
  N = dim(adj)[1]
  if(vertex < 1){
    stop(paste('Vertex index cannot be less than 1: index = ', vertex, ' of N = ', N, sep = '' ))
  }
  if(vertex > N){
    stop(paste('Vertex index cannot be larger than the amount of elements: index = ', vertex, ' of N = ', N, sep = '' ))
  }
  #Array with indices
  indices = 1:N
  #Initializes the rank and the residue
  rank = rep(0,N)
  residue = rep(0,N)
  residue[vertex] = 1
  #out going degree array
  degree = rowSums(adj)
  if(min(degree) == 0){
    stop('The minimum outdegree of every node must be at least one (all must include self loops)')
  }
  
  #Division od residue/degree
  eps_vector = residue/degree
  flag = max(eps_vector) >= eps

  if(!flag){
    return(vertex)
  }
  
  while(flag){
    
    #Gets max
    index = which.max(eps_vector)
    #Extracts residue and sets it to zero
    res = residue[index]
    residue[index] = 0
    #updates own rank
    rank[index] = rank[index] + alpha*res
    neighbors = indices[as.logical(adj[index,])]
    V = length(neighbors)
    #print(V)
    plus = (1- alpha)*res/(2*V)
    residue[neighbors] = residue[neighbors] + plus
    
    eps_vector = residue/degree
    flag = max(eps_vector) >= eps                
  }
  
  return(indices[which(rank > 0)])
  
}

#Defines a function that given an adjacency matrix,
#Calculates all clusters inside it

#' @param adj The adjacency matrix of the current graph
#' @param alpha Float between 0 and 1, corresponding to the alpha value in PPR
#' @param eps Float larger than zero. The epsylon value indicating when to stop squeeshing.
#'
#' @return A list, where each coordiante contains the set of vertex index in that cluster

iterative_ppr = function(adj, alpha = 0.5, eps = 0.001){
  
  #Exctracts the dimension
  N = dim(adj)[1]
  #Declares indices
  indices = 1:N
  #declares unused indices
  unused = 1:N
  
  clusters = list()
  num_clusters = 0
  while(length(unused) > 0){
    
    #Extract a random element
    vertex = sample(1:length(unused),1)
    adj_temp = adj[unused,unused]
    #finds clusters
    selected = unused[find_cluster_ppr(adj_temp, vertex, alpha = alpha, eps = eps)]
    print(paste('Cluster of size: ', length(selected), ' found. Nodes to go: ', length(unused), sep = ''))
    #adds cluster
    num_clusters = num_clusters + 1
    clusters[[num_clusters]] = selected
    #Removes the found vertices
    unused = setdiff(unused,selected)
    print(paste('Cluster of size: ', length(selected), ' found. Nodes to go: ', length(unused), sep = ''))
    if(length(unused) == 1){
      num_clusters = num_clusters + 1
      clusters[[num_clusters]] = unused
      unused = c()
    }
  }
  
  return(clusters)
  
}

load("~/Documents/tesis_mat/master_math_thesis/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_complete_all/tsne/TDA/tda.RData")


size_array = sapply(tda$result$points_in_vertex,length)
min_size = min(size_array)
max_size = max(size_array)

min_include = 2
max_include = max_size

only_node_ids = NULL

if(min_include > min_size || max_include < max_size)
{
  only_node_ids = intersect(which(size_array >= min_include), which(size_array <= max_include))
  
}




colors = c('red','greenyellow','blue','pink','yellow','orange','lightcyan1','darkolivegreen', 'brown','purple')
colors_cluster = c('deeppink','midnightblue','seagreen1','mediumpurple1','peachpuff4', 'deeppink4','purple', colors, colors, colors, colors, colors)
adj = tda$result$adjacency
g  <- graph.adjacency(adj)
adjusted_adj = adj
diag(adjusted_adj) = 1
#adjusted_adj = adjusted_adj[only_node_ids,only_node_ids]
fc <- iterative_ppr(adjusted_adj, alpha = 0.001, eps = 0.000004)

print(length(fc))

clusters = list()

for( i in 1:length(fc))
{
  clusters[[i]] = sort(unique(unlist(tda$result$points_in_vertex[fc[[i]]])))
}



#Assigns row number id
working_data$data$id = seq(dim(working_data$data)[1])


eth_fun_select = function(x)
{
  if(length(unique(x$label)) == 1)
  {
    i = x$label[[1]] + 1
    return(as.vector(col2rgb(colors[i], alpha = FALSE)[,1]))
  }
  
  i = as.integer(row.names(sort(table(x$label),decreasing=TRUE))[1]) + 1
  return(as.vector(col2rgb(colors[i], alpha = FALSE)[,1]))
  
  
}

# Plots the graph for the TDA anaylsis
eth_fun_select_clus = function(x)
{
  
  color = 'black'
  score = c()
  for( i in 1:length(fc))
  {
    score[i] = sum(x$id %in% clusters[[i]])/length(x$id)
  }
  #print(which.max(score))
  color = colors_cluster[which.max(score)]
  return(as.vector(col2rgb(color, alpha = FALSE)[,1]))
  
  
}

intensity = function(x)
{
  
  return(sort(table(x$label),decreasing=TRUE)[[1]] / length(x$label))
  
  
}


size_array = sapply(tda$result$points_in_vertex,length)
min_size = min(size_array)
max_size = max(size_array)

min_include = 2
max_include = max_size

only_node_ids = NULL

if(min_include > min_size || max_include < max_size)
{
  only_node_ids = intersect(which(size_array >= min_include), which(size_array <= max_include))
  
}


df = working_data$data
tda1 = tda$result
min_draw_size = 2 
max_draw_size = 14
size_aprox_fun = function(x){x**(0.45)}
color_scheme = 'LINEAR'
color_attributes = c('label','pixel_1','id') 
color_fun = function(x){1}
color = NULL
color_select_fun = eth_fun_select_clus
label_scheme = 'NONE'
label_fun = NULL
label_attributes = NULL
min_label_node_size = 0
max_label_node_size = Inf 
topology_only = FALSE


#cluster_graph
g_clus = create_graph(df=df,
                      tda=tda1,
                      only_node_ids = only_node_ids,
                      min_draw_size = min_draw_size,
                      max_draw_size = max_draw_size,
                      color_scheme = color_scheme,
                      color_attributes = color_attributes,
                      color_fun = color_fun,
                      color = color,
                      color_select_fun = color_select_fun,
                      label_scheme = label_scheme,
                      label_fun = label_fun,
                      label_attributes = label_attributes,
                      min_label_node_size = min_label_node_size,
                      max_label_node_size = max_label_node_size, 
                      topology_only = topology_only)


lay = layout_nicely(g_clus)


#saves cluster graph
png(height = 1000,
    width = 1000,
    file = paste(location,'plot_scenario_clusters',scenario,'.png', sep = '') )


plot(g_clus,  layout = lay, edge.color = rgb(20,20,20, max = 255))

legend('bottomleft', sapply(seq(1,5), function(i){paste('Cluster', i, sep = '')}), 
       lty=c(1), lwd=c(8), col=colors_cluster, cex = 2) # gives the legend lines the correct color and width

title(main = paste('TDA Graph - Identified Custers for Scenario ', scenario , sep = ''), cex.main = 3.5)

dev.off()



#Original graph

df = working_data$data
tda1 = tda$result
min_draw_size = 2 
max_draw_size = 14
size_aprox_fun = function(x){x**(0.45)}
color_scheme = 'LINEAR'
color_attributes = c('label','pixel_1') 
color_fun = intensity
color = NULL
color_select_fun = eth_fun_select
label_scheme = 'NONE'
label_fun = NULL
label_attributes = NULL
min_label_node_size = 0
max_label_node_size = Inf 
topology_only = FALSE





png(height = 1000,
    width = 1000,
    file = paste(location,'plot_scenario',scenario,'.png', sep = '') )


plot(create_graph(df=df,
                  tda=tda1,
                  only_node_ids = only_node_ids,
                  min_draw_size = min_draw_size,
                  max_draw_size = max_draw_size,
                  color_scheme = color_scheme,
                  color_attributes = color_attributes,
                  color_fun = color_fun,
                  color = color,
                  color_select_fun = color_select_fun,
                  label_scheme = label_scheme,
                  label_fun = label_fun,
                  label_attributes = label_attributes,
                  min_label_node_size = min_label_node_size,
                  max_label_node_size = max_label_node_size, 
                  topology_only = topology_only),
     layout = lay, edge.color = rgb(20,20,20, max = 255))

legend('bottomleft', sapply(seq(0,9), function(i){paste(i, sep = '')}), 
       lty=c(1), lwd=c(8), col=colors, cex = 2) # gives the legend lines the correct color and width

title(main = paste('TDA Graph - Scenario ', scenario , sep = ''), cex.main = 3.5)

dev.off()


