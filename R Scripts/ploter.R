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



#Export parameteters
# Trying to meet the PLos standards
height = 6.1
width = 5.1
units = 'in'
resolution = 300  
font_family = 'Arial'
font_size = 12
compression = 'lzw'

scenario = 5

#loads the data
load("~/Documents/tesis_mat/master_math_thesis/TDAmapper/interactive_results/MNIST_test_R784/RData/data.RData")

#Loads TDA
load("~/Documents/tesis_mat/master_math_thesis/TDAmapper/interactive_results/MNIST_test_R784/cosine_distance_approx/tsne/TDA/tda.RData")
#load("~/Documents/tesis_mat/master_math_thesis/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_approx_all_angular/tsne/TDA/tda.RData")

source('~/Documents/tda_mapper/RScripts/create_graph.R')

location = '~/Documents/tesis_mat/master_math_thesis/TDAmapper/interactive_results/MNIST_test_R784/cosine_distance_approx/tsne/plots/'
#location = '~/Documents/tesis_mat/master_math_thesis/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_approx_all_angular/tsne/plots/'


dir.create(location, showWarnings = TRUE)


compute_purity = function(tda_result, dta)
{
  
  result = data.frame(label = c(),  node = c(), p_value = c())
  node_id = 1
  for(v in tda_result$points_in_vertex)
  {
   
    print(paste(node_id, ' of ', tda_result$num_vertices, sep = '' ))
    temp = data.frame(label = dta$label[v],  node = node_id, p_value = -1  )
    total = nrow(temp)
    
    for(i in seq(0,9))
    {
      temp_index = which(temp$label == i)
      temp$p_value[temp_index] = length(temp_index)/total
      
    }
    
    node_id = node_id + 1
    result = rbind(result, temp)
    
  }
  
  print('Finished Purity')
  return(sapply(seq(0,9), function(i){ mean(result$p_value[which(result$label == i)])}))
  
}

compute_groupness = function(tda_result, dta)
{
  
  result = data.frame(node = c(),label = c(), total_elements = c())
  node_id = 1
  for(v in tda_result$points_in_vertex)
  {
    print(paste(node_id, ' of ', tda_result$num_vertices, sep = '' ))
    sample_labels = dta$label[v]
    temp = data.frame( node = node_id,  label = seq(0,9), total_elements = -1  )
    
    for(i in seq(0,9))
    {
      temp$total_elements[i + 1] = length(which(sample_labels == i))
      
    }
    
    node_id = node_id + 1
    result = rbind(result, temp)
    
  }
  
  totals = sapply(seq(0,9), function(i){ length(which(dta$label == i))})
  avgs = sapply(seq(0,9), function(i){ mean(result$total_elements[which(result$label == i)])})
    
  print('Finished Groupness')
  return(avgs)
  
}


compute_conectivity = function(tda_result, dta)
{
  arches = sum(tda_result$adjacency)/2
  total = choose(tda_result$num_vertices,2)
  return(arches/total) 
}

#returns
# [num_components, edge_connectivity, vertex_connectivity]
compute_connectivity_values = function(adj_matrix)
{
  
    #Removes isolated nodes
    
    out_flow = colSums(adj_matrix)
    selected_ids = which(out_flow > 0)
    adj_matrix = adj_matrix[selected_ids,selected_ids]
    
  
    g = graph.adjacency(adj_matrix, mode="undirected")
    comp = components(g)
    
    edge = 0
    vertex = 0
    
    
    for(i in 1:comp$no)
    {
      
      iter_ids = which(comp$membership == i)
      adj_temp = adj_matrix[iter_ids,iter_ids]
      g_temp = graph.adjacency(adj_temp, mode="undirected")
      v = cohesion(g_temp)
      e = edge_connectivity(g_temp)
      
      if(v == 0 || e == 0)
      {
        stop('They are strongly connected components, both connectivities should be larger than zero')
      }
      
      edge = edge + e
      vertex = vertex + v
      
    }
    
    
    return(c(comp$no, edge/comp$no, vertex/comp$no))
  
}


print_table = function(tda_result, dta)
{
  frame = data.frame(matrix(ncol = 12, nrow = 0))
  colnames(frame) = c('Value', sapply(seq(0,9), function(i){ paste(i, sep = '') }), 'Avg.')
  
  
  group = compute_groupness(tda_result, dta)
  groupness = round(group*100, digits = 2)
  mean_g = round(mean(groupness),2)
  temp_row_g = c('Groupness' ,groupness, mean_g)
  
  purity = round(compute_purity(tda_result, dta)*100, digits =1)
  mean_p = round(mean(purity), digits = 1)
  temp_row_p = c('Purity (%)' ,purity, mean_p )
  
  frame[1,] = temp_row_p
  frame[2,] = temp_row_g
  
  
  tb = xtable(frame)
  print(tb)
  
}

print(round(compute_conectivity(tda$result, working_data$data)*100, digits = 1))
print_table(tda$result, working_data$data)




colors = c('red','greenyellow','blue','pink','yellow','orange','lightcyan1','darkolivegreen', 'brown','purple')


# Plots the graph for the TDA anaylsis
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
                  only_node_ids = new_ids,
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
     edge.color = rgb(20,20,20, max = 255))

legend('bottomleft', sapply(seq(0,9), function(i){paste(i, sep = '')}), 
       lty=c(1), lwd=c(8), col=colors, cex = 2) # gives the legend lines the correct color and width

title(main = paste('TDA Graph - Scenario ', scenario , sep = ''), cex.main = 3.5)

dev.off()



#load("~/Documents/tesis_mat/master_math_thesis/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_complete_all_angular/tsne/TDA/tda.RData")

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

adj_matrix = tda$result$adjacency[only_node_ids,only_node_ids]
connectivity = compute_connectivity_values(adj_matrix)
print(paste('Componentes: ', connectivity[1], sep = ''))
print(paste('Edge Connectivity: ', round(connectivity[2],2), sep = ''))
print(paste('Vertex Connectivity: ', round(connectivity[3],2), sep = ''))


adj_matrix = tda$result$adjacency
g = graph.adjacency(adj_matrix, mode="undirected")
comp = components(g)

new_ids = which(comp$membership == 1)

only_node_ids = intersect(total_neighbors, which(size_array >= min_include))


df = working_data$data
tda1 = tda$result
min_draw_size = 4 
max_draw_size = 8
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
     edge.color = rgb(20,20,20, max = 255))

legend('bottomleft', sapply(seq(0,9), function(i){paste(i, sep = '')}), 
       lty=c(1), lwd=c(8), col=colors, cex = 2) # gives the legend lines the correct color and width

#title(main = paste('Selected Component - Scenario ', scenario , sep = ''), cex.main = 3.5)












ids = 1:length(only_node_ids)

for(i in ids)
{
  temp_ids = setdiff(ids,c(i))
  g2 = graph.adjacency(adj_matrix[temp_ids,temp_ids], mode="undirected")
  comp = components(g)
  print(paste(i,comp$no))
  
}


central_id = 120

g = graph.adjacency(adj_matrix, mode="undirected")
V(g)$color = 'blue'
V(g)$label = rep(NA,dim(adj_matrix)[1])
V(g)$size = 3
V(g)$color[central_id] = 'red'
V(g)$size[central_id] = 4
plot(g)








neighbors = which(adj_matrix[central_id,] == 1)
next_neighbors = c()
for(j in neighbors)
{
  next_neighbors = c(next_neighbors, which(adj_matrix[j,] == 1))
}

total_neighbors = sort(unique(c(neighbors,next_neighbors)))
adj_n = adj_matrix[total_neighbors,total_neighbors]

loc = which(total_neighbors == 29)

g = graph.adjacency(adj_n, mode="undirected")
V(g)$color = 'blue'
V(g)$label = rep(NA,dim(adj_n)[1])
V(g)$size = 6
V(g)$color[loc] = 'red'
V(g)$size[loc] = 9
plot(g)


#load("~/Documents/tesis_mat/master_math_thesis/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_complete_all_angular/tsne/TDA/tda.RData")

tda$result$points_in_vertex[1538]
