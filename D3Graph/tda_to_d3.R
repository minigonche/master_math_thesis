library(rjson)
library(igraph)
#TDA into D3 converter


rm(list = ls())

scenario = 10
filter = TRUE



label_names = c('Zero', 'One', 'Two', 'Three', 'Four', 'Five','Six','Seven','Eight','Nine')

if(scenario == 1)
{
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_approx_all_angular/tsne/TDA/tda.RData")
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_RP500/RData/data.RData")
}

if(scenario == 2)
{
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_complete_all_angular/tsne/TDA/tda.RData")
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_RP500/RData/data.RData")
}


if(scenario == 3)
{
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_approx_all_angular/pca/TDA/tda.RData")
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_RP500/RData/data.RData")
}


if(scenario == 4)
{
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_complete_all_angular/pca/TDA/tda.RData")
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_RP500/RData/data.RData")
}



if(scenario == 5)
{
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_R784/cosine_distance_approx/tsne/TDA/tda.RData")
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_R784/RData/data.RData")
}

if(scenario == 6)
{
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_R784/cosine_distance_complete/tsne/TDA/tda.RData")
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_R784/RData/data.RData")
}


if(scenario == 7)
{
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_R784/cosine_distance_approx/pca/TDA/tda.RData")
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_R784/RData/data.RData")
}


if(scenario == 8)
{
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_R784/cosine_distance_complete/pca/TDA/tda.RData")
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_R784/RData/data.RData")
}



if(scenario == 9)
{
 
  label_names = c('T-Shirt', 'Trouser', 'Pullover', 'Dress', 'Coat', 'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot')
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_fashion_test_RP500/angular_distance_approx/tsne/TDA/tda.RData")
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_fashion_test_R784/RData/data.RData")
}

if(scenario == 10)
{
  
  label_names = c('T-Shirt', 'Trouser', 'Pullover', 'Dress', 'Coat', 'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot')
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_fashion_test_R784/cosine_distance_complete/tsne/TDA/tda.RData")
  load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_fashion_test_R784/RData/data.RData")
}


#Filters the resulting tda graph in case is to big

size_array = sapply(tda$result$points_in_vertex,length)

if(filter)
{
  min_size = 1
  while(tda$result$num_vertices > 1000)
  {
    min_size = min_size + 1
    only_ids = which(size_array >= min_size)
    
    size_array = size_array[only_ids]
    
    tda$result$adjacency = tda$result$adjacency[only_ids,only_ids]
    tda$result$points_in_vertex = tda$result$points_in_vertex[only_ids]
    tda$result$num_vertices = length(only_ids)
    
  }
  
  print(paste('Final Size:', tda$result$num_vertices))
  

}





#Compute the purity of each node

purity = sapply(tda$result$points_in_vertex, function(vec){(sort(table(working_data$data$label[vec]),decreasing=TRUE)[[1]] / length(vec))})
purity = sapply(purity, function(pur){round(pur,2)})

extract_common_label = function(vec)
{
  tab = sort(table(working_data$data$label[vec]),decreasing=TRUE)
  if(length(tab == 1))
  {
    return(working_data$data$label[vec][1])
  }
  
  return(as.integer(row.names(sort(table(working_data$data$label[vec]),decreasing=TRUE))[1]))
}

labels = sapply(tda$result$points_in_vertex, extract_common_label)
  
  
g = graph.adjacency(tda$result$adjacency, mode="undirected")

centrality = betweenness(g, directed = FALSE)
centrality = centrality/max(centrality)
centrality = sapply(centrality, function(num){round(num,2)})

fc <- fastgreedy.community(as.undirected(g))
#fc = iterative_ppr(adjusted_adj, alpha = 0.99, eps = 1e-9)


clusters = data.frame( id = c(), cluster = c())

for( i in 1:length(fc))
{
  clusters = rbind(clusters, data.frame( id = fc[[i]], cluster = i))
}

clusters = clusters[order(clusters$id),]

all_label_names = sapply(labels, function(i){label_names[i+1]})

nodes = data.frame(id = seq(tda$result$num_vertices), size = size_array, cen = centrality, label = labels, label_name = all_label_names, purity = purity , cluster = clusters$cluster  )

links = data.frame(as_edgelist(g))
colnames(links) = c('source','target')
links$weight = 1


#Javascript starts at 0
nodes$id = nodes$id  - 1
links$source = links$source - 1
links$target = links$target - 1

used_nodes = nodes[nodes$size>0,]
used_links = links[links$source %in% used_nodes$id,]
used_links = used_links[used_links$target %in% used_nodes$id,]

json_nodes <- toJSON(unname(split(used_nodes, 1:nrow(used_nodes))))
json_links <- toJSON(unname(split(used_links, 1:nrow(used_links))))

final_json = paste('{ "nodes":', json_nodes , ', \n "links": ', json_links , '}', sep = '')

write( final_json, file = '~/Documents/tesis_mat/master_math_thesis/D3Graph/jsons/graphFile.json')


cat(json_nodes)


#For the the json inside evry node

#loads tsne and tda
load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_approx_all/tsne/TDA/filter.RData")
tsne = filter_function$values

load("~/Documents/tesis_mat/TDAmapper/interactive_results/MNIST_test_RP500/cosine_distance_approx_all/pca/TDA/filter.RData")
pca = filter_function$values

final_string = ''

#iterate over all the nodes

for(i in seq(tda$result$num_vertices))
{
  current_ids = tda$result$points_in_vertex[[i]]
  current_labels = working_data$data[current_ids,]$label
  current_label_names = sapply(current_labels, function(i){label_names[i+1]})
  current_pca = data.frame(pca_x = pca[[1]][current_ids], pca_y = pca[[2]][current_ids])
  current_tsne = data.frame(tsne_x = tsne[[1]][current_ids], tsne_y = tsne[[2]][current_ids])
  
  #rounds
  current_tsne = round(current_tsne,2)
  current_pca = round(current_pca,2)
  
  #Javascripts starts from zero
  current_ids = current_ids -1
  
  current_frame = cbind(data.frame(id = current_ids ), data.frame(label = current_labels, label_name = current_label_names ), current_pca, current_tsne)
  
  current_json <- toJSON(unname(split(current_frame, 1:nrow(current_frame))))
  
  current_string = paste('{"node_id":', (i-1), ', "size":', length(tda$result$points_in_vertex[[i]]), ', "nodes_coord":', current_json, "}")
  final_string = paste(final_string, current_string)
  
  if(i < tda$result$num_vertices) 
  {
    final_string = paste(final_string, ',\n')
  }
  
  print(paste(i,'of', tda$result$num_vertices))
}

write( paste('{"node_info": [\n',final_string, ']}'), file = '~/Documents/tesis_mat/master_math_thesis/D3Graph/jsons/node_info_1.json')




