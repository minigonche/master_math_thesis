
source('~/Documents/tda_mapper/RScripts/create_graph.R')

#Scripts path
scripts_path = '~/Documents/tesis_mat/master_math_thesis/TDAmapper/interactive_results/RScripts/'

setwd(scripts_path)
source('mapperKD_low_RAM.R')



numpoints = 1000


datos_curva_momento<-data.frame(x1=cos((1:numpoints)),y1=sin((1:numpoints)),x2=cos(2*(1:numpoints)),y2=sin(2*(1:numpoints)))

# Number of dimensions
k = 1
# Number of Intervals
num_intervals = c(10)
# percentage of overlap
percent_overlap = 40
# number of bins when clustering
num_bins_when_clustering = 12



tda = mapperKD(k = k,
                distance_matrix = dist(datos_curva_momento),
                filter_values = list(datos_curva_momento$x1),
                num_intervals = num_intervals,
                percent_overlap = percent_overlap,
                num_bins_when_clustering = num_bins_when_clustering,
                low_ram = FALSE,
                distance_function = NULL,
                data = NULL,
                print_iterations = print_iterations)



df = datos_curva_momento
only_node_ids = NULL
min_draw_size = 10 
max_draw_size = 15
size_aprox_fun = function(x){x**(0.45)}
color_scheme = 'LINEAR'
color_attributes = c('x1','x2') 
color_fun = function(x){(mean(x$x1 + 1)/2)}
color = NULL
color_select_fun = function(x){as.vector(col2rgb('red', alpha = FALSE)[,1])}
label_scheme = 'NONE'
label_fun = NULL
label_attributes = NULL
min_label_node_size = 0
max_label_node_size = Inf 
topology_only = FALSE





png(height = 1000,
    width = 1000,
    file = '~/Documents/tesis_mat/master_math_thesis/images/moment_curve_graph.png')


plot(create_graph(df=df,
                  tda=tda,
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

legend('bottomleft', c('-1',' 1'), title = 'x Value', bg = 'gray82',
       lty=c(1), lwd=c(8), col=c('white', 'red'), cex = 2) # gives the legend lines the correct color and width

title(main = paste('TDA Graph - Second Moment Curve ', scenario , sep = ''), cex.main = 3.5)

dev.off()
