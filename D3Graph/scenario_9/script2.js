
var node_location = 'http://localhost:8000/scenario_9/jsons/node_info_1.json'
var link_location = 'http://localhost:8000/scenario_9/jsons/graphFile.json';

var color_scale = chroma.scale(['#ffffff','#ff0000']).mode('lch').colors(101)
var cluster_scale;
var label_scale = ['red','greenyellow','blue','pink','yellow','orange','lightcyan','darkolivegreen', 'brown','purple']

var no_color = '#BDBDBD'

size_rescale = 6

var max_draw_size = 20
var min_draw_size = 5
var sizes = null

//Supletory function that calculates mean
function mean(numbers) {
    // mean of [3, 5, 4, 4, 1, 1, 2, 3] is 2.875
    var total = 0,
        i;
    for (i = 0; i < numbers.length; i += 1) {
        total += numbers[i];
    }
    return total / numbers.length;
}

//Function for the size of the nodes
function get_size(size)
{
  return(size*(max_draw_size - min_draw_size) + min_draw_size)
}

//Function that changes the coor of the nodes depending on their centrality
function centrality()
{

  svg.selectAll("circle")
      .attr("fill", function(d, i){ 
        last_colors[i] = ''+ color_scale[Math.round(100*d.cen)];
        return ''+ color_scale[Math.round(100*d.cen)];})
      .attr("opacity", 1)

      if(toggle)
      {
        focused_color = last_colors[focused_index]
        last_colors[focused_index] = 'white' 
        
      }

      if(toggle == 0)
      {
        document.getElementById("labels").style.visibility = 'hidden';
      }
      set_colors()
}


function by_label()
{
  svg.selectAll("circle")
      .attr("fill", function(d, i){ 
        last_colors[i] = ''+ label_scale[d.label];
        return ''+ label_scale[d.label];})
      .attr("opacity", function(d, i){ return d.purity })

      if(toggle)
      {
        focused_color = last_colors[focused_index]
        last_colors[focused_index] = 'white' 

      }
    set_colors()

    document.getElementById("labels").style.visibility = 'visible';
      
}

function by_cluster()
{
  svg.selectAll("circle")
      .attr("fill", function(d, i){ 
        last_colors[i] = ''+ cluster_scale[d.cluster];
        return ''+ cluster_scale[d.cluster];})
      .attr("opacity", 1)

      if(toggle)
      {
        focused_color = last_colors[focused_index]
        last_colors[focused_index] = 'white' 

      }

      if(toggle == 0)
      {
        document.getElementById("labels").style.visibility = 'hidden';
      }

    set_colors()
      
}

function same_color()
{
    svg.selectAll("circle")
    .attr("fill", function(d,i){
            last_colors[i] = no_color;
            return no_color;})    

      if(toggle)
      {
        focused_color = last_colors[focused_index]
        last_colors[focused_index] = 'white' 
  
      }

    set_colors()

    document.getElementById("labels").style.visibility = 'hidden';
}

function agglomerate()
{
    svg.selectAll("circle")
      .attr("r", function(d,i) { return get_size(sizes[i]);})
}


function same_size()
{

    svg.selectAll("circle")
      .attr("r", min_draw_size)

}

function tsne()
{

    if(images_on)
        draw_images()
    if(!images_on)
        draw_points()

    center_x = mean(node_info.nodes_coord.map( function(n) {return n.tsne_x;}));
    center_y = mean(node_info.nodes_coord.map( function(n) {return n.tsne_y;}));

    max_x = Math.max.apply( null, node_info.nodes_coord.map( function(n) {return Math.abs(n.tsne_x - center_x);} ));
    max_y = Math.max.apply( null, node_info.nodes_coord.map( function(n) {return Math.abs(n.tsne_y - center_y);} ));



    offset_x = zoom_points*0.9*(actual_width/2)/max_x
    offset_y = zoom_points*(actual_height/2)/max_y

    node_info.nodes_coord.map( function(d) {d.fx = offset_x*(d.tsne_x - center_x) + actual_width/2;})
    node_info.nodes_coord.map( function(d) {d.fy = offset_y*(d.tsne_y - center_y) + actual_height/2;})

}



function pca()
{

    if(images_on)
        draw_images()
    if(!images_on)
        draw_points()

    center_x = mean(node_info.nodes_coord.map( function(n) {return n.pca_x;}));
    center_y = mean(node_info.nodes_coord.map( function(n) {return n.pca_y;}));

    max_x = Math.max.apply( null, node_info.nodes_coord.map( function(n) {return Math.abs(n.pca_x - center_x);} ));
    max_y = Math.max.apply( null, node_info.nodes_coord.map( function(n) {return Math.abs(n.pca_y - center_y);} ));



    offset_x = zoom_points*0.9*(actual_width/2)/max_x
    offset_y = zoom_points*(actual_height/2)/max_y

    node_info.nodes_coord.map( function(d) {d.fx = offset_x*(d.pca_x - center_x) + actual_width/2;})
    node_info.nodes_coord.map( function(d) {d.fy = offset_y*(d.pca_y - center_y) + actual_height/2;})

}




function select_image(id, label, label_name)
{
    document.getElementById("single_focus").src = 'http://localhost:8000/testing_2/' + label + '/' + id +'.png'
    document.getElementById("single_focus_col").style.visibility = 'visible';
    document.getElementById('current_label').innerText = 'Label: ' + label_name;
}


//Toggle stores whether the highlighting is on
var toggle = 0;
//Create an array logging what is connected to what
var linkedByIndex = {};


//This function looks up whether a pair are neighbours
function neighboring(a, b) {
    return linkedByIndex[a.index + "," + b.index];
}


function connectedNodes() {
    if (toggle == 0) {
        //Reduce the opacity of all but the neighbouring nodes
        d = d3.select(this).node().__data__;
        temp_node = d3.select(this);

        node.style("opacity", function (o) {
            return neighboring(d, o) | neighboring(o, d) ? 1 : 0.1;
        });
        link.style("opacity", function (o) {
            return d.index==o.source.index | d.index==o.target.index ? 1 : 0.1;
        });
        //Reduce the op
        toggle = 1;

        temp_node.style("stroke-width", 4);
        temp_node.style("stroke", label_scale[d.label]);

        //Zoom
        focused_index = d.index
        focused_color = last_colors[d.index]
        last_colors[d.index] = 'white' 
        document.getElementById("labels").style.visibility = 'visible';
        get_info(d.index)

    } else {
        //Put them back to opacity=1
        node.style("opacity", 1)
        .style("stroke-width", 0.5)
        .style("stroke", "#757575");
        link.style("opacity", 1);
        toggle = 0;
        document.getElementById("focus_layout").style.visibility = 'hidden';
        document.getElementById("focus_buttons").style.visibility = 'hidden';
        document.getElementById("labels").style.visibility = 'hidden';
        last_colors[focused_index] = focused_color
        set_colors();
    }
}


function iluminate() {
        //Reduce the opacity of all but the neighbouring nodes
        
        temp_node = d3.select(this);
        node_data = temp_node.node().__data__;

        node.style("fill", function (o,i) {

            if(neighboring(node_data, o) | neighboring(o, node_data))
            {
                return  chroma(last_colors[i]).brighten(2)                
            }                      
            return  last_colors[i];
        });


        temp_node.style("fill", chroma(temp_node.style('fill')).brighten(3));


        link.style("stroke", function (o) {
            return node_data.index==o.source.index | node_data.index==o.target.index ? 'white' : 'gray';
        });
        //Reduce the op
        link.style("stroke-width", function (o) {
            return node_data.index==o.source.index | node_data.index==o.target.index ? 1 : 0.5;
        });
  

   
}

function set_colors()
{
    node.style("fill", function (d,i) { return last_colors[i] });
}

function return_colors()
{
    node.style("fill", function (d,i) { return last_colors[i] });
    link.style("stroke", 'gray' )
        .style("stroke-width", 0.5);
}



//create somewhere to put the force directed graph
var svg = d3.select('#general_graph').append("svg")
    .attr("width", '100%')
    .attr("height", d3.select('#general_graph_col').node().clientWidth);


var graph_actual_width = d3.select('#general_graph_col').node().clientWidth; 
var graph_actual_height = d3.select('#general_graph_col').node().clientHeight;

//PCA and TSNE
var svg2 = d3.select('#focus_layout').append("svg")
    .attr("width", '100%')
    .attr("height", d3.select('#focus_layout_col').node().clientWidth);   

var actual_width = d3.select('#focus_layout_col').node().clientWidth; 
var actual_height = d3.select('#focus_layout_col').node().clientHeight;

// Starts the single focus image
document.getElementById("single_focus").height =  d3.select('#single_focus_col').node().clientWidth;
document.getElementById("single_focus").width =   d3.select('#single_focus_col').node().clientWidth;

var node_info, points, simulation_layout, g_layout, drag_handler_layout, charge_force_layout, center_force_layout;
var zoom_points = 0.9    

var all_nodes;

d3.json(node_location, function(json) {all_nodes = json.node_info;});

current_id = 0


var radius = 15; 
var stroke_width = 0.5

var simulation, g, link, node, drag_handler, zoom_handler, nodes_data, links_data;
var last_colors = {}
var focused_index, focused_color;

var images_on = false;



// Draws the graph
d3.json(link_location, function(json) {
  
nodes_data = json.nodes;
links_data = json.links;


//Loads the links ids on an array to to keep track of who is connected to who
for (i = 0; i < nodes_data.length; i++) {
    linkedByIndex[i + "," + i] = 1;
};
links_data.forEach(function (d) {
    linkedByIndex[d.source + "," + d.target] = 1;
});


sizes = nodes_data.map( function(n) {return n.size;})

max_size = Math.max.apply( null, nodes_data.map( function(n) {return n.size;} ) );
min_size = Math.min.apply( null, nodes_data.map( function(n) {return n.size;} ) );


cluster_scale = chroma.scale(['#fafa6e','#2A4858']).mode('lch').colors(Math.max.apply( null, nodes_data.map( function(n) {return n.cluster;} ) ) + 1)

//Updates sizes from 0 to 1
for(var i = 0; i < sizes.length; i++)
{
  sizes[i] = (sizes[i] - min_size)/(max_size - min_size)
}  


//set up the simulation and add forces  
simulation = d3.forceSimulation()
                    .nodes(nodes_data);
                              
link_force =  d3.forceLink(links_data)
                        .id(function(d) { return d.id; });            
         
charge_force = d3.forceManyBody()
    .strength(-10); 
    
center_force = d3.forceCenter(graph_actual_width / 2, graph_actual_height / 2);  
                      
simulation
    .force("charge_force", charge_force)
    .force("center_force", center_force)
    .force("links",link_force)
 ;

        
//add tick instructions: 
simulation.on("tick", tickActions );

//add encompassing group for the zoom 
g = svg.append("g")
    .attr("class", "everything");

//draw lines for the links 
link = g.append("g")
      .attr("class", "links")
    .selectAll("line")
    .data(links_data)
    .enter().append("line")
      .style("stroke-width", stroke_width )
      .style("stroke", 'gray');
             

//draw circles for the nodes 
 node = g.append("g")
        .attr("class", "nodes") 
        .selectAll("circle")
        .data(nodes_data)
        .enter()
        .append("circle")
        .attr("r", function(d,i) { return get_size(sizes[i]);})
        .attr("fill", function(d,i){
            last_colors[i] = no_color;
            return no_color;})
        .style("stroke-width", 0.5)
        .style("stroke", "#757575")
        .on("mouseover",iluminate )
        .on("mouseleave",return_colors)
        .on('click', connectedNodes); //Added code ;
 
 
//add drag capabilities  
 drag_handler = d3.drag()
    .on("start", drag_start)
    .on("drag", drag_drag)
    .on("end", drag_end);   
    
drag_handler(node);


//add zoom capabilities 
 zoom_handler = d3.zoom()
    .on("zoom", zoom_actions);

zoom_handler(svg);



});


function load_json_data(node_id)
{
    current_id = node_id
    node_info = all_nodes[current_id]
}

function get_info(node_id){

    document.getElementById("single_focus_col").style.visibility = 'hidden';
    document.getElementById("focus_layout").style.visibility = 'visible';
    document.getElementById("focus_buttons").style.visibility = 'visible';
    load_json_data(node_id);
    draw_points();

}

// Function that draws the info for the node selected node
function draw_points(){

    document.getElementById("labels").style.visibility = 'visible';
    node_info.nodes_coord.map( function(d) {d.fx = null;})
    node_info.nodes_coord.map( function(d) {d.fy = null;})
    node_info.nodes_coord.map( function(d) {d.x = NaN;})
    node_info.nodes_coord.map( function(d) {d.y = NaN;})

    images_on = false;


    if(typeof points !== 'undefined')
        points.exit().remove()
    if(typeof simulation_layout !== 'undefined')
        simulation_layout.stop()
    
    svg2.selectAll(".everything_layout").remove()

    simulation_layout = d3.forceSimulation()
    .force("charge", d3.forceManyBody().strength(-1.5))
    .force("center", d3.forceCenter(actual_width / 2, actual_height / 2));
    

    points = svg2.append("g")
      .attr("class", "everything_layout")
      .selectAll("circle")
      .data(node_info.nodes_coord)
      .enter().append("circle")
      .attr("fill", function(d, i){ return ''+ label_scale[d.label]})
      .style("stroke-width", 0.7)
      .style("stroke", "#000000")
      .attr("r", 7 )
      .on("mouseover",function(d,i){ 
        document.getElementById("single_focus_col").style.visibility = 'visible';
        select_image(d.id, d.label, d.label_name); })
      .on("mouseleave",function(d,i){ document.getElementById("single_focus_col").style.visibility = 'hidden'; });



    simulation_layout
      .nodes(node_info.nodes_coord)
      .on("tick", tickActions_layout);

                  //add drag capabilities  
    drag_handler_layout = d3.drag()
        .on("start", drag_start_static)
        .on("drag", drag_drag_static)
        .on("end", drag_end_static);   
    
    drag_handler_layout(points);
}



function draw_images()
{   
    document.getElementById("labels").style.visibility = 'hidden';

    node_info.nodes_coord.map( function(d) {d.fx = null;})
    node_info.nodes_coord.map( function(d) {d.fy = null;})
    node_info.nodes_coord.map( function(d) {d.x = NaN;})
    node_info.nodes_coord.map( function(d) {d.y = NaN;})

    images_on = true;

    if(typeof points !== 'undefined')
        points.exit().remove()
    if(typeof simulation_layout !== 'undefined')
        simulation_layout.stop()

    svg2.selectAll(".everything_layout").remove()

    simulation_layout = d3.forceSimulation()
    .force("charge", d3.forceManyBody().strength(-1.5))
    .force("center", d3.forceCenter(actual_width / 2, actual_height / 2));
    

    points = svg2.append("g")
      .attr("class", "everything_layout")
      .selectAll("image")
      .data(node_info.nodes_coord)
      .enter().append("image")
      .attr("xlink:href", function(d, i){return 'http://localhost:8000/testing_2/' + d.label + '/' + d.id +'.png' })
      .attr("x", -8)
      .attr("y", -8)
      .attr("width", 28)
      .attr("height", 28)
      .on("mouseover",function(d,i){ 
        document.getElementById("single_focus_col").style.visibility = 'visible';
        select_image(d.id, d.label, d.label_name); })
      .on("mouseleave",function(d,i){ document.getElementById("single_focus_col").style.visibility = 'hidden'; })
      .call(d3.drag()
        .on("start", drag_start_static)
        .on("drag", drag_drag_static)
        .on("end", drag_end_static));

    simulation_layout
      .nodes(node_info.nodes_coord)
      .on("tick", ticked_image);

}






//Drag functions 
//d is the node 
function drag_start(d) {
 if (!d3.event.active) simulation.alphaTarget(0.3).restart();
    d.fx = d.x;
    d.fy = d.y;
}

//make sure you can't drag the circle outside the box
function drag_drag(d) {
  d.fx = d3.event.x;
  d.fy = d3.event.y;
}

function drag_end(d) {
  if (!d3.event.active) simulation.alphaTarget(0);
  d.fx = null;
  d.fy = null;
}

var global_x, global_y

function drag_start_static(d) {
    global_x = d.x;
    global_y = d.y;
    d.fx = d.x;
    d.fy = d.y;
}

//make sure you can't drag the circle outside the box
function drag_drag_static(d) {
  d.fx = d3.event.x;
  d.fy = d3.event.y;
}

function drag_end_static(d) {
  d.fx = global_x;
  d.fy = global_y;
  d.x = global_x;
  d.y = global_y;
}

//Zoom functions 
function zoom_actions(){
    g.attr("transform", d3.event.transform)   
}

function tickActions() {
    //update circle positions each tick of the simulation 
       node
        .attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; });
        
    //update link positions 
    link
        .attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });
} 

function tickActions_layout() {
    //update circle positions each tick of the simulation 
       points
        .attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; });
        
} 


function ticked_image() {

      points
        .attr("transform", function(d) {
          return "translate(" + d.x + "," + d.y + ")";

        });
}



function getWidth() {
  return Math.max(
    document.body.scrollWidth,
    document.documentElement.scrollWidth,
    document.body.offsetWidth,
    document.documentElement.offsetWidth,
    document.documentElement.clientWidth
  );
}