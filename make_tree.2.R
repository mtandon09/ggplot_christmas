rm(list=ls())

library(igraph)
library(ggnetwork)
library(network)
library(intergraph)
# library(ggplot2)

setwd("~/Documents/xmas")
set.seed(1234)

### Network data for the tree
tree_data <- rbind(#left branch,
  c("topper" ,"ornament_1_1" , "tree"),
  c("ornament_1_1","ornament_1_2" , "tree"),
  c("ornament_1_2","ornament_1_3" , "tree"),
  c("ornament_1_2","ornament_1_4" , "tree"),
  c("ornament_1_3","ornament_1_5" , "tree"),
  c("ornament_1_3","ornament_1_6" , "tree"),
  c("ornament_1_2","ornament_1_12" , "tree"),
  c("ornament_1_12","ornament_1_13" , "tree"),
  ## Right branch, "tree" 
  c("topper" ,"ornament_2_1" , "tree"),
  c("ornament_2_1","ornament_2_2" , "tree"),
  c("ornament_2_1","ornament_2_3" , "tree"),
  c("ornament_2_3","ornament_2_4" , "tree"),
  c("ornament_2_4","ornament_2_5" , "tree"),
  c("ornament_2_4","ornament_2_6" , "tree"),
  c("ornament_2_2","ornament_2_7" , "tree"),
  c("ornament_2_2","ornament_2_8" , "tree"),
  c("ornament_2_7","ornament_2_9" , "tree"),
  c("ornament_2_7","ornament_2_10", "tree"),
  c("ornament_2_8","ornament_2_11", "tree"),
  c("ornament_2_5","trunk_1"      , "trunk"),
  c("ornament_2_6","trunk_2"      , "trunk")
)
tree_data <- data.frame(tree_data)
colnames(tree_data) <- c("start_node", "end_node", "tree_part")

### Convert to igraph object
xmastree_network <- graph_from_data_frame(tree_data)
### Set node type attribute to differntiate tree-topper/ornaments/trunk
xmastree_network <- set_vertex_attr(xmastree_network,"node_type", 
                                    V(xmastree_network)$name, 
                                    unlist(lapply(strsplit(V(xmastree_network)$name,"_"),"[[",1)))

### Randomize ornament sizes
node_sizes <- sample(x = 1:50, size=length(V(xmastree_network)$name), replace = T)
### But set explicit values for the topper and trunk
node_sizes[V(xmastree_network)$node_type=="topper"]=9
node_sizes[V(xmastree_network)$node_type=="trunk"]=1
### Set sizes as network attributes
xmastree_network <- set_vertex_attr(xmastree_network,"node_size", 
                                    V(xmastree_network)$name, 
                                    node_sizes)

### Layout network as tree
mylayout <- layout_as_tree(xmastree_network, root = "topper")
### Set up ggplot plotting data
xmastree_plotdata <- ggnetwork(asNetwork(xmastree_network),layout=mylayout)

### We're gonna color in the tree by trying to guess the shape based on the ornaments
### Select just the ornaments
ornaments <- xmastree_plotdata[xmastree_plotdata$node_type %in% c("ornament","topper"),]
### Get convex hull for the tree
outline_tree <- ornaments[chull(ornaments[,c("x","y")]),]

### Get convex hull for the trunk
outline_trunk <- xmastree_plotdata[xmastree_plotdata$node_type=="trunk",c("x","y")]
outline_trunk <- rbind(outline_trunk,data.frame(x=outline_trunk$x, y=min(outline_tree$y)))
outline_trunk <- outline_trunk[chull(outline_trunk),]

### Define custom color and shape values
shape_vals <- c("topper"=11,"ornament"=16, "trunk"=15)
color_vals <- c("topper"="gold","ornament"="red", "trunk"="#663300","tree"="gold")

### Define plot limits
canvas_x_lim=c(-2,2)
canvas_lim <- list(x=canvas_x_lim, y=range(xmastree_plotdata$y))

### Make data for snowflokes
n_snowflakes=200  # Number of flakes to draw
dist_from_edge=0.95 # % area to draw the flakes in
snowflake_lims <- lapply(canvas_lim, function(x) {x*dist_from_edge})

snowflake_data <- data.frame(x=runif(n = n_snowflakes,min = min(snowflake_lims$x), max = max(snowflake_lims$x)),
                             y=runif(n = n_snowflakes,min = min(snowflake_lims$y), max = max(snowflake_lims$y)))

### Make data for presents
### Let's define the area under the tree (plus wiggle room) as the area for the presents
presents_area <- c(range(outline_tree$x), range(outline_trunk$y))
presents_area[1] <- presents_area[1] - mean(presents_area)
presents_area[2] <- presents_area[2] + mean(presents_area)
presents_area[3:4] <- presents_area[3:4] + 2*mean(presents_area)
names(presents_area) <- c("xmin","xmax","ymin","ymax")

npresents=2  # Number of presents to draw
presents_data <- data.frame(x=sample(seq(presents_area["xmin"],presents_area["xmax"], length.out = 30) ,size = npresents, replace=T))
presents_data$y <- 0
presents_data$height <- presents_area["ymax"]* sample(seq(20,60,length.out = 10), npresents)/100
presents_data$width <- presents_area["xmax"]* sample(seq(30,80,length.out = 10), npresents)/100
presents_data$y <- presents_data$y+(0.5*presents_data$height)


### Add text 
greeting_text_data <- data.frame(x=min(canvas_lim$x)+(0.5*range(canvas_lim$x)), 
                                 y=max(canvas_lim$y)-(0.2*range(canvas_lim$y)),
                                 text="Oh ggplot, oh ggplot,\nMuch pleasure doth thou bring me!")

greeting_text_data_2 <- data.frame(x=min(canvas_lim$x)+(0.5*range(canvas_lim$x)), 
                                 y=max(canvas_lim$y)-(0.5*range(canvas_lim$y)),
                                 text="Merry Christmas\n&\nHappy Holidays")

### Plot it all together (i.e diaRRhea)
xmastree_plot <- ggplot(xmastree_plotdata, 
                         aes(x = x, y = y, xend = xend, yend = yend)) +
                    ## Presents first, so they can be behind the tree
                    geom_tile(data=presents_data, 
                              aes(x=x,y=y,height=height, width=width), 
                              inherit.aes = F, fill="darkred",
                              color="black", size=2) +
                    ## Bows on the presents
                    geom_segment(data=presents_data, 
                              aes(x=x-(width*0.5),xend=x+(width*0.5),
                                  y=y,yend=y), 
                              inherit.aes = F, size=5,
                              color="steelblue") +
                    geom_segment(data=presents_data, 
                                 aes(x=x,xend=x,
                                     y=y-(height*0.5),yend=y+(height*0.5)), 
                                 inherit.aes = F, size=5,
                                 color="steelblue") +
                    ## Color in the tree and trunk with polygons
                    geom_polygon(inherit.aes = T,
                                 color="black",
                                 data=outline_tree,
                                 show.legend = F,
                                 alpha=0.8, fill="darkgreen") +  
                    geom_polygon(inherit.aes = F,
                                 mapping=aes(x=x,y=y),
                                 color="black", size=2,
                                 data=outline_trunk,
                                 show.legend = F,
                                 alpha=1, fill="#663300") +
                    ## Plot network for tree + ornaments
                    ## First edges, then each node-type sequentially
                    geom_edges(aes(color=tree_part), size=1,show.legend = F) +
                    geom_nodes(mapping=aes(color=node_type,size=node_size, shape=node_type),
                               data = subset(xmastree_plotdata, node_type %in% "topper"),
                               stroke=6,show.legend = F,
                               inherit.aes = T) +
                    geom_nodes(mapping=aes(color=node_type,size=node_size, shape=node_type),
                               data = subset(xmastree_plotdata, node_type %in% "ornament"),
                               inherit.aes = T,show.legend = F) +
                    geom_nodes(mapping=aes(color=node_type,size=node_size, shape=node_type),
                               data = subset(xmastree_plotdata, node_type %in% "trunk"),
                               inherit.aes = T,show.legend = F) +
                    geom_point(data=snowflake_data, inherit.aes = F,
                               mapping=aes(x=x,y=y),
                               color="grey80", pch=8, size=3) +
                    ## Set custom color, fill, shape
                    scale_fill_manual(values=color_vals) +
                    scale_color_manual(values=color_vals) +
                    scale_shape_manual(values=shape_vals) +
                    ## Add greeting text
                    geom_text(data=greeting_text_data, aes(x=x, y=y, label=text),
                              inherit.aes = F,color="black",
                              size=7.5, family="Palatino")+
                    geom_text(data=greeting_text_data_2, aes(x=x, y=y, label=text),
                              inherit.aes = F,color="black",
                              size=9, family="Palatino", fontface="bold")+
                    ## Further graphics options
                    xlim(canvas_x_lim) +
                    theme(legend.position = "none") + 
                    theme_blank()

### Print it
# xmastree_plot

### Save it
ggsave("xmas_tree.png",xmastree_plot, height=6, width=9)

