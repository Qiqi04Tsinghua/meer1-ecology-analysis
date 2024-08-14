#Author: Dr. Qi Qi
#Readme
#Please use the csv files "unique_phylum _color","taxo.csv",and "./input/*.txt"
setwd("./input")
# step1: load library
library(dplyr)
library(tidyr)
library(stringr)
library(ggraph)
library(tidygraph)

name_qi<-c("6-8","10-12","12-14","18-20","20-22","22-24","24-26","26-28","8-10","0-2","14-16","2-4","4-6","28-30")
# step2: create list
prefix <- 14
nodes1 <- vector("list", prefix)
node1 <- vector("list", prefix)
nodes2 <- vector("list", prefix)
node2 <- vector("list", prefix)
node3 <- vector("list", prefix)
node4 <- vector("list", prefix)
node5 <- vector("list", prefix)
phylum_col_match_se <- vector("list", prefix)
link <- vector("list", prefix)
link2 <- vector("list", prefix)
nodes1 <- vector("list", prefix)
nodes2 <- vector("list", prefix)
nodes3 <- vector("list", prefix)
nodes4 <- vector("list", prefix)
nodes5 <- vector("list", prefix)
phylum_col_match_se <- vector("list", prefix)
link <- vector("list", prefix)
link2 <- vector("list", prefix)
results1 <- vector("list", prefix)
results1_taxa <- vector("list", prefix)
results1_taxa_colo <- vector("list", prefix)
test2 <- vector("list", prefix)
edge1 <- vector("list", prefix)
edge2 <- vector("list", prefix)
edge3 <- vector("list", prefix)
edge4 <- vector("list", prefix)
temp1 <- vector("list", prefix)
temp2 <- vector("list", prefix)
colors <- vector("list", prefix)
# set working directory


phylum_color <- read.csv(
  "./unique_phylum _color.csv",
  header = TRUE, as.is = TRUE
)
taxo <- read.csv("./taxo.csv", header = TRUE, row.names = NULL, sep = ",")
#' head(phylum_co)
#' i<-1

node <- list.files(
  path = ".", pattern = "*node_attribute.txt",
  full.names = TRUE, recursive = FALSE
)
edge <- list.files(
  path = "./input/", pattern = "*edge_attribute.txt",
  full.names = TRUE, recursive = FALSE
)
temp <- list.files(
  path = "./input/", pattern = "*node_attribute.txt",
  full.names = TRUE, recursive = FALSE
)
length(node)
length(edge)
length(temp)

#########
########## step2:revise node and edge file
##########
for (i in 1:14) {
  print(i)
  node2[[i]] <-
    node[i] %>%
    str_split_1("/") %>%
    .[length(.)] %>%
    gsub(" node_attribute.txt", "", .)
  node5[[i]] <-
    read.delim(file = node[i], header = TRUE, sep = "\t", dec = ".") %>%
    {
      colnames(.)[1] <- "Name"
      .
    } %>%
    left_join(taxo, by = "Name") %>%
    left_join(phylum_color, by = "Phylum") %>%
    {
      colnames(.)[1] <- "ID"
      .
    }
  head(node5[[i]])
  
  
  edge3[[i]] <-
    read.delim(
      file = edge[i], header = FALSE, sep = " ", dec = ".",
      col.names = c("Source", "Type", "Target", "=", "Direction")
    ) %>%
    mutate(Type = gsub("[()]", "", get("Type"))) %>%
    .[, c("Source", "Target", "Direction", "Type")]
  
  write.csv(
    edge3[[i]],
    file = paste0(node2[[i]], "edge3_1_0728.csv"), row.names = FALSE
  )
  write.csv(
    node5[[i]],
    file = paste0(node2[[i]], "node3_1_0728.csv"), row.names = FALSE
  )
}


for (i in 1:14) {
  print(i)
  temp2[[i]] <-
    gsub("./", "", temp[[i]]) %>%
    gsub(" node_attribute.txt", "", .)
  
  results1_taxa_colo[[i]] <-
    read.delim(file = temp[[i]], header = TRUE, sep = "\t", dec = ".") %>%
    # merge taxo information with node information
    left_join(taxo, by = "Name") %>%
    # add phylum color
    left_join(phylum_color, by = "Phylum") %>%
    {
      test2[[i]] <<- phylum_color[
        phylum_color$Phylum %in% unique(.$Phylum),
        ] # test2 is the color in the network
      mutate(
        .,
        Phylum = factor(get("Phylum"), test2[[i]]$Phylum),
        Phylum_color =
          factor(get("Phylum_color"), unique(test2[[i]]$Phylum_color)),
        data_set = rep(temp2[[i]], each = dim(.)[1])
      )
    }
  
  print(length(unique(results1_taxa_colo[[i]]$Phylum)))
  
  write.csv(
    results1_taxa_colo[[i]],
    file = paste0("./", node2[[i]], "taxo_node_1_0728.csv")
  )
} #######


unique(phylum_color[, 2])
for (i in 1:14) {
  print(i)
  nodes2[[i]] <-
    node5[[i]][, c("ID", "node.degree", "Phylum")] %>%
    left_join(phylum_color, by = "Phylum") %>%
    {
      phylum_col_match_se[[i]] <<-
        phylum_color[phylum_color$Phylum %in% unique(.$Phylum), ]
      mutate(
        .,
        Phylum = factor(get("Phylum"), phylum_col_match_se[[i]]$Phylum),
        Phylum_color = factor(
          get("Phylum_color"), unique(phylum_col_match_se[[i]]$Phylum_color)
        )
      )
    }
  
  write.csv(
    ".", file = paste0("./", node2[[i]], "node_0728_v3.csv")
  )
  
  split_output <- NULL
  results <- NULL
  
  link2[[i]] <-
    read.delim(file = edge[i], header = FALSE, sep = " ", dec = ".") %>%
    mutate(from = get("V1"), to = get("V3"), weight = get("V5")) %>%
    .[, c("from", "to", "weight")] %>%
    mutate(
      weight_label = ifelse(
        get("weight") == 1, "1",
        ifelse(get("weight") == -1, "2", "no")
      ) %>%
        factor(levels = c("1", "2")),
      weight_ = get("weight"),
      weight = abs(get("weight"))
    )
  write.csv(link2[[i]], file = paste0(name_qi[i], "link_0728_v1.csv"))
}




for (i in 1:14) {
  print(i)
  colors[[i]] <-
    nodes2[[i]][c("Phylum", "Phylum_color")] %>%
    unique() %>%
    {
      co <- as.character(.$Phylum_color)
      names(co) <- .$Phylum
      co
    }
  
  
  nodes3[[i]] <-
    nodes2[[i]] %>%
    {
      # extract phylum
      dis1 <- as.vector(unique(.$Phylum)) %>% .[!is.na(.)]
      phylum_col_match1 <- phylum_color$Phylum %>% .[. %in% dis1]
      .[complete.cases(.), ] %>%
        mutate(Phylum = factor(get("Phylum"), phylum_col_match1))
    }
}
deter_data <- read.csv(
  "/Users/qiqi/2work/Meer-1/network/topo/overall/overall_network_using_r/determinisitic.csv",
  header = TRUE, as.is = TRUE
)
unique(nodes4[[i]]$deter_label)
levels(nodes4[[i]]$deter_label)



for (i in 1:14) {
  
  nodes4[[i]]<-left_join(nodes3[[i]],deter_data,by="ID")
}
  
  for (i in 1:14) {
    nodes4[[i]]<-left_join(nodes3[[i]],deter_data,by="ID")
    nodes4[[i]]$deter_label<-factor(nodes4[[i]]$deter_label,levels=c("stochastic","deterministic"))
    ggra_net1 <- tbl_graph(edges = link2[[i]], directed = F, nodes = nodes4[[i]])
    
    layout.nicely <- create_layout(ggra_net1, layout = "nicely")
    
    bp <- ggraph(layout.nicely) +
      geom_edge_fan(
        aes(edge_colour = factor(weight_),alpha = 0.75), show.legend = TRUE
      ) +
      
     
      scale_edge_colour_manual(values = c("#00BFFF","#F08080")) +
     
      geom_node_point(alpha = 1,
                      aes(colour = as.factor(deter_label), size = node.degree)
      ) +
      scale_color_manual(values = c("#D97B54","#6CAD74")) +
      theme_graph(background=NULL,foreground = NULL)+
      theme(legend.position = "none",
            plot.background=element_blank(),
            panel.background = element_blank()) 
    
    ggsave(filename = paste0(name_qi[[i]],"_network1_red_green_0801_v10123.pdf"), plot = bp, width = 5, height = 5, dpi = 600)
  }
  
######
#######
########
for(i in 1:14) {
  
  nodes4[[i]]<-left_join(nodes3[[i]],deter_data,by="ID")
  nodes4[[i]]$deter_label<-factor(nodes4[[i]]$deter_label,levels=c("stochastic","deterministic"))
  phylum_color_se<-phylum_color[phylum_color$Phylum %in% unique(nodes4[[i]]$Phylum),]
  
  nodes4[[i]]$Phylum<-factor(nodes4[[i]]$Phylum,levels=phylum_color_se$Phylum)
  ggra_net1 <- tbl_graph(edges = link2[[i]], directed = F, nodes = nodes4[[i]])
  
  layout.nicely <- create_layout(ggra_net1, layout = "nicely")
  
  bp <- ggraph(layout.nicely) +
    geom_edge_fan(
      aes(edge_colour = factor(weight_),alpha = 0.75), show.legend = TRUE
    ) +
    
    scale_edge_colour_manual(values = c("#00BFFF","#F08080")) +

    geom_node_point(alpha = 1,
                    aes(colour = as.factor(Phylum), size = node.degree)
    ) +
    scale_color_manual(values = colors[[i]]) +
    theme(legend.position = "none",
          plot.background=element_blank(),
          panel.background = element_blank())
  ggsave(filename = paste0(node2[[i]] , "_network_phylum_0801_v10.pdf"), plot = bp, width = 5, height = 5, dpi = 600)
}
