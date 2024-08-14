setwd("/Users/qiqi/2work/Meer-1/code/chaos_distribution_plot")
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)
library(wesanderson)
library(RColorBrewer)
library(paletteer)
library(ggsci)
library(UpSetR)
library(data.table)
library(cowplot)
##########step1 input file
metadata_qi<- list.files(path=".", pattern="*csvmin_mean.csv$", full.names=TRUE)

taxo<-read.table(file="taxo_copy.csv",sep=",",header = T,row.names=NULL)

phylum_color<-read.csv(file="phylum_colour_name.csv",sep=",",header = T,row.names=NULL,as.is = T)

colors1 <-
  phylum_color[c("Phylum", "Phylum_color")] %>%
  unique() %>%
  {
    co <- .$Phylum_color
    names(co) <- .$Phylum
    co
  }
##########step2 create list
gpdd_d<-vector("list",10)
gpdd_d2<-vector("list",10)
gpdd_d2_posi<-vector("list",10)
gpdd_d3<-vector("list",10)
plot_input1<-vector("list",10)
plot_input2<-vector("list",10)
vector_qi<-vector("list",10)
vector_q3<-vector("list",10)
vector_q4<-vector("list",10)
vector_q5<-vector("list",10)
unique_phylum_color_small_select<-vector("list",10)
vector_qi99<-c(1:10)

i<-1
for (i in 1:10) {# read in raw data
  print(i)
  gpdd_d[[i]]<-read.table(file=metadata_qi[i],sep=",",header = T,row.names=NULL)#input genome and LE values
  
  colnames(gpdd_d[[i]])<-c("number","Name","values")#change column name
  gpdd_d[[i]]$Name<-str_replace_all(gpdd_d[[i]]$Name,"\\.","") %>%
    str_replace_all(.,"-","") %>% str_replace_all(.,"_","")#replace genome name
  
  gpdd_d2[[i]]<-left_join(gpdd_d[[i]],taxo,by="Name")#add taxonomic information for each genome
  aa<-unique(gpdd_d2[[i]]$Phylum)#unique phylum in each cores
  unique_phylum_color_small_select[[i]]<-unique_phylum_color[unique_phylum_color$Phylum %in% aa,]#selct phyla exists in one specific core 
  
  plot_input1[[i]] <- gpdd_d2[[i]] %>%
    count(Phylum) #create summary for phyla
  plot_input2[[i]]<-plot_input1[[i]][order(-plot_input1[[i]]$n),]#the most abundant taxo on the top
  plot_input2[[i]]$Phylum<-factor(plot_input2[[i]]$Phylum,unique_phylum_color_small_select[[i]]$Phylum)
  gpdd_d2[[i]]$Phylum<-factor(gpdd_d2[[i]]$Phylum,unique_phylum_color_small_select[[i]]$Phylum)
  
  
  colnumber<-dim(plot_input2[[i]])[1]
  plot_input2[[i]]$data_type<-rep(gsub("_mag_data__select.csv_meer_time_series_data1.csv_metadata2.csvmin_mean.csv","",metadata_qi[[i]]) %>% gsub("./","",.),colnumber)
  
}
#######
########
#######

library(vegan)
library(ggplot2)
for (i in 1:10) {
  bp<-ggplot(gpdd_d2[[i]], aes(x=values,fill=Phylum)) + 
    geom_histogram(position="identity",aes(fill=Phylum)) +
    scale_fill_manual(values=colors1)+
    labs(y = "Count",x = "Chaotic index")+
    geom_vline(xintercept=0,linetype="dotdash")+
    theme(axis.text.x=element_text(vjust = 0.5,hjust = 0.5,size = 8,family = "sans",angle = 0,color = "black"),
          axis.text.y=element_text(vjust = 0.5,hjust = 0.5,size = 8,family = "sans",color = "black"),
          axis.title=element_text(vjust = 0.5,hjust = 0.5,size = 8,family = "sans",color = "black"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          legend.position="none",
          #plot.background=element_rect(fill = "white",colour = "white"),
          legend.text=element_text(vjust = 0.5,hjust = 0.5,size = 8,family = "sans"),
          legend.title=element_text(vjust = 0.5,hjust = 0.5,size = 8,family = "sans"),
          legend.background=element_rect(fill = 'transparent',colour = NULL),
          panel.background=element_rect(fill = 'transparent', colour = NULL),
          panel.border=element_rect(fill = 'transparent'),
          plot.background=element_rect(fill='transparent'),
          strip.text.x=element_text(face = "bold",size=8,color="black"),
          strip.background = element_rect(colour = "black", fill = "white",size = 1,linetype="solid"))
  #my_legend <- get_legend(
  # create some space to the left of the legend
  #bp + theme(legend.box.margin = margin(0, 0, 0, 12))
  #)
  
  #my_plot_nl <-  plot_grid(
  #bp + theme(legend.position="none"),
  #align = 'vh',
  #hjust = -1,
  #nrow = 1
  #)
  #bm<- plot_grid(my_plot_nl, my_legend, nrow = 2,align = "v",rel_heights = c(3,1))
  #scale_fill_brewer(palette="Paired")
  #paletteer_d(paletteer_d("ggsci::default_igv",n=21))
  ggsave(paste0(unique(plot_input2[[i]]$data_type),"file_bar_plot123_0813.pdf"),plot=bp,width = 1.5,height = 1.5)
  
}