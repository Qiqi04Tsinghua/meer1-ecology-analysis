library(ggbeeswarm)
library(dplyr)
library(ggplot2)

setwd("/Users/qiqi/2work/Meer-1/code/functional_gene_quasi_plot")
#we replace the values in column “cat” in "input2_0728.csv" to "input3_0812_v1.csv".
##replace  "Central carbohydrate metabolism","Other carbonhydrate metabolism","Aromatic degradation","Aromatic amino acid metabolism","Carbon fixation" to "Carbon".
## replace "Nitrogen metabolism" to "Nitrogen"
## replace "Sulfur metabolism" to "Sulfur"
## replace "ATP synthesis" to "ATP"
## replace "Cofactor and vitamin metabolism" to "Cofactor & Vitamin"
## replace other not mentioned values to "Others"

data4<- read.csv(file = "input3_0812_v1.csv")
colnames(data4)
unique(data4$cat)
data4$cat<-factor(data4$cat,levels=rev(c("Carbon","Nitrogen","Sulfur","ATP","Cofactor & Vitamin","Others")))
data4$cat<-factor(data4$cat,levels=rev(c("Carbon","Nitrogen","Sulfur","ATP","Cofactor & Vitamin","Others")))


data5<-tibble(data4) %>%
  mutate(CAT_COLOR = case_when(
    difference.deter.stocha > 5~"deter", 
    difference.deter.stocha > -5~"median",
    .default = "stochas")) 
#head(data5)
#unique(data5$cat)
data5$CAT_COLOR<-factor(data5$CAT_COLOR,levels=c("deter","median","stochas"))
data5<-data5[data5$difference.deter.stocha>-30,]

data6<-data5[data5$difference.deter.stocha>-30,]###deleet values smaller than -30
data6$CAT_COLOR<-factor(data6$CAT_COLOR,levels=c("deter","median","stochas"))

bp<-ggplot(data6, aes(x=difference.deter.stocha, y=cat)) +
  
  geom_quasirandom(alpha=0.8,aes(colour=CAT_COLOR),size=1)+
  labs(y = "Metabolic category", x = "pd - ps")+
  xlim(-30,30)+ 
  geom_vline(xintercept=-5,linetype=2)+
  geom_vline(xintercept=5,linetype=2)+
  scale_color_manual(values = c("#6CAD74","#BFBFBF","#D97B54"))+
  scale_x_continuous(breaks=seq(-30,30,by=5))+
  theme(axis.text.x=element_text(vjust = 0.5,hjust = 0.5,size = 10,family = "sans",angle = 0,color = "black"),
        axis.text.y=element_text(vjust = 0.5,hjust = 0.5,size = 10,family = "sans",color = "black"),
        axis.title=element_text(vjust = 0.5,hjust = 0.5,size = 10,family = "sans",color = "black"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position="none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",fill = NA),
        
        legend.text=element_text(vjust = 0.5,hjust = 0.5,size = 10,family = "sans"),
        legend.title=element_text(vjust = 0.5,hjust = 0.5,size = 10,family = "sans"),
        legend.background=element_rect(fill = NULL,colour = "white"),
        
        plot.background=element_blank(),
        strip.text.x=element_text(face = "bold",size=12,color="black"),
        strip.background = element_rect(colour = "black", fill = "white",size = 1,linetype="solid"),
        plot.margin = margin(t=0.3,r=0.3,b=0.3,l=0.3,unit="cm"))
ggsave(filename = paste0("geom_quasi2_0816_v4_30_302.pdf"),plot = bp, width = 6.5, height = 3.5, dpi = 600)
