setwd("/Users/qiqi/2work/Meer-1/code/chaos calculation/")#change to your folder

temp <- list.files(path=".", pattern="*transfer.csv$", full.names=TRUE)
length(temp)
results<-vector("list",10)
results_select<-vector("list",10)
for (i in 1:10) {
  print(i)
  results[[i]]<-read.table(file=temp[[i]],sep=",",header = T,row.names=1)
  print(dim(results[[i]]))
}

for (i in 1:10) {
  print(i)
  results_select[[i]]<-results[[i]][,colSums(results[[i]]!=0)==(dim(results[[i]])[1])]
  print(dim(results_select[[i]]))
  write.csv(as.data.frame(results_select[[i]]),file = paste0(temp[[i]],"_select.csv"))
}
#output file in 
#####use 10 data
#setwd("/Users/qiqi/2work/Meer-1/Chaos/chaos_real_data /input/output2")
raw_data10<-vector("list",10)
temp<-NULL
temp <- list.files(path=".", pattern="*select.csv$", full.names=TRUE)
temp
for (i in 1:10) {
  print(i)
  raw_data10[[i]]<-read.table(file=temp[[i]],sep=",",header = T,row.names=1)
  print(dim(raw_data10[[i]]))
}

results_after_sd<-vector("list",10)
sd_results<-vector("list",10)
##Poprescale
for (kk in 1:10) {
  col_qi_data<-dim(raw_data10[[kk]])[2]
  row_qi_data<-dim(raw_data10[[kk]])[1]
  #sd_results<-c(1:col_qi_data)
  sd_results[[kk]]<-c(1:col_qi_data)
  col_qi_dataa<-(col_qi_data+1)
  results_after_sd[[kk]]<-as.data.frame(matrix(data=NA,ncol=col_qi_data,nrow = row_qi_data))
  print(paste0("kk=",kk))
  for (i in 1:col_qi_data) {
    sd_results[[kk]][i]<-sd(raw_data10[[kk]][,i])
    print(paste0("i=",i))
    for (j in 1:row_qi_data) {
      results_after_sd[[kk]][j,i]<-raw_data10[[kk]][j,i]/sd_results[[kk]][i]
    }
  }
  row.names(results_after_sd[[kk]])<-row.names(raw_data10[[kk]])
  colnames(results_after_sd[[kk]])<-colnames(raw_data10[[kk]])
  write.csv(results_after_sd[[kk]],file = paste0(temp[[kk]],"_rescale_step1.csv"))
}


step2_PopRescale_fd<-vector("list",10)
step3_PopRescale_log<-vector("list",10)
step4_PopRescale_gr<-vector("list",10)



for (kk in 1:10) {
  col_qi_data<-dim(raw_data10[[kk]])[2]
  row_qi_data<-dim(raw_data10[[kk]])[1]
  step2_PopRescale_fd[[kk]]<-as.data.frame(matrix(data=NA,ncol=col_qi_data,nrow = row_qi_data))
  step3_PopRescale_log[[kk]]<-as.data.frame(matrix(data=NA,ncol=col_qi_data,nrow = row_qi_data))
  step4_PopRescale_gr[[kk]]<-as.data.frame(matrix(data=NA,ncol=col_qi_data,nrow = row_qi_data))
  for (i in 1:col_qi_data){
    step2_PopRescale_fd[[kk]][,i]=c(NA, diff(results_after_sd[[kk]][,i]))
    step3_PopRescale_log[[kk]][,i]=log(results_after_sd[[kk]][,i])
    step4_PopRescale_gr[[kk]][,i]=c(NA, diff(results_after_sd[[kk]][,i]))
  }
  row.names(step2_PopRescale_fd[[kk]])<-row.names(raw_data10[[kk]])
  row.names(step3_PopRescale_log[[kk]])<-row.names(raw_data10[[kk]])
  row.names(step4_PopRescale_gr[[kk]])<-row.names(raw_data10[[kk]])
  colnames(step2_PopRescale_fd[[kk]])<-colnames(raw_data10[[kk]])
  colnames(step3_PopRescale_log[[kk]])<-colnames(raw_data10[[kk]])
  colnames(step4_PopRescale_gr[[kk]])<-colnames(raw_data10[[kk]])
  
  write.csv(step2_PopRescale_fd[[kk]],file = paste0(temp[[kk]],"_fd_step2.csv"))
  write.csv(step3_PopRescale_log[[kk]],file = paste0(temp[[kk]],"_log_step3.csv"))
  write.csv(step4_PopRescale_gr[[kk]],file = paste0(temp[[kk]],"_gr_step4.csv")) 
  
}
#######
#merge
########

temp1<- list.files(path=".", pattern="*step1.csv", full.names=TRUE)
temp1
temp2<- list.files(path=".", pattern="*step2.csv", full.names=TRUE)

temp3<- list.files(path=".", pattern="*step3.csv", full.names=TRUE)
temp4<- list.files(path=".", pattern="*step4.csv", full.names=TRUE)

length(temp1)
length(temp2)
length(temp3)
length(temp4)

temp1_i<-vector("list",10)
temp2_i<-vector("list",10)
temp3_i<-vector("list",10)
temp4_i<-vector("list",10)
for (i in 1:10) {
  print(i)
  temp1_i[[i]]<-read.table(file=temp1[i],sep=",",header = T,row.names=NULL)
  temp2_i[[i]]<-read.table(file=temp2[i],sep=",",header = T,row.names=NULL)
  temp3_i[[i]]<-read.table(file=temp3[i],sep=",",header = T,row.names=NULL)
  temp4_i[[i]]<-read.table(file=temp4[i],sep=",",header = T,row.names=NULL)
}
head(temp1_i[[1]][,1:3])
library(reshape2)

merge_qi<-vector("list",10)
merge_qi_select<-vector("list",10)
merge_qi_select1<-vector("list",10)
merge_qi_select2<-vector("list",10)
merge_qi_select1_p<-vector("list",10)
merge_qi_select2_p<-vector("list",10)
raw_data10_melt<-vector("list",10)
raw_for_melt<-vector("list",10)

a<-vector("list",10)
b<-vector("list",10)
c<-vector("list",10)
d<-vector("list",10)
####
#melt and merge
########

raw_data10<-vector("list",10)
temp<-NULL
temp <- list.files(path=".", pattern="*select.csv$", full.names=TRUE)
library(data.table)
for (i in 1:10) {# read in raw data
  print(i)
  raw_for_melt[[i]]<-read.table(file=temp[[i]],sep=",",header = T,row.names=NULL)
  print(dim(raw_for_melt[[i]]))
}

for (kk in 1:10) {
  a[[kk]]<-melt(temp1_i[[kk]])
  b[[kk]]<-melt(temp2_i[[kk]])
  c[[kk]]<-melt(temp3_i[[kk]])
  d[[kk]]<-melt(temp4_i[[kk]])
  raw_data10_melt[[kk]]<-melt(raw_for_melt[[kk]])
  merge_qi[[kk]]<-cbind(raw_data10_melt[[kk]],a[[kk]],b[[kk]],c[[kk]],d[[kk]])
  merge_qi_select[[kk]]<-merge_qi[[kk]][,c(1,2,3,6,9,12,15)]
  colnames(merge_qi_select[[kk]])<-c("sample","bin","Population","PopRescale","PopRescale_fd",
                                     "PopRescale_log","PopRescale_gr")
  depth_qi<-dim(raw_for_melt[[kk]])[1]
  bin_number_qi<-(dim(raw_for_melt[[kk]])[2]-1)
  #merge_qi_select[[i]]
  merge_qi_select[[kk]]$PopulationUntransformed<-merge_qi_select[[kk]]$Population
  merge_qi_select[[kk]]$MainID<-rep(1:bin_number_qi,each=depth_qi)
  merge_qi_select[[kk]]$CommonName<-rep(colnames(raw_for_melt[[kk]])[-1],each=depth_qi)
  merge_qi_select[[kk]]$SeriesStep<-rep(0:(depth_qi-1),bin_number_qi)
  merge_qi_select[[kk]]$SampleYear<-(rep(0:(depth_qi-1),bin_number_qi)+1980)
  merge_qi_select[[kk]]$SampleYear1<-(rep(0:(depth_qi-1),bin_number_qi)+1990)
  #colnames(merge_qi_select[[kk]])
  merge_qi_select1[[kk]]<-merge_qi_select[[kk]][,c("MainID","CommonName","SeriesStep","SampleYear",
                                                   "Population","PopulationUntransformed","PopRescale",
                                                   "PopRescale_fd","PopRescale_log","PopRescale_gr",
                                                   "sample","bin","SampleYear1")]
  merge_qi_select2[[kk]]<-merge_qi_select[[kk]][,c("MainID","CommonName","SeriesStep","SampleYear1",
                                                   "Population","PopulationUntransformed","PopRescale",
                                                   "PopRescale_fd","PopRescale_log","PopRescale_gr",
                                                   "sample","bin","SampleYear")]
  merge_qi_select1_p[[kk]]<-merge_qi_select1[[kk]][,1:10]
  merge_qi_select2_p[[kk]]<-merge_qi_select2[[kk]][,1:10]
  write.csv(merge_qi_select1_p[[kk]],file = paste0(temp[[kk]],"_meer_time_series_data1.csv"))
  write.csv(merge_qi_select2_p[[kk]],file = paste0(temp[[kk]],"_meer_time_series_data2.csv"))
  
}

############
#########making metadata
#############
rm(list=ls())

tax_qi=read.csv("taxnomic_taxa.csv")
head(tax_qi)
colnames(tax_qi)[1]<-"CommonName"

temp4<- list.files(path=".", pattern="*step4.csv", full.names=TRUE)
raw_temp<- list.files(path=".", pattern="*_data1.csv$", full.names=TRUE)
gr_step4<-vector("list",10)
raw<-vector("list",10)
cbind_merge<-vector("list",10)
cbind_merge1<-vector("list",10)
try_data<-vector("list",10)
qi123<-vector("list",10)


for (i in 1:10) {# read in raw data
  print(i)
  gr_step4[[i]]<-read.table(file=temp4[[i]],sep=",",header = T,row.names=NULL)
  raw[[i]]<-read.table(file=raw_temp[[i]],sep=",",header = T,row.names=NULL)
  print(dim(raw[[i]]))
  print(dim(gr_step4[[i]]))
}
library(dplyr)

for (i in 1:10) {
  print(paste0("i=",i))
  length_qi<-length(unique(raw[[i]]$CommonName))#374 bin number
  try_data[[i]]<-as.data.frame(unique(raw[[i]]$CommonName))# a data frame with 374 rows and 1 column
  colnames(try_data[[i]])[1]<-"CommonName"
  try_data[[i]]$TaxonID<-c(1:length_qi)
  try_data[[i]]$MainID<-c(1:length_qi)
  try_data[[i]]$DataSourceID<-rep(1,each=length_qi)
  try_data[[i]]$BiotopeID<-rep(1,each=length_qi)
  try_data[[i]]$LocationID<-rep(1,each=length_qi)
  try_data[[i]]$SamplingInterval<-rep("annual",each=length_qi)
  
  raw_select<-raw[[i]][raw[[i]]$MainID==unique(raw[[i]]$MainID)[1],]
  rep_for_each_bin<-dim(raw_select)[1]#depth number
  number_of_bin<-(dim(raw[[i]])[1])/rep_for_each_bin# number_of_bin 
  #print(number_of_bin)
  monotonicR2<-as.data.frame(matrix(data=NA,ncol = 1,nrow = number_of_bin))
  CV<-as.data.frame(matrix(data=NA,ncol = 1,nrow = number_of_bin))
  mediangr<-as.data.frame(matrix(data=NA,ncol = 1,nrow = number_of_bin))
  mediangr_mo<-as.data.frame(matrix(data=NA,ncol = 1,nrow = number_of_bin))
  
  for (j in 1:number_of_bin) {
    print(paste0("i=",i,"j=",j))
    bb<-i*rep_for_each_bin
    aa<-(bb-(rep_for_each_bin-1))
    data<-raw[[i]][aa:bb,]
    monotonicR2[j,1]<-(cor(data$SeriesStep, data$PopRescale, use="p", method="spearman"))^2
    CV[j,1]<-sd(data$PopRescale, na.rm=T)/mean(data$PopRescale, na.rm=T)
    mediangr[j,1]<-median(gr_step4[[i]][,(j+1)], na.rm=T)
    mediangr_mo[j,1]<-mediangr[j,1]/1
    
  }
  cbind_merge[[i]]<-cbind(try_data[[i]],monotonicR2,CV,mediangr,mediangr_mo)
  colnames(cbind_merge[[i]])[8:11]<-c("monotonicR2","CV","mediangr","mediangr_mo")
  #dim(try_data)
  print(dim(try_data[[i]]))
  print(dim(cbind_merge[[i]]))
  cbind_merge1[[i]]<-left_join(cbind_merge[[i]],as.data.frame(tax_qi),by="CommonName")
  cbind_merge1[[i]]$TaxonName<-cbind_merge1[[i]]$CommonName
  qi123[[i]]<-cbind_merge1[[i]][,c("MainID","TaxonID","DataSourceID","BiotopeID",
                                   "LocationID","TaxonName","CommonName","Kindom","Phylum","Class","Order",
                                   "Family","Genus","Species","SamplingInterval",
                                   "monotonicR2","CV","mediangr","mediangr_mo")]
  write.csv(qi123[[i]],file = paste0(raw_temp[[i]],"_metadata2.csv"))
  colnames(cbind_merge1[[i]])
  print(paste0("i=",i,"j=",j,"finish"))
}



#############
rm(list=ls())

source("./LE_ChaosDetectionMethods.R")



metadata_qi<- list.files(path=".", pattern="*_metadata2.csv", full.names=TRUE)
length(metadata_qi)
gpdd_d<-vector("list",10)
time_series_qi<- list.files(path=".", pattern="*_time_series_data2.csv", full.names=TRUE)
length(time_series_qi)
gpdd_d_ts<-vector("list",10)
gpdd_results<-vector("list",10)

for (i in 1:10) {# read in raw data
  print(i)
  gpdd_d[[i]]<-read.table(file=metadata_qi[[i]],sep=",",header = T,row.names=NULL)
  gpdd_d_ts[[i]]<-read.table(file=time_series_qi[[i]],sep=",",header = T,row.names=NULL)
}


library(tidyr)
library(purrr)
for (i in 1:10) {
  gpdd_d_ts[[i]]=gpdd_d_ts[[i]] %>% group_by(MainID) %>% nest(.key="data_rescale") %>% 
    mutate(data_rescale=map(data_rescale, as.data.frame))
  gpdd_d[[i]]=left_join(gpdd_d[[i]], gpdd_d_ts[[i]], by="MainID")
  
  gpdd_results[[i]]=select(gpdd_d[[i]], MainID)
}


#### Jacobian LE Method ####
#dim(gpdd_d$data_rescale[])
ratio_qi<-as.data.frame(matrix(data = NA,ncol = 1,nrow = 10))
results_exponent_qi<-as.data.frame(matrix(data = NA,ncol = 1,nrow = 10))
#fit models
#models 1 and 2 were dropped because the results of 1&3 and 2&5 are identical
library(rEDM)
for (i in 4:10) {
  gpdd_results[[i]]$modelresults3=map(gpdd_d[[i]]$data_rescale, smap_model_options, y="PopRescale", model=3) #fd-ut
  print("1")
  gpdd_results[[i]]$modelresults4=map(gpdd_d[[i]]$data_rescale, smap_model_options, y="PopRescale", model=4) #gr-ut
  print("2")
  gpdd_results[[i]]$modelresults5=map(gpdd_d[[i]]$data_rescale, smap_model_options, y="PopRescale", model=5) #gr-log
  print("3")
  #pull out R2 values for each model
  gpdd_d[[i]]$R3m=map_dbl(gpdd_results[[i]]$modelresults3, ~.x$modelstats$R2model)
  
  gpdd_d[[i]]$R4m=map_dbl(gpdd_results[[i]]$modelresults4, ~.x$modelstats$R2model)
  gpdd_d[[i]]$R5m=map_dbl(gpdd_results[[i]]$modelresults5, ~.x$modelstats$R2model)
  gpdd_d[[i]]$R3a=map_dbl(gpdd_results[[i]]$modelresults3, ~.x$modelstats$R2abund)
  gpdd_d[[i]]$R4a=map_dbl(gpdd_results[[i]]$modelresults4, ~.x$modelstats$R2abund)
  gpdd_d[[i]]$R5a=map_dbl(gpdd_results[[i]]$modelresults5, ~.x$modelstats$R2abund)
  
  gpdd_d[[i]]$bestmodel=select(gpdd_d[[i]],R3a,R4a,R5a) %>% apply(1,which.max)
  gpdd_d[[i]]$bestR2=select(gpdd_d[[i]],R3a,R4a,R5a) %>% apply(1,max) #R2 for abundance
  gpdd_d[[i]]$bestR2m=select(gpdd_d[[i]],R3m,R4m,R5m) %>% apply(1,max) #R2 for growth rate
  gpdd_results[[i]]$modelresultsbest=cbind(select(gpdd_results[[i]], modelresults3, modelresults4, modelresults5),gpdd_d[[i]]$bestmodel) %>% apply(1, function(x) {m=as.numeric(x["gpdd_d[[i]]$bestmodel"]); x[m][[1]]})
  
  
 
  #get E, tau, theta values from best model
  gpdd_d[[i]]$E=map_dbl(gpdd_results[[i]]$modelresultsbest, ~.x$modelstats$E)
  gpdd_d[[i]]$tau=map_dbl(gpdd_results[[i]]$modelresultsbest, ~.x$modelstats$tau)
  gpdd_d[[i]]$theta=map_dbl(gpdd_results[[i]]$modelresultsbest, ~.x$modelstats$theta)
  #get best model form
  gpdd_d[[i]]$modelform=map_chr(gpdd_results[[i]]$modelresultsbest, ~.x$form)
  
  #get jacobian matrices
  gpdd_results[[i]]$jacobians=map(gpdd_results[[i]]$modelresultsbest, getJacobians)
  
  
  
  #get LE estimates
  gpdd_results[[i]]$JLE=map2(gpdd_results[[i]]$modelresultsbest, gpdd_results[[i]]$jacobians, LEshift)
  gpdd_d[[i]]$minmean=map_dbl(gpdd_results[[i]]$JLE, ~.x$minmean) #LE mean
  theimpor<-cbind(as.character(gpdd_d[[i]]$CommonName),gpdd_d[[i]]$minmean)
  write.csv(theimpor,file = paste0(metadata_qi[[i]],"min_mean.csv"))
  gpdd_d[[i]]$minci=map_dbl(gpdd_results[[i]]$JLE, ~.x$minci) #LE lower confidence bound (*this is the LE estimate to use!*)
  gpdd_d[[i]]$mincisign=ifelse(gpdd_d[[i]]$minci>0.01, "chaotic", "not chaotic")
  ratio_qi[i,1]<-(sum(gpdd_d[[i]]$minmean>0)/length(gpdd_d[[i]]$minmean))

}


write.csv(ratio_qi,file = paste0(metadata_qi[[i]],"ratio_qi_summary1.csv"))
write.csv(results_exponent_qi,file = paste0(metadata_qi[[i]],"results_exponent_qi_summary1.csv"))
#convert LE estimate to common timescale 
gpdd_d$minci_mo=gpdd_d$minci/timescale_mo(gpdd_d$SamplingInterval, 1)
gpdd_d$minci_gen=gpdd_d$minci_mo*gpdd_d$MinAge_mo##没有这个参数
gpdd_d$minmean_mo=gpdd_d$minmean/timescale_mo(gpdd_d$SamplingInterval, 1)
gpdd_d$minmean_gen=gpdd_d$minmean_mo*gpdd_d$MinAge_mo

#predictability of time series (abundance, growth rate, both, neither)
predthreshold=0.2
gpdd_d$predictable_ag=ifelse(gpdd_d$bestR2>predthreshold & gpdd_d$bestR2m>predthreshold, "ag",
                             ifelse(gpdd_d$bestR2>predthreshold & gpdd_d$bestR2m<=predthreshold, "a",
                                    ifelse(gpdd_d$bestR2<=predthreshold & gpdd_d$bestR2m>predthreshold, "g", "none")))

#fix E to 1 for best model, recompute LE ####



#get best model of the 3 forms

gpdd_results$LE1d=map2(gpdd_d$data_rescale, gpdd_d$bestmodel+2, LE1d, y="PopRescale")
gpdd_d$minci1d=map_dbl(gpdd_results$LE1d, ~.x$JLE$minci)
gpdd_d$minmean1d=map_dbl(gpdd_results$LE1d, ~.x$JLE$minmean)
gpdd_d$mincisign1d=ifelse(gpdd_d$minci1d>0.01, "chaotic", "not chaotic")
length(which(gpdd_d$minci1d>0.01))/length(which(!is.na(gpdd_d$minci1d)))

#shorten time series, recompute LE ####
gpdd1=select(gpdd_d, MainID:Notes, monotonicR2, data_rescale, bestmodel:predictable_ag) %>% 
  mutate(tslengthcat="full") %>% filter(mincisign=="chaotic")
gpdd2=gpdd1 %>% 
  mutate(datasetlength=ifelse(datasetlength==30,NA,ifelse(datasetlength/2<=30,30,ceiling(datasetlength/2))),
         tslengthcat="short") %>% filter(!is.na(datasetlength))
gpdd3=gpdd2 %>% 
  mutate(datasetlength=ifelse(datasetlength==30,NA,ifelse(datasetlength/2<=30,30,ceiling(datasetlength/2)))) %>% 
  filter(!is.na(datasetlength))
gpdd4=gpdd3 %>% 
  mutate(datasetlength=ifelse(datasetlength==30,NA,ifelse(datasetlength/2<=30,30,ceiling(datasetlength/2)))) %>% 
  filter(!is.na(datasetlength))
gpdd_short=rbind(gpdd2, gpdd3, gpdd4) %>% 
  mutate(timescale_MinAge=datasetlength/timescale_mo(SamplingInterval, MinAge_mo),
         timescale_Lifespan=datasetlength/timescale_mo(SamplingInterval, Lifespan_mo))
shortents=function(datasetlength, data) {
  data[(nrow(data)-datasetlength+1):nrow(data),]
}
gpdd_short$data_rescale=map2(gpdd_short$datasetlength, gpdd_short$data_rescale, shortents)

#run models
gpdd_short_results=select(gpdd_short, MainID, datasetlength)
gpdd_short_results$modelresults3=map(gpdd_short$data_rescale, smap_model_options, y="PopRescale", model=3)
gpdd_short_results$modelresults4=map(gpdd_short$data_rescale, smap_model_options, y="PopRescale", model=4)
gpdd_short_results$modelresults5=map(gpdd_short$data_rescale, smap_model_options, y="PopRescale", model=5)

gpdd_short_results$R3m=map_dbl(gpdd_short_results$modelresults3, ~.x$modelstats$R2model)
gpdd_short_results$R4m=map_dbl(gpdd_short_results$modelresults4, ~.x$modelstats$R2model)
gpdd_short_results$R5m=map_dbl(gpdd_short_results$modelresults5, ~.x$modelstats$R2model)
gpdd_short_results$R3a=map_dbl(gpdd_short_results$modelresults3, ~.x$modelstats$R2abund)
gpdd_short_results$R4a=map_dbl(gpdd_short_results$modelresults4, ~.x$modelstats$R2abund)
gpdd_short_results$R5a=map_dbl(gpdd_short_results$modelresults5, ~.x$modelstats$R2abund)

#best model
gpdd_short$bestmodel=select(gpdd_short_results,R3a,R4a,R5a) %>% apply(1,which.max)
gpdd_short$bestR2=select(gpdd_short_results,R3a,R4a,R5a) %>% apply(1,max)
gpdd_short$bestR2m=select(gpdd_short_results,R3m,R4m,R5m) %>% apply(1,max)
gpdd_short_results$modelresultsbest=cbind(select(gpdd_short_results, modelresults3, modelresults4, modelresults5),gpdd_short$bestmodel) %>% apply(1, function(x) {m=as.numeric(x["gpdd_short$bestmodel"]); x[m][[1]]})
gpdd_short$E=map_dbl(gpdd_short_results$modelresultsbest, ~.x$modelstats$E)
gpdd_short$tau=map_dbl(gpdd_short_results$modelresultsbest, ~.x$modelstats$tau)
gpdd_short$theta=map_dbl(gpdd_short_results$modelresultsbest, ~.x$modelstats$theta)
gpdd_short$modelform=map_chr(gpdd_short_results$modelresultsbest, ~.x$form)

#jacobians and LEs
gpdd_short_results$jacobians=map(gpdd_short_results$modelresultsbest, getJacobians)
gpdd_short_results$JLE=map2(gpdd_short_results$modelresultsbest, gpdd_short_results$jacobians, LEshift)
gpdd_short$minci=map_dbl(gpdd_short_results$JLE, ~.x$minci)
gpdd_short$minmean=map_dbl(gpdd_short_results$JLE, ~.x$minmean)
gpdd_short$mincisign=ifelse(gpdd_short$minci>0.01, "chaotic", "not chaotic")
#convert to common timescale
gpdd_short$minci_mo=gpdd_short$minci/timescale_mo(gpdd_short$SamplingInterval, 1)
gpdd_short$minci_gen=gpdd_short$minci_mo*gpdd_short$MinAge_mo
gpdd_short$minmean_mo=gpdd_short$minmean/timescale_mo(gpdd_short$SamplingInterval, 1)
gpdd_short$minmean_gen=gpdd_short$minmean_mo*gpdd_short$MinAge_mo

#predictability
gpdd_short$predictable_ag=ifelse(gpdd_short$bestR2>predthreshold & gpdd_short$bestR2m>predthreshold, "ag",
                                 ifelse(gpdd_short$bestR2>predthreshold & gpdd_short$bestR2m<=predthreshold, "a",
                                        ifelse(gpdd_short$bestR2<=predthreshold & gpdd_short$bestR2m>predthreshold, "g", "none")))

gpdd_combo=rbind(gpdd1,gpdd_short)

# sensitivity to changes in E and tau ####
gpdd_d$modnum=case_when(gpdd_d$modelform =="ut-ut" ~ 1,
                        gpdd_d$modelform =="log-log" ~ 2,
                        gpdd_d$modelform =="fd-ut" ~ 3,
                        gpdd_d$modelform =="gr-ut" ~ 4,
                        gpdd_d$modelform =="gr-log" ~ 5)
gpdd_d$Eplus=ifelse(gpdd_d$E+1>6,6,gpdd_d$E+1)
gpdd_d$Eminus=ifelse(gpdd_d$E-1<1,1,gpdd_d$E-1)
gpdd_d$tauplus=ifelse(gpdd_d$tau+1>6,6,gpdd_d$tau+1)
gpdd_d$tauminus=ifelse(gpdd_d$tau-1<1,1,gpdd_d$tau-1)

gpdd_results$sens1=pmap(list(data=gpdd_d$data_rescale, model=gpdd_d$modnum,
                             Efix=gpdd_d$Eplus,taufix=gpdd_d$tau),LEfix, y="PopRescale")
gpdd_results$sens2=pmap(list(data=gpdd_d$data_rescale, model=gpdd_d$modnum,
                             Efix=gpdd_d$Eminus,taufix=gpdd_d$tau),LEfix, y="PopRescale")
gpdd_results$sens3=pmap(list(data=gpdd_d$data_rescale, model=gpdd_d$modnum,
                             Efix=gpdd_d$E,taufix=gpdd_d$tauplus),LEfix, y="PopRescale")
gpdd_results$sens4=pmap(list(data=gpdd_d$data_rescale, model=gpdd_d$modnum,
                             Efix=gpdd_d$E,taufix=gpdd_d$tauminus),LEfix, y="PopRescale")

gpdd_d$sens1LEmin=map_dbl(gpdd_results$sens1, ~.x$JLE$minci)
gpdd_d$sens2LEmin=map_dbl(gpdd_results$sens2, ~.x$JLE$minci)
gpdd_d$sens3LEmin=map_dbl(gpdd_results$sens3, ~.x$JLE$minci)
gpdd_d$sens4LEmin=map_dbl(gpdd_results$sens4, ~.x$JLE$minci)

gpdd_d$sens1LEsign=ifelse(gpdd_d$sens1LEmin>0.01, "chaotic", "not chaotic")
gpdd_d$sens2LEsign=ifelse(gpdd_d$sens2LEmin>0.01, "chaotic", "not chaotic")
gpdd_d$sens3LEsign=ifelse(gpdd_d$sens3LEmin>0.01, "chaotic", "not chaotic")
gpdd_d$sens4LEsign=ifelse(gpdd_d$sens4LEmin>0.01, "chaotic", "not chaotic")

gpdd_d$sens1R2=map_dbl(gpdd_results$sens1, ~.x$modelresults$modelstats$R2abund)
gpdd_d$sens2R2=map_dbl(gpdd_results$sens2, ~.x$modelresults$modelstats$R2abund)
gpdd_d$sens3R2=map_dbl(gpdd_results$sens3, ~.x$modelresults$modelstats$R2abund)
gpdd_d$sens4R2=map_dbl(gpdd_results$sens4, ~.x$modelresults$modelstats$R2abund)

sensresults=data.frame(model=c("best","E+1","E-1","tau+1","tau-1"), propchaotic=NA, meanR2=NA, medianR2=NA)

sensresults$propchaotic[1]=length(which(gpdd_d$mincisign=="chaotic"))/length(which(!is.na(gpdd_d$mincisign)))
sensresults$propchaotic[2]=length(which(gpdd_d$sens1LEsign=="chaotic"))/length(which(!is.na(gpdd_d$mincisign)))
sensresults$propchaotic[3]=length(which(gpdd_d$sens2LEsign=="chaotic"))/length(which(!is.na(gpdd_d$mincisign)))
sensresults$propchaotic[4]=length(which(gpdd_d$sens3LEsign=="chaotic"))/length(which(!is.na(gpdd_d$mincisign)))
sensresults$propchaotic[5]=length(which(gpdd_d$sens4LEsign=="chaotic"))/length(which(!is.na(gpdd_d$mincisign)))

sensresults$meanR2[1]=mean(gpdd_d$bestR2)
sensresults$meanR2[2]=mean(gpdd_d$sens1R2)
sensresults$meanR2[3]=mean(gpdd_d$sens2R2)
sensresults$meanR2[4]=mean(gpdd_d$sens3R2)
sensresults$meanR2[5]=mean(gpdd_d$sens4R2)

sensresults$medianR2[1]=median(gpdd_d$bestR2)
sensresults$medianR2[2]=median(gpdd_d$sens1R2)
sensresults$medianR2[3]=median(gpdd_d$sens2R2)
sensresults$medianR2[4]=median(gpdd_d$sens3R2)
sensresults$medianR2[5]=median(gpdd_d$sens4R2)

sensresults[,2:4]=round(sensresults[,2:4],2)

#### Export Results ####

#save results
save(gpdd_d, gpdd_results, gpdd_combo, gpdd_short, gpdd_short_results, file = "./data/gpdd_results_update3.Rdata")

#export E and tau for use in other analyses
exportEtau=select(gpdd_d, MainID, E, tau)
write.csv(exportEtau, "./data/gpdd_Etau_smap.csv", row.names = F)

#export main results
#change names of some variables, so more intuitive
exportres=select(gpdd_d, MainID, R2abund=bestR2, R2gr=bestR2m, predictable_ag, modelform, E, tau, theta, 
                 LEmean=minmean, LEmean_mo=minmean_mo, LEmean_gen=minmean_gen, 
                 LEmin=minci, LEmin_mo=minci_mo, LEmin_gen=minci_gen, LEclass=mincisign, 
                 LEmin1d=minci1d, LEmean1d=minmean1d, LEclass1d=mincisign1d)
write.csv(exportres, "./data/gpdd_results_smap.csv", row.names = F)
#export results with shortened time series
exportres2=select(gpdd_combo, MainID, datasetlength, tslengthcat, timescale_MinAge, MinAge_mo, Mass_g, R2abund=bestR2, R2gr=bestR2m, predictable_ag, E, tau, theta, 
                  LEmean=minmean, LEmin=minci, LEmin_mo=minci_mo, LEmin_gen=minci_gen, LEclass=mincisign)
write.csv(exportres2, "./data/gpdd_results_truncation_smap.csv", row.names = F)