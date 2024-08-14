
#Readme
#the script is mainly based on the script in github website (https://github.com/DaliangNing/iCAMP1), you could directly use the github R script or use this one.

rm(list = ls())
#step1:input parameter
args = commandArgs(trailingOnly=TRUE)
args[1]<-"."#input folder
args[2]<-"meer_v1_test2.rpkm_trench.csv"#MAGs abundance file
args[3]<-"layer_treatment_new_new11245.csv"#layer file
args[4]<-"concatenated.fasta.fasttree"#tree file
args[5]<-1#specify the column used to select samples
args[6]<-"confidence_random_100_96"#this is a label used for each job in HPC
args[7]<-200 #set random time for iCAMP
args[8]<-64 # number of CPUs in your server
args[9]<-480 # memory (G) used in server
args[10]<-48 # bin size for iCAMP
args[11]<-"Confidence"#the index for null model significance test
args[12]<-"bray"#taxonomic beta diversity index
args[13]<-"TRUE"#detial.null
args[14]<-"/dssg/home/acct-trench/trench-0/clsxx/Users/hwrn.aou/rep_genome_abd_1028/iCAMP_chaos_qi/iCAMP/iCAMP_input2/output_confidence_random_200__new_layer_bMPD_after__1202/"#save folder
args[15]<-"bMPD"#character, the metric for phylogenetic null model analysis"

setwd(args[1])
library(iCAMP)
library(ape)
library(NST)
t0=Sys.time() 

#step2:input tables
S16S=read.table(args[2],header=T,sep=",",row.name=1)
treat=read.table(args[3],header=T,sep=",",row.name=1)
tre.file=args[4]
i_treatment<-as.numeric(args[5])
prefix<-args[6]
rand.time<-as.numeric(args[7])
nworker<-as.numeric(args[8])
memory.G <- nworker * as.numeric(args[9])
bin.size.limit<-as.numeric(args[10]) 
sig.index<-args[11]
taxo.metric<-args[12]
detail.null<-as.logical(args[13])  # this is very important          
is.logical(detail.null)
save.wd<-args[14]
if(!dir.exists(save.wd)){dir.create(save.wd)}
S16S<-round(S16S*100000,digits=0)
phylo.metric<-args[15]

tree=read.tree(file = tre.file)
comm<-t(S16S)
sampid.check=match.name(rn.list=list(comm=comm,treat=treat))
treat=sampid.check$treat
comm=sampid.check$comm
comm=comm[,colSums(comm)>0,drop=FALSE] 

#step3:match OTU IDs in OTU table and tree file
spid.check=match.name(cn.list=list(comm=comm),tree.list=list(tree=tree))

# for the example data, the output should be "All match very well".
# for your data files, if you have not matched the IDs before, the unmatched OTUs will be removed
comm=spid.check$comm
tree=spid.check$tree
comm=spid.check$comm
tree$root.edge <- 0
is.rooted(tree)
comm=comm[,colSums(comm)>0,drop=FALSE] 
library(iCAMP)
print("start calculate pd.big")
setwd(save.wd)

#step4:running iCAMP
if(!file.exists("pd.desc")) 
{
  pd.big=iCAMP::pdist.big(tree = tree, wd=save.wd, nworker = nworker, memory.G = memory.G,treepath.file=paste0(prefix,"path.rda"),pd.spname.file=paste0(prefix,"pd.taxon.name.csv"),pd.backingfile=paste0(prefix,"pd.bin"),pd.desc.file=paste0(prefix,"pd.desc"))
  
}else{
  
  pd.big=list()
  pd.big$tip.label=read.csv(paste0(save.wd,"/pd.taxon.name",prefix,".csv"),row.names = 1,stringsAsFactors = FALSE)[,1]
  pd.big$pd.wd=save.wd
  pd.big$pd.file=paste0(prefix,".pd.desc")
  pd.big$pd.name.file=paste0(prefix,".pd.taxon.name.csv")
}

print("start calculate iCAMP.big")
icres=iCAMP::icamp.big(comm=comm, pd.desc = pd.big$pd.file, pd.spname=pd.big$tip.label,
                       pd.wd = pd.big$pd.wd, rand = rand.time, tree=tree,
                       prefix = prefix, ds = 0.2, pd.cut = NA, sp.check = TRUE,
                       phylo.rand.scale = "within.bin", taxa.rand.scale = "across.all",
                       phylo.metric = phylo.metric, sig.index=sig.index, bin.size.limit = bin.size.limit, 
                       nworker = nworker, memory.G = memory.G, rtree.save = FALSE, detail.save = TRUE, 
                       qp.save = TRUE, detail.null = detail.null, ignore.zero = TRUE, output.wd = save.wd, 
                       correct.special = TRUE, unit.sum = rowSums(comm), special.method = "depend",
                       ses.cut = 1.96, rc.cut = 0.95, conf.cut=0.975, omit.option = "no",meta.ab = NULL,taxo.metric=taxo.metric)
save(icres,file = paste0(prefix,".iCAMP.save_important.rda"))
print("start nntest")

write.csv(icres$detail$processes,file = paste0(prefix,"icres2_detail_processes.csv"))
write.csv(icres$detail$taxabin$sp.bin,file = paste0(prefix,"icres2_detail_taxabin_sp_bin.csv"))

icbin<-iCAMP::icamp.bins(icamp.detail = icres$detail,treat = NULL)
write.csv(icbin$Pt,file = paste0(prefix,".ProcessImportance_EachGroup.csv"),row.names = FALSE)
write.csv(icbin$Ptk,file = paste0(prefix,".ProcessImportance_EachBin_EachGroup.csv"),row.names = FALSE)
write.csv(icbin$Ptuv,file = paste0(prefix,".ProcessImportance_EachTurnover.csv"),row.names = FALSE)
write.csv(icbin$BPtk,file = paste0(prefix,".BinContributeToProcess_EachGroup.csv"),row.names = FALSE)
