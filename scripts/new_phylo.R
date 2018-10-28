###NEW PHYLO APPROACH
#Tree for MCMC
library(phytools)
library(ape)
library(brms)

##LOAD TREE
bee.trees=read.tree(file="data/hedtke_genera_tree.txt")
##Use tree 1 (376 genera) #Genera-level phylogney
bee.mcmc=bee.trees[[1]]
range(bee.mcmc$edge.length) #all positive
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) #all positive
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)
is.ultrametric(bee.mcmc)
plot(bee.mcmc)
range(bee.mcmc$edge.length)
bee.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,unique(bee_all$Genus)))

species=unique(as.character(bee_all$Species))

species=read.csv("data/species.csv",stringsAsFactors = FALSE)

is.ultrametric(bee.tree)

## Will's suggestion
bee.tree$tip.label<-paste(bee.tree$tip.label,"_dum",sep="")
for(i in 1:length(species$Genus_species)){
  bee.tree<-add.species.to.genus(bee.tree,species$Genus_species[i],
                                where="root")
}
## prune out these same taxa
ii<-grep("dum",bee.tree$tip.label)
bee.tree<-drop.tip(bee.tree,bee.tree$tip.label[ii])

plot(bee.tree)
setdiff(species$Genus_species,bee.tree$tip.label)

bee.tree<-add.species.to.genus(bee.tree,"Lasioglossum_paradmirandum",
                               where="root")
bee.tree<-add.species.to.genus(bee.tree,"Lasioglossum_admirandum",
                               where="root")
is.ultrametric(bee.tree)
range(bee.tree$edge.length)
bee.tree$node.label=NULL

##PHYLO SIGNAL

WGT=as.data.frame(bee_phylo[,c("Spec.wgt")])
WGTSE=as.data.frame(bee_phylo[,c("Spec.wgt")])

str(WGT)
str(bee_phylo)
WGT=log(WGT)
rownames(WGT)=rownames(bee_phylo)

WGT<-as.matrix((WGT))[,1]
setdiff(bee.tree$tip.label,names(WGT))
phylosig(tree=bee.tree,x=WGT,method="lambda",test=TRUE)
plot(bee.tree)
setdiff(bee.tree$tip.label,names(WGT))
bee_wgt_phy=contMap(bee.tree,WGT,plot=FALSE)
n<-length(bee_wgt_phy$cols)
bee_wgt_phy$cols[1:n]<-colorRampPalette(c("blue","white","red"),space="Lab")(n)
bee_wgt_phy
plot(bee_wgt_phy,ftype="off",
     fsize=c(0.4,1),lwd=0.95,
     xlim=c(0,1.25*max(nodeHeights(bee.tree))),
     ftype="i",leg.txt="ln Dry weight")

mel_tips<-c("Macropis_europaea","Hesperapis_carinata")
megachil_tips<-c("Lithurgus_spp","Anthidium_sticticum")
apid_tips<-c("Ceratina_mikmaqi","Amegilla_chlorocyanea")
andren_tips<-c("Panurgus_dargius","Andrena_bicolor")
halicti_tips<-c("Dufourea_marginata","Lipotriches_flavoviridis")
colletid_tips<-c("Leioproctus_leisp1","Hylaeus_communis")
cladelabels(bee.tree,node=findMRCA(bee.tree,mel_tips),text="Melittidae",
            orientation="horizontal",offset=2)
cladelabels(bee.tree,node=findMRCA(bee.tree,megachil_tips),text="Megachilidae",
            orientation="horizontal",offset=2)
cladelabels(bee.tree,node=findMRCA(bee.tree,apid_tips),text="Apidae",
            orientation="horizontal",offset=2.0)
cladelabels(bee.tree,node=findMRCA(bee.tree,andren_tips),text="Andrenidae",
            orientation="horizontal",offset=2)
cladelabels(bee.tree,node=findMRCA(bee.tree,halicti_tips),text="Halictidae",
            orientation="horizontal",offset=2)
cladelabels(bee.tree,node=findMRCA(bee.tree,colletid_tips),text="Colletidae",
            orientation="horizontal",offset=2)

###RUN MODELS

inv.phylo <- MCMCglmm::inverseA(bee.tree, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
isSymmetric(A, check.attributes = FALSE)

#nested
B=A
bee_all$Rspecies=paste(bee_all$Region,bee_all$Species,sep="_")
region_species=unique(bee_all[order(match(bee_all$Species, bee.tree$tip.label)), ]$Rspecies)
rownames(B)=unique(region_species)
isSymmetric(B,check.attributes=FALSE)


bphyprior1 <- prior(normal(0,0.4), class = b) + 
  prior(normal(1,1),class=b,coef="logIT") +
  prior(normal(0,1), class = Intercept) + 
  prior(normal(0,1), class = sd) +
  prior(normal(0,0.4), class = sigma) 

bee_phy1<- brm(log(Spec.wgt) ~ Sex + log(IT) + Sex:log(IT) + (1 | Region/Species) , data = bee_all, cores=4,
             family = gaussian(),cov_ranef = list("Region:Species" = B),
             prior=bphyprior1,control = list(adapt_delta = 0.99,max_treedepth=15))

#good

bee_phy2<- brm(log(Spec.wgt) ~ Sex + log(IT) + (1 | Region/Species), data = bee_all,
               prior=bphyprior1, cores=4,
             family = gaussian(),cov_ranef = list("Region:Species" = B),
             control = list(adapt_delta = 0.99,max_treedepth=15))
#good

bee_phy3<- brm(log(Spec.wgt) ~ log(IT) + (1 | Region/Species), data = bee_all,
               prior=bphyprior1, cores=4,
             family = gaussian(),cov_ranef = list("Region:Species" = B),
             control = list(adapt_delta = 0.99,max_treedepth=15))

bee_phy_list=list(bee_phy1,bee_phy2,bee_phy3)

#str(bee_all_list)
#bee_phy_loo=lapply(bee_all_list,function (x) add_ic(x,c("loo")))

#bee_phy_R2=lapply(bee_phy_list,function (x) bayes_R2(x))
#bee_phy_ic=lapply(bee_phy_list,function (x) add_ic(x,c("waic","loo")))
bee_phy_ic=lapply(bee_phy_list,function (x) add_ic(x,c("loo")))
sapply(bee_phy_ic, "[[","loo")
lapply(bee_phy_ic,function (x) loo(x))

bee_tax_list=list(bee,bee1,bee2,bee3,bee4,bee5,bee6,bee7,bee8,bee9)
bee_tax_ic=lapply(bee_tax_list,function (x) add_ic(x,c("loo")))
bee_tax_ic=lapply(bee_tax_list,function (x) add_ic(x,c("loo"),reloo=TRUE))

lapply(bee_tax_ic,function (x) loo(x))


bee_phy_kfold=lapply(bee_phy_list,function (x) kfold(x,cores=4))
