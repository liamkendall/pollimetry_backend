library(lattice)
library(ModelMetrics)
bee_kfold

heatmapbee=rbind.data.frame(rmse(bee_all$Spec.wgt,exp(predict(bee2))[,1]),
                            rmse(bee_all$Spec.wgt,exp(predict(bee9))[,1]),
                            rmse(bee_all$Spec.wgt,exp(predict(bee7))[,1]),
                            rmse(bee_all$Spec.wgt,exp(predict(bee_phy1))[,1]),
                            rmse(bee_all$Spec.wgt,exp(predict(bee_phy3))[,1]),
                            rmse(bee_all$Spec.wgt,exp(predict(bee1))[,1]),
                            rmse(bee_all$Spec.wgt,exp(predict(bee))[,1]),
                            rmse(bee_all$Spec.wgt,Cane(bee_all$IT)))
    


#dis=dist(heatmapbee,diag=FALSE) 
distmatbee<-as.data.frame(apply(as.matrix(heatmapbee),1,function(x) heatmapbee-x))

m=as.matrix(distmatbee)
colnames(m)=c("Tax+Sex","Sex","Tax","Phy+Sex","Phy","ITD+RE","ITD","Cane 1987")
rownames(m)=c("Tax+Sex","Sex","Tax","Phy+Sex","Phy","ITD+RE","ITD","Cane 1987")

m[upper.tri(m)]=NA
p1=levelplot(m,main="A) Bees",
             col.regions=new.palette,
             border="black",
             scales=list(y=(list(cex=1)), 
                         tck = c(1,0), 
                         x=list(cex=1, 
                                rot=90)), 
             xlab="", ylab="", 
             legend=list(top=list(fun=grid::textGrob("DRMSE", y=-0.5, x=1.15))))
p1


##HOVERFLIES
heatmaphov=rbind.data.frame(rmse(hov_all$Spec.wgt,exp(predict(hov2,newdata = hov_all)[,1])),
                            rmse(hov_all$Spec.wgt,exp(predict(hov3,newdata = hov_all)[,1])),
                            rmse(hov_all$Spec.wgt,exp(predict(hov9,newdata = hov_all)[,1])),
                            rmse(hov_all$Spec.wgt,exp(predict(hov7,newdata = hov_all)[,1])),
                            rmse(hov_all$Spec.wgt,exp(predict(hov,newdata = hov_all)[,1])))


distmathov<-as.data.frame(apply(as.matrix(heatmaphov),1,function(x) heatmaphov-x))

#dish=dist(distmathov,diag=FALSE) 

mh=as.matrix(distmathov)
colnames(mh)=c("Sex","Tax+Sex","Tax","ITD+RE","ITD")
rownames(mh)=c("Sex","Tax+Sex","Tax","ITD+RE","ITD")
mh

new.palette=colorRampPalette(c("#0571b0","#92c5de","#f7f7f7","#f4a582","#ca0020")
                             ,space="rgb")

mh[upper.tri(mh)]=NA
p2=levelplot(mh,main="B) Hoverflies",
             col.regions=new.palette,border="black",
             scales=list(y=(list(cex=1)), tck = c(1,0), x=list(cex=1, rot=90)), 
             xlab="", ylab="", legend=list(top=list(fun=grid::textGrob("DRMSE", y=-0.5, x=1.15))))
p2
library(gridExtra)
grid.arrange(p1,p2, ncol=2) #6 x 10


