library(magrittr)
library(dplyr)
library(forcats)
library(tidyr)
library(modelr)
library(tidybayes)
library(ggplot2)
library(ggstance)
library(ggridges)
library(cowplot)
library(rstan)
library(brms)
library(grid)
library(gtable)
library(gridExtra)
library(ggplot2)   
library(gtable)    
library(grid)
library(gridExtra) 

<<<<<<< HEAD
=======

>>>>>>> ce36b7860017ecf456e9442bea97fa57d0e72595
##REGION - refit without species term 

BR=bee_all %>%
  data_grid(bee_all,IT = seq_range((IT), n = 50)) %>%
  add_fitted_draws(model=bee1,re_formula= ~(1|Region),n=50) %>%
  ggplot(aes(x = log(IT),color=Region,shape=Region))+
  xlab("ln ITD (mm)")+
  ylab("ln dry weight (mg)")+
  ggtitle("A) Bees: Biogeography")+
  geom_point(aes(y = log(Spec.wgt)),col=1, data = bee_all)+
  stat_lineribbon(aes(y = .value), .width = c(.0)) +
  scale_fill_brewer() + 
  scale_colour_brewer("Region", palette="Paired", direction=-1) + 
  scale_shape_discrete(solid=FALSE)+theme_bw()

##FAMILY

BRM=bee_all%>%
  data_grid(bee_all, IT = seq_range((IT), n = 50)) %>%
  add_fitted_draws(model=bee6,re_formula= NA,n=50) %>%
  ggplot(aes(x = log(IT),color=Family,shape=Family))+
  xlab("ln ITD (mm)")+
  ylab("ln dry weight (mg)")+
  ggtitle("B) Bees: Taxonomy")+
  geom_point(aes(y = log(Spec.wgt)), col=1,data = bee_all) +
  stat_lineribbon(aes(y = .value), .width = c(.0)) +
  scale_fill_brewer()+ scale_colour_brewer("Family", palette="Paired", direction=-1) +
  scale_shape_discrete(solid=FALSE)+theme_bw()

##Sex

BRS=bee_all %>%
  data_grid(bee_all, IT = seq_range((IT), n = 50)) %>%
  add_fitted_draws(model=bee8,re_formula= NA,n=50) %>%
  ggplot(aes(x = log(IT),color=Sex,shape=Sex))+
  xlab("ln ITD (mm)")+
  ylab("ln dry weight (mg)")+
  ggtitle("C) Bees: Sexual dimorphism")+
  geom_point(aes(y = log(Spec.wgt)), col=1,data = bee_all) +
  stat_lineribbon(aes(y = .value), .width = c(.0)) +
  scale_fill_brewer()+
  scale_shape_discrete(solid=FALSE)+theme_bw()


##HOV REGION
HR=hov_all%>%
  data_grid(hov_all,IT = seq_range((IT), n = 100)) %>%
  add_fitted_draws(model=hov7,re_formula= ~ (1|Region), n = 100) %>%
  ggplot(aes(x = log(IT),color=Region,shape=Region)) + 
  xlab("ln ITD (mm)")+
  ylab("ln dry weight (mg)")+
  ggtitle("D) Hoverflies: Biogeography")+
  geom_point(aes(y = log(Spec.wgt)), col=1, data = hov_all) +
  stat_lineribbon(aes(y = .value), .width = c(.0)) +
  scale_fill_brewer()+ 
  scale_colour_brewer("Region", palette="Paired"[0:1])+ 
  scale_shape_discrete(solid=FALSE)+
  theme_bw()


#Subfamily

HSS=hov_all%>%
  data_grid(hov_all,IT = seq_range(log(IT), n = 100)) %>%
  add_fitted_draws(model=hov9,re_formula=NA,n=100) %>%
  ggplot(aes(x = log(IT),y=log(Spec.wgt),color=Subfamily,shape=Subfamily)) + 
  xlab("ln ITD (mm)")+
  ylab("ln dry weight (mg)")+
  ggtitle("E) Hoverflies: Taxonomy")+
  geom_point(aes(y = log(Spec.wgt)), data = hov_all,col=1) +
  stat_lineribbon(aes(y = .value), .width = c(.0)) +
  scale_fill_brewer()+
  scale_colour_brewer("Subfamily", palette="Paired")+ 
  scale_shape_discrete(solid=FALSE)+
  theme_bw()  
<<<<<<< HEAD

=======
HSS
>>>>>>> ce36b7860017ecf456e9442bea97fa57d0e72595
#Sex

HSX=hov_all %>%
  data_grid(hov_all,IT = seq_range(log(IT), n = 100)) %>%
<<<<<<< HEAD
  add_fitted_draws(model=hov3,re_formula=NA,n=100) %>%
=======
  add_fitted_draws(model=hov2,re_formula=NA,n=100) %>%
>>>>>>> ce36b7860017ecf456e9442bea97fa57d0e72595
  ggplot(aes(x = log(IT),y=log(Spec.wgt),color=Sex,shape=Sex)) + 
  xlab("ln ITD (mm)")+
  ylab("ln dry weight (mg)")+
  ggtitle("F) Hoverflies: Sexual dimorphism")+
  geom_point(aes(y = log(Spec.wgt)), data = hov_all,col=1) + 
  stat_lineribbon(aes(y = .value), .width = c(.0)) +
  scale_fill_brewer()+ 
  scale_shape_discrete(solid=FALSE)+
  theme_bw()

Fig2=align_plots(BR, BRM, BRS,HR, HSS, HSX,align="hv", axis="tblr")
Fig2A <- ggdraw(Fig2[[1]])
Fig2B <- ggdraw(Fig2[[2]])
Fig2C <- ggdraw(Fig2[[3]])
Fig2D <- ggdraw(Fig2[[4]])
Fig2E <- ggdraw(Fig2[[5]])
Fig2F <- ggdraw(Fig2[[6]])
grid.arrange(Fig2A,Fig2B,Fig2C,Fig2D,Fig2E,Fig2F,ncol=3,nrow=2) #7 x 14 inches


