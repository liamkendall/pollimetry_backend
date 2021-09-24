
pp_check()
ppc_violin_grouped(log(bee_all$Spec.wgt), 
                     yrep = posterior_predict(bee4, nsamples = 3000), group = bee_all$Tribe)

ppc_violin_grouped(log(bee_all$Spec.wgt), 
                   yrep = posterior_predict(bee4C, nsamples = 3000), group = bee_all$Country)

bee4C<- brm(log(Spec.wgt) ~ Sex + Family + log(IT) + (1 | Region/Country/Species) + 
             Sex:log(IT), data = bee_all, cores=4,
           family = gaussian(),prior=bprior1,control=list(adapt_delta=0.99,max_treedepth=15))

bee4

waic(bee4C)

waic(bee4)

ppc_violin_grouped(log(bee_all$Spec.wgt), 
                   yrep = posterior_predict(bee4C, nsamples = 1000), group = bee_all$Country)
pp_check(bee4,nsamples=3000)
pp_check(bee4C,nsamples=3000)

bee4C<- brm(log(Spec.wgt) ~ Sex + Family + log(IT) + (1| Region/Country/Species) + 
              Sex:log(IT), data = bee_all, cores=4,iter = 1000,
            family = gaussian(),prior=bprior1,control=list(adapt_delta=0.99,max_treedepth=15))
check_all_diagnostics(bee4C$fit)
#nested
B=A
bee_all$RCspecies=paste(bee_all$Region,bee_all$Country,bee_all$Species,sep="_")
region_country_species=unique(bee_all[order(match(bee_all$Species, bee.tree$tip.label)), ]$Rspecies)
rownames(B)=unique(region_species)
isSymmetric(B,check.attributes=FALSE)
         