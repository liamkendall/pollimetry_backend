###update 2021

bprior1 <- prior(normal(0,0.4), class = b) + 
  prior(normal(1,1),class=b,coef="logIT") +
  prior(normal(0,1), class = Intercept) + 
  prior(normal(0,0.4), class = sd) +
  prior(normal(0,0.4), class = sigma) 

options(brms.backend="cmdstanr")

bee_tax_mod_21<- brm(log(Spec.wgt) ~ Sex + Family + 
                log(IT) + 
             (1|Region/Species) + 
             Family:log(IT), 
           data = bee_all,
           cores=4,
           family = gaussian(),
           prior=bprior1,
           control=list(adapt_delta=0.99,
                        max_treedepth=15))

bee_phy_mod_21<- brm(log(Spec.wgt) ~ Sex + log(IT) + 
                 (1|Region) + Sex:log(IT)+
                 (1|gr("Region:Species", cov = B)),
               data = bee_all,
               data2 = list(B = B),
               cores=4,
               family = gaussian(),
               #cov_ranef = list("Region:Species" = B),
               prior=bprior1,
               control = list(adapt_delta = 0.99,
                              max_treedepth=15))

bee_IT_21<- brm(log(Spec.wgt)~log(IT)+(1|Region/Species),
           data = bee_all,
           cores=4,
           family = gaussian(),
           prior=bprior1,
           control=list(adapt_delta=0.99,
                        max_treedepth=15))

bee_sex_21<- brm(log(Spec.wgt) ~ Sex + log(IT) + 
                   (1 | Region/Species),
                 data = bee_all,
                 cores=4,
           family = gaussian(),
           prior=bprior1,
           control=list(adapt_delta=0.99,
                        max_treedepth=15))


###########
#HOVERFLIES
###########
hprior1 <- prior(normal(0,0.4), class = b) + 
  prior(normal(1,1),class=b,coef="logIT") +
  prior(normal(0,1), class = Intercept) + 
  prior(normal(0,0.4), class = sd) +
  prior(normal(0,0.4), class = sigma) 

hov_tax_mod_21<- brm(log(Spec.wgt) ~ Sex + Subfamily + log(IT) + 
             (1 | Region/Species),
           data = hov_all,
           cores=4,
           family = gaussian(),
           prior=hprior1,
           control = list(adapt_delta = 0.999))

hov_IT_21<- brm(log(Spec.wgt) ~ log(IT) + (1 | Region/Species),
           data = hov_all,
           cores=4,
           family = gaussian(),
           prior=hprior1,
           control = list(adapt_delta = 0.999,
                          max_treedepth=20))

hov_sex_21<- brm(log(Spec.wgt) ~ Sex + log(IT) + 
                   (1 | Region/Species) + Sex:log(IT),
                 data = hov_all,
                 cores=4,
           family = gaussian(),
           prior=hprior1,
           control = list(adapt_delta = 0.999,
                          max_treedepth=15))

bee_tax_mod=bee_tax_mod_21
bee_phy_mod=bee_phy_mod_21
bee_IT=bee_IT_21
bee_sex=bee_sex_21

hov_tax_mod=hov_tax_mod_21
hov_IT=hov_IT_21
hov_sex=hov_sex_21

#FOR data package
save(bee_tax_mod,file="models_9_21/bee_tax_mod.rdata",compress="xz")
save(bee_phy_mod,file="models_9_21/bee_phy_mod.rdata",compress="xz")
save(bee_sex,file="models_9_21/bee_sex_mod.rdata",compress="xz")
save(bee_IT,file="models_9_21/bee_IT.rdata",compress="xz")

save(hov_tax_mod,file="models_9_21/hov_tax_mod.rdata",compress="xz")
save(hov_sex,file="models_9_21/hov_sex_mod.rdata",compress="xz")
save(hov_IT,file="models_9_21/hov_IT.rdata",compress="xz")
