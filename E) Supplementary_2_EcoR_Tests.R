## TEST WITH 416 ECOREGION Disp __________________________________________________________-####

# Test for Italian Peninsula
load("RDatas_for_SLURM/2.BTW_1000_10k/SLURM_416/Out_test_416.RData")

# Distances
dispersal_distances <- c(0.0001,0.001,0.1,1,10,15,20,40,100,1000)

# Gradient of loss
habitat_lost

library(doParallel)
registerDoParallel(cores = detectCores())

lost_scenarios <- foreach(loss=1:length(habitat_lost), .combine=rbind)%:%
  foreach(dispersal=1:length(dispersal_distances), .combine=rbind)%dopar% {
    
    # Scenarios of loss
    Perc_Hab_Loss <- habitat_lost[loss]
    FW_area_lost <- FW_area-(Perc_Hab_Loss*FW_area)
    J.freshwater<-ceiling((-Jmin+(J.max/(area.max.europa^b.ef))*FW_area_lost^b.ef))
    J.freshwater <- ifelse(J.freshwater<=0,0,J.freshwater)
    # 
    a <- NULL
    #b <- list()
    for (it in 1:3) {
      output <- H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom_tempIT(
        Meta.pool = pool_200,
        m.pool = 0.01,
        Js = J.freshwater,
        id.module = id_NOmodule,
        filter.env = filter_NOfilter,
        M.dist = Dist_Matrix,
        D50 = dispersal_distances[dispersal],
        m.max = 1,
        tempo_imp = 0.1,
        temp_Metacom = Meta_t0,
        temp_it = 4,
        id.fixed=NULL, D50.fixed=0, m.max.fixed=0, comm.fixed=pool_200,
        Lottery=F, it=1, prop.dead.by.it=1, id.obs=1:nrow(Coord_EcoReg))
      a <- rbind(a,output[[1]])
      #b[[it]] <- output[[2]]
    }
    resume.out(a)
  }

out_out <- list()
for (rounds in 1:c(length(dispersal_distances)*length(habitat_lost))) {
  outer <- list(
    lost_scenarios[[rounds]],
    lost_scenarios[[c(rounds+(1*(length(dispersal_distances)*length(habitat_lost))))]],
    lost_scenarios[[c(rounds+(2*(length(dispersal_distances)*length(habitat_lost))))]],
    lost_scenarios[[c(rounds+(3*(length(dispersal_distances)*length(habitat_lost))))]]
  )
  names(outer)<-c("Median", "Standard Deviation", "out.IC.up","out.IC.inf")
  out_out[[rounds]] <- outer  
}

final_out <- list()
for (place in 1:length(habitat_lost)){
  beg <- seq(1,length(out_out),length(dispersal_distances))  
  end <- seq(length(dispersal_distances),length(out_out),length(dispersal_distances))  
  final_out[[place]] <- out_out[beg[place]:end[place]]
}

length(final_out)
length(final_out[[1]])
length(final_out[[1]][[1]])

# Dispersal levels
dispersal_distances
dispersal_distances_plot <- c(paste(dispersal_distances[1],"km",sep = " "),
                              paste(dispersal_distances[2],"km",sep = " "),
                              paste(dispersal_distances[3],"km",sep = " "))
# Loss levels
habitat_lost <- habitat_lost*100
library(viridis);library(tidyverse)

# Alpha diversity _______####
Mean_Alpha <- data.frame()
for (dispe in 1:length(dispersal_distances)) {
  mean_all_temp <- data.frame()
  for (lost_hab in 1:(length(habitat_lost))) {
    Dataset_to_use <- final_out[[lost_hab]][[dispe]]
    Dataset_alpha_position <- 10:(9+nrow(Coord_EcoReg))
    Dataset_equal_to_zero <- which(Dataset_to_use$Median[Dataset_alpha_position]!=0)
    
    out <- data.frame("EcoR"=416,"loss"=habitat_lost[lost_hab],"disp"=dispersal_distances[dispe],
                      "S"=mean(Dataset_to_use$Median[Dataset_alpha_position][Dataset_equal_to_zero]),
                      "S_UP"=mean(Dataset_to_use$out.IC.up[Dataset_alpha_position][Dataset_equal_to_zero]),
                      "S_Low"=mean(Dataset_to_use$out.IC.inf[Dataset_alpha_position][Dataset_equal_to_zero])
                      )
    mean_all_temp <- bind_rows(mean_all_temp,out)
  }# Lost Habitat
  Mean_Alpha <- bind_rows(Mean_Alpha,mean_all_temp)
  rownames(Mean_Alpha) <- NULL
}# Dispe

# Beta diversity _______####
Mean_Beta <- data.frame()
for (dispe in 1:length(dispersal_distances)) {
  mean_all_temp <- data.frame()
  for (lost_hab in 1:(length(habitat_lost))) {
    Dataset_to_use <- final_out[[lost_hab]][[dispe]]
    Dataset_beta_position <- (10+nrow(Coord_EcoReg)):((10+nrow(Coord_EcoReg))+(nrow(Coord_EcoReg)-1))
    Dataset_equal_to_zero <- which(Dataset_to_use$Median[Dataset_beta_position]!=0)
    
    out <- data.frame("EcoR"=416,
                      "loss"=habitat_lost[lost_hab],
                      "disp"=dispersal_distances[dispe],
                      "B"=mean(Dataset_to_use$Median[Dataset_beta_position][Dataset_equal_to_zero]),
                      "B_UP"=mean(Dataset_to_use$out.IC.up[Dataset_beta_position][Dataset_equal_to_zero]),
                      "B_Low"=mean(Dataset_to_use$out.IC.inf[Dataset_beta_position][Dataset_equal_to_zero])
    )
    mean_all_temp <- bind_rows(mean_all_temp,out)
  }# Lost Habitat
  Mean_Beta <- bind_rows(Mean_Beta,mean_all_temp)
  rownames(Mean_Beta) <- NULL
}# Dispe

# Gamma diversity & Coef _______####
Mean_Rel_Gamma <- data.frame()
  for (dispe in 1:length(dispersal_distances)) {
    mean_all_filt <- Mean_Alpha %>% filter(disp==dispersal_distances[dispe])
    meanB_all_filt <- Mean_Beta %>% filter(disp==dispersal_distances[dispe])
    
    meanG_all_temp<- data.frame(
      "EcoR"=416,
      "loss"=mean_all_filt$loss,
      "disp"=dispersal_distances[dispe],
      "Rel_G"=((mean_all_filt$S*(1+meanB_all_filt$B))/max((mean_all_filt$S*(1+meanB_all_filt$B))))*100,
      "Rel_G_UP"=((mean_all_filt$S_UP*(1+meanB_all_filt$B_UP))/max((mean_all_filt$S_UP*(1+meanB_all_filt$B_UP))))*100,
      "Rel_G_Low"=((mean_all_filt$S_Low*(1+meanB_all_filt$B_Low))/max((mean_all_filt$S_Low*(1+meanB_all_filt$B_Low))))*100
      )                             
    Mean_Rel_Gamma <- bind_rows(Mean_Rel_Gamma,meanG_all_temp)
  } # Dispe

library(drc)
models_list <- matrix(nrow = nrow(Mean_Rel_Gamma))
for (dispersal in 1:length(dispersal_distances)) {
  IDs_dips <- which(Mean_Rel_Gamma$disp==dispersal_distances[dispersal])
  s <- Mean_Rel_Gamma[IDs_dips,4]
  f <- Mean_Rel_Gamma[IDs_dips,2]
  
  model_S <- nls(s~(a-b*f)-exp(f^q),start=list(a=s[1],b=0.1, q=1), control = list(maxiter=10000))
  
  p<-coefficients(model_S)
  Mean_Rel_Gamma[IDs_dips,7]<- p[2]
  Mean_Rel_Gamma[IDs_dips,8]<- p[3]
  Mean_Rel_Gamma[IDs_dips,9]<- predict(model_S)
  cat("We are at", dispersal_distances[dispersal], "and", p[2],"and", p[3], "\n")
}
colnames(Mean_Rel_Gamma)[7:9] <- c("Coef_b","Coef_q","Mod_Pred")



Mean_Rel_Gamma %>%
  mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
    disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
      disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
        disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
          disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
            disp==dispersal_distances[6], dispersal_distances_plot[6],
            dispersal_distances_plot[7]))))))) %>% 
  ggplot(aes(x=loss,y=Rel_G,color=as.factor(disp)))+
  geom_point(size=1.2)+
  geom_errorbar(aes(x=loss, ymax=Rel_G_UP,ymin=Rel_G_Low))+
  geom_line(aes(x=loss, Mod_Pred))+
  scale_y_continuous(limits = c(0,c(max(Mean_Rel_Gamma$Rel_G)+1)))+
  scale_colour_viridis(discrete = T)+
  labs(title =416)+
  theme_classic()


gridExtra::grid.arrange(
Mean_Rel_Gamma %>%
  ggplot(aes(x=loss,y=Rel_G,color=as.factor(disp),shape=as.factor(pool), linetype=as.factor(pool)))+
  geom_point(size=1.2)+
  geom_errorbar(aes(x=loss, ymax=Rel_G_UP,ymin=Rel_G_Low))+
  geom_line(aes(x=loss, Mod_Pred))+
  scale_color_viridis(discrete = T)+  scale_fill_viridis(discrete = T)+
  scale_y_continuous(limits = c(0,105))+
  labs(fill="Dispersal ability",colour="Dispersal ability",title="Relative Gamma")+
  theme_classic(),

Mean_Alpha %>% 
  ggplot(aes(x=loss,y=S,fill=as.factor(disp),colour=as.factor(disp),shape=as.factor(pool), linetype=as.factor(pool)))+
  geom_smooth(method="loess",se=F, linewidth=0.3)+
  geom_point(size=2, shape=22)+
  geom_errorbar(aes(x=loss,y=S, ymax=S_UP,ymin=S_Low))+
  scale_color_viridis(discrete = T)+  
  scale_fill_viridis(discrete = T)+
  labs(fill="Dispersal ability",colour="Dispersal ability",title="Alpha diversity")+
  theme_classic(),

Mean_Beta %>% 
  ggplot(aes(x=loss,y=B,fill=as.factor(disp),colour=as.factor(disp),shape=as.factor(pool), linetype=as.factor(pool)))+
  geom_smooth(method="loess",se=F, linewidth=0.3)+
  geom_point(size=2, shape=22)+
  geom_errorbar(aes(x=loss,y=B, ymax=B_UP,ymin=B_Low))+
  scale_color_viridis(discrete = T)+  
  scale_fill_viridis(discrete = T)+
  labs(fill="Dispersal ability",colour="Dispersal ability",title="Beta diversity")+
  theme_classic(),
ncol=3)


## TEST WITH 416 ECOREGION Pools __________________________________________________________-####

# Test for Italian Peninsula
load("RDatas_for_SLURM/2.BTW_1000_10k/SLURM_416/Out_test_416.RData")

# Distances
#dispersal_distances <- c(0.0001,0.001,0.1,1,10,15,20,40,100,1000)
dispersal_distances <- c(0.001,1,10)
spp_poool <- c(100,200,400)

crossing_levels <- tidyr::crossing(dispersal_distances,spp_poool)

dispersal_distances <- crossing_levels$dispersal_distances
spp_poool <- crossing_levels$spp_poool

# Gradient of loss
habitat_lost

library(doParallel)
registerDoParallel(cores = detectCores())

lost_scenarios <- foreach(loss=1:length(habitat_lost), .combine=rbind)%:%
  foreach(dispersal=1:length(dispersal_distances), .combine=rbind)%dopar% {
    
    id_NOmodule <- rep(1,nrow(Coord_EcoReg))
    filter_NOfilter <- matrix(nrow = spp_poool[dispersal], ncol=nrow(Coord_EcoReg), data = 1)
    pool_200 <- rlnorm(n = spp_poool[dispersal],5,1)
    Meta_t0 <- matrix(nrow = length(pool_200), ncol =nrow(Coord_EcoReg), 1)
    
    # Scenarios of loss
    Perc_Hab_Loss <- habitat_lost[loss]
    FW_area_lost <- FW_area-(Perc_Hab_Loss*FW_area)
    J.freshwater<-ceiling((-Jmin+(J.max/(area.max.europa^b.ef))*FW_area_lost^b.ef))
    J.freshwater <- ifelse(J.freshwater<=0,0,J.freshwater)
    # 
    a <- NULL
    #b <- list()
    for (it in 1:3) {
      output <- H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom_tempIT(
        Meta.pool = pool_200,
        m.pool = 0.01,
        Js = J.freshwater,
        id.module = id_NOmodule,
        filter.env = filter_NOfilter,
        M.dist = Dist_Matrix,
        D50 = dispersal_distances[dispersal],
        m.max = 1,
        tempo_imp = 0.1,
        temp_Metacom = Meta_t0,
        temp_it = 4,
        id.fixed=NULL, D50.fixed=0, m.max.fixed=0, comm.fixed=pool_200,
        Lottery=F, it=1, prop.dead.by.it=1, id.obs=1:nrow(Coord_EcoReg))
      a <- rbind(a,output[[1]])
      #b[[it]] <- output[[2]]
    }
    resume.out(a)
  }

out_out <- list()
for (rounds in 1:c(length(dispersal_distances)*length(habitat_lost))) {
  outer <- list(
    lost_scenarios[[rounds]],
    lost_scenarios[[c(rounds+(1*(length(dispersal_distances)*length(habitat_lost))))]],
    lost_scenarios[[c(rounds+(2*(length(dispersal_distances)*length(habitat_lost))))]],
    lost_scenarios[[c(rounds+(3*(length(dispersal_distances)*length(habitat_lost))))]]
  )
  names(outer)<-c("Median", "Standard Deviation", "out.IC.up","out.IC.inf")
  out_out[[rounds]] <- outer  
}

final_out <- list()
for (place in 1:length(habitat_lost)){
  beg <- seq(1,length(out_out),length(dispersal_distances))  
  end <- seq(length(dispersal_distances),length(out_out),length(dispersal_distances))  
  final_out[[place]] <- out_out[beg[place]:end[place]]
}

length(final_out)
length(final_out[[1]])
length(final_out[[1]][[1]])

# Dispersal levels
dispersal_distances
dispersal_distances_plot <- c(paste(dispersal_distances[1],"km",sep = " "),
                              paste(dispersal_distances[2],"km",sep = " "),
                              paste(dispersal_distances[3],"km",sep = " "))
# Loss levels
habitat_lost <- habitat_lost*100
library(viridis);library(tidyverse)

# Alpha diversity _______####
Mean_Alpha <- data.frame()
for (dispe in 1:length(dispersal_distances)) {
  mean_all_temp <- data.frame()
  for (lost_hab in 1:(length(habitat_lost))) {
    Dataset_to_use <- final_out[[lost_hab]][[dispe]]
    Dataset_alpha_position <- 10:(9+nrow(Coord_EcoReg))
    Dataset_equal_to_zero <- which(Dataset_to_use$Median[Dataset_alpha_position]!=0)
    
    out <- data.frame("EcoR"=416,"loss"=habitat_lost[lost_hab],"disp"=dispersal_distances[dispe],
                      "pool"=spp_poool[dispe],
                      "S"=mean(Dataset_to_use$Median[Dataset_alpha_position][Dataset_equal_to_zero]),
                      "S_UP"=mean(Dataset_to_use$out.IC.up[Dataset_alpha_position][Dataset_equal_to_zero]),
                      "S_Low"=mean(Dataset_to_use$out.IC.inf[Dataset_alpha_position][Dataset_equal_to_zero])
    )
    mean_all_temp <- bind_rows(mean_all_temp,out)
  }# Lost Habitat
  Mean_Alpha <- bind_rows(Mean_Alpha,mean_all_temp)
  rownames(Mean_Alpha) <- NULL
}# Dispe

# Beta diversity _______####
Mean_Beta <- data.frame()
for (dispe in 1:length(dispersal_distances)) {
  mean_all_temp <- data.frame()
  for (lost_hab in 1:(length(habitat_lost))) {
    Dataset_to_use <- final_out[[lost_hab]][[dispe]]
    Dataset_beta_position <- (10+nrow(Coord_EcoReg)):((10+nrow(Coord_EcoReg))+(nrow(Coord_EcoReg)-1))
    Dataset_equal_to_zero <- which(Dataset_to_use$Median[Dataset_beta_position]!=0)
    
    out <- data.frame("EcoR"=416,
                      "loss"=habitat_lost[lost_hab],
                      "disp"=dispersal_distances[dispe],
                      "pool"=spp_poool[dispe],
                      "B"=mean(Dataset_to_use$Median[Dataset_beta_position][Dataset_equal_to_zero]),
                      "B_UP"=mean(Dataset_to_use$out.IC.up[Dataset_beta_position][Dataset_equal_to_zero]),
                      "B_Low"=mean(Dataset_to_use$out.IC.inf[Dataset_beta_position][Dataset_equal_to_zero])
    )
    mean_all_temp <- bind_rows(mean_all_temp,out)
  }# Lost Habitat
  Mean_Beta <- bind_rows(Mean_Beta,mean_all_temp)
  rownames(Mean_Beta) <- NULL
}# Dispe

# Gamma diversity & Coef _______####
Mean_Rel_Gamma <- data.frame()
for (dispe in 1:length(dispersal_distances)) {
  mean_all_filt <- Mean_Alpha %>% filter(disp==dispersal_distances[dispe] , pool==spp_poool[dispe])
  meanB_all_filt <- Mean_Beta %>% filter(disp==dispersal_distances[dispe], pool==spp_poool[dispe])
  
  meanG_all_temp<- data.frame(
    "EcoR"=416,
    "loss"=mean_all_filt$loss,
    "disp"=dispersal_distances[dispe],
    "pool"=spp_poool[dispe],
    "Rel_G"=((mean_all_filt$S*(1+meanB_all_filt$B))/max((mean_all_filt$S*(1+meanB_all_filt$B))))*100,
    "Rel_G_UP"=((mean_all_filt$S_UP*(1+meanB_all_filt$B_UP))/max((mean_all_filt$S_UP*(1+meanB_all_filt$B_UP))))*100,
    "Rel_G_Low"=((mean_all_filt$S_Low*(1+meanB_all_filt$B_Low))/max((mean_all_filt$S_Low*(1+meanB_all_filt$B_Low))))*100
  )                             
  Mean_Rel_Gamma <- bind_rows(Mean_Rel_Gamma,meanG_all_temp)
} # Dispe

library(drc)
models_list <- matrix(nrow = nrow(Mean_Rel_Gamma))
for (dispersal in 1:length(dispersal_distances)) {
  IDs_dips <- which(Mean_Rel_Gamma$disp==dispersal_distances[dispersal] & Mean_Rel_Gamma$pool==spp_poool[dispersal])
  s <- Mean_Rel_Gamma[IDs_dips,5]
  f <- Mean_Rel_Gamma[IDs_dips,2]
  
  model_S <- nls(s~(a-b*f)-exp(f^q),start=list(a=s[1],b=0.1, q=1), control = list(maxiter=10000))
  
  p<-coefficients(model_S)
  Mean_Rel_Gamma[IDs_dips,8]<- p[2]
  Mean_Rel_Gamma[IDs_dips,9]<- p[3]
  Mean_Rel_Gamma[IDs_dips,10]<- predict(model_S)
  cat("We are at", dispersal_distances[dispersal], "and", p[2],"and", p[3], "\n")
}
colnames(Mean_Rel_Gamma)[8:10] <- c("Coef_b","Coef_q","Mod_Pred")



Mean_Rel_Gamma %>%
  mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
    disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
      disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
        disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
          disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
            disp==dispersal_distances[6], dispersal_distances_plot[6],
            dispersal_distances_plot[7]))))))) %>% 
  ggplot(aes(x=loss,y=Rel_G,color=as.factor(pool)))+
  geom_point(size=1.2)+
  geom_errorbar(aes(x=loss, ymax=Rel_G_UP,ymin=Rel_G_Low))+
  geom_line(aes(x=loss,colour=as.factor(pool), Mod_Pred))+
  scale_y_continuous(limits = c(0,c(max(Mean_Rel_Gamma$Rel_G)+1)))+
  scale_colour_viridis(discrete = T)+
  labs(title =416)+
  theme_classic()


Mean_Alpha %>% ggplot(aes(x=loss,y=S ,fill=as.factor(disp)))+geom_point(shape=21)
Mean_Beta %>% ggplot(aes(x=loss,y=B,fill=as.factor(disp)))+geom_point(shape=21)
Mean_Rel_Gamma %>%
  ggplot(aes(x=loss,y=Rel_G,color=as.factor(disp)))+
  geom_point(size=1.2)+
  geom_errorbar(aes(x=loss, ymax=Rel_G_UP,ymin=Rel_G_Low))+
  geom_line(aes(x=loss, Mod_Pred))+
  scale_color_viridis(discrete = T)+
  scale_y_continuous(limits = c(0,105))





