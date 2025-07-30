
# Charghe and depurate the original EU database from the 
source("BDD_PanEU/Upload_Depur_PanEU.R")

Water_colors <- c("white","#9191F0","#5E92F2","#6CB8A0")

# Both datasets must have the same length
nrow(Output_GRIDvsLAKES_TvsM_dep)
nrow(Centroides_coordenadas_dep)

EcoR_length <- c()
EcoReg_ID <- unique(Centroides_coordenadas_dep$EcoR_ID)
for (EcoRegion in 1:length(unique(Centroides_coordenadas_dep$EcoR_ID))) {
  EcoR_length[EcoRegion] <- nrow(Centroides_coordenadas_dep %>% filter(EcoR_ID==EcoReg_ID[EcoRegion]))
}

EcoR_size <- data.frame(EcoReg_ID,EcoR_length) %>%
  filter(EcoReg_ID %in%EcoReg_ID[-c((length(EcoReg_ID)-2):length(EcoReg_ID))])%>% 
  arrange(EcoR_length) 

EcoR_size_load <- EcoR_size 


# TOTAL FW ####
Outputs_SLURM <- list()
Outputs_SLURM_xyCoords <- list()
Outputs_SLURM_xyFWarea <- list()
for (ID_load_ecor in 1:length(EcoR_size_load$EcoReg_ID)) { 
  
  load(paste("RDatas_from_SLURM/","Out_test_", EcoR_size_load$EcoReg_ID[ID_load_ecor],".RData", sep=""))
  
  Outputs_SLURM[[ID_load_ecor]] <- final_out
  names(Outputs_SLURM[[ID_load_ecor]]) <- EcoR_size_load$EcoReg_ID[ID_load_ecor]
  Outputs_SLURM_xyCoords[[ID_load_ecor]] <- Coord_EcoReg
  Outputs_SLURM_xyFWarea[[ID_load_ecor]] <- FW_area
}

# Dispersal levels
dispersal_distances

dispersal_distances_plot <- c(paste(dispersal_distances[1],"km",sep = " "),
                              paste(dispersal_distances[2],"km",sep = " "),
                              paste(dispersal_distances[3],"km",sep = " "),
                              paste(dispersal_distances[4],"km",sep = " "),
                              paste(dispersal_distances[5],"km",sep = " "),
                              paste(dispersal_distances[6],"km",sep = " "),
                              paste(dispersal_distances[7],"km",sep = " "))
# Loss levels
habitat_lost <- habitat_lost*100

library(viridis)
globalEU <- data.frame()
Degradation_loss <- c("0% degradation","99% degradation")
Dispersers_name <- dispersal_distances_plot
for (ID_Ecoregion in 1:length(Outputs_SLURM)) {
  for (dispe in c(1:7)) {
    for (lost_hab in c(1,9)) {
  
  out <- data.frame("EcoR"=as.numeric(names(Outputs_SLURM[[ID_Ecoregion]])[1]),
                    "EcoR_name"=unique(Outputs_SLURM_xyCoords[[ID_Ecoregion]]$EcoR_Name),
                    "X"=Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_X,
                    "Y"=Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_Y,
                    "loss"=Degradation_loss[which(c(1,9)==lost_hab)],
                    "disp"=Dispersers_name[which(c(1:7)==dispe)],
                    "Alpha"=Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[10:(9+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))],
                    "Beta"=Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[(10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])):
                                                                            ((10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))+
                                                                               (nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])-1))]) 
  
  globalEU <- bind_rows(globalEU,out)
    }}}

model_data <- globalEU%>%
  mutate(disp=factor(disp,levels=Dispersers_name)) %>%
  pivot_longer(cols=7:8) %>%filter(value!=0) 

mod <- lmerTest::lmer(sqrt(value)~disp*loss+(1|EcoR_name), data=model_data %>% filter(name=="Alpha"))
anova(mod)
emmeans::emmeans(mod, list(pairwise ~ disp|loss), adjust = "Scheffe",pbkrtest.limit = 883190,lmerTest.limit = 883190)

mod <- glmmTMB::glmmTMB(value~disp*loss+(1|EcoR_name), data=model_data %>% filter(name=="Beta"),family = binomial(link = "logit"))
emmeans::emmeans(mod, list(pairwise ~ disp|loss), adjust = "Scheffe",lmerTest.limit = 999999)
summary(mod)


png(filename ="FW_AlphaBetaGamma.png",
    width =608*4 ,height =450*4 ,units ="px",res = 300)
globalEU%>%
  mutate(disp=factor(disp,levels=Dispersers_name)) %>% 
  pivot_longer(cols=7:8) %>% 
  filter(value!=0) %>% 
  group_by(EcoR_name,loss,disp,name) %>% 
  summarise(value=mean(value)) %>% 
  group_by(loss,disp,name) %>% 
  mutate(m_value=mean(value),vl_sd=sd(value)) %>% 
  ggplot() + 
  geom_vline(aes(xintercept=as.factor(disp)),colour="grey60",alpha=0.3)+
  geom_jitter(aes(y=value,x=as.factor(disp)), colour="grey30",alpha=0.2,width = 0.1)+
  geom_errorbar(aes(x=as.factor(disp),ymax=m_value+vl_sd,ymin=m_value-vl_sd,colour=as.factor(disp)))+
  geom_point(aes(y=m_value,x=as.factor(disp),colour=as.factor(disp)),size=4,shape=18)+
  scale_colour_viridis(discrete = T,option = "D")+
    facet_wrap(name~loss,scales="free_y",ncol=2)+
  labs(y="",x="")+
  theme_classic()+
  theme(legend.position = "none",
        plot.title = element_text(size=15,face = "bold",color="black"),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1),
        plot.background =element_rect(fill=Water_colors[1]))
dev.off()


FW_area <- Outputs_SLURM_xyFWarea[[23]]
area.max.europa<-max(Outputs_SLURM_xyFWarea[[23]])
Jmin <- 400*0.02
J.max<-400+Jmin
b.ef<-0.5 
Perc_Hab_Loss <- 0.99
FW_area_lost <- FW_area-(Perc_Hab_Loss*FW_area)
J.freshwater<-ceiling((-Jmin+(J.max/(area.max.europa^b.ef))*FW_area_lost^b.ef))
J.freshwater <- ifelse(J.freshwater<=0,0,J.freshwater)

png(filename ="416_Area_99.png",
    width =608*3.5 ,height =550*3.5 ,units ="px",res = 300)
data.frame("EcoR"=as.numeric(names(Outputs_SLURM[[23]])[1]),
           "EcoR_name"=unique(Outputs_SLURM_xyCoords[[23]]$EcoR_Name),
           "X"=Outputs_SLURM_xyCoords[[23]]$CENTROID_X,
           "Y"=Outputs_SLURM_xyCoords[[23]]$CENTROID_Y,
           "Area"=J.freshwater) %>% 
  ggplot()+
  geom_point(shape=22,size=1, aes(x=X, y=Y,fill=Area, colour=Area))+
  scale_fill_viridis(option = "A", name="FW Area",limits=c(0.00000000001,max(J.freshwater)))+
  scale_color_viridis(option = "A", name="FW Area",limits=c(0.00000000001,max(J.freshwater)))+
  labs(title = "Surface water")+xlab("")+ylab("")+
  theme_classic()+
  theme(panel.background = element_rect(fill = 'white', color = 'black'), 
        legend.position = "none",
        axis.text =element_blank(),
        axis.ticks = element_blank(),
        plot.background =element_rect(fill=Water_colors[1]))  
dev.off()


# Permanent FW ####
Outputs_SLURM <- list()
Outputs_SLURM_xyCoords <- list()
Outputs_SLURM_xyFWarea <- list()
for (ID_load_ecor in 1:length(EcoR_size_load$EcoReg_ID)) { 
  
  load(paste("RDatas_from_SLURM/","Out_test_", EcoR_size_load$EcoReg_ID[ID_load_ecor],".RData", sep=""))
  
  Outputs_SLURM[[ID_load_ecor]] <- final_out_perm
  names(Outputs_SLURM[[ID_load_ecor]]) <- EcoR_size_load$EcoReg_ID[ID_load_ecor]
  Outputs_SLURM_xyCoords[[ID_load_ecor]] <- Coord_EcoReg
  Outputs_SLURM_xyFWarea[[ID_load_ecor]] <- FW_area_perm
}
# Dispersal levels
dispersal_distances

dispersal_distances_plot <- c(paste(dispersal_distances[1],"km",sep = " "),
                              paste(dispersal_distances[2],"km",sep = " "),
                              paste(dispersal_distances[3],"km",sep = " "),
                              paste(dispersal_distances[4],"km",sep = " "),
                              paste(dispersal_distances[5],"km",sep = " "),
                              paste(dispersal_distances[6],"km",sep = " "),
                              paste(dispersal_distances[7],"km",sep = " "))
# Loss levels
habitat_lost <- habitat_lost*100


globalEU <- data.frame()
Degradation_loss <- c("0% degradation","99% degradation")
Dispersers_name <- dispersal_distances_plot
for (ID_Ecoregion in 1:length(Outputs_SLURM)) {
  for (dispe in c(1:7)) {
    for (lost_hab in c(1,9)) {
      
      out <- data.frame("EcoR"=as.numeric(names(Outputs_SLURM[[ID_Ecoregion]])[1]),
                        "EcoR_name"=unique(Outputs_SLURM_xyCoords[[ID_Ecoregion]]$EcoR_Name),
                        "X"=Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_X,
                        "Y"=Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_Y,
                        "loss"=Degradation_loss[which(c(1,9)==lost_hab)],
                        "disp"=Dispersers_name[which(c(1:7)==dispe)],
                        "Alpha"=Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[10:(9+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))],
                        "Beta"=Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[(10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])):
                                                                                           ((10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))+
                                                                                              (nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])-1))]) 
      
      globalEU <- bind_rows(globalEU,out)
    }}}

model_data <- globalEU%>%
  mutate(Beta=ifelse(Beta>1,1,Beta)) %>%
  mutate(disp=factor(disp,levels=Dispersers_name)) %>%
  pivot_longer(cols=7:8) %>%filter(value!=0) 

mod <- lmerTest::lmer(sqrt(value)~disp*loss+(1|EcoR_name), data=model_data %>% filter(name=="Alpha"))
anova(mod)
emmeans::emmeans(mod, list(pairwise ~ disp|loss), adjust = "Scheffe")

mod <- glmmTMB::glmmTMB(value~disp*loss+(1|EcoR_name), data=model_data %>% filter(name=="Beta"),family = binomial(link = "logit"))
emmeans::emmeans(mod, list(pairwise ~ disp|loss), adjust = "Scheffe")
summary(mod)


png(filename ="PermFW_AlphaBetaGamma.png",
    width =608*4 ,height =450*4 ,units ="px",res = 300)
globalEU%>%
  mutate(disp=factor(disp,levels=Dispersers_name)) %>% 
  mutate(Beta=ifelse(Beta>1,1,Beta)) %>%
  pivot_longer(cols=7:8) %>% 
  filter(value!=0) %>%
  group_by(EcoR_name,loss,disp,name) %>% 
  summarise(value=mean(value)) %>% 
  group_by(loss,disp,name) %>% 
  mutate(m_value=mean(value),vl_sd=sd(value)) %>% 
  ggplot() + 
  geom_vline(aes(xintercept=as.factor(disp)),colour="grey60",alpha=0.3)+
  geom_jitter(aes(y=value,x=as.factor(disp)), colour="grey30",alpha=0.2,width = 0.1)+
  geom_errorbar(aes(x=as.factor(disp),ymax=m_value+vl_sd,ymin=m_value-vl_sd,colour=as.factor(disp)))+
  geom_point(aes(y=m_value,x=as.factor(disp),colour=as.factor(disp)),size=4,shape=18)+
  scale_colour_viridis(discrete = T,option = "D")+
  facet_wrap(name~loss,scales="free_y",ncol=2)+
  labs(y="",x="")+
  theme_classic()+
  theme(legend.position = "none",
        plot.title = element_text(size=15,face = "bold",color="black"),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1),
        plot.background =element_rect(fill=Water_colors[2]))
dev.off()


FW_area <- Outputs_SLURM_xyFWarea[[23]]
area.max.europa<-max(Outputs_SLURM_xyFWarea[[23]])
Jmin <- 400*0.02
J.max<-400+Jmin
b.ef<-0.5 
Perc_Hab_Loss <- 0
FW_area_lost <- FW_area-(Perc_Hab_Loss*FW_area)
J.freshwater<-ceiling((-Jmin+(J.max/(area.max.europa^b.ef))*FW_area_lost^b.ef))
J.freshwater <- ifelse(J.freshwater<=0,0,J.freshwater)

png(filename ="Perm416_Area_0.png",
    width =608*3.5 ,height =550*3.5 ,units ="px",res = 300)
data.frame("EcoR"=as.numeric(names(Outputs_SLURM[[23]])[1]),
           "EcoR_name"=unique(Outputs_SLURM_xyCoords[[23]]$EcoR_Name),
           "X"=Outputs_SLURM_xyCoords[[23]]$CENTROID_X,
           "Y"=Outputs_SLURM_xyCoords[[23]]$CENTROID_Y,
           "Area"=J.freshwater) %>% 
  ggplot()+
  geom_point(shape=22,size=1, aes(x=X, y=Y,fill=Area, colour=Area))+
  scale_fill_viridis(option = "A", name="FW Area",limits=c(0.00000000001,max(J.freshwater)))+
  scale_color_viridis(option = "A", name="FW Area",limits=c(0.00000000001,max(J.freshwater)))+
  labs(title = "Surface water")+xlab("")+ylab("")+
  theme_classic()+
  theme(panel.background = element_rect(fill = 'white', color = 'black'), 
        legend.position = "none",
        axis.text =element_blank(),
        axis.ticks = element_blank(),
        plot.background =element_rect(fill=Water_colors[2]))  
dev.off()


# Temporal FW ####
Outputs_SLURM <- list()
Outputs_SLURM_xyCoords <- list()
Outputs_SLURM_xyFWarea <- list()
for (ID_load_ecor in 1:length(EcoR_size_load$EcoReg_ID)) { 
  
  load(paste("RDatas_from_SLURM/","Out_test_", EcoR_size_load$EcoReg_ID[ID_load_ecor],".RData", sep=""))
  
  Outputs_SLURM[[ID_load_ecor]] <- final_out_temp
  names(Outputs_SLURM[[ID_load_ecor]]) <- EcoR_size_load$EcoReg_ID[ID_load_ecor]
  Outputs_SLURM_xyCoords[[ID_load_ecor]] <- Coord_EcoReg
  Outputs_SLURM_xyFWarea[[ID_load_ecor]] <- FW_area_temp
}

# Dispersal levels
dispersal_distances

dispersal_distances_plot <- c(paste(dispersal_distances[1],"km",sep = " "),
                              paste(dispersal_distances[2],"km",sep = " "),
                              paste(dispersal_distances[3],"km",sep = " "),
                              paste(dispersal_distances[4],"km",sep = " "),
                              paste(dispersal_distances[5],"km",sep = " "),
                              paste(dispersal_distances[6],"km",sep = " "),
                              paste(dispersal_distances[7],"km",sep = " "))
# Loss levels
habitat_lost <- habitat_lost*100

library(viridis)
globalEU <- data.frame()
Degradation_loss <- c("0% degradation","99% degradation")
Dispersers_name <- dispersal_distances_plot
for (ID_Ecoregion in 1:length(Outputs_SLURM)) {
  for (dispe in c(1:7)) {
    for (lost_hab in c(1,9)) {
      
      out <- data.frame("EcoR"=as.numeric(names(Outputs_SLURM[[ID_Ecoregion]])[1]),
                        "EcoR_name"=unique(Outputs_SLURM_xyCoords[[ID_Ecoregion]]$EcoR_Name),
                        "X"=Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_X,
                        "Y"=Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_Y,
                        "loss"=Degradation_loss[which(c(1,9)==lost_hab)],
                        "disp"=Dispersers_name[which(c(1:7)==dispe)],
                        "Alpha"=Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[10:(9+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))],
                        "Beta"=Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[(10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])):
                                                                                           ((10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))+
                                                                                              (nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])-1))]) 
      
      globalEU <- bind_rows(globalEU,out)
    }}}

model_data <- globalEU%>%
  mutate(disp=factor(disp,levels=Dispersers_name)) %>%
  pivot_longer(cols=7:8) %>%filter(value!=0) 

mod <- lmerTest::lmer(sqrt(value)~disp*loss+(1|EcoR_name), data=model_data %>% filter(name=="Alpha"))
anova(mod)
emmeans::emmeans(mod, list(pairwise ~ disp|loss), adjust = "Scheffe")

mod <- glmmTMB::glmmTMB(value~disp*loss+(1|EcoR_name), data=model_data %>% filter(name=="Beta"),family = binomial(link = "logit"))
emmeans::emmeans(mod, list(pairwise ~ disp|loss), adjust = "Scheffe")
summary(mod)


png(filename ="Temp_AlphaBetaGamma.png",
    width =608*4 ,height =450*4 ,units ="px",res = 300)
globalEU%>%
  mutate(disp=factor(disp,levels=Dispersers_name)) %>% 
  pivot_longer(cols=7:8) %>% 
  filter(value!=0) %>% 
  group_by(EcoR_name,loss,disp,name) %>% 
  summarise(value=mean(value)) %>% 
  group_by(loss,disp,name) %>% 
  mutate(m_value=mean(value),vl_sd=sd(value)) %>% 
  ggplot() + 
  geom_vline(aes(xintercept=as.factor(disp)),colour="grey60",alpha=0.3)+
  geom_jitter(aes(y=value,x=as.factor(disp)), colour="grey30",alpha=0.2,width = 0.1)+
  geom_errorbar(aes(x=as.factor(disp),ymax=m_value+vl_sd,ymin=m_value-vl_sd,colour=as.factor(disp)))+
  geom_point(aes(y=m_value,x=as.factor(disp),colour=as.factor(disp)),size=4,shape=18)+
  scale_colour_viridis(discrete = T,option = "D")+
  facet_wrap(name~loss,scales="free_y",ncol=2)+
  labs(y="",x="")+
  theme_classic()+
  theme(legend.position = "none",
        plot.title = element_text(size=15,face = "bold",color="black"),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1),
        plot.background =element_rect(fill=Water_colors[3]))
dev.off()


FW_area <- Outputs_SLURM_xyFWarea[[23]]
area.max.europa<-max(Outputs_SLURM_xyFWarea[[23]])
Jmin <- 400*0.02
J.max<-400+Jmin
b.ef<-0.5 
Perc_Hab_Loss <- 0
FW_area_lost <- FW_area-(Perc_Hab_Loss*FW_area)
J.freshwater<-ceiling((-Jmin+(J.max/(area.max.europa^b.ef))*FW_area_lost^b.ef))
J.freshwater <- ifelse(J.freshwater<=0,0,J.freshwater)

png(filename ="Temp_416_Area_0.png",
    width =608*3.5 ,height =550*3.5 ,units ="px",res = 300)
data.frame("EcoR"=as.numeric(names(Outputs_SLURM[[23]])[1]),
           "EcoR_name"=unique(Outputs_SLURM_xyCoords[[23]]$EcoR_Name),
           "X"=Outputs_SLURM_xyCoords[[23]]$CENTROID_X,
           "Y"=Outputs_SLURM_xyCoords[[23]]$CENTROID_Y,
           "Area"=J.freshwater) %>% 
  ggplot()+
  geom_point(shape=22,size=1, aes(x=X, y=Y,fill=Area, colour=Area))+
  scale_fill_viridis(option = "A", name="FW Area",limits=c(0.00000000001,max(J.freshwater)))+
  scale_color_viridis(option = "A", name="FW Area",limits=c(0.00000000001,max(J.freshwater)))+
  labs(title = "Surface water")+xlab("")+ylab("")+
  theme_classic()+
  theme(panel.background = element_rect(fill = 'white', color = 'black'), 
        legend.position = "none",
        axis.text =element_blank(),
        axis.ticks = element_blank(),
        plot.background =element_rect(fill=Water_colors[3]))  
dev.off()


# Ephemeral FW ####
Outputs_SLURM <- list()
Outputs_SLURM_xyCoords <- list()
Outputs_SLURM_xyFWarea <- list()
for (ID_load_ecor in 1:length(EcoR_size_load$EcoReg_ID)) { 
  
  load(paste("RDatas_from_SLURM/","Out_test_", EcoR_size_load$EcoReg_ID[ID_load_ecor],".RData", sep=""))
  
  Outputs_SLURM[[ID_load_ecor]] <- final_out_ephe
  names(Outputs_SLURM[[ID_load_ecor]]) <- EcoR_size_load$EcoReg_ID[ID_load_ecor]
  Outputs_SLURM_xyCoords[[ID_load_ecor]] <- Coord_EcoReg
  Outputs_SLURM_xyFWarea[[ID_load_ecor]] <- FW_area_ephe
}

# Dispersal levels
dispersal_distances

dispersal_distances_plot <- c(paste(dispersal_distances[1],"km",sep = " "),
                              paste(dispersal_distances[2],"km",sep = " "),
                              paste(dispersal_distances[3],"km",sep = " "),
                              paste(dispersal_distances[4],"km",sep = " "),
                              paste(dispersal_distances[5],"km",sep = " "),
                              paste(dispersal_distances[6],"km",sep = " "),
                              paste(dispersal_distances[7],"km",sep = " "))
# Loss levels
habitat_lost <- habitat_lost*100

library(viridis)
globalEU <- data.frame()
Degradation_loss <- c("0% degradation","99% degradation")
Dispersers_name <- dispersal_distances_plot
for (ID_Ecoregion in 1:length(Outputs_SLURM)) {
  for (dispe in c(1:7)) {
    for (lost_hab in c(1,9)) {
      
      out <- data.frame("EcoR"=as.numeric(names(Outputs_SLURM[[ID_Ecoregion]])[1]),
                        "EcoR_name"=unique(Outputs_SLURM_xyCoords[[ID_Ecoregion]]$EcoR_Name),
                        "X"=Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_X,
                        "Y"=Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_Y,
                        "loss"=Degradation_loss[which(c(1,9)==lost_hab)],
                        "disp"=Dispersers_name[which(c(1:7)==dispe)],
                        "Alpha"=Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[10:(9+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))],
                        "Beta"=Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[(10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])):
                                                                                           ((10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))+
                                                                                              (nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])-1))]) 
      
      globalEU <- bind_rows(globalEU,out)
    }}}

model_data <- globalEU%>%
  mutate(disp=factor(disp,levels=Dispersers_name)) %>%
  pivot_longer(cols=7:8) %>%filter(value!=0) 

mod <- lmerTest::lmer(sqrt(value)~disp*loss+(1|EcoR_name), data=model_data %>% filter(name=="Alpha"))
anova(mod)
emmeans::emmeans(mod, list(pairwise ~ disp|loss), adjust = "Scheffe")

mod <- glmmTMB::glmmTMB(value~disp*loss+(1|EcoR_name), data=model_data %>% filter(name=="Beta"),family = binomial(link = "logit"))
emmeans::emmeans(mod, list(pairwise ~ disp|loss), adjust = "Scheffe")
summary(mod)


png(filename ="Ephe_AlphaBetaGamma.png",
    width =608*4 ,height =450*4 ,units ="px",res = 300)
globalEU%>%
  mutate(disp=factor(disp,levels=Dispersers_name)) %>% 
  pivot_longer(cols=7:8) %>% 
  filter(value!=0) %>% 
  group_by(EcoR_name,loss,disp,name) %>% 
  summarise(value=mean(value)) %>% 
  group_by(loss,disp,name) %>% 
  mutate(m_value=mean(value),vl_sd=sd(value)) %>% 
  ggplot() + 
  geom_vline(aes(xintercept=as.factor(disp)),colour="grey60",alpha=0.3)+
  geom_jitter(aes(y=value,x=as.factor(disp)), colour="grey30",alpha=0.2,width = 0.1)+
  geom_errorbar(aes(x=as.factor(disp),ymax=m_value+vl_sd,ymin=m_value-vl_sd,colour=as.factor(disp)))+
  geom_point(aes(y=m_value,x=as.factor(disp),colour=as.factor(disp)),size=4,shape=18)+
  scale_colour_viridis(discrete = T,option = "D")+
  facet_wrap(name~loss,scales="free_y",ncol=2)+
  labs(y="",x="")+
  theme_classic()+
  theme(legend.position = "none",
        plot.title = element_text(size=15,face = "bold",color="black"),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1),
        plot.background =element_rect(fill=Water_colors[4]))
dev.off()


FW_area <- Outputs_SLURM_xyFWarea[[23]]
area.max.europa<-max(Outputs_SLURM_xyFWarea[[23]])
Jmin <- 400*0.02
J.max<-400+Jmin
b.ef<-0.5 
Perc_Hab_Loss <- 0.99
FW_area_lost <- FW_area-(Perc_Hab_Loss*FW_area)
J.freshwater<-ceiling((-Jmin+(J.max/(area.max.europa^b.ef))*FW_area_lost^b.ef))
J.freshwater <- ifelse(J.freshwater<=0,0,J.freshwater)

png(filename ="Ephe_416_Area_99.png",
    width =608*3.5 ,height =550*3.5 ,units ="px",res = 300)
data.frame("EcoR"=as.numeric(names(Outputs_SLURM[[23]])[1]),
           "EcoR_name"=unique(Outputs_SLURM_xyCoords[[23]]$EcoR_Name),
           "X"=Outputs_SLURM_xyCoords[[23]]$CENTROID_X,
           "Y"=Outputs_SLURM_xyCoords[[23]]$CENTROID_Y,
           "Area"=J.freshwater) %>% 
  ggplot()+
  geom_point(shape=22,size=1, aes(x=X, y=Y,fill=Area, colour=Area))+
  scale_fill_viridis(option = "A", name="FW Area",limits=c(0.00000000001,max(J.freshwater)))+
  scale_color_viridis(option = "A", name="FW Area",limits=c(0.00000000001,max(J.freshwater)))+
  labs(title = "Surface water")+xlab("")+ylab("")+
  theme_classic()+
  theme(panel.background = element_rect(fill = 'white', color = 'black'), 
        legend.position = "none",
        axis.text =element_blank(),
        axis.ticks = element_blank(),
        plot.background =element_rect(fill=Water_colors[4]))  
dev.off()

