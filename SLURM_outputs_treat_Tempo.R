
# We set the working direcory 
setwd("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation")
# Nice colours? CUNILLERA_palette is what you need
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/CUNILLERA_palette.R")
# Charghe and depurate the original EU database from the 
source("BDD_PanEU/Upload_Depur_PanEU.R")
setwd("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation")
# Charge the function to run the coalescent model
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/H2020_Lattice_expKernel_Jenv_TempMeta.R")

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

EcoR_size_load <- EcoR_size %>% filter(EcoReg_ID!=406)%>% filter(EcoReg_ID!=404)%>% filter(EcoReg_ID!=407)

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

# Alpha diversity
mean_all <- data.frame()
for (ID_Ecoregion in 1:length(Outputs_SLURM)) {
  for (dispe in 1:length(dispersal_distances)) {
    mean_all_temp <- data.frame()
    for (lost_hab in 1:(length(habitat_lost))) {
      out <- c(as.numeric(names(Outputs_SLURM[[ID_Ecoregion]])[1]),
               habitat_lost[lost_hab],
               dispersal_distances[dispe],
               mean(Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[10:(9+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))]),
               mean(Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$out.IC.up[10:(9+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))]),
               mean(Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$out.IC.inf[10:(9+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))])
      )
      out <- data.frame("EcoR"=out[1],"loss"=out[2],"disp"=out[3], "Mean_S"=out[4],"Mean_UP"=out[5],"Mean_Low"=out[6])
      mean_all_temp <- bind_rows(mean_all_temp,out)
    }
    mean_all <- bind_rows(mean_all,mean_all_temp)
  }
}

# Beta diversity
meanB_all <- data.frame()
for (ID_Ecoregion in 1:length(Outputs_SLURM)) {
  for (dispe in 1:length(dispersal_distances)) {
    for (lost_hab in 1:(length(habitat_lost))) {
      
      mean_Median <- Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[(10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])):
                                                                                 ((10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))+
                                                                                    (nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])-1))]
      mean_UP <- Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$out.IC.up[(10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])):
                                                                                ((10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))+
                                                                                   (nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])-1))]
      mean_LOW <- Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$out.IC.inf[(10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])):
                                                                                  ((10+nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]]))+
                                                                                     (nrow(Outputs_SLURM_xyCoords[[ID_Ecoregion]])-1))]
      
      out <- c(as.numeric(names(Outputs_SLURM[[ID_Ecoregion]])[1]),
               habitat_lost[lost_hab],
               dispersal_distances[dispe],
               mean(mean_Median[-which(mean_Median==0)]),
               mean(mean_UP[-which(mean_UP==0)]),
               mean(mean_LOW[-which(mean_LOW==0)])
               
      )
      out <- data.frame("EcoR"=out[1],"loss"=out[2],"disp"=out[3], "B"=out[4],"Mean_UP"=out[5],"Mean_Low"=out[6])
      meanB_all <- bind_rows(meanB_all,out)
    }
  }
}


color <- viridis(length(dispersal_distances))
plot_S_total <- list()
for (ID_Ecoregion in 1:length(Outputs_SLURM)) {
  
  mean_all_filt <- mean_all %>% filter(EcoR==names(Outputs_SLURM[[ID_Ecoregion]])[1])  
  meanB_all_filt <- meanB_all %>% filter(EcoR==names(Outputs_SLURM[[ID_Ecoregion]])[1])
  
  plot_S_total[[ID_Ecoregion]] <- gridExtra::arrangeGrob(
    
    ggplot(data.frame(cbind(Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_X/max(Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_X),
                            Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_Y/max(Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_Y))),aes(x=X1, y=X2))+
      geom_point(shape=21,colour="grey90", size=2, aes(fill=Outputs_SLURM_xyFWarea[[ID_Ecoregion]]))+
      scale_fill_CUNILLERA(palette = "estelada", discrete = F, reverse = T, name="FW Area",
                           limits=c(0.00000000001,max(Outputs_SLURM_xyFWarea[[ID_Ecoregion]])))+
      labs(title = "FW area")+theme_classic()+theme(panel.background = element_rect(fill = 'grey15', color = 'black')),  
    
    mean_all_filt %>% #mutate(Mod_Pred=model_list_EcoR[[ID_Ecoregion]] ) %>% 
      mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
        disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
          disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
            disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
              disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
                disp==dispersal_distances[6], dispersal_distances_plot[6],
                dispersal_distances_plot[7]))))))) %>% 
      ggplot(aes(x=loss, y=Mean_S, colour=disp_plot))+
      geom_point(size=2)+
      geom_errorbar(aes(x=loss, ymax=Mean_UP,ymin=Mean_Low))+
      scale_colour_manual(values = color, breaks=dispersal_distances_plot)+
      labs(title =names(Outputs_SLURM[[ID_Ecoregion]])[1])+
      theme_classic(),
    
    meanB_all_filt %>% #mutate(Mod_Pred=model_list_EcoR[[ID_Ecoregion]] ) %>% 
      mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
        disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
          disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
            disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
              disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
                disp==dispersal_distances[6], dispersal_distances_plot[6],
                dispersal_distances_plot[7]))))))) %>% 
      ggplot(aes(x=loss, y=B, colour=disp_plot))+
      geom_point(size=2)+
      geom_errorbar(aes(x=loss, ymax=Mean_UP,ymin=Mean_Low))+
      scale_colour_manual(values = color, breaks=dispersal_distances_plot)+
      labs(title =names(Outputs_SLURM[[ID_Ecoregion]])[1])+
      theme_classic(),
    
    ncol = 3)
}

png(filename ="Loss_Alph_Bet_Tempo.png",
    width =500*6 ,height =1800*7 ,units ="px",res = 300)
gridExtra::grid.arrange(plot_S_total[[1]],plot_S_total[[2]],plot_S_total[[3]],
                        plot_S_total[[4]],plot_S_total[[5]],plot_S_total[[6]],
                        plot_S_total[[7]],plot_S_total[[8]],plot_S_total[[9]],
                        plot_S_total[[10]],plot_S_total[[11]],plot_S_total[[12]],
                        plot_S_total[[13]],plot_S_total[[14]],plot_S_total[[15]],
                        plot_S_total[[16]],plot_S_total[[17]],plot_S_total[[18]],
                        plot_S_total[[19]],plot_S_total[[20]],plot_S_total[[21]],
                        plot_S_total[[22]],plot_S_total[[23]],plot_S_total[[24]],
                        plot_S_total[[25]],plot_S_total[[26]],plot_S_total[[27]],
                        plot_S_total[[28]],plot_S_total[[29]],plot_S_total[[30]],
                        plot_S_total[[31]],plot_S_total[[32]],plot_S_total[[33]],
                        ncol=1)
dev.off()


# Gamma diversity
meanG_all <- data.frame()
for (ID_Ecoregion in 1:length(Outputs_SLURM)) {
  for (dispe in 1:length(dispersal_distances)) {
    meanG_all_temp <- data.frame()
    
    mean_all_filt <- mean_all %>% filter(EcoR==names(Outputs_SLURM[[ID_Ecoregion]])[1]) %>% 
      filter(disp==dispersal_distances[dispe])
    meanB_all_filt <- meanB_all %>% filter(EcoR==names(Outputs_SLURM[[ID_Ecoregion]])[1]) %>% 
      filter(disp==dispersal_distances[dispe])
    
    for (lost_hab in 1:(length(habitat_lost))) {
      
      gamma_Postitoin <- length(Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median)
      
      out <- c(as.numeric(names(Outputs_SLURM[[ID_Ecoregion]])[1]),
               habitat_lost[lost_hab],
               dispersal_distances[dispe],
               Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$Median[(gamma_Postitoin-5):gamma_Postitoin][2],
               Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$out.IC.up[(gamma_Postitoin-5):gamma_Postitoin][2],
               Outputs_SLURM[[ID_Ecoregion]][[lost_hab]][[dispe]]$out.IC.inf[(gamma_Postitoin-5):gamma_Postitoin][2]
      )
      out <- data.frame("EcoR"=out[1],"loss"=out[2],"disp"=out[3], "G"=out[4],"Mean_UP"=out[5],"Mean_Low"=out[6])
      meanG_all_temp <- bind_rows(meanG_all_temp,out)
      
      meanG_all_temp$G <- (meanG_all_temp$G/max(meanG_all_temp$G))*100
      meanG_all_temp$Mean_UP <- (meanG_all_temp$Mean_UP/max(meanG_all_temp$Mean_UP))*100
      meanG_all_temp$Mean_Low <- (meanG_all_temp$Mean_Low/max(meanG_all_temp$Mean_Low))*100
      
    }
    
    meanG_all_temp<- meanG_all_temp %>% 
      mutate(New_G=((mean_all_filt$Mean_S*(1+meanB_all_filt$B))/max((mean_all_filt$Mean_S*(1+meanB_all_filt$B))))*100)%>% 
      mutate(New_G_UP=((mean_all_filt$Mean_UP*(1+meanB_all_filt$Mean_UP))/max((mean_all_filt$Mean_UP*(1+meanB_all_filt$Mean_UP))))*100)%>%
      mutate(New_G_Low=((mean_all_filt$Mean_Low*(1+meanB_all_filt$Mean_Low))/max((mean_all_filt$Mean_Low*(1+meanB_all_filt$Mean_Low))))*100)                             
    
    meanG_all <- bind_rows(meanG_all,meanG_all_temp)
  }
}


library(drc)
model_list_EcoR <- list()
out_coef_b_list <- list()
out_coef_f0_list <- list()
out_x50_list <- list()
for (ID_Ecoregion in 1:length(Outputs_SLURM)) {
  meanG_all_filt <- meanG_all %>% filter(EcoR==names(Outputs_SLURM[[ID_Ecoregion]])[1])
  models_list <- matrix(nrow = nrow(meanG_all_filt))
  out_coef_f0<- c()
  out_coef_b <- c()
  for (dispersal in 1:length(dispersal_distances)) {
    IDs_dips <- which(meanG_all_filt$disp==dispersal_distances[dispersal])
    s <- meanG_all_filt[IDs_dips,7]
    f <- meanG_all_filt[IDs_dips,2]
    
    model_S <- nls(s~(a-b*f)-exp((f^q)),start=list(a=s[1],b=0.0001, q=0.5), control = list(maxiter=10000))
    #model_S <- nls(s~(a-b*f),start=list(a=s[1],b=0.0001), control = list(maxiter=10000))
    model_f <- nls(f~(a-b*s)-exp((s^q)),start=list(a=s[1],b=0.0001, q=0.5), control = list(maxiter=10000))
    
    p<-coefficients(model_S)
    out_coef_b[dispersal] <- p[2]
    out_coef_f0[dispersal] <- predict(model_f, newdata = data.frame(s=c(50)))
    
    models_list[IDs_dips,] <- predict(model_S)
  }
  model_list_EcoR[[ID_Ecoregion]] <- models_list
  out_coef_b_list[[ID_Ecoregion]] <- out_coef_b
  out_coef_f0_list[[ID_Ecoregion]] <- out_coef_f0
}


color <- viridis(length(dispersal_distances))
plot_G_total <- list()
for (ID_Ecoregion in 1:length(Outputs_SLURM)){
  
  meanG_all_filt <- meanG_all %>% filter(EcoR==names(Outputs_SLURM[[ID_Ecoregion]])[1])
  
  plot_G_total[[ID_Ecoregion]] <- gridExtra::arrangeGrob(
    meanG_all_filt %>% mutate(Mod_Pred=model_list_EcoR[[ID_Ecoregion]] ) %>% 
      mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
        disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
          disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
            disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
              disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
                disp==dispersal_distances[6], dispersal_distances_plot[6],
                dispersal_distances_plot[7]))))))) %>% 
      ggplot(aes(x=loss, y=New_G, colour=disp_plot))+
      geom_point(size=1.2)+
      geom_errorbar(aes(x=loss, ymax=New_G_UP,ymin=New_G_Low))+
      geom_line(aes(x=loss, Mod_Pred))+
      geom_vline(xintercept =out_coef_f0_list[[ID_Ecoregion]],colour=color, size=1, alpha=0.5, linetype=2)+
      scale_y_continuous(limits = c(0,c(max(meanG_all$New_G)+1)))+
      scale_colour_manual(values = color, breaks=dispersal_distances_plot)+
      labs(caption =paste(".001=", as.character(round(out_coef_f0_list[[ID_Ecoregion]],0))[1],
                          ".1=", as.character(round(out_coef_f0_list[[ID_Ecoregion]],0))[2],
                          ".5=", as.character(round(out_coef_f0_list[[ID_Ecoregion]],0))[3],
                          "1=", as.character(round(out_coef_f0_list[[ID_Ecoregion]],0))[4],
                          "2=", as.character(round(out_coef_f0_list[[ID_Ecoregion]],0))[5],
                          "5=", as.character(round(out_coef_f0_list[[ID_Ecoregion]],0))[6],
                          "10=", as.character(round(out_coef_f0_list[[ID_Ecoregion]],0))[7],
                          sep="-"),
           title =names(Outputs_SLURM[[ID_Ecoregion]])[1])+
      theme_classic(),
    
    ncol = 1)
}

png(filename ="Loss_Gamm_Tempo.png",
    width =900*8 ,height =700*8 ,units ="px",res = 300)
gridExtra::grid.arrange(plot_G_total[[1]],plot_G_total[[2]],plot_G_total[[3]],
                        plot_G_total[[4]],plot_G_total[[5]],plot_G_total[[6]],
                        plot_G_total[[7]],plot_G_total[[8]],plot_G_total[[9]],
                        plot_G_total[[10]],plot_G_total[[11]],plot_G_total[[12]],
                        plot_G_total[[13]],plot_G_total[[14]],plot_G_total[[15]],
                        plot_G_total[[16]],plot_G_total[[17]],plot_G_total[[18]],
                        plot_G_total[[19]],plot_G_total[[20]],plot_G_total[[21]],
                        plot_G_total[[22]],plot_G_total[[23]],plot_G_total[[24]],
                        plot_G_total[[25]],plot_G_total[[26]],plot_G_total[[27]],
                        plot_G_total[[28]],plot_G_total[[29]],plot_G_total[[30]],
                        plot_G_total[[31]],plot_G_total[[32]],plot_G_total[[33]],
                        ncol=4)
dev.off()


plot_amazing <- list()
for (dispers in 1:length(dispersal_distances)) {
  data_coef_all <- data.frame()
  for (ID_Ecoregion in c(1:length(Outputs_SLURM))){
    out <- data.frame("EcoR"=as.numeric(names(Outputs_SLURM[[ID_Ecoregion]])[1]),
                      "disp"=dispersal_distances_plot[dispers],
                      "disp_plot"=dispersal_distances[dispers],
                      "coef_b"=out_coef_b_list[[ID_Ecoregion]][dispers],
                      "coef_f0"=out_coef_f0_list[[ID_Ecoregion]][dispers],
                      "CENTROID_X"=Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_X,      
                      "CENTROID_Y"=Outputs_SLURM_xyCoords[[ID_Ecoregion]]$CENTROID_Y)
    data_coef_all <- bind_rows(data_coef_all,out)
  }
  
  plot_amazing[[dispers]] <- gridExtra::arrangeGrob(
    data_coef_all %>% 
      ggplot(aes(x=CENTROID_X , y=CENTROID_Y, fill=coef_f0, colour=coef_f0))+
      geom_point(shape=20, size=1,alpha=0.15)+
      scale_fill_CUNILLERA(palette = "estelada", discrete = F, reverse = F)+
      scale_color_CUNILLERA(palette = "estelada", discrete = F, reverse = F)+
      guides(color="none")+
      theme_classic(),
    
    data_coef_all %>% 
      ggplot(aes(x=CENTROID_X , y=CENTROID_Y, fill=coef_b, colour=coef_b))+
      geom_point(shape=20, size=1,alpha=0.15)+
      scale_fill_CUNILLERA(palette = "estelada", discrete = F, reverse = F)+
      scale_color_CUNILLERA(palette = "estelada", discrete = F, reverse = F)+
      guides(color="none")+
      theme_classic(),
    ncol=2, top = dispersal_distances_plot[dispers])
}

png(filename ="EU_degr_Tempo.png",
    width =350*7 ,height =800*7 ,units ="px",res = 300)
gridExtra::grid.arrange(
  plot_amazing[[1]],plot_amazing[[2]],plot_amazing[[3]],
  plot_amazing[[4]],plot_amazing[[5]],plot_amazing[[6]],
  plot_amazing[[7]],
  ncol=1)

dev.off()








