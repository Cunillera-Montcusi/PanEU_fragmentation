

Plot_Alp_Bet_Loc <- function(Alpha, Beta, xyCoords, xyFWarea, EcoR_size, Water_Type){
require(viridis);require(ggplot2);require(gridExtra); library(cowplot)
# Nice colours? CUNILLERA_palette is what you need
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/CUNILLERA_palette.R")
  
count_zeros_after_decimal <- function(x) {
    # Convert the number to a string
    x_str <- as.character(format(x, scientific = FALSE))
    
    # Check if there's a decimal point
    if (grepl("\\.", x_str)) {
      # Extract the part after the decimal point
      decimal_part <- unlist(strsplit(x_str, "\\."))[2]
      
      # Count leading zeros in the decimal part
      num_leading_zeros <- nchar(gsub("^(0*).*", "\\1", decimal_part))
    return(num_leading_zeros)
  } else {
    # If there's no decimal point, return 0
    return(0)
  }
}
  
Water_colors <- c("white","#9191F0","#5E92F2","#6CB8A0")

dispersal_distances_plot <- c(paste(dispersal_distances[1],"km",sep = " "),
                              paste(dispersal_distances[2],"km",sep = " "),
                              paste(dispersal_distances[3],"km",sep = " "),
                              paste(dispersal_distances[4],"km",sep = " "),
                              paste(dispersal_distances[5],"km",sep = " "),
                              paste(dispersal_distances[6],"km",sep = " "),
                              paste(dispersal_distances[7],"km",sep = " "))

output <- list()
color <- viridis(length(unique(Alpha$disp)))
plot_Alp_Bet_Loc_total <- list()
lay = rbind(c(1,2),c(1,3))

Alpha <- Alpha%>%left_join(read.csv2("NoComp_ECOR.csv"),by=c("EcoR"="Number_EcoR"),relationship = "many-to-many") %>%
                 filter(Name!="Unassigned") %>%  filter(X!="no complet")

Beta <- Beta%>%left_join(read.csv2("NoComp_ECOR.csv"),by=c("EcoR"="Number_EcoR"),relationship = "many-to-many") %>%
               filter(Name!="Unassigned") %>%  filter(X!="no complet")

Ecor_to_filter <- Alpha$EcoR

for (ID_Ecoregion in 1:length(unique(Alpha$EcoR))) {
  EcoR_to_Filter_Pos <- which(EcoR_size$EcoReg_ID==unique(Alpha$EcoR)[ID_Ecoregion])
  EcoR_to_Filter <- EcoR_size[which(EcoR_size$EcoReg_ID==unique(Alpha$EcoR)[ID_Ecoregion]),1]
  
  Alpha_filt <- Alpha %>% filter(EcoR==EcoR_to_Filter)  
  Beta_filt <- Beta %>% filter(EcoR==EcoR_to_Filter)
  
  FW_area <- xyFWarea[[EcoR_to_Filter_Pos]]
  area.max.europa<-max(xyFWarea[[EcoR_to_Filter_Pos]])
  Jmin <- 400*0.02
  J.max<-400+Jmin
  b.ef<-0.5 
  Perc_Hab_Loss <- 0
  FW_area_lost <- FW_area-(Perc_Hab_Loss*FW_area)
  J.freshwater<-ceiling((-Jmin+(J.max/(area.max.europa^b.ef))*FW_area_lost^b.ef))
  J.freshwater <- ifelse(J.freshwater<=0,0,J.freshwater)
  
  Surface_Plot <- gridExtra::arrangeGrob(
  data.frame("EcoR"=EcoR_to_Filter,
             "EcoR_name"=unique(Alpha_filt$Name),
             "X"=xyCoords[[EcoR_to_Filter_Pos]]$CENTROID_X,
             "Y"=xyCoords[[EcoR_to_Filter_Pos]]$CENTROID_Y,
             "Area"=J.freshwater) %>% 
    ggplot()+
    geom_point(shape=22,size=1, aes(x=X, y=Y,fill=Area, colour=Area))+
    scale_fill_viridis(option = "A", name="FW Area",limits=c(0.00000000001,max(J.freshwater)))+
    scale_color_viridis(option = "A", name="FW Area",limits=c(0.00000000001,max(J.freshwater)))+
    labs(title = paste(Alpha_filt$Name,Alpha_filt$EcoR))+xlab("")+ylab("")+
    theme_classic()+
    theme(panel.background = element_rect(fill = 'white', color = 'black'), 
          legend.position = "none",
          axis.text =element_blank(),
          axis.ticks = element_blank(),
          plot.background =element_rect(fill=Water_colors[Water_Type]))  
  ) 

plot_Alp_Bet_Loc_total<- gridExtra::arrangeGrob(
    Alpha_filt %>% 
      mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
        disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
          disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
            disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
              disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
                disp==dispersal_distances[6], dispersal_distances_plot[6],
                dispersal_distances_plot[7]))))))) %>% 
      ggplot(aes(x=loss, y=S, colour=disp_plot))+
      geom_point(size=2)+
      geom_errorbar(aes(x=loss, ymax=S_UP,ymin=S_Low))+
      scale_colour_manual(values = color, breaks=unique(Alpha$disp_plot))+
      ylab("Mean alpha diversity")+xlab("% Habitat degradation")+
      labs(title =paste("Alpha",unique(Alpha_filt$Name) , sep=" "))+
      theme_classic()+ 
      theme(legend.position = "none",
            plot.background =element_rect(fill=Water_colors[Water_Type])),
    
    Beta_filt %>% 
      mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
        disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
          disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
            disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
              disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
                disp==dispersal_distances[6], dispersal_distances_plot[6],
                dispersal_distances_plot[7]))))))) %>% 
      ggplot(aes(x=loss, y=B, colour=disp_plot))+
      geom_point(size=2)+
      geom_errorbar(aes(x=loss, ymax=B_UP,ymin=B_Low))+
      scale_colour_manual(values = color, breaks=unique(Beta$disp_plot))+
      ylab("Mean beta diversity")+xlab("% Habitat degradation")+
      labs(title =paste("Beta",unique(Alpha_filt$Name), sep=" "))+
      theme_classic()+ 
      theme(legend.position = "none",
            plot.background =element_rect(fill=Water_colors[Water_Type])),
    nrow = 2)


# Exctracting legends
legend_dispersal <- get_legend(
  Beta_filt %>% 
    mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
      disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
        disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
          disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
            disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
              disp==dispersal_distances[6], dispersal_distances_plot[6],
              dispersal_distances_plot[7]))))))) %>% 
    ggplot(aes(x=loss, y=B,colour=disp_plot))+
    geom_point(size=10)+
    scale_colour_manual(values = color, breaks=dispersal_distances_plot, name="Dispersal ability")+
    theme_classic()+
    guides(colour = guide_legend(override.aes = list(size=7),title.position = "top"))+
    theme(legend.direction = "horizontal",
          legend.text = element_text(size = 23),
          legend.key.size = unit(2.3, 'cm'), 
          legend.title = element_text(size=30))
  )

legend_FW_area <- get_legend(
  ggplot(data.frame(cbind(xyCoords[[EcoR_to_Filter_Pos]]$CENTROID_X/max(xyCoords[[EcoR_to_Filter_Pos]]$CENTROID_X),
                          xyCoords[[EcoR_to_Filter_Pos]]$CENTROID_Y/max(xyCoords[[EcoR_to_Filter_Pos]]$CENTROID_Y))),
                          aes(x=X1, y=X2))+
                          geom_point(shape=22,size=1, 
                                          aes(fill=xyFWarea[[EcoR_to_Filter_Pos]]/max(xyFWarea[[EcoR_to_Filter_Pos]]))
                                                )+
                               scale_fill_viridis(option = "A")+
                               labs(fill="% Surface water")+
                               guides(fill = guide_colourbar(title.position = "top"))+
                               theme(panel.background = element_rect(fill = 'grey15', color = 'black'),
                                     legend.direction = "horizontal",
                                     legend.text = element_text(size = 23),
                                     legend.key.size = unit(1.5, 'cm'), 
                                     legend.title = element_text(size=30))
)


output[[ID_Ecoregion]] <- list(Surface_Plot,plot_Alp_Bet_Loc_total,legend_dispersal,legend_FW_area)
}
output
}

Plot_Gam <- function(Gamma, Water_Type,Mod_Coef){
require(viridis);require(ggplot2);require(gridExtra); require(cowplot)
# Nice colours? CUNILLERA_palette is what you need
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/CUNILLERA_palette.R")

Water_colors <- c("white","#9191F0","#5E92F2","#6CB8A0")
  
color <- viridis(length(unique(Gamma$disp)))
plot_G_total <- list()

dispersal_distances_plot <- c(paste(dispersal_distances[1],"km",sep = " "),
                              paste(dispersal_distances[2],"km",sep = " "),
                              paste(dispersal_distances[3],"km",sep = " "),
                              paste(dispersal_distances[4],"km",sep = " "),
                              paste(dispersal_distances[5],"km",sep = " "),
                              paste(dispersal_distances[6],"km",sep = " "),
                              paste(dispersal_distances[7],"km",sep = " "))

Gamma <- Gamma%>%left_join(read.csv2("NoComp_ECOR.csv"),by=c("EcoR"="Number_EcoR"),relationship = "many-to-many") %>%
                 filter(Name!="Unassigned") %>%  filter(X!="no complet")

Ecor_to_filter <- Gamma$EcoR

for (ID_Ecoregion in 1:length(unique(Gamma$EcoR))){
  
  Gamma_filt <- Gamma %>% filter(EcoR==unique(Ecor_to_filter)[[ID_Ecoregion]])
  Mod_Coef_filt <- Mod_Coef%>% filter(EcoR==unique(Ecor_to_filter)[[ID_Ecoregion]])%>% 
    mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
      disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
        disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
          disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
            disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
              disp==dispersal_distances[6], dispersal_distances_plot[6],
              dispersal_distances_plot[7])))))))
  
  plot_G_total[[ID_Ecoregion]] <- gridExtra::arrangeGrob(
    Gamma_filt %>% 
      mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
        disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
          disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
            disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
              disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
                disp==dispersal_distances[6], dispersal_distances_plot[6],
                dispersal_distances_plot[7]))))))) %>% 
      ggplot(aes(x=loss, y=Rel_G, colour=disp_plot))+
      geom_point(size=1.2)+
      geom_errorbar(aes(x=loss, ymax=Rel_G_UP,ymin=Rel_G_Low))+
      geom_line(data=Mod_Coef_filt,aes(x=loss,y=Mod_Pred))+
      scale_y_continuous(limits = c(-5,c(max(Gamma$Rel_G)+10)))+
      scale_colour_manual(values = color, breaks=unique(Gamma$disp_plot))+
      labs(title =unique(Gamma_filt$Name),
           colour="Dispersal ability")+
      ylab("Relative gamma (%)")+xlab("% Habitat degradation")+
      theme_classic()+
      theme(plot.background =element_rect(fill=Water_colors[Water_Type]),
            legend.background = element_rect(fill=Water_colors[Water_Type]),
            legend.position = "none"),
    ncol = 1)
}

legend_dispersal <- get_legend(
  Gamma_filt %>% 
    mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
      disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
        disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
          disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
            disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
              disp==dispersal_distances[6], dispersal_distances_plot[6],
              dispersal_distances_plot[7]))))))) %>% 
    ggplot(aes(x=loss, y=Rel_G, colour=disp_plot))+
    geom_point(size=1.2)+
    geom_errorbar(aes(x=loss, ymax=Rel_G_UP,ymin=Rel_G_Low))+
    geom_line(data=Mod_Coef_filt,aes(x=loss,y=Mod_Pred))+
    scale_y_continuous(limits = c(-5,c(max(Gamma$Rel_G)+10)))+
    scale_colour_manual(values = color, breaks=dispersal_distances_plot)+
    labs(title =unique(Gamma_filt$Name),
         colour="Dispersal ability")+
    ylab("Relative gamma (%)")+xlab("% Habitat degradation")+
    theme_classic()+theme(plot.background =element_rect(fill=Water_colors[Water_Type]),
                          legend.background = element_rect(fill=Water_colors[Water_Type]),
                          legend.position = "right")
  )

plot_G_total_withLegend <- list()
for (ID_Ecoregion in 1:(length(unique(Gamma$EcoR))+1)){
  if(ID_Ecoregion==(length(unique(Gamma$EcoR))+1)){
    plot_G_total_withLegend[[ID_Ecoregion]] <- gridExtra::grid.arrange(legend_dispersal)  
  }else{
    plot_G_total_withLegend[[ID_Ecoregion]] <- plot_G_total[[ID_Ecoregion]]  
  }
}

plot_G_total <- plot_G_total_withLegend

}


EU_coef_vs_dispersal_plots <- function(Curve_coeficients, xy_Coordin,shape_Boundaries,back_colour){
  # Nice colours? CUNILLERA_palette is what you need
  source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/CUNILLERA_palette.R")
  require(ggspatial)
  require(sf)
  require(tidyverse)
  
  dispersal_distances <- c(0.001,0.1,0.5,1,2,5,10)
  dispersal_distances_plot <- c(paste(dispersal_distances[1],"km",sep = " "),
                                paste(dispersal_distances[2],"km",sep = " "),
                                paste(dispersal_distances[3],"km",sep = " "),
                                paste(dispersal_distances[4],"km",sep = " "),
                                paste(dispersal_distances[5],"km",sep = " "),
                                paste(dispersal_distances[6],"km",sep = " "),
                                paste(dispersal_distances[7],"km",sep = " "))
  
  Curve_coeficients <- Curve_coeficients %>%  
    mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
    disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
      disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
        disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
          disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
            disp==dispersal_distances[6], dispersal_distances_plot[6],
            dispersal_distances_plot[7]))))))) 
  
  plot_amazing <- list()
  for (dispers in 1:length(unique(Curve_coeficients$disp))) {
    data_coef_all <- data.frame()
    
    for (ID_Ecoregion in 1:length(unique(Curve_coeficients$EcoR))){
      coef_values <- Curve_coeficients %>% filter(EcoR==unique(Curve_coeficients$EcoR)[ID_Ecoregion],
                                                  disp==unique(Curve_coeficients$disp)[dispers]) %>% 
                                            group_by(EcoR,disp) %>% 
                                            summarise(Coef_b=mean(Coef_b),Coef_q=mean(Coef_q))
      
      out <- data.frame("EcoR"=coef_values$EcoR,
                        "disp"=unique(Curve_coeficients$disp)[dispers],
                        "disp_plot"=unique(Curve_coeficients$disp_plot)[dispers],
                        "coef_b"=coef_values$Coef_b,
                        "coef_q"=coef_values$Coef_q,
                        "EcoR_Name"=unique(xy_Coordin[[ID_Ecoregion]]$EcoR_Name))
      data_coef_all <- bind_rows(data_coef_all,out)
    }
    
    
    data_coef_all <- data_coef_all %>% mutate(coef_b=ifelse(coef_b>0,-0.01,coef_b)) %>% 
                                      left_join(read.csv2("NoComp_ECOR.csv"),by=c("EcoR_Name"="Name")) %>% 
                                      filter(X!="no complet")
    
    shape_to_plot <- shape_Boundaries%>% left_join(data_coef_all,by = c("Name"="EcoR_Name")) %>%
                                         filter(X!="no complet")
                                
    
    plot_amazing[[dispers]] <- gridExtra::arrangeGrob(
        ggplot()+
        tidyterra::geom_spatvector(data=st_zm(shape_to_plot),aes(fill=coef_b))+
        #geom_point(shape=20, size=0.1)+
        #scale_alpha_continuous()+
        scale_fill_viridis(option = "F",direction = 1,limits=c(-1,0))+
        #scale_fill_CUNILLERA(palette = "Gradient_Red", discrete = F, reverse = F, limits=c(-1,0))+
        #scale_color_CUNILLERA(palette = "Gradient_Red", discrete = F, reverse = F, limits=c(-1,0))+
        guides(color="none", alpha="none", fill=guide_colourbar("b"))+
        labs(subtitle="Proportional decay rate")+
        xlab("")+ylab("")+
        theme_classic()+ theme(panel.background = element_rect(fill = 'grey95', color = 'black'),
                               plot.background = element_rect(fill = back_colour),
                               legend.background = element_rect(fill = back_colour)),
      
        ggplot()+
        tidyterra::geom_spatvector(data=st_zm(shape_to_plot), aes(fill=coef_q))+
        #geom_point(shape=20, size=0.1)+
        #scale_alpha_continuous()+
        scale_fill_viridis(option = "G",direction = -1, limits=c(0.25,0.36))+
        #scale_fill_CUNILLERA(palette = "Gradient_Violet", discrete = F, reverse = T, limits=c(0.25,0.36))+
        #scale_color_CUNILLERA(palette = "Gradient_Violet", discrete = F, reverse = T, limits=c(0.25,0.36))+
        guides(color="none", alpha="none", fill=guide_colourbar("q"))+
        labs(subtitle="Collapsing rate")+
        xlab("")+ylab("")+
        theme_classic()+ theme(panel.background = element_rect(fill = 'grey95', color = 'black'),
                               plot.background = element_rect(fill = back_colour),
                               legend.background = element_rect(fill = back_colour)),
      
      ncol=2, top = unique(Total_FW$Gamma$disp_plot)[dispers])
  }
  plot_amazing  
}



#Water type must be either 1-Total, 2-Ephemeral, 3-Temporary, 4-Permanent 
Plot_Relation_EU <- function(Water_Type_List,
                             WaterType,
                             MeanMed_or_CV,
                             Centrality_data,
                             Environmental_data){
  CV_calc <- function(x){((sd(x,na.rm = TRUE)/mean(x,na.rm = TRUE)))}
  mean_calc <- function(x){mean(x, na.rm = TRUE)}
  require(viridis);require(ggplot2);require(gridExtra); library(cowplot)
  
  EU_degr_relat <- list()
  WaterTypes <- unique(Centrality_data$WaterSystem)
  WaterTitle <- c("Total Freshwater", "Ephemeral", "Temporal", "Permanent")
  
  if(MeanMed_or_CV=="MeanMed"){Metric_value <- c("Mean","Med")}else{Metric_value <- c("CV","CV")}
  
  data_coef_all <- data.frame()
  for (dispers in 1:length(unique(Water_Type_List$Curv_Coef$disp))) {
    data_coef_all_disp <- data.frame()
    for (ID_Ecoregion in 1:length(unique(Water_Type_List$Curv_Coef$EcoR))){
      
      coef_values <- Water_Type_List$Curv_Coef %>% filter(EcoR==unique(Water_Type_List$Curv_Coef$EcoR)[ID_Ecoregion],
                                                    disp==unique(Water_Type_List$Curv_Coef$disp)[dispers]) %>% 
                                                   group_by(EcoR,disp) %>% 
                                                   summarise(Coef_b=mean(Coef_b),Coef_q=mean(Coef_q)) %>%
        mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
          disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
            disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
              disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
                disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
                  disp==dispersal_distances[6], dispersal_distances_plot[6],
                  dispersal_distances_plot[7]))))))) 
      
      env_data_filt <- Environmental_data %>% filter(EcoR==unique(Water_Type_List$Curv_Coef$EcoR)[ID_Ecoregion],
                                                     Metric==Metric_value[1]) %>% 
                                              pivot_wider(names_from = Geog_Variab,values_from = Value) %>% 
                                              mutate_at("EcoR", as.numeric)
      
      centr_data_filt <- Centrality_data %>% filter(EcoR==unique(Water_Type_List$Curv_Coef$EcoR)[ID_Ecoregion], 
                                                    WaterSystem==WaterTypes[WaterType]) %>% 
        pivot_wider(names_from = Centr_Metric,values_from = Value) %>% 
        mutate_at("EcoR", as.numeric)
      
      EcoR_data <- data.frame("EcoR"=as.numeric(unique(Water_Type_List$Curv_Coef$EcoR))[ID_Ecoregion],
                              "WatCov_Mean"=mean_calc((Water_Type_List$xyFWarea[[ID_Ecoregion]]/0.0081)*100),
                              "WatCov_CV"=CV_calc((Water_Type_List$xyFWarea[[ID_Ecoregion]]/0.0081)*100),
                              "EcoRSize"=length(Water_Type_List$xyFWarea[[ID_Ecoregion]]))
      
      out <- data.frame("EcoR"=as.numeric(coef_values$EcoR),
                        "disp"=coef_values$disp,
                        "disp_plot"=coef_values$disp_plot,
                        "coef_b"=coef_values$Coef_b,
                        "coef_q"=coef_values$Coef_q) %>% 
        left_join(env_data_filt, by=c("EcoR")) %>% 
        left_join(centr_data_filt, by=c("EcoR")) %>% 
        left_join(EcoR_data, by=c("EcoR"))
      
      
      data_coef_all_disp <- bind_rows(data_coef_all_disp,out)
    }
    data_coef_all <- bind_rows(data_coef_all_disp,data_coef_all) %>% arrange(disp)
  }
  
  EU_degr_relat[[1]] <- data_coef_all
  
  color_b <- viridis(length(unique(data_coef_all$disp)),option = "viridis")
  color_q <- viridis(length(unique(data_coef_all$disp)),option = "viridis")
  
  data_coef_all_plot <- data_coef_all %>%  
    dplyr::select(-c(Metric,WaterSystem)) %>% 
    pivot_longer(cols=Long:EcoRSize,
                 names_to= "Variable",
                 values_to="Value")
  
  data_coef_all_plot$Variable <- factor(data_coef_all_plot$Variable, 
                                        levels = c("Long","Lat","precAn","elev","CV_Betw","Med_Betw","CV_out","Med_out",
                                                   "WatCov_Mean","WatCov_CV","EcoRSize"))
  
  EU_degr_relat[[2]] <- 
    data_coef_all_plot %>% 
      ggplot(aes(x=Value, y=coef_b, fill=disp_plot))+ 
      geom_point(shape=21,size=2, alpha=0.25)+
      geom_smooth(method = "gam",se = F, aes(color=disp_plot),size=1)+
      scale_fill_manual(values = color_b, breaks=unique(data_coef_all$disp_plot))+
      scale_color_manual(values =color_b, breaks=unique(data_coef_all$disp_plot))+
      #scale_y_continuous(limits=c(-1.2,0))+
      xlab(MeanMed_or_CV)+ylab("Proportional decay rate (b)")+
      guides(fill="none")+
      labs(title=WaterTitle[WaterType],
           subtitle = "Proportional decay rate", color="Dispersal ability")+
      facet_wrap(.~Variable,nrow = 3,scales = "free_x")+
      theme_classic()+theme(legend.position = "bottom")
  
  EU_degr_relat[[3]] <- 
    data_coef_all_plot %>% 
      ggplot(aes(x=Value, y=coef_q, fill=disp_plot))+ 
      geom_point(shape=21,size=2, alpha=0.25)+
      geom_smooth(method = "gam",se = F, aes(color=disp_plot),size=1)+
      scale_fill_manual(values = color_q, breaks=unique(data_coef_all$disp_plot))+
      scale_color_manual(values =color_q, breaks=unique(data_coef_all$disp_plot))+
      #scale_y_continuous(limits=c(0.2,0.36))+
      xlab(MeanMed_or_CV)+ylab("Collapsing rate (q)")+
      guides(fill="none")+
      labs(title=WaterTitle[WaterType],
           subtitle = "Collapsing rate", color="Dispersal ability")+
      facet_wrap(.~Variable,nrow = 3,scales = "free_x")+
      theme_classic()+theme(legend.position = "bottom")
  
  EU_degr_relat
}








