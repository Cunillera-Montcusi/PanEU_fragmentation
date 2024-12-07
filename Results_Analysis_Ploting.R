
library(viridis);library(tidyverse)

source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/Functions_to_plot.R")
setwd("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation")

# We charge TOTAL freshwater Data
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/SLURM_outputs_treat_Total.R")
Total_FW <- list(Alpha=Mean_Alpha,Beta=Mean_Beta,Gamma=Mean_Gamma,Curv_Coef=Curves_Coef,
                 EcoR_size=EcoR_size,xy_Coor=Outputs_SLURM_xyCoords,xyFWarea=Outputs_SLURM_xyFWarea)

PLot_AB_TOTAL <- Plot_Alp_Bet_Loc(Alpha =Total_FW$Alpha,Beta = Total_FW$Beta,
                 xyCoords = Total_FW$xy_Coor,xyFWarea =Total_FW$xyFWarea,EcoR_size =Total_FW$EcoR_size,Water_Type = 1)

Plot_Gam_TOTAL <- Plot_Gam(Gamma = Total_FW$Gamma, Water_Type = 1)

png(filename ="Alph_Bet_TOTAL.png",width =1500*7 ,height =1500*6 ,units ="px",res = 300)
out <- list(lapply(PLot_AB_TOTAL, `[[`, 1));out[[1]][[27]] <- PLot_AB_TOTAL[[1]][[3]];out[[1]][[28]] <- PLot_AB_TOTAL[[1]][[2]]
grid.arrange(arrangeGrob(grobs = out[[1]], ncol=4))
dev.off()

png(filename ="Gam_TOTAL.png",
    width =500*8 ,height =550*8 ,units ="px",res = 300)
grid.arrange(arrangeGrob(grobs = Plot_Gam_TOTAL,ncol=4))
dev.off()


# We charge PERMANENT freshwater Data
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/SLURM_outputs_treat_Perm.R")
Perm_FW <- list(Alpha=Mean_Alpha,Beta=Mean_Beta,Gamma=Mean_Gamma,Curv_Coef=Curves_Coef,
                 EcoR_size=EcoR_size,xy_Coor=Outputs_SLURM_xyCoords,xyFWarea=Outputs_SLURM_xyFWarea)

PLot_AB_Perm <- Plot_Alp_Bet_Loc(Alpha =Perm_FW$Alpha,Beta = Perm_FW$Beta,
                                  xyCoords = Perm_FW$xy_Coor,xyFWarea =Perm_FW$xyFWarea,EcoR_size =Perm_FW$EcoR_size,Water_Type = 2)

Plot_Gam_Perm <- Plot_Gam(Gamma = Perm_FW$Gamma,Water_Type = 2)

png(filename ="Alph_Bet_PERM.png",width =1500*7 ,height =1500*6 ,units ="px",res = 300)
out <- list(lapply(PLot_AB_Perm, `[[`, 1));out[[1]][[27]] <- PLot_AB_Perm[[1]][[3]];out[[1]][[28]] <- PLot_AB_Perm[[1]][[2]]
grid.arrange(arrangeGrob(grobs = out[[1]], ncol=4))
dev.off()

png(filename ="Gam_PERM.png",
    width =800*8 ,height =850*8 ,units ="px",res = 300)
grid.arrange(arrangeGrob(grobs = Plot_Gam_Perm,ncol=4))
dev.off()

# We charge TEMPORARY freshwater Data
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/SLURM_outputs_treat_Tempo.R")
Tempo_FW <- list(Alpha=Mean_Alpha,Beta=Mean_Beta, Gamma=Mean_Gamma,Curv_Coef=Curves_Coef,
                EcoR_size=EcoR_size,xy_Coor=Outputs_SLURM_xyCoords,xyFWarea=Outputs_SLURM_xyFWarea)

PLot_AB_Tempo <- Plot_Alp_Bet_Loc(Alpha =Tempo_FW$Alpha,Beta = Tempo_FW$Beta,
                                  xyCoords = Tempo_FW$xy_Coor,xyFWarea =Tempo_FW$xyFWarea,EcoR_size =Tempo_FW$EcoR_size,Water_Type = 3)

Plot_Gam_Tempo <- Plot_Gam(Gamma = Tempo_FW$Gamma,Water_Type = 3)

png(filename ="Alph_Bet_TEMP.png",width =1500*7 ,height =1500*6 ,units ="px",res = 300)
out <- list(lapply(PLot_AB_Tempo, `[[`, 1));out[[1]][[27]] <- PLot_AB_Tempo[[1]][[3]];out[[1]][[28]] <- PLot_AB_Tempo[[1]][[2]]
grid.arrange(arrangeGrob(grobs = out[[1]], ncol=4))
dev.off()

png(filename ="Gam_TEMP.png",
    width =800*8 ,height =850*8 ,units ="px",res = 300)
grid.arrange(arrangeGrob(grobs = Plot_Gam_Tempo,ncol=4))
dev.off()

# We charge EPHEMERAL freshwater Data
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/SLURM_outputs_treat_Ephem.R")
Ephem_FW <- list(Alpha=Mean_Alpha,Beta=Mean_Beta,Gamma=Mean_Gamma,Curv_Coef=Curves_Coef,
                EcoR_size=EcoR_size,xy_Coor=Outputs_SLURM_xyCoords,xyFWarea=Outputs_SLURM_xyFWarea)

PLot_AB_Ephem <- Plot_Alp_Bet_Loc(Alpha =Ephem_FW$Alpha,Beta = Ephem_FW$Beta,
                                  xyCoords = Ephem_FW$xy_Coor,xyFWarea =Ephem_FW$xyFWarea,EcoR_size =Ephem_FW$EcoR_size,Water_Type =4)

Plot_Gam_Ephem <- Plot_Gam(Gamma = Ephem_FW$Gamma,Water_Type = 4)

png(filename ="Alph_Bet_EPHEM.png",width =1500*7 ,height =1500*6 ,units ="px",res = 300)
out <- list(lapply(PLot_AB_Ephem, `[[`, 1));out[[1]][[27]] <- PLot_AB_Ephem[[1]][[3]];out[[1]][[28]] <- PLot_AB_Ephem[[1]][[2]]
grid.arrange(arrangeGrob(grobs = out[[1]], ncol=4))
dev.off()

png(filename ="Gam_EPHEM.png",
    width =800*8 ,height =850*8 ,units ="px",res = 300)
grid.arrange(arrangeGrob(grobs = Plot_Gam_Ephem,ncol=4))
dev.off()

png(filename ="Alph_Bet_Gam_Article.png",
    width =1500*5 ,height =450*5 ,units ="px",res = 300)
grid.arrange(
grid.arrange(PLot_AB_TOTAL[[23]][[1]]),
grid.arrange(PLot_AB_Perm[[22]][[1]]),
grid.arrange(PLot_AB_Tempo[[23]][[1]]),
grid.arrange(PLot_AB_Ephem[[23]][[1]]),
grid.arrange(arrangeGrob(grobs = Plot_Gam_TOTAL[[23]])),
grid.arrange(arrangeGrob(grobs = Plot_Gam_Perm[[22]])),
grid.arrange(arrangeGrob(grobs = Plot_Gam_Tempo[[23]])),
grid.arrange(arrangeGrob(grobs = Plot_Gam_Ephem[[23]])),
ncol=4,nrow=2)
dev.off()

# Final list values ####
#Total_FW
#Perm_FW
#Tempo_FW
#Ephem_FW
save(list = c("Total_FW","Perm_FW","Tempo_FW","Ephem_FW"),file = "treated_data.RData")
#load("treated_data.RData")

# European maps Figure 3 ####
EU_FW_EcoR<- sf::st_read("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/BDD_PanEU/ecoregions_EUR_poly_AE/ecoregions_EUR_poly_AE.shp")
EU_FW_EcoR$Name <- gsub("- ","",EU_FW_EcoR$Name)# We polish some errors in names to make them equal to the ones in the DataBase
EU_FW_EcoR$Name <- gsub(" ","_",EU_FW_EcoR$Name)# We polish some errors in names to make them equal to the ones in the DataBase

# We define the colours
#6CB8A0 - Ephemeral
#5E92F2 - Temporary
#9191F0 - Permanent

EU_loss_TOTAL <- EU_coef_vs_dispersal_plots(Curve_coeficients = Total_FW$Curv_Coef, 
                                            xy_Coordin = Total_FW$xy_Coor,
                                            shape_Boundaries = EU_FW_EcoR,
                                            back_colour ="white")
EU_loss_Perm <- EU_coef_vs_dispersal_plots(Perm_FW$Curv_Coef, Perm_FW$xy_Coor,EU_FW_EcoR,"#9191F0")
EU_loss_Tempo <- EU_coef_vs_dispersal_plots(Tempo_FW$Curv_Coef,Tempo_FW$xy_Coor,EU_FW_EcoR,"#5E92F2")
EU_loss_Ephem <- EU_coef_vs_dispersal_plots(Ephem_FW$Curv_Coef,Ephem_FW$xy_Coor,EU_FW_EcoR,"#6CB8A0")

png(filename ="EU_degr_map.png",
    width =1000*12 ,height =800*12 ,units ="px",res = 300)
grid.arrange(arrangeGrob(grobs = EU_loss_TOTAL,ncol=1),
             arrangeGrob(grobs = EU_loss_Perm,ncol=1),
             arrangeGrob(grobs = EU_loss_Tempo,ncol=1),
             arrangeGrob(grobs = EU_loss_Ephem,ncol=1),
             ncol=4)
dev.off()

png(filename ="EU_degr_map_Artic.png",
    width =1500*7 ,height =450*7 ,units ="px",res = 300)
grid.arrange(
  arrangeGrob(EU_loss_TOTAL[[2]], EU_loss_Perm[[2]],EU_loss_Tempo[[2]],  EU_loss_Ephem[[2]]),
  arrangeGrob(EU_loss_TOTAL[[6]],EU_loss_Perm[[6]],EU_loss_Tempo[[6]],EU_loss_Ephem[[6]]),
ncol=2)
dev.off()

# Obtaining and treating the diversity decay coefficients ####
out_Table <- data.frame()
for (ecoR in 1:length(Total_FW$xy_Coor)) {
sumary <- unique(Total_FW$xy_Coor[[ecoR]][,c(7,9,10)])
sumary <- sumary[which(sumary[,1]!=""),]
out_Table <- bind_rows(out_Table,sumary)
}
# Print for TOTAL
out_Table$EcoR_ID <- as.numeric(out_Table$EcoR_ID)
Table_coef <- left_join(out_Table,Total_FW$Curv_Coef, by=c("EcoR_ID"="EcoR")) %>% 
              group_by(disp_plot,EcoR_ID,EcoR_Name,NAME_ENGL) %>% 
              dplyr::select(EcoR_ID,EcoR_Name,NAME_ENGL,disp_plot,b_Coef,q_Coef)
write.table(Table_coef,file = "Table_coef_TOT.txt")

# Print for Permanent
out_Table$EcoR_ID <- as.numeric(out_Table$EcoR_ID)
Table_coef <- left_join(out_Table,Perm_FW$Curv_Coef, by=c("EcoR_ID"="EcoR")) %>% 
  group_by(disp_plot,EcoR_ID,EcoR_Name,NAME_ENGL) %>% 
  dplyr::select(EcoR_ID,EcoR_Name,NAME_ENGL,disp_plot,b_Coef,q_Coef)
write.table(Table_coef,file = "Table_coef_Perm.txt")

# Print for Temporary 
out_Table$EcoR_ID <- as.numeric(out_Table$EcoR_ID)
Table_coef <- left_join(out_Table,Tempo_FW$Curv_Coef, by=c("EcoR_ID"="EcoR")) %>% 
  group_by(disp_plot,EcoR_ID,EcoR_Name,NAME_ENGL) %>% 
  dplyr::select(EcoR_ID,EcoR_Name,NAME_ENGL,disp_plot,b_Coef,q_Coef)
write.table(Table_coef,file = "Table_coef_Tempo.txt")

# Print for Ephemeral
out_Table$EcoR_ID <- as.numeric(out_Table$EcoR_ID)
Table_coef <- left_join(out_Table,Ephem_FW$Curv_Coef, by=c("EcoR_ID"="EcoR")) %>% 
  group_by(disp_plot,EcoR_ID,EcoR_Name,NAME_ENGL) %>% 
  dplyr::select(EcoR_ID,EcoR_Name,NAME_ENGL,disp_plot,b_Coef,q_Coef)
write.table(Table_coef,file = "Table_coef_Ephem.txt")

# DIVERSITY DECAY AGAINST LANDSCAPE STRUCTURAL DESCRIPTORS #### 
# Charging landscape structural descriptors 
# We charge and prepare all the Geographical and centrality variables
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/BDD_ExternalPanEU/Charge_treat_env_centr_values.R")
centr_data
env_data

# Several descriptors are charges but we will only use:  
# Long, Lat, Betw_CV, Betw_Med, OutDeg_CV, OutDeg_Med, WatCov_Mean, WatCov_CV, EcoRSize

# TOTAL FW relations ####
#Water type must be either 1-Total, 2-Ephemeral, 3-Temporary, 4-Permanent 
EU_relat_TOTAL <- Plot_Relation_EU(Water_Type_List = Total_FW, WaterType =1,
                                   MeanMed_or_CV = "MeanMed",
                                   Centrality_data = centr_data,
                                   Environmental_data = env_data)

colnames(EU_relat_TOTAL[[1]])[13:16] <- c("Betw_CV","Betw_Med","OutDeg_CV","OutDeg_Med")

# We apply this code to create a gam and the cooresponding plots with the data. It is not a function as the 
#the responde variables that are being used are different for each type of water
library(mgcv);library(car)
Coef_value <- c("coef_b","coef_q")
Out_Results_GAM <-list() 
for (dec_coef in 1:2) {
Out_FW_type <- data.frame()
pred_FW_type<- data.frame()
GAM_out <- list()
for (disper in 1:length(unique(EU_relat_TOTAL[[1]]$disp_plot))) {
# We filter and select the data that we need for each dispersal group. 
#- we log transform heavily skewed data. 
data_to_gam<- EU_relat_TOTAL[[1]] %>% filter(disp_plot==unique(EU_relat_TOTAL[[1]]$disp_plot)[disper],
                                             OutDeg_Med!=is.na(OutDeg_Med)) %>% 
                                       dplyr::select(-c(precAn,elev,WaterSystem)) %>% 
                                       mutate(EcoRSize=log(EcoRSize+1),
                                              Betw_CV=log(Betw_CV+1),
                                              Betw_Med=log(Betw_Med+1)) %>% 
                                        dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,WatCov_Mean))
# We scale data to homogenize its strength on the patterns detected (strong differenced between response values)
data_to_gam_scaled <- scale(data_to_gam[,8:ncol(data_to_gam)],center = T,scale = T)
# We detect highly correlated variables and eliminate them from the run
hc = findCorrelation(cor(data_to_gam_scaled, use="complete.obs", method="spearman"),
                     cutoff=0.80,names = F, exact = T)

data_to_gam<- data_to_gam %>% dplyr::select(-c(colnames(data_to_gam_scaled)[hc]))
data_to_gam_scaled <- data_to_gam_scaled[,-hc]
data_to_gam[,colnames(data_to_gam_scaled)] <- data_to_gam_scaled 

coef_val <- data_to_gam %>% dplyr::select(c(coef_b,coef_q))
data_to_gam <- data_to_gam %>% dplyr::select(-c(coef_b,coef_q)) %>% 
                               mutate("Coef"=coef_val[,dec_coef])
# We trandform in positive the pattern to perform the gam and run it for the family "betareg". 
if (Coef_value[dec_coef]=="coef_b") {data_to_gam$Coef <- data_to_gam$Coef*-1}
# Gam per se
b <- gam(Coef~
         #s(Long,k=3, bs="cr")+
         #s(Lat,k=3, bs="cr")+
         #s(Betw_CV,k=3, bs="cr")+
         #s(Betw_Med,k=3, bs="cr")+
         s(OutDeg_CV,k=3, bs="cr")+
         #s(OutDeg_Med,k=3, bs="cr")+
         #s(WatCov_Mean,k=3, bs="cr")+
         s(WatCov_CV,k=3, bs="cr")+
         s(EcoRSize,k=3, bs="cr"),
    select=TRUE,method = "REML",family=betar(link="logit"), 
    data = data_to_gam)
# We store the gam result
out <- summary.gam(b)
# We obrain the significant values
sign_values <- rep("NoSign",length(out$s.pv))
sign_values[which(out$s.pv<0.05)] <- "Sign"

# We build up a table with the same information used in the gam and including the sign values (plot)
Out_Table <- EU_relat_TOTAL[[1]] %>% filter(disp_plot==unique(EU_relat_TOTAL[[1]]$disp_plot)[disper],
                                            OutDeg_Med!=is.na(OutDeg_Med)) %>% 
                                    dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,WatCov_Mean,precAn,elev)) %>% 
                                     mutate(EcoRSize=log(EcoRSize+1),
                                            Betw_CV=log(Betw_CV+1)) %>% 
                                     pivot_longer(cols=colnames(data_to_gam_scaled)) %>% 
                                     mutate(Sign_value=rep(sign_values,(length(unique(data_to_gam$EcoR)))))

# Model prediction to later plot the relationships and the trends
pred_Out <- data.frame()
# We run the prediction for eahc significant variable
for (col_noms in 1:length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])) {
data_to_gam_scaled_depur <- data_to_gam_scaled[,colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]]

Main_variable <- colnames(data_to_gam_scaled_depur)[col_noms]  
Other_variables <- colnames(data_to_gam_scaled)[-which(colnames(data_to_gam_scaled)==Main_variable)]
new_data <- with(data_to_gam, expand.grid("Main_variable" = seq(min(data_to_gam[,Main_variable]), 
                                                        max(data_to_gam[,Main_variable]),
                                                        length = nrow(data_to_gam)),
                                          "Var1" = median(data_to_gam[,Other_variables[1]]),
                                          "Var2"= median(data_to_gam[,Other_variables[2]])))
colnames(new_data) <- c(Main_variable,Other_variables)

# Using the dataset that we created (maintaining the significant variable fixed) we reproduce the values for the 
#predicted gam and store them in a data frame to later plot them. 
ilink <- family(b)$linkinv
pred <- predict(b,new_data, type = "link", se.fit = TRUE)
pred <- cbind(pred, new_data)
pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),upr_ci = ilink(fit + (2 * se.fit)),fitted = ilink(fit))
pred <- data.frame("Var"=Main_variable,"disp"=Out_Table$disp,"disp_plot"=Out_Table$disp_plot,pred,row.names = NULL)
pred_Out <- bind_rows(pred_Out,pred)
}
# Output saving 
pred_FW_type <- bind_rows(pred_FW_type,pred_Out)
Out_FW_type <- bind_rows(Out_FW_type,Out_Table)
GAM_out[[disper]] <-out
names(GAM_out)[disper] <- unique(Out_Table$disp_plot)
temp_list <- list("GAM_Result"=GAM_out,"Prediction"=pred_FW_type,"Raw_values"=Out_FW_type)
Out_Results_GAM[[dec_coef]] <- temp_list
}# Closing the dispersal
}# Closing coefficient
names(Out_Results_GAM) <- Coef_value
TOTAL_Out_Results_GAM <- Out_Results_GAM

#png(filename ="EU_relat_TOTAL.png",
#    width =750*7 ,height =300*7 ,units ="px",res = 300)
#grid.arrange(EU_relat_TOTAL[[2]],EU_relat_TOTAL[[3]],nrow=1)
#dev.off()

# Perm FW relations ####
#Water type must be either 1-Total, 2-Ephemeral, 3-Temporary, 4-Permanent 
EU_relat_Perm <- Plot_Relation_EU(Water_Type_List = Perm_FW,WaterType =4,
                                  MeanMed_or_CV = "MeanMed",Centrality_data = centr_data,Environmental_data = env_data)

colnames(EU_relat_Perm[[1]])[13:16] <- c("Betw_CV","Betw_Med","OutDeg_CV","OutDeg_Med")

# We apply this code to create a gam and the cooresponding plots with the data. It is not a function as the 
#the responde variables that are being used are different for each type of water
library(mgcv);library(car)
Coef_value <- c("coef_b","coef_q")
Out_Results_GAM <-list() 
for (dec_coef in 1:2) {
  Out_FW_type <- data.frame()
  pred_FW_type<- data.frame()
  GAM_out <- list()
  for (disper in 1:length(unique(EU_relat_Perm[[1]]$disp_plot))) {
    # We filter and select the data that we need for each dispersal group. 
    #- we log transform heavily skewed data. 
    data_to_gam<- EU_relat_Perm[[1]] %>% filter(disp_plot==unique(EU_relat_Perm[[1]]$disp_plot)[disper],
                                                 OutDeg_Med!=is.na(OutDeg_Med)) %>%
      dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,WatCov_Mean,precAn,elev)) %>%
      mutate(EcoRSize=log(EcoRSize+1),
             Betw_CV=log(Betw_CV+1))
    # We scale data to homogenize its strength on the patterns detected (strong differenced between response values)
    data_to_gam_scaled <- scale(data_to_gam[,9:ncol(data_to_gam)],center = T,scale = T)
    # We detect highly correlated variables and eliminate them from the run
    hc = findCorrelation(cor(data_to_gam_scaled, use="complete.obs", method="spearman"),
                         cutoff=0.80,names = F, exact = T)
    
    data_to_gam<- data_to_gam %>% dplyr::select(-c(colnames(data_to_gam_scaled)[hc]))
    #data_to_gam_scaled <- data_to_gam_scaled[,-hc] # No "hc" so we block this line
    data_to_gam[,colnames(data_to_gam_scaled)] <- data_to_gam_scaled 

    coef_val <- data_to_gam %>%  dplyr::select(c(coef_b,coef_q))
    data_to_gam <- data_to_gam %>%  dplyr::select(-c(coef_b,coef_q)) %>% 
                                    mutate("Coef"=coef_val[,dec_coef])
    # We trandform in positive the pattern to perform the gam and run it for the family "betareg". 
    if (Coef_value[dec_coef]=="coef_b") {data_to_gam$Coef <- data_to_gam$Coef*-1}
    # Gam per se
    b <- gam(Coef~
               #s(Long,k=3, bs="cr")+
               #s(Lat,k=3, bs="cr")+
               s(Betw_CV,k=3, bs="cr")+
               #s(Betw_Med,k=3, bs="cr")+
               s(OutDeg_CV,k=3, bs="cr")+
               #s(OutDeg_Med,k=3, bs="cr")+
               #s(WatCov_Mean,k=3, bs="cr")+
               s(WatCov_CV,k=3, bs="cr")+
               s(EcoRSize,k=3, bs="cr"),
             select=TRUE,method = "REML",family=betar(link="logit"), 
             data = data_to_gam)
    # We store the gam result
    out <- summary.gam(b)
    # We obrain the significant values
    sign_values <- rep("NoSign",length(out$s.pv))
    sign_values[which(out$s.pv<0.05)] <- "Sign"
    
    # We build up a table with the same information used in the gam and including the sign values (plot)
    Out_Table <- EU_relat_Perm[[1]] %>% filter(disp_plot==unique(EU_relat_Perm[[1]]$disp_plot)[disper],
                                                OutDeg_Med!=is.na(OutDeg_Med)) %>%
      dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,WatCov_Mean,precAn,elev)) %>% 
      mutate(EcoRSize=log(EcoRSize+1),
             Betw_CV=log(Betw_CV+1)) %>% 
      pivot_longer(cols=colnames(data_to_gam_scaled)) %>% 
      mutate(Sign_value=rep(sign_values,(length(unique(data_to_gam$EcoR)))))
    
    # Model prediction to later plot the relationships and the trends
    pred_Out <- data.frame()
    # We run the prediction for eahc significant variable
    for (col_noms in 1:length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])) {
      data_to_gam_scaled_depur <- data_to_gam_scaled[,colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]]
      
      Main_variable <- colnames(data_to_gam_scaled_depur)[col_noms]  
      Other_variables <- colnames(data_to_gam_scaled)[-which(colnames(data_to_gam_scaled)==Main_variable)]
      new_data <- with(data_to_gam, expand.grid("Main_variable" = seq(min(data_to_gam[,Main_variable]), 
                                                                      max(data_to_gam[,Main_variable]),
                                                                      length = nrow(data_to_gam)),
                                                "Var1" = median(data_to_gam[,Other_variables[1]]),
                                                "Var2"= median(data_to_gam[,Other_variables[2]]),
                                                "Var3" = median(data_to_gam[,Other_variables[3]])))
      colnames(new_data) <- c(Main_variable,Other_variables)
      
      # Using the dataset that we created (maintaining the significant variable fixed) we reproduce the values for the 
      #predicted gam and store them in a data frame to later plot them. 
      ilink <- family(b)$linkinv
      pred <- predict(b,new_data, type = "link", se.fit = TRUE)
      pred <- cbind(pred, new_data)
      pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),upr_ci = ilink(fit + (2 * se.fit)),fitted = ilink(fit))
      pred <- data.frame("Var"=Main_variable,"disp"=Out_Table$disp,"disp_plot"=Out_Table$disp_plot,pred,row.names = NULL)
      pred_Out <- bind_rows(pred_Out,pred)
    }
    # Output saving 
    pred_FW_type <- bind_rows(pred_FW_type,pred_Out)
    Out_FW_type <- bind_rows(Out_FW_type,Out_Table)
    GAM_out[[disper]] <-out
    names(GAM_out)[disper] <- unique(Out_Table$disp_plot)
    temp_list <- list("GAM_Result"=GAM_out,"Prediction"=pred_FW_type,"Raw_values"=Out_FW_type)
    Out_Results_GAM[[dec_coef]] <- temp_list
  }# Closing the dispersal
}# Closing coefficient
names(Out_Results_GAM) <- Coef_value
Perm_Out_Results_GAM <- Out_Results_GAM

#png(filename ="EU_relat_Perm.png",
#    width =750*7 ,height =300*7 ,units ="px",res = 300)
#grid.arrange(EU_relat_Perm[[2]],EU_relat_Perm[[3]],nrow=1)
#dev.off()

# Tempo FW relations ####
#Water type must be either 1-Total, 2-Ephemeral, 3-Temporary, 4-Permanent 
EU_relat_Tempo <- Plot_Relation_EU(Water_Type_List = Tempo_FW,WaterType =3,
                                  MeanMed_or_CV = "MeanMed",Centrality_data = centr_data,Environmental_data = env_data)

colnames(EU_relat_Tempo[[1]])[13:16] <- c("Betw_CV","Betw_Med","OutDeg_CV","OutDeg_Med")

# We apply this code to create a gam and the cooresponding plots with the data. It is not a function as the 
#the responde variables that are being used are different for each type of water
library(mgcv);library(car)
Coef_value <- c("coef_b","coef_q")
Out_Results_GAM <-list() 
for (dec_coef in 1:2) {
  Out_FW_type <- data.frame()
  pred_FW_type<- data.frame()
  GAM_out <- list()
  for (disper in 1:length(unique(EU_relat_Tempo[[1]]$disp_plot))) {
    # We filter and select the data that we need for each dispersal group. 
    #- we log transform heavily skewed data. 
    data_to_gam<- EU_relat_Tempo[[1]] %>% filter(disp_plot==unique(EU_relat_Tempo[[1]]$disp_plot)[disper],
                                                 OutDeg_Med!=is.na(OutDeg_Med)) %>%
      dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,WatCov_Mean,precAn,elev)) %>%
      mutate(EcoRSize=log(EcoRSize+1),
             Betw_CV=log(Betw_CV+1))
    # We scale data to homogenize its strength on the patterns detected (strong differenced between response values)
    data_to_gam_scaled <- scale(data_to_gam[,9:ncol(data_to_gam)],center = T,scale = T)
    # We detect highly correlated variables and eliminate them from the run
    hc = findCorrelation(cor(data_to_gam_scaled, use="complete.obs", method="spearman"),
                         cutoff=0.80,names = F, exact = T)
    
    data_to_gam<- data_to_gam %>% dplyr::select(-c(colnames(data_to_gam_scaled)[hc]))
    data_to_gam_scaled <- data_to_gam_scaled[,-hc] 
    data_to_gam[,colnames(data_to_gam_scaled)] <- data_to_gam_scaled 
    
    coef_val <- data_to_gam %>% dplyr::select(c(coef_b,coef_q))
    data_to_gam <- data_to_gam %>% dplyr::select(-c(coef_b,coef_q)) %>%mutate("Coef"=coef_val[,dec_coef])
    # We trandform in positive the pattern to perform the gam and run it for the family "betareg". 
    if (Coef_value[dec_coef]=="coef_b") {data_to_gam$Coef <- data_to_gam$Coef*-1}
    # Gam per se
    b <- gam(Coef~
               #s(Long,k=3, bs="cr")+
               #s(Lat,k=3, bs="cr")+
               s(Betw_CV,k=3, bs="cr")+
               #s(Betw_Med,k=3, bs="cr")+
               #s(OutDeg_CV,k=3, bs="cr")+
               #s(OutDeg_Med,k=3, bs="cr")+
               #s(WatCov_Mean,k=3, bs="cr")+
               s(WatCov_CV,k=3, bs="cr"),
               #s(EcoRSize,k=3, bs="cr"),
             select=TRUE,method = "REML",family=betar(link="logit"), 
             data = data_to_gam)
    # We store the gam result
    out <- summary.gam(b)
    # We obrain the significant values
    sign_values <- rep("NoSign",length(out$s.pv))
    sign_values[which(out$s.pv<0.05)] <- "Sign"
    
    # We build up a table with the same information used in the gam and including the sign values (plot)
    Out_Table <- EU_relat_Tempo[[1]] %>% filter(disp_plot==unique(EU_relat_Tempo[[1]]$disp_plot)[disper],
                                                OutDeg_Med!=is.na(OutDeg_Med)) %>% 
      dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,WatCov_Mean,precAn,elev)) %>% 
      mutate(EcoRSize=log(EcoRSize+1),
             Betw_CV=log(Betw_CV+1)) %>% 
      pivot_longer(cols=colnames(data_to_gam_scaled)) %>% 
      mutate(Sign_value=rep(sign_values,(length(unique(data_to_gam$EcoR)))))
    
    # Model prediction to later plot the relationships and the trends
    pred_Out <- data.frame()
    # We run the prediction for eahc significant variable
    for (col_noms in 1:length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])) {
      
      if (length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])==0) {cat("NoSign everywhere","\n")}else{
      data_to_gam_scaled_depur <- as.data.frame(data_to_gam_scaled[,colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]])
      colnames(data_to_gam_scaled_depur) <- colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]
      
      Main_variable <- colnames(data_to_gam_scaled_depur)[col_noms]  
      Other_variables <- colnames(data_to_gam_scaled)[-which(colnames(data_to_gam_scaled)==Main_variable)]
      new_data <- with(data_to_gam, expand.grid("Main_variable" = seq(min(data_to_gam[,Main_variable]), 
                                                                      max(data_to_gam[,Main_variable]),
                                                                      length = nrow(data_to_gam)),
                                                "Var1" = median(data_to_gam[,Other_variables[1]])))
      colnames(new_data) <- c(Main_variable,Other_variables)
      
      # Using the dataset that we created (maintaining the significant variable fixed) we reproduce the values for the 
      #predicted gam and store them in a data frame to later plot them. 
      ilink <- family(b)$linkinv
      pred <- predict(b,new_data, type = "link", se.fit = TRUE)
      pred <- cbind(pred, new_data)
      pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),upr_ci = ilink(fit + (2 * se.fit)),fitted = ilink(fit))
      pred <- data.frame("Var"=Main_variable,"disp"=Out_Table$disp,"disp_plot"=Out_Table$disp_plot,pred,row.names = NULL)
      pred_Out <- bind_rows(pred_Out,pred)
    }}
    # Output saving 
    pred_FW_type <- bind_rows(pred_FW_type,pred_Out)
    Out_FW_type <- bind_rows(Out_FW_type,Out_Table)
    GAM_out[[disper]] <-out
    names(GAM_out)[disper] <- unique(Out_Table$disp_plot)
    temp_list <- list("GAM_Result"=GAM_out,"Prediction"=pred_FW_type,"Raw_values"=Out_FW_type)
    Out_Results_GAM[[dec_coef]] <- temp_list
  }# Closing the dispersal
}# Closing coefficient
names(Out_Results_GAM) <- Coef_value
Tempo_Out_Results_GAM <- Out_Results_GAM

#png(filename ="EU_relat_Tempo.png",
#    width =750*7 ,height =300*7 ,units ="px",res = 300)
#grid.arrange(EU_relat_Tempo[[2]],EU_relat_Tempo[[3]],nrow=1)
#dev.off()

# Ephem FW relations ####
#Water type must be either 1-Total, 2-Ephemeral, 3-Temporary, 4-Permanent 
EU_relat_Ephem <- Plot_Relation_EU(Water_Type_List = Ephem_FW,WaterType =2,
                                  MeanMed_or_CV = "MeanMed",Centrality_data = centr_data,Environmental_data = env_data)
colnames(EU_relat_Ephem[[1]])[13:16] <- c("Betw_CV","Betw_Med","OutDeg_CV","OutDeg_Med")

# We apply this code to create a gam and the cooresponding plots with the data. It is not a function as the 
#the responde variables that are being used are different for each type of water
library(mgcv);library(car)
Coef_value <- c("coef_b","coef_q")
Out_Results_GAM <-list() 
for (dec_coef in 1:2) {
  Out_FW_type <- data.frame()
  pred_FW_type<- data.frame()
  GAM_out <- list()
  for (disper in 1:length(unique(EU_relat_Ephem[[1]]$disp_plot))) {
    # We filter and select the data that we need for each dispersal group. 
    #- we log transform heavily skewed data. 
    data_to_gam<- EU_relat_Ephem[[1]] %>% filter(disp_plot==unique(EU_relat_Ephem[[1]]$disp_plot)[disper],
                                                 OutDeg_Med!=is.na(OutDeg_Med)) %>% 
      dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,WatCov_Mean,precAn,elev)) %>%
      mutate(EcoRSize=log(EcoRSize+1),
             Betw_CV=log(Betw_CV+1))
    # We scale data to homogenize its strength on the patterns detected (strong differenced between response values)
    data_to_gam_scaled <- scale(data_to_gam[,9:ncol(data_to_gam)],center = T,scale = T)
    # We detect highly correlated variables and eliminate them from the run
    hc = findCorrelation(cor(data_to_gam_scaled, use="complete.obs", method="spearman"),
                         cutoff=0.80,names = F, exact = T)
    
    data_to_gam<- data_to_gam %>% dplyr::select(-c(colnames(data_to_gam_scaled)[hc]))
    data_to_gam_scaled <- data_to_gam_scaled[,-hc] 
    data_to_gam[,colnames(data_to_gam_scaled)] <- data_to_gam_scaled 
    
    coef_val <- data_to_gam %>% dplyr::select(c(coef_b,coef_q))
    data_to_gam <- data_to_gam %>% dplyr::select(-c(coef_b,coef_q)) %>% 
      mutate("Coef"=coef_val[,dec_coef])
    # We trandform in positive the pattern to perform the gam and run it for the family "betareg". 
    if (Coef_value[dec_coef]=="coef_b") {data_to_gam$Coef <- data_to_gam$Coef*-1}
    # Gam per se
    b <- gam(Coef~
               #s(Long,k=3, bs="cr")+
               #s(Lat,k=3, bs="cr")+
               s(Betw_CV,k=3, bs="cr")+
               #s(Betw_Med,k=3, bs="cr")+
               #s(OutDeg_CV,k=3, bs="cr")+
               #s(OutDeg_Med,k=3, bs="cr")+
               #s(WatCov_Mean,k=3, bs="cr")+
               s(WatCov_CV,k=3, bs="cr")+
               s(EcoRSize,k=3, bs="cr"),
             select=TRUE,method = "REML",family=betar(link="logit"), 
             data = data_to_gam)
    # We store the gam result
    out <- summary.gam(b)
    # We obrain the significant values
    sign_values <- rep("NoSign",length(out$s.pv))
    sign_values[which(out$s.pv<0.05)] <- "Sign"
    
    # We build up a table with the same information used in the gam and including the sign values (plot)
    Out_Table <- EU_relat_Ephem[[1]] %>% filter(disp_plot==unique(EU_relat_Ephem[[1]]$disp_plot)[disper],
                                                OutDeg_Med!=is.na(OutDeg_Med)) %>% 
      dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,WatCov_Mean,precAn,elev)) %>% 
      mutate(EcoRSize=log(EcoRSize+1),
             Betw_CV=log(Betw_CV+1)) %>% 
      pivot_longer(cols=colnames(data_to_gam_scaled)) %>% 
      mutate(Sign_value=rep(sign_values,(length(unique(data_to_gam$EcoR)))))
    
    # Model prediction to later plot the relationships and the trends
    pred_Out <- data.frame()
    # We run the prediction for eahc significant variable
    for (col_noms in 1:length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])) {
      if (length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])==0) {cat("NoSign everywhere","\n")}else{
        data_to_gam_scaled_depur <- as.data.frame(data_to_gam_scaled[,colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]])
        colnames(data_to_gam_scaled_depur) <- colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]
      
      Main_variable <- colnames(data_to_gam_scaled_depur)[col_noms]  
      Other_variables <- colnames(data_to_gam_scaled)[-which(colnames(data_to_gam_scaled)==Main_variable)]
      new_data <- with(data_to_gam, expand.grid("Main_variable" = seq(min(data_to_gam[,Main_variable]), 
                                                                      max(data_to_gam[,Main_variable]),
                                                                      length = nrow(data_to_gam)),
                                                "Var1" = median(data_to_gam[,Other_variables[1]]),
                                                "Var2"= median(data_to_gam[,Other_variables[2]])))
      colnames(new_data) <- c(Main_variable,Other_variables)
      
      # Using the dataset that we created (maintaining the significant variable fixed) we reproduce the values for the 
      #predicted gam and store them in a data frame to later plot them. 
      ilink <- family(b)$linkinv
      pred <- predict(b,new_data, type = "link", se.fit = TRUE)
      pred <- cbind(pred, new_data)
      pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),upr_ci = ilink(fit + (2 * se.fit)),fitted = ilink(fit))
      pred <- data.frame("Var"=Main_variable,"disp"=Out_Table$disp,"disp_plot"=Out_Table$disp_plot,pred,row.names = NULL)
      pred_Out <- bind_rows(pred_Out,pred)
      }}
    # Output saving 
    pred_FW_type <- bind_rows(pred_FW_type,pred_Out)
    Out_FW_type <- bind_rows(Out_FW_type,Out_Table)
    GAM_out[[disper]] <-out
    names(GAM_out)[disper] <- unique(Out_Table$disp_plot)
    temp_list <- list("GAM_Result"=GAM_out,"Prediction"=pred_FW_type,"Raw_values"=Out_FW_type)
    Out_Results_GAM[[dec_coef]] <- temp_list
  }# Closing the dispersal
}# Closing coefficient
names(Out_Results_GAM) <- Coef_value
Ephem_Out_Results_GAM <- Out_Results_GAM

#png(filename ="EU_relat_Ephem.png",
#    width =750*7 ,height =300*7 ,units ="px",res = 300)
#grid.arrange(EU_relat_Ephem[[2]],EU_relat_Ephem[[3]],nrow=1)
#dev.off()

# OUTPUT managing and gam values/tables extraction ####
a <- data.frame("Type_System"=NA,"Parameter"=NA,"Var"=NA,"disp"=NA,"disp_plot"=NA,"fit"=NA,"se.fit"=NA,
  "Long"=NA,"Lat"=NA,"Betw_Med"=NA, "Betw_CV"=NA,"OutDeg_Med"=NA, "OutDeg_CV"=NA,"WatCov_Mean"=NA,"WatCov_CV"=NA,"EcoRSize"=NA,
  "lwr_ci"=NA,"upr_ci"=NA,"fitted"=NA)

Gam_Output <- bind_rows(a,
# Total
data.frame(Type_System= "Total freshwaters",
           Parameter="Proportional decay rate (b)",
           TOTAL_Out_Results_GAM$coef_b$Prediction),
data.frame(Type_System= "Total freshwaters",
           Parameter="Collapsing rate (q)",
           TOTAL_Out_Results_GAM$coef_q$Prediction),
# Permanent
data.frame(Type_System= "Permanent freshwaters",
           Parameter="Proportional decay rate (b)",
           Perm_Out_Results_GAM$coef_b$Prediction),
data.frame(Type_System= "Permanent freshwaters",
           Parameter="Collapsing rate (q)",
           Perm_Out_Results_GAM$coef_q$Prediction),
# Temporary
data.frame(Type_System= "Temporary freshwaters",
           Parameter="Proportional decay rate (b)",
           Tempo_Out_Results_GAM$coef_b$Prediction),
data.frame(Type_System= "Temporary freshwaters",
           Parameter="Collapsing rate (q)",
           Tempo_Out_Results_GAM$coef_q$Prediction),
# Ephemeral
data.frame(Type_System= "Ephemeral freshwaters",
           Parameter="Proportional decay rate (b)",
           Ephem_Out_Results_GAM$coef_b$Prediction),
data.frame(Type_System= "Ephemeral freshwaters",
           Parameter="Collapsing rate (q)",
           Ephem_Out_Results_GAM$coef_q$Prediction)
)
Gam_Output <- Gam_Output %>% filter(is.na(Type_System)!=T)

Gam_Output <- Gam_Output %>% mutate(Type_System=factor(Type_System, 
levels=c("Total freshwaters","Permanent freshwaters","Temporary freshwaters","Ephemeral freshwaters")))

# Figure 4 supplementary ####
png(filename ="GAM_final_complet.png",width =550*5 ,height =1050*5 ,units ="px",res = 300)
gridExtra::grid.arrange(
  Gam_Output %>% filter(Parameter=="Proportional decay rate (b)") %>%  
    mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Gam_Output$Var)) %>% 
    mutate(Plot_YesNo=ifelse(Var==name,1,0)) %>% 
    filter(Plot_YesNo==1) %>% 
    mutate(name=factor(name, 
      levels=c("Betw_CV","OutDeg_CV","WatCov_CV","EcoRSize"))) %>% 
    ggplot(aes(x=value, y = -fitted)) +
    geom_ribbon(aes(ymin = -lwr_ci, ymax = -upr_ci, fill=disp_plot), alpha = 0.1)+
    geom_line(aes(colour=disp_plot))+
    scale_color_viridis(discrete = T)+
    scale_fill_viridis(discrete = T)+
    labs(fill="Dispersal ability",colour="Dispersal ability")+
    scale_y_continuous(breaks = c(0,-0.5,-1))+
    theme_classic()+labs(y="Proportional decay rate (b)",x="")+
    theme(panel.background = element_rect(colour = "grey50"),
          panel.grid.major.x = element_line(colour = "grey70"))+
    facet_grid(name~Type_System,scales = "free_x"),
  
  Gam_Output %>% filter(Parameter=="Collapsing rate (q)") %>% 
    mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Gam_Output$Var)) %>% 
    mutate(Plot_YesNo=ifelse(Var==name,1,0)) %>% 
    filter(Plot_YesNo==1) %>% 
    mutate(name=factor(name, 
    levels=c("Betw_CV","OutDeg_CV","WatCov_CV","EcoRSize"))) %>% 
    ggplot(aes(x=value, y = fitted)) +
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci,fill=disp_plot), alpha = 0.1)+
    geom_line(aes(colour=disp_plot))+
    scale_color_viridis(discrete = T)+
    scale_fill_viridis(discrete = T)+
    labs(fill="Dispersal ability",colour="Dispersal ability")+
    #scale_y_continuous()+
    scale_y_reverse(breaks = c(0.275,0.312,0.350))+
    theme_classic()+labs(y="Collapsing rate (q)",x="")+
    theme(panel.background = element_rect(colour = "grey50"),
          panel.grid.major.x = element_line(colour = "grey70"))+
    facet_grid(name~Type_System,scales = "free_x"),
  nrow=2)
dev.off()

# Figure 4 ####
# Total freshwater significant values (manual selection)
# for b 
select_variables <- c("OutDeg_CV","WatCov_CV","EcoRSize")
name_select_variables <- c("Out degree CV","Water Cover CV","Ecoregion size")
select_plotList <- list()
for (sel_vari in 1:length(select_variables)) {
select_plotList[[sel_vari]] <- Gam_Output %>% 
  filter(Parameter=="Proportional decay rate (b)",Type_System=="Total freshwaters") %>%  
  mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
  pivot_longer(cols = unique(Gam_Output$Var)) %>% 
  filter(Var==select_variables[sel_vari],name==select_variables[sel_vari]) %>% 
  ggplot(aes(x=value, y = -fitted))+
  geom_ribbon(aes(ymin = -lwr_ci, ymax = -upr_ci,fill=disp_plot), alpha = 0.1)+
  geom_line(aes(color=disp_plot))+
  scale_color_viridis(discrete = T)+
  scale_fill_viridis(discrete = T)+
  labs(fill="Dispersal ability",colour="Dispersal ability",subtitle=name_select_variables[sel_vari])+
  theme_classic()+labs(y="Proportional decay rate (b)",x="")+
  theme(panel.background = element_rect(colour = "grey50"),
        panel.grid.major.x = element_line(colour = "grey70"),
        axis.title.y = element_text(colour = "#b30000"))
}
leg_plot <- get_legend(select_plotList[[1]])
blank <- grid::grid.rect(gp=grid::gpar(col="white"))
b_plot <- gridExtra::arrangeGrob(arrangeGrob(select_plotList[[1]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[2]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[3]]+theme(legend.position = "none")),
                                 ncol=3)

select_plotList <- list()
for (sel_vari in 1:length(select_variables)) {
  select_plotList[[sel_vari]] <- Gam_Output %>% 
    filter(Parameter=="Collapsing rate (q)",Type_System=="Total freshwaters") %>%  
    mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Gam_Output$Var)) %>% 
    filter(Var==select_variables[sel_vari],name==select_variables[sel_vari]) %>% 
    ggplot(aes(x=value, y = fitted))+
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci,fill=disp_plot), alpha = 0.1)+
    geom_line(aes(color=disp_plot))+
    scale_y_reverse()+
    scale_color_viridis(discrete = T)+
    scale_fill_viridis(discrete = T)+
    labs(fill="Dispersal ability",colour="Dispersal ability",subtitle=name_select_variables[sel_vari])+
    theme_classic()+labs(y="Collapsing rate (q)",x="")+
    theme(panel.background = element_rect(colour = "grey50"),
          panel.grid.major.x = element_line(colour = "grey70"),
          axis.title.y = element_text(colour = "#8A2BE2"))
}
leg_plot <- get_legend(select_plotList[[1]])
blank <- grid::grid.rect(gp=grid::gpar(col="white"))
q_plot <- gridExtra::arrangeGrob(arrangeGrob(select_plotList[[1]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[2]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[3]]+theme(legend.position = "none")),
                                 ncol=3)

Total_Fresh<- arrangeGrob(b_plot,q_plot,nrow=2,
                          top=grid::textGrob("D) Total freshwaters",
                                            gp=grid::gpar(fontsize=20,font=2)))

# Permanent freshwater significant values (manual selection)
# for b 
select_variables <- c("Betw_CV","OutDeg_CV","WatCov_CV")
name_select_variables <- c("Betweenness CV","Out degree CV","Water Cover CV")
select_plotList <- list()
for (sel_vari in 1:length(select_variables)) {
  select_plotList[[sel_vari]] <- Gam_Output %>% 
    filter(Parameter=="Proportional decay rate (b)",Type_System=="Permanent freshwaters") %>%  
    mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Gam_Output$Var)) %>% 
    filter(Var==select_variables[sel_vari],name==select_variables[sel_vari]) %>% 
    ggplot(aes(x=value, y = -fitted))+
    geom_ribbon(aes(ymin = -lwr_ci, ymax = -upr_ci,fill=disp_plot), alpha = 0.1)+
    geom_line(aes(color=disp_plot))+
    scale_color_viridis(discrete = T)+
    scale_fill_viridis(discrete = T)+
    labs(fill="Dispersal ability",colour="Dispersal ability",subtitle=name_select_variables[sel_vari])+
    theme_classic()+labs(y="Proportional decay rate (b)",x="")+
    theme(panel.background = element_rect(colour = "grey50"),
          panel.grid.major.x = element_line(colour = "grey70"),
          axis.title.y = element_text(colour = "#b30000"))
}
leg_plot <- get_legend(select_plotList[[1]])
blank <- grid::grid.rect(gp=grid::gpar(col="white"))
b_plot <- gridExtra::arrangeGrob(arrangeGrob(select_plotList[[1]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[2]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[3]]+theme(legend.position = "none")),
                                 ncol=3)

select_variables <- c("Betw_CV","OutDeg_CV","WatCov_CV")
name_select_variables <- c("Betweenness CV","Out degree CV","Water Cover CV")
select_plotList <- list()
for (sel_vari in 1:length(select_variables)) {
  select_plotList[[sel_vari]] <- Gam_Output %>% 
    filter(Parameter=="Collapsing rate (q)",Type_System=="Permanent freshwaters") %>%  
    mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Gam_Output$Var)) %>% 
    filter(Var==select_variables[sel_vari],name==select_variables[sel_vari]) %>% 
    ggplot(aes(x=value, y = fitted))+
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci,fill=disp_plot), alpha = 0.1)+
    geom_line(aes(color=disp_plot))+
    scale_y_reverse()+
    scale_color_viridis(discrete = T)+
    scale_fill_viridis(discrete = T)+
    labs(fill="Dispersal ability",colour="Dispersal ability",subtitle=name_select_variables[sel_vari])+
    theme_classic()+labs(y="Collapsing rate (q)",x="")+
    theme(panel.background = element_rect(colour = "grey50"),
          panel.grid.major.x = element_line(colour = "grey70"),
          axis.title.y = element_text(colour = "#8A2BE2"))
    
}
leg_plot <- get_legend(select_plotList[[1]])
blank <- grid::grid.rect(gp=grid::gpar(col="white"))
q_plot <- gridExtra::arrangeGrob(arrangeGrob(select_plotList[[1]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[2]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[3]]+theme(legend.position = "none")),
                                 ncol=3)

Perm_Fresh<- arrangeGrob(b_plot,q_plot,nrow=2,
                         top=grid::textGrob("A) Permanent freshwaters",
                                            gp=grid::gpar(fontsize=20,font=2)))


# Temporary freshwater significant values (manual selection)
# for b 
select_variables <- c("Betw_CV","WatCov_CV")
name_select_variables <- c("Betweenness CV","Water Cover CV")
select_plotList <- list()
for (sel_vari in 1:length(select_variables)) {
  select_plotList[[sel_vari]] <- Gam_Output %>% 
    filter(Parameter=="Proportional decay rate (b)",Type_System=="Temporary freshwaters") %>%  
    mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Gam_Output$Var)) %>% 
    filter(Var==select_variables[sel_vari],name==select_variables[sel_vari]) %>% 
    ggplot(aes(x=value, y = -fitted))+
    geom_ribbon(aes(ymin = -lwr_ci, ymax = -upr_ci,fill=disp_plot), alpha = 0.1)+
    geom_line(aes(color=disp_plot))+
    scale_color_viridis(discrete = T)+
    scale_fill_viridis(discrete = T)+
    labs(fill="Dispersal ability",colour="Dispersal ability",subtitle=name_select_variables[sel_vari])+
    theme_classic()+labs(y="Proportional decay rate (b)",x="")+
    theme(panel.background = element_rect(colour = "grey50"),
          panel.grid.major.x = element_line(colour = "grey70"),
          axis.title.y = element_text(colour = "#b30000"))
}
leg_plot <- get_legend(select_plotList[[1]])
blank <- grid::grid.rect(gp=grid::gpar(col="white"))
b_plot <- gridExtra::arrangeGrob(arrangeGrob(select_plotList[[1]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[2]]+theme(legend.position = "none")),
                                 blank,
                                 ncol=3)

select_variables <- c("Betw_CV","WatCov_CV")
name_select_variables <- c("Betweenness CV","Water Cover CV")
select_plotList <- list()
for (sel_vari in 1:length(select_variables)) {
  select_plotList[[sel_vari]] <- Gam_Output %>% 
    filter(Parameter=="Collapsing rate (q)",Type_System=="Temporary freshwaters") %>%  
    mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Gam_Output$Var)) %>% 
    filter(Var==select_variables[sel_vari],name==select_variables[sel_vari]) %>% 
    ggplot(aes(x=value, y = fitted))+
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci,fill=disp_plot), alpha = 0.1)+
    geom_line(aes(color=disp_plot))+
    scale_y_reverse()+
    scale_color_viridis(discrete = T)+
    scale_fill_viridis(discrete = T)+
    labs(fill="Dispersal ability",colour="Dispersal ability",subtitle=name_select_variables[sel_vari])+
    theme_classic()+labs(y="Collapsing rate (q)",x="")+
    theme(panel.background = element_rect(colour = "grey50"),
          panel.grid.major.x = element_line(colour = "grey70"),
          axis.title.y = element_text(colour = "#8A2BE2"))
  
}
leg_plot <- get_legend(select_plotList[[1]])
blank <- grid::grid.rect(gp=grid::gpar(col="white"))
q_plot <- gridExtra::arrangeGrob(arrangeGrob(select_plotList[[1]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[2]]+theme(legend.position = "none")),
                                 blank,
                                 ncol=3)

Temp_Fresh<- arrangeGrob(b_plot,q_plot,nrow=2,
                         top=grid::textGrob("B) Temporary freshwaters",
                                            gp=grid::gpar(fontsize=20,font=2)))

# Ephemeral freshwater significant values (manual selection)
# for b 
select_variables <- c("Betw_CV","WatCov_CV")
name_select_variables <- c("Betweenness CV","Water Cover CV")
select_plotList <- list()
for (sel_vari in 1:length(select_variables)) {
  select_plotList[[sel_vari]] <- Gam_Output %>% 
    filter(Parameter=="Proportional decay rate (b)",Type_System=="Ephemeral freshwaters") %>%  
    mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Gam_Output$Var)) %>% 
    filter(Var==select_variables[sel_vari],name==select_variables[sel_vari]) %>% 
    ggplot(aes(x=value, y = -fitted))+
    geom_ribbon(aes(ymin = -lwr_ci, ymax = -upr_ci,fill=disp_plot), alpha = 0.1)+
    geom_line(aes(color=disp_plot))+
    scale_color_viridis(discrete = T)+
    scale_fill_viridis(discrete = T)+
    labs(fill="Dispersal ability",colour="Dispersal ability",subtitle=name_select_variables[sel_vari])+
    theme_classic()+labs(y="Proportional decay rate (b)",x="")+
    theme(panel.background = element_rect(colour = "grey50"),
          panel.grid.major.x = element_line(colour = "grey70"),
          axis.title.y = element_text(colour = "#b30000"))
}
leg_plot <- get_legend(select_plotList[[1]])
blank <- grid::grid.rect(gp=grid::gpar(col="white"))
b_plot <- gridExtra::arrangeGrob(arrangeGrob(select_plotList[[1]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[2]]+theme(legend.position = "none")),
                                 blank,
                                 ncol=3)

select_variables <- c("WatCov_CV","EcoRSize")
name_select_variables <- c("Water Cover CV","Ecoregion size")
select_plotList <- list()
for (sel_vari in 1:length(select_variables)) {
  select_plotList[[sel_vari]] <- Gam_Output %>% 
    filter(Parameter=="Collapsing rate (q)",Type_System=="Ephemeral freshwaters") %>%  
    mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Gam_Output$Var)) %>% 
    filter(Var==select_variables[sel_vari],name==select_variables[sel_vari]) %>% 
    ggplot(aes(x=value, y = fitted))+
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci,fill=disp_plot), alpha = 0.1)+
    geom_line(aes(color=disp_plot))+
    scale_y_reverse()+
    scale_color_viridis(discrete = T)+
    scale_fill_viridis(discrete = T)+
    labs(fill="Dispersal ability",colour="Dispersal ability",subtitle=name_select_variables[sel_vari])+
    theme_classic()+labs(y="Collapsing rate (q)",x="")+
    theme(panel.background = element_rect(colour = "grey50"),
          panel.grid.major.x = element_line(colour = "grey70"),
          axis.title.y = element_text(colour = "#8A2BE2"))
  
}
leg_plot <- get_legend(select_plotList[[1]]+theme(legend.direction = "horizontal",legend.position = "top"))
blank <- grid::grid.rect(gp=grid::gpar(col="white"))
q_plot <- gridExtra::arrangeGrob(arrangeGrob(select_plotList[[1]]+theme(legend.position = "none")),
                                 arrangeGrob(select_plotList[[2]]+theme(legend.position = "none")),
                                 blank,
                                 ncol=3)

Ephem_Fresh<- arrangeGrob(b_plot,q_plot,nrow=2,
                          top=grid::textGrob("C) Ephemeral freshwaters",
                                             gp=grid::gpar(fontsize=20,font=2)))

png(filename ="GAM_Figure4.png",width =550*5 ,height =1050*7 ,units ="px",res = 300)
grid.arrange(Perm_Fresh,Temp_Fresh,Ephem_Fresh,Total_Fresh,leg_plot,nrow=5)
dev.off()






# Supplemenetary 7 table with Rsqrd 
Total_list_RESULTS <- list("Total"=TOTAL_Out_Results_GAM,"Permanent"=Perm_Out_Results_GAM,
                           "Temporary"=Tempo_Out_Results_GAM,"Ephemeral"=Ephem_Out_Results_GAM)
Gam_RESULTS_Table <- data.frame()
for (typ_fre in 1:length(Total_list_RESULTS)) {
temp_out <- bind_rows(
  data.frame("Freshwater type"=names(Total_list_RESULTS)[[typ_fre]],
             "Diversity decay parameter"="Proportional decay rate (b)",
             "Dispersal"=names(Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result),
             "Adj.R2"=round(rbind(
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`0.001 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`0.1 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`0.5 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`1 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`2 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`5 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`10 km`$r.sq),3),
             "Dev.Expl"=round(rbind(
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`0.001 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`0.1 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`0.5 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`1 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`2 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`5 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result$`10 km`$dev.expl),3)),
  data.frame("Freshwater type"=names(Total_list_RESULTS)[[typ_fre]],
             "Diversity decay parameter"="Collapsing rate (q)",
             "Dispersal"=names(Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result),
             "Adj.R2"=round(rbind(
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`0.001 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`0.1 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`0.5 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`1 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`2 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`5 km`$r.sq,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`10 km`$r.sq),3),
             "Dev.Expl"=round(rbind(
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`0.001 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`0.1 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`0.5 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`1 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`2 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`5 km`$dev.expl,
               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result$`10 km`$dev.expl),3))
)

Gam_RESULTS_Table <- bind_rows(Gam_RESULTS_Table,temp_out)
}

write.table(Gam_RESULTS_Table,file = "Gam_RESULTS_Table.txt")

Gam_RESULTS_Table %>% mutate(Dispersal=factor(Dispersal, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
  mutate(Freshwater.type=factor(Freshwater.type, levels=c("Total","Permanent","Temporary","Ephemeral"))) %>% 
  mutate(Diversity.decay.parameter=factor(Diversity.decay.parameter, levels=c("Proportional decay rate (b)","Collapsing rate (q)"))) %>% 
  pivot_longer(cols = 4:5) %>% 
  ggplot()+geom_bar(aes(y=value, x=Dispersal,fill=Dispersal), stat = "identity")+
  scale_y_continuous(limits = c(0,1))+
  facet_grid(Freshwater.type~name*Diversity.decay.parameter)+scale_fill_viridis(discrete = T)

Gam_RESULTS_Table %>% group_by(Freshwater.type,Diversity.decay.parameter) %>% summarise(Mean=mean(Adj.R2))

# Supplementary material 8 table 
Gam_RESULTS_TOTAL_Table <- data.frame()
for (typ_fre in 1:length(Total_list_RESULTS)) {
disp_Output <- data.frame()
for (disp in 1:7) {
tempo_out <- bind_rows(
data.frame(
  "Freshwater type"=names(Total_list_RESULTS)[[typ_fre]],
  "Diversity decay parameter"="Proportional decay rate (b)",
  "Dispersal"=names(Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result)[[disp]],
  "Descriptors"=rownames(rbind(Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result[[disp]]$p.table,
                               Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result[[disp]]$s.table)),
  rbind(Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result[[disp]]$p.table,
        Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result[[disp]]$s.table)),
data.frame(
  "Freshwater type"=names(Total_list_RESULTS)[[typ_fre]],
  "Diversity decay parameter"="Collapsing rate (q)",
  "Dispersal"=names(Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result)[[disp]],
  "Descriptors"=rownames(rbind(Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result[[disp]]$p.table,
                               Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result[[disp]]$s.table)),
  rbind(Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result[[disp]]$p.table,
        Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result[[disp]]$s.table))
)
disp_Output <- bind_rows(disp_Output,tempo_out)
}

Gam_RESULTS_TOTAL_Table <- bind_rows(Gam_RESULTS_TOTAL_Table,disp_Output)
}

write.table(Gam_RESULTS_TOTAL_Table,file = "Gam_RESULTS_TOTAL_Table.txt",row.names = F)


# OLD Individual plots for freshwater types ####

# Plot small values, 
Out_Results_GAM$coef_b$Raw_values %>%
  group_by(disp_plot,Sign_value,name) %>% 
  summarise(Mean_coef=mean(coef_b),SD_coef=sd(coef_b)) %>%  
  mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
  mutate(name=factor(name,levels=c("Long","Lat","Betw_Med", "Betw_CV","OutDeg_Med", "OutDeg_CV","WatCov_Mean","WatCov_CV","EcoRSize"))) %>% 
  ggplot()+
  geom_errorbar(aes(y=Mean_coef,x=disp_plot,ymin=Mean_coef-SD_coef,ymax=Mean_coef+SD_coef),colour="grey80",alpha=0.7)+
  geom_point(aes(y=Mean_coef,x=disp_plot, fill=disp_plot, alpha=Sign_value), 
             colour="grey20",size=5,shape=21) +
  scale_alpha_manual(values = c(0.2,0.8))+
  scale_fill_viridis(discrete = T)+
  facet_grid(.~name)+
  labs(y="coef_b")+
  theme_classic()+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())

Out_Results_GAM$coef_q$Raw_values %>%
  group_by(disp_plot,Sign_value,name) %>% 
  summarise(Mean_coef=mean(coef_b),SD_coef=sd(coef_b)) %>%  
  mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
  mutate(name=factor(name,levels=c("Long","Lat","Betw_Med", "Betw_CV","OutDeg_Med", "OutDeg_CV","WatCov_Mean","WatCov_CV","EcoRSize"))) %>% 
  ggplot()+
  geom_errorbar(aes(y=Mean_coef,x=disp_plot,ymin=Mean_coef-SD_coef,ymax=Mean_coef+SD_coef),colour="grey80",alpha=0.7)+
  geom_point(aes(y=Mean_coef,x=disp_plot, fill=disp_plot, alpha=Sign_value), 
             colour="grey20",size=5,shape=21) +
  scale_alpha_manual(values = c(0.2,0.8))+
  scale_fill_viridis(discrete = T)+
  facet_grid(.~name)+
  labs(y="coef_q")+
  theme_classic()+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
# Final plots of the trends
gridExtra::grid.arrange(
  Out_Results_GAM$coef_b$Prediction %>% 
    mutate(disp_plot=factor(disp_plot, levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Out_Results_GAM$coef_b$Prediction$Var)) %>% 
    mutate(Plot_YesNo=ifelse(Var==name,1,0)) %>% 
    filter(Plot_YesNo==1) %>% 
    mutate(name=factor(name, levels=c("Long","Lat","Betw_Med", "Betw_CV","OutDeg_Med", "OutDeg_CV","WatCov_Mean","WatCov_CV","EcoRSize"))) %>% 
    ggplot(aes(x=value, y = -fitted)) +
    geom_ribbon(aes(ymin = -lwr_ci, ymax = -upr_ci,fill=disp_plot), alpha = 0.05)+
    geom_line(aes(colour=disp_plot))+
    scale_color_viridis(discrete = T)+  scale_fill_viridis(discrete = T)+
    theme_classic()+labs(y="coef_b",x="")+
    facet_grid(.~name,scales="free"),
  
  Out_Results_GAM$coef_q$Prediction %>% 
    mutate(disp_plot=factor(disp_plot, 
                            levels=c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))) %>% 
    pivot_longer(cols = unique(Out_Results_GAM$coef_q$Prediction$Var)) %>% 
    mutate(Plot_YesNo=ifelse(Var==name,1,0)) %>% 
    filter(Plot_YesNo==1) %>% 
    mutate(name=factor(name, 
                       levels=c("Long","Lat","Betw_Med", "Betw_CV","OutDeg_Med", "OutDeg_CV","WatCov_Mean","WatCov_CV","EcoRSize"))) %>% 
    ggplot(aes(x=value, y = fitted)) +
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci,fill=disp_plot), alpha = 0.05)+
    geom_line(aes(colour=disp_plot))+
    scale_color_viridis(discrete = T)+  scale_fill_viridis(discrete = T)+
    theme_classic()+labs(y="coef_q",x="")+
    facet_grid(.~name,scales="free"),
  nrow=2)

### OLD THINGS ####
# Plot altogether - FinalAmazingPlot ####
color <- viridis(length(unique(gam_Res_TOTAL_b$disp)))

# merge b values
gam_Res_plot_b <- gam_Res_TOTAL_b %>% mutate(Syst_Type="Total") %>% 
  bind_rows(gam_Res_Perm_b %>% mutate(Syst_Type="Perm")) %>%
  bind_rows(gam_Res_Tempo_b %>% mutate(Syst_Type="Tempo")) %>% 
  bind_rows(gam_Res_Ephem_b %>% mutate(Syst_Type="Ephem"))%>% 
  mutate(TOTAL_perc_ExplDev=TOTAL_ExplDev *100) 

gam_Res_plot_b$Syst_Type <- factor(gam_Res_plot_b$Syst_Type, 
                                   levels = c("Total","Perm","Tempo","Ephem"))
gam_Res_plot_b$disp_plot <- factor(gam_Res_plot_b$disp_plot, 
                                   levels = c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))

# merge q values
gam_Res_plot_q <- gam_Res_TOTAL_q %>% mutate(Syst_Type="Total") %>% 
  bind_rows(gam_Res_Perm_q %>% mutate(Syst_Type="Perm")) %>%
  bind_rows(gam_Res_Tempo_q %>% mutate(Syst_Type="Tempo")) %>% 
  bind_rows(gam_Res_Ephem_q %>% mutate(Syst_Type="Ephem")) %>% 
  mutate(TOTAL_perc_ExplDev=TOTAL_ExplDev *100) 

gam_Res_plot_q$Syst_Type <- factor(gam_Res_plot_q$Syst_Type, 
                                   levels = c("Total","Perm","Tempo","Ephem"))
gam_Res_plot_q$disp_plot <- factor(gam_Res_plot_q$disp_plot, 
                                   levels = c("0.001 km","0.1 km","0.5 km","1 km","2 km","5 km","10 km"))

png(filename ="EU_gam_relat.png",
    width =700*7 ,height =300*7 ,units ="px",res = 300)
grid.arrange(
  gam_Res_plot_b%>%ggplot(aes(x=Pred,y=ExplDev))+
    #geom_violin(aes(color=Syst_Type), alpha=0.2, size=0.5, fill="grey95")+
    geom_rect(xmin = -Inf, xmax = 4.5,   ymin = -Inf, ymax = 100,   fill = "#d9f2d9")+
    geom_rect(xmin = 4.5, xmax = 8.5,   ymin = -Inf, ymax = 100,   fill = "#ecd9c6")+
    geom_rect(xmin = 8.5, xmax = +Inf,   ymin = -Inf, ymax = 100,   fill = "#cce6ff")+
    geom_jitter(aes(fill=disp_plot), alpha=0.5,shape=21,size=3,width = 0.2)+
    scale_fill_manual(values=color)+
    scale_alpha_manual(values=c(0.1,0.8))+
    scale_colour_manual(values=c("grey55","#9191F0","#5E92F2","#6CB8A0"))+
    labs(fill="Dipsersal ability",title="Explained Deviation - Proportional decay rate (b)", color="FW type")+
    xlab("")+ylab("Relative Explained Deviance (%)")+
    theme_classic()+theme(axis.text.x = element_text(angle = 45,hjust = 1))+
    facet_grid(.~Syst_Type)+
    guides(alpha="none",color="none"),
  
  gam_Res_plot_q%>%ggplot(aes(x=Pred,y=ExplDev))+
    #geom_violin(aes(color=Syst_Type), alpha=0.2, size=0.5, fill="grey95")+
    geom_rect(xmin = -Inf, xmax = 6.5,   ymin = -Inf, ymax = 100,   fill = "#d9f2d9")+
    geom_rect(xmin = 6.5, xmax = 10.5,   ymin = -Inf, ymax = 100,   fill = "#ecd9c6")+
    geom_rect(xmin = 10.5, xmax = +Inf,   ymin = -Inf, ymax = 100,   fill = "#cce6ff")+
    geom_jitter(aes(fill=disp_plot), alpha=0.5,shape=21,size=3,width = 0.2)+
    scale_fill_manual(values=color)+
    scale_alpha_manual(values=c(0.1,0.8))+
    scale_colour_manual(values=c("grey55","#9191F0","#5E92F2","#6CB8A0"))+
    labs(fill="Dipsersal ability",title="Explained Deviation -Collapsing rate (q)", color="FW type")+
    xlab("")+ylab("Relative Explained Deviance (%)")+
    theme_classic()+theme(axis.text.x = element_text(angle = 45,hjust = 1))+
    scale_x_discrete(limits=c(
      "Long","Lat","precAn","elev","PCA1","PCA2",
      "CV_Betw","Med_Betw","CV_out","Med_out",
      "Mean_WatCover","CV_WatCover","EcoR_size"))+
    facet_grid(.~Syst_Type)+
    guides(alpha="none",fill="none"),
  ncol=1)
dev.off()

png(filename ="EU_FULLgam_relat.png",
    width =250*7 ,height =300*7 ,units ="px",res = 300)
grid.arrange(
gam_Res_plot_b %>% 
  group_by(Syst_Type, disp_plot) %>% 
  summarise(Sum_TotExDe=mean(TOTAL_perc_ExplDev)) %>% 
  mutate(Mean_Perc_Tot=mean(Sum_TotExDe)) %>% 
ggplot()+
  geom_violin(aes(x=Syst_Type,y=Sum_TotExDe, color=Syst_Type), alpha=0.2, size=0.5, fill="grey95")+
  geom_point(aes(x=Syst_Type,y=Mean_Perc_Tot ,color=Syst_Type),shape=16,size=8, alpha=0.5)+
  geom_jitter(aes(x=Syst_Type,y=Sum_TotExDe ,fill=disp_plot),shape=21,size=4, width = 0.15)+
  scale_fill_manual(values=color)+
  scale_alpha_manual(values=c(0.1,0.8))+
  scale_colour_manual(values=c("grey55","#9191F0","#5E92F2","#6CB8A0"))+
  labs(fill="Dipsersal ability",title="Full Model Explained Deviation - Proportional decay rate (b)", color="FW type")+
  xlab("")+ylab("Full model Explained Deviance (%)")+
  theme_classic()+theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_y_continuous(limits = c(0,100))+
  guides(alpha="none",color="none"),

gam_Res_plot_q %>% group_by(Syst_Type, disp_plot) %>% 
  summarise(Sum_TotExDe=mean(TOTAL_perc_ExplDev)) %>%
  mutate(Mean_Perc_Tot=mean(Sum_TotExDe)) %>% 
  ggplot()+
  geom_violin(aes(x=Syst_Type,y=Sum_TotExDe, color=Syst_Type), alpha=0.2, size=0.5, fill="grey95")+
  geom_point(aes(x=Syst_Type,y=Mean_Perc_Tot ,color=Syst_Type),shape=16,size=8, alpha=0.5)+
  geom_jitter(aes(x=Syst_Type,y=Sum_TotExDe ,fill=disp_plot),shape=21,size=4, width = 0.15)+
  scale_fill_manual(values=color)+
  scale_alpha_manual(values=c(0.1,0.8))+
  scale_colour_manual(values=c("grey55","#9191F0","#5E92F2","#6CB8A0"))+
  labs(fill="Dipsersal ability",title="Full Model Explained Deviation - Collapsing rate (q)", color="FW type")+
  xlab("")+ylab("Full model Explained Deviance (%)")+
  theme_classic()+theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_y_continuous(limits = c(0,100))+
  guides(alpha="none",fill="none"),
ncol=1)
dev.off()

# Just small test

#gam_Res_plot_b %>% 
#  mutate(Type_Pred = case_when(
#    str_detect(Pred,"Long")~ "Climat",
#    str_detect(Pred,"Lat")~ "Climat",
#    str_detect(Pred,"precAn")~ "Climat",
#    str_detect(Pred,"elev")~ "Climat",
#    str_detect(Pred,"CV_Betw")~ "Central",
#    str_detect(Pred,"Med_Betw")~ "Central",
#    str_detect(Pred,"CV_out")~ "Central",
#    str_detect(Pred,"Med_out")~ "Central",
#    str_detect(Pred,"Mean_WatCover")~ "EcoRegion",
#    str_detect(Pred,"CV_WatCover")~ "EcoRegion",
#    str_detect(Pred,"EcoR_size")~ "EcoRegion",
#               TRUE ~ "ERROR")) %>% 
#group_by(Syst_Type, disp_plot,Type_Pred) %>%  
#  summarise(Mean_TypeExDe=mean(ExplDev),
#            SD_TypeExDe=sd(ExplDev)) %>% 
#  filter(Syst_Type=="Permanent") %>% 
#  ggplot()+
#  geom_col(aes(y=Mean_TypeExDe, x=Type_Pred,fill=Type_Pred))+
#  geom_errorbar(aes(y=Mean_TypeExDe,
#                    ymax=Mean_TypeExDe+SD_TypeExDe,
#                    ymin=Mean_TypeExDe-SD_TypeExDe,
#                    x=Type_Pred))+
#  ylab("Mean relative contibution (%)")+xlab("Group of predictors")+
#  facet_grid(.~Syst_Type*disp_plot)+
#  scale_fill_manual(values = c("#d9f2d9","#ecd9c6","#cce6ff"))+
#  scale_x_discrete(limits=c("Climat","Central","EcoRegion"),
#                   labels=c("Climatic","Network","Ecoregion"))+
#    theme_classic()