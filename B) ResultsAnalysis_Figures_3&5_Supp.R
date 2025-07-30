
library(viridis);library(tidyverse)

source("functions/Functions_to_plot.R")

# Charging TOTAL freshwater data ####
source("functions/SLURM_outputs_treat_Total.R")
Total_FW <- list(Alpha=Mean_Alpha,
                 Beta=Mean_Beta,Gamma=Mean_Rel_Gamma,
                 Curv_Coef=Mean_Rel_Gamma_Coef %>% dplyr::select(-c(Rel_G,Rel_G_UP,Rel_G_Low)),
                 EcoR_size=EcoR_size,xy_Coor=Outputs_SLURM_xyCoords,xyFWarea=Outputs_SLURM_xyFWarea)

PLot_AB_TOTAL <- Plot_Alp_Bet_Loc(Alpha =Total_FW$Alpha,Beta = Total_FW$Beta,
                 xyCoords = Total_FW$xy_Coor,xyFWarea =Total_FW$xyFWarea,EcoR_size =Total_FW$EcoR_size,Water_Type = 1)

# Manually printing plots
grid.arrange(PLot_AB_TOTAL[[26]][[1]])
grid.arrange(PLot_AB_TOTAL[[26]][[2]])
grid.arrange(PLot_AB_TOTAL[[26]][[3]])
grid.arrange(PLot_AB_TOTAL[[26]][[4]])


Plot_Gam_TOTAL <- Plot_Gam(Gamma = Total_FW$Gamma, Water_Type = 1,Mod_Coef=Mean_Rel_Gamma_Coef)

png(filename ="Gam_TOTAL.png",
    width =500*8 ,height =550*8 ,units ="px",res = 300)
grid.arrange(arrangeGrob(grobs = Plot_Gam_TOTAL,ncol=4))
dev.off()


# Charging PERMANENT freshwater data ####
source("functions/SLURM_outputs_treat_Perm.R")
Perm_FW <- list(Alpha=Mean_Alpha,Beta=Mean_Beta,Gamma=Mean_Rel_Gamma,Curv_Coef=Mean_Rel_Gamma_Coef %>% dplyr::select(-c(Rel_G,Rel_G_UP,Rel_G_Low)),
                 EcoR_size=EcoR_size,xy_Coor=Outputs_SLURM_xyCoords,xyFWarea=Outputs_SLURM_xyFWarea)

PLot_AB_Perm <- Plot_Alp_Bet_Loc(Alpha =Perm_FW$Alpha,Beta = Perm_FW$Beta,
                                  xyCoords = Perm_FW$xy_Coor,xyFWarea =Perm_FW$xyFWarea,EcoR_size =Perm_FW$EcoR_size,Water_Type = 2)
# Manually printing plots
grid.arrange(PLot_AB_Perm[[1]][[1]])
grid.arrange(PLot_AB_Perm[[1]][[2]])

Plot_Gam_Perm <- Plot_Gam(Gamma = Perm_FW$Gamma,Water_Type = 2,Mod_Coef=Mean_Rel_Gamma_Coef)
png(filename ="Gam_PERM.png",
    width =800*8 ,height =850*8 ,units ="px",res = 300)
grid.arrange(arrangeGrob(grobs = Plot_Gam_Perm,ncol=4))
dev.off()

# Charging TEMPORAL freshwater data ####
source("functions/SLURM_outputs_treat_Tempo.R")
Tempo_FW <- list(Alpha=Mean_Alpha,Beta=Mean_Beta, Gamma=Mean_Rel_Gamma,Curv_Coef=Mean_Rel_Gamma_Coef %>% dplyr::select(-c(Rel_G,Rel_G_UP,Rel_G_Low)),
                EcoR_size=EcoR_size,xy_Coor=Outputs_SLURM_xyCoords,xyFWarea=Outputs_SLURM_xyFWarea)

PLot_AB_Tempo <- Plot_Alp_Bet_Loc(Alpha =Tempo_FW$Alpha,Beta = Tempo_FW$Beta,
                                  xyCoords = Tempo_FW$xy_Coor,xyFWarea =Tempo_FW$xyFWarea,EcoR_size =Tempo_FW$EcoR_size,Water_Type = 3)
# Manually printing plots
grid.arrange(PLot_AB_Tempo[[26]][[1]])
grid.arrange(PLot_AB_Tempo[[26]][[2]])

Plot_Gam_Tempo <- Plot_Gam(Gamma = Tempo_FW$Gamma,Water_Type = 3,Mod_Coef = Mean_Rel_Gamma_Coef)

png(filename ="Gam_TEMP.png",
    width =800*8 ,height =850*8 ,units ="px",res = 300)
grid.arrange(arrangeGrob(grobs = Plot_Gam_Tempo,ncol=4))
dev.off()

# Charging EPHEMERAL freshwater data ####
source("functions/SLURM_outputs_treat_Ephem.R")
Ephem_FW <- list(Alpha=Mean_Alpha,Beta=Mean_Beta,Gamma=Mean_Rel_Gamma,Curv_Coef=Mean_Rel_Gamma_Coef %>% dplyr::select(-c(Rel_G,Rel_G_UP,Rel_G_Low)),
                EcoR_size=EcoR_size,xy_Coor=Outputs_SLURM_xyCoords,xyFWarea=Outputs_SLURM_xyFWarea)

PLot_AB_Ephem <- Plot_Alp_Bet_Loc(Alpha =Ephem_FW$Alpha,Beta = Ephem_FW$Beta,
                                  xyCoords = Ephem_FW$xy_Coor,xyFWarea =Ephem_FW$xyFWarea,EcoR_size =Ephem_FW$EcoR_size,Water_Type =4)
# Manually printing plots
grid.arrange(PLot_AB_Ephem[[26]][[1]])
grid.arrange(PLot_AB_Ephem[[26]][[2]])

Plot_Gam_Ephem <- Plot_Gam(Gamma = Ephem_FW$Gamma,Water_Type = 4,Mod_Coef = Mean_Rel_Gamma_Coef)

png(filename ="Gam_EPHEM.png",
    width =800*8 ,height =850*8 ,units ="px",res = 300)
grid.arrange(arrangeGrob(grobs = Plot_Gam_Ephem,ncol=4))
dev.off()

# Final values storage in RData ####
save(list = c("Total_FW","Perm_FW","Tempo_FW","Ephem_FW"),file = "Final_treated_data.RData")


# Figure3 -- European maps  ####
EU_FW_EcoR<- sf::st_read("BDD_PanEU/ecoregions_EUR_poly_AE/ecoregions_EUR_poly_AE.shp")
EU_FW_EcoR$Name <- gsub("- ","",EU_FW_EcoR$Name)# We polish some errors in names to make them equal to the ones in the DataBase
EU_FW_EcoR$Name <- gsub(" ","_",EU_FW_EcoR$Name)# We polish some errors in names to make them equal to the ones in the DataBase

# We define the colours
#6CB8A0 - Ephemeral; #5E92F2 - Temporary; #9191F0 - Permanent
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

# Supplementary 6 - Printing decay coefficients ####
out_Table <- data.frame()
for (ecoR in 1:length(Total_FW$xy_Coor)) {
sumary <- unique(Total_FW$xy_Coor[[ecoR]][,c(7,9,10)])
sumary <- sumary[which(sumary[,1]!=""),]
out_Table <- bind_rows(out_Table,sumary)
}
# Print for TOTAL
out_Table$EcoR_ID <- as.numeric(out_Table$EcoR_ID)
Table_coef <- left_join(out_Table,Total_FW$Curv_Coef%>%group_by(EcoR,disp) %>%summarise(Coef_b=mean(Coef_b),Coef_q=mean(Coef_q))
                        , by=c("EcoR_ID"="EcoR")) %>%
  mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
    disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
      disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
        disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
          disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
            disp==dispersal_distances[6], dispersal_distances_plot[6],
            dispersal_distances_plot[7]))))))) %>%  
              group_by(disp_plot,EcoR_ID,EcoR_Name,NAME_ENGL) %>% 
              dplyr::select(EcoR_ID,EcoR_Name,NAME_ENGL,disp_plot,Coef_b,Coef_q)
write.table(Table_coef,file = "Table_coef_TOT.txt")

# Print for Permanent
out_Table$EcoR_ID <- as.numeric(out_Table$EcoR_ID)
Table_coef <- left_join(out_Table,Perm_FW$Curv_Coef%>%group_by(EcoR,disp) %>%summarise(Coef_b=mean(Coef_b),Coef_q=mean(Coef_q))
                        , by=c("EcoR_ID"="EcoR")) %>%
  mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
    disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
      disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
        disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
          disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
            disp==dispersal_distances[6], dispersal_distances_plot[6],
            dispersal_distances_plot[7]))))))) %>% 
  group_by(disp_plot,EcoR_ID,EcoR_Name,NAME_ENGL) %>% 
  dplyr::select(EcoR_ID,EcoR_Name,NAME_ENGL,disp_plot,Coef_b,Coef_q)
write.table(Table_coef,file = "Table_coef_Perm.txt")

# Print for Temporary 
out_Table$EcoR_ID <- as.numeric(out_Table$EcoR_ID)
Table_coef <- left_join(out_Table,Tempo_FW$Curv_Coef%>%group_by(EcoR,disp) %>%summarise(Coef_b=mean(Coef_b),Coef_q=mean(Coef_q))
                        , by=c("EcoR_ID"="EcoR")) %>%
  mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
    disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
      disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
        disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
          disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
            disp==dispersal_distances[6], dispersal_distances_plot[6],
            dispersal_distances_plot[7]))))))) %>% 
  group_by(disp_plot,EcoR_ID,EcoR_Name,NAME_ENGL) %>% 
  dplyr::select(EcoR_ID,EcoR_Name,NAME_ENGL,disp_plot,Coef_b,Coef_q)
write.table(Table_coef,file = "Table_coef_Tempo.txt")

# Print for Ephemeral
out_Table$EcoR_ID <- as.numeric(out_Table$EcoR_ID)
Table_coef <- left_join(out_Table,Ephem_FW$Curv_Coef%>%group_by(EcoR,disp) %>%summarise(Coef_b=mean(Coef_b),Coef_q=mean(Coef_q))
                        , by=c("EcoR_ID"="EcoR")) %>%
  mutate(disp_plot=ifelse(disp==dispersal_distances[1], dispersal_distances_plot[1],ifelse(
    disp==dispersal_distances[2], dispersal_distances_plot[2],ifelse(
      disp==dispersal_distances[3], dispersal_distances_plot[3],ifelse(
        disp==dispersal_distances[4], dispersal_distances_plot[4],ifelse(  
          disp==dispersal_distances[5], dispersal_distances_plot[5],ifelse(  
            disp==dispersal_distances[6], dispersal_distances_plot[6],
            dispersal_distances_plot[7]))))))) %>% 
  group_by(disp_plot,EcoR_ID,EcoR_Name,NAME_ENGL) %>% 
  dplyr::select(EcoR_ID,EcoR_Name,NAME_ENGL,disp_plot,Coef_b,Coef_q)
write.table(Table_coef,file = "Table_coef_Ephem.txt")

# Figure 5 - Diversity decay against predictors  #### 
# Charging landscape structural descriptors 
# We charge and prepare all the Geographical and centrality variables
source("BDD_ExternalPanEU/Charge_treat_env_centr_values.R")
centr_data
env_data

# Functions to plot pair correlations
# Custom function for the upper triangle (Spearman correlation value only)
my_cor <- function(data, mapping, method = "spearman", digits = 2, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr_val <- cor(x, y, method = method, use = "complete.obs")
  col <- ifelse(corr_val > 0, "blue", "red")
  col <- ifelse(corr_val>0.8|corr_val<(-0.8), col, "black")
  ggplot(data = data, mapping = mapping) +
    annotate("text", x = 0.5, y = 0.5, 
             label = formatC(corr_val, format = "f", digits = digits),
             size = 5, color = col) +
    theme_void()
}

# Lower triangle: 2D density + points
lower_density <- function(data, mapping, method = "spearman", ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr_val <- cor(x, y, method = method, use = "complete.obs")
  col <- ifelse(corr_val > 0, "blue", "red")
  ggplot(data = data, mapping = mapping) +
    geom_smooth(method = "lm",se=F,linetype=2,colour=col,linewidth=0.8)+
    geom_point(alpha = 0.5, size = 1)+
    theme_classic()
}

# Diagonal: density per variable
diag_density <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_density(aes(y = ..density..), fill = "lightgray", alpha = 0.6)+
    theme_classic()
}

# Several descriptors are charged but we will only use:  
# Long, Lat, Betw_CV, Betw_Med, OutDeg_CV, OutDeg_Med, WatCov_Mean, WatCov_CV, EcoRSize

# Gam Fig.5 - TOTAL FW relations ####
#Water type must be either 1-Total, 2-Ephemeral, 3-Temporary, 4-Permanent 
EU_relat_TOTAL <- Plot_Relation_EU(Water_Type_List = Total_FW, WaterType =1,
                                   MeanMed_or_CV = "MeanMed",
                                   Centrality_data = centr_data,
                                   Environmental_data = env_data)

colnames(EU_relat_TOTAL[[1]])[12:15] <- c("Betw_CV","Betw_Med","OutDeg_CV","OutDeg_Med")

EU_relat_TOTAL[[1]] <- EU_relat_TOTAL[[1]]%>%left_join(read.csv2("data/NoComp_ECOR.csv"),by=c("EcoR"="Number_EcoR"),relationship = "many-to-many") %>%
  filter(Name!="Unassigned") %>%  filter(X!="no complet") %>% 
  dplyr::select(colnames(EU_relat_TOTAL[[1]]))

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
                                        dplyr::select(-c(Long,Lat,EcoR,disp,disp_plot,Metric))
# We scale data to homogenize its strength on the patterns detected (strong differenced between response values)
data_to_gam_scaled <- scale(data_to_gam[,3:ncol(data_to_gam)],center = T,scale = T)
# We detect highly correlated variables and eliminate them from the run
hc = findCorrelation(cor(data_to_gam_scaled, use="complete.obs", method="spearman"),
                     cutoff=0.8,names = F, exact = T)

print(colnames(data_to_gam_scaled)[hc])

# Final plot
as.data.frame(data_to_gam_scaled) %>% 
  rename("OutDeg_Mean"="OutDeg_Med",
         "Betw_Mean"="Betw_Med") %>%  
  ggpairs(upper = list(continuous = my_cor),
          lower = list(continuous = lower_density),
          diag = list(continuous = diag_density))+
  labs(title="Total FW")

data_to_gam<- data_to_gam %>% dplyr::select(-c("Betw_Med","OutDeg_Med","EcoRSize"))
data_to_gam_scaled <- data_to_gam_scaled[,colnames(data_to_gam)[3:ncol(data_to_gam)]]
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
         s(OutDeg_CV,k=3, bs="cr")+
         #s(OutDeg_Med,k=3, bs="cr")+
         s(WatCov_Mean,k=3, bs="cr")+
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
Out_Table <- EU_relat_TOTAL[[1]] %>% filter(disp_plot==unique(EU_relat_TOTAL[[1]]$disp_plot)[disper],
                                            OutDeg_Med!=is.na(OutDeg_Med)) %>% 
                                     dplyr::select(-c(precAn,elev,WaterSystem)) %>% 
                                     mutate(EcoRSize=log(EcoRSize+1),
                                            Betw_CV=log(Betw_CV+1),
                                            Betw_Med=log(Betw_Med+1)) %>%  
                                     dplyr::select(c("disp_plot","coef_b","coef_q",colnames(data_to_gam_scaled))) %>% 
                                     pivot_longer(cols=colnames(data_to_gam_scaled)) %>% 
                                     mutate(Sign_value=rep(sign_values,nrow(.)/ncol(data_to_gam_scaled)))

# Model prediction to later plot the relationships and the trends
pred_Out <- data.frame()
# We run the prediction for eahc significant variable
for (col_noms in 1:length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])) {
data_to_gam_scaled_depur <- data_to_gam_scaled[,colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]]

if(length(which(out$s.pv<0.05))==1){
  data_to_gam_scaled_depur <- as.matrix(data_to_gam_scaled_depur)
  colnames(data_to_gam_scaled_depur) <- colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]
}

Main_variable <- colnames(data_to_gam_scaled_depur)[col_noms]  
Other_variables <- colnames(data_to_gam_scaled)[-which(colnames(data_to_gam_scaled)==Main_variable)]
new_data <- with(data_to_gam, expand.grid("Main_variable" = seq(min(data_to_gam[,Main_variable]), 
                                                        max(data_to_gam[,Main_variable]),
                                                        length = nrow(data_to_gam)),
                                          "Var1" = median(data_to_gam[,Other_variables[1]]),
                                          "Var2"= median(data_to_gam[,Other_variables[2]]),
                                          "Var3"= median(data_to_gam[,Other_variables[3]])))
colnames(new_data) <- c(Main_variable,Other_variables)

# Using the dataset that we created (maintaining the significant variable fixed) we reproduce the values for the 
#predicted gam and store them in a data frame to later plot them. 
ilink <- family(b)$linkinv
pred <- predict(b,new_data, type = "link", se.fit = TRUE)
pred <- cbind(pred, new_data)
pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),upr_ci = ilink(fit + (2 * se.fit)),fitted = ilink(fit))
pred <- data.frame("Var"=Main_variable,"disp"=unique(EU_relat_TOTAL[[1]]$disp)[disper],"disp_plot"=unique(EU_relat_TOTAL[[1]]$disp_plot)[disper],
                   pred,row.names = NULL)
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

# Gam Fig.5 - Perm FW relations ####
#Water type must be either 1-Total, 2-Ephemeral, 3-Temporary, 4-Permanent 
EU_relat_Perm <- Plot_Relation_EU(Water_Type_List = Perm_FW,WaterType =4,
                                  MeanMed_or_CV = "MeanMed",
                                  Centrality_data = centr_data,Environmental_data = env_data)

colnames(EU_relat_Perm[[1]])[12:15] <- c("Betw_CV","Betw_Med","OutDeg_CV","OutDeg_Med")

EU_relat_Perm[[1]] <- EU_relat_Perm[[1]]%>%left_join(read.csv2("data/NoComp_ECOR.csv"),by=c("EcoR"="Number_EcoR"),relationship = "many-to-many") %>%
                     filter(Name!="Unassigned") %>%  filter(X!="no complet") %>% 
                     dplyr::select(colnames(EU_relat_Perm[[1]]))

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
                                         dplyr::select(-c(precAn,elev,WaterSystem)) %>% 
                                         mutate(EcoRSize=log(EcoRSize+1),
                                                Betw_CV=log(Betw_CV+1),
                                                Betw_Med=log(Betw_Med+1)) %>%  
                                         dplyr::select(-c(Long,Lat,EcoR,disp,disp_plot,Metric))
                                     
    # We scale data to homogenize its strength on the patterns detected (strong differenced between response values)
    data_to_gam_scaled <- scale(data_to_gam[,3:ncol(data_to_gam)],center = T,scale = T)
    # We detect highly correlated variables and eliminate them from the run
    hc = findCorrelation(cor(data_to_gam_scaled, use="complete.obs", method="spearman"),
                         cutoff=0.8,names = F, exact = T)
    
    print(colnames(data_to_gam_scaled)[hc])
    
    # Final plot
    as.data.frame(data_to_gam_scaled) %>% 
      rename("OutDeg_Mean"="OutDeg_Med",
             "Betw_Mean"="Betw_Med") %>%  
      ggpairs(upper = list(continuous = my_cor),
              lower = list(continuous = lower_density),
              diag = list(continuous = diag_density))+
      labs(title="Permanent FW")
    
    data_to_gam<- data_to_gam %>% dplyr::select(-c("Betw_Med","OutDeg_Med"))
    data_to_gam_scaled <- data_to_gam_scaled[,colnames(data_to_gam)[3:ncol(data_to_gam)]]
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
               s(OutDeg_CV,k=3, bs="cr")+
               #s(OutDeg_Med,k=3, bs="cr")+
               s(WatCov_Mean,k=3, bs="cr")+
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
                                        dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,precAn,elev)) %>% 
                                        mutate(EcoRSize=log(EcoRSize+1),
                                               Betw_CV=log(Betw_CV+1)) %>% 
                                        dplyr::select(c("disp_plot","coef_b","coef_q",colnames(data_to_gam_scaled))) %>% 
                                        pivot_longer(cols=colnames(data_to_gam_scaled)) %>% 
                                        mutate(Sign_value=rep(sign_values,nrow(.)/ncol(data_to_gam_scaled)))
    
    # Model prediction to later plot the relationships and the trends
    pred_Out <- data.frame()
    # We run the prediction for eahc significant variable
    for (col_noms in 1:length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])) {
      data_to_gam_scaled_depur <- data_to_gam_scaled[,colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]]
      
      if(length(which(out$s.pv<0.05))==1){
        data_to_gam_scaled_depur <- as.matrix(data_to_gam_scaled_depur)
        colnames(data_to_gam_scaled_depur) <- colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]
      }
      
      Main_variable <- colnames(data_to_gam_scaled_depur)[col_noms]  
      Other_variables <- colnames(data_to_gam_scaled)[-which(colnames(data_to_gam_scaled)==Main_variable)]
      new_data <- with(data_to_gam, expand.grid("Main_variable" = seq(min(data_to_gam[,Main_variable]), 
                                                                      max(data_to_gam[,Main_variable]),
                                                                      length = nrow(data_to_gam)),
                                                "Var1" = median(data_to_gam[,Other_variables[1]]),
                                                "Var2"= median(data_to_gam[,Other_variables[2]]),
                                                "Var3"= median(data_to_gam[,Other_variables[3]]),
                                                "Var4" = median(data_to_gam[,Other_variables[4]])))
      colnames(new_data) <- c(Main_variable,Other_variables)
      
      # Using the dataset that we created (maintaining the significant variable fixed) we reproduce the values for the 
      #predicted gam and store them in a data frame to later plot them. 
      ilink <- family(b)$linkinv
      pred <- predict(b,new_data, type = "link", se.fit = TRUE)
      pred <- cbind(pred, new_data)
      pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),upr_ci = ilink(fit + (2 * se.fit)),fitted = ilink(fit))
      pred <- data.frame("Var"=Main_variable,"disp"=unique(EU_relat_TOTAL[[1]]$disp)[disper],"disp_plot"=unique(EU_relat_TOTAL[[1]]$disp_plot)[disper],
                         pred,row.names = NULL)
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

# Gam Fig.5 - Tempo FW relations ####
#Water type must be either 1-Total, 2-Ephemeral, 3-Temporary, 4-Permanent 
EU_relat_Tempo <- Plot_Relation_EU(Water_Type_List = Tempo_FW,WaterType =3,
                                  MeanMed_or_CV = "MeanMed",Centrality_data = centr_data,Environmental_data = env_data)

colnames(EU_relat_Tempo[[1]])[12:15] <- c("Betw_CV","Betw_Med","OutDeg_CV","OutDeg_Med")

EU_relat_Tempo[[1]] <- EU_relat_Tempo[[1]]%>%left_join(read.csv2("data/NoComp_ECOR.csv"),by=c("EcoR"="Number_EcoR"),relationship = "many-to-many") %>%
  filter(Name!="Unassigned") %>%  filter(X!="no complet") %>% 
  dplyr::select(colnames(EU_relat_Tempo[[1]]))

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
                                          dplyr::select(-c(precAn,elev,WaterSystem)) %>% 
                                          mutate(EcoRSize=log(EcoRSize+1),
                                                 Betw_CV=log(Betw_CV+1),
                                                 Betw_Med=log(Betw_Med+1)) %>%  
                                          dplyr::select(-c(Long,Lat,EcoR,disp,disp_plot,Metric))
    # We scale data to homogenize its strength on the patterns detected (strong differenced between response values)
    data_to_gam_scaled <- scale(data_to_gam[,3:ncol(data_to_gam)],center = T,scale = T)
    # We detect highly correlated variables and eliminate them from the run
    hc = findCorrelation(cor(data_to_gam_scaled, use="complete.obs", method="spearman"),
                         cutoff=0.8,names = F, exact = T)
    
    print(colnames(data_to_gam_scaled)[hc])
    
    # Final plot
    as.data.frame(data_to_gam_scaled) %>% 
      rename("OutDeg_Mean"="OutDeg_Med",
             "Betw_Mean"="Betw_Med") %>%  
      ggpairs(upper = list(continuous = my_cor),
              lower = list(continuous = lower_density),
              diag = list(continuous = diag_density))+
      labs(title="Temporal FW")
    
    data_to_gam<- data_to_gam %>% dplyr::select(-c("Betw_Med","OutDeg_Med","EcoRSize","WatCov_CV"))
    data_to_gam_scaled <- data_to_gam_scaled[,colnames(data_to_gam)[3:ncol(data_to_gam)]]
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
               s(OutDeg_CV,k=3, bs="cr")+
               #s(OutDeg_Med,k=3, bs="cr")+
               s(WatCov_Mean,k=3, bs="cr"),
               #s(WatCov_CV,k=3, bs="cr"),
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
                                         dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,precAn,elev)) %>% 
                                         mutate(EcoRSize=log(EcoRSize+1),
                                                Betw_CV=log(Betw_CV+1)) %>% 
                                         dplyr::select(c("disp_plot","coef_b","coef_q",colnames(data_to_gam_scaled))) %>% 
                                         pivot_longer(cols=colnames(data_to_gam_scaled)) %>% 
                                         mutate(Sign_value=rep(sign_values,nrow(.)/ncol(data_to_gam_scaled)))
    
    # Model prediction to later plot the relationships and the trends
    pred_Out <- data.frame()
    # We run the prediction for eahc significant variable
    for (col_noms in 1:length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])) {
      data_to_gam_scaled_depur <- data_to_gam_scaled[,colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]]
      
      if(length(which(out$s.pv<0.05))==1){
        data_to_gam_scaled_depur <- as.matrix(data_to_gam_scaled_depur)
        colnames(data_to_gam_scaled_depur) <- colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]
      }
      
      if (length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])==0) {cat("NoSign everywhere","\n")}else{
      data_to_gam_scaled_depur <- as.data.frame(data_to_gam_scaled[,colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]])
      colnames(data_to_gam_scaled_depur) <- colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]
      
      Main_variable <- colnames(data_to_gam_scaled_depur)[col_noms]  
      Other_variables <- colnames(data_to_gam_scaled)[-which(colnames(data_to_gam_scaled)==Main_variable)]
      new_data <- with(data_to_gam, expand.grid("Main_variable" = seq(min(data_to_gam[,Main_variable]), 
                                                                      max(data_to_gam[,Main_variable]),
                                                                      length = nrow(data_to_gam)),
                                                "Var1" = median(data_to_gam[,Other_variables[1]]),
                                                "Var2" = median(data_to_gam[,Other_variables[2]])))
      colnames(new_data) <- c(Main_variable,Other_variables)
      
      # Using the dataset that we created (maintaining the significant variable fixed) we reproduce the values for the 
      #predicted gam and store them in a data frame to later plot them. 
      ilink <- family(b)$linkinv
      pred <- predict(b,new_data, type = "link", se.fit = TRUE)
      pred <- cbind(pred, new_data)
      pred <- transform(pred, lwr_ci = ilink(fit - (2 * se.fit)),upr_ci = ilink(fit + (2 * se.fit)),fitted = ilink(fit))
      pred <- data.frame("Var"=Main_variable,"disp"=unique(EU_relat_TOTAL[[1]]$disp)[disper],"disp_plot"=unique(EU_relat_TOTAL[[1]]$disp_plot)[disper],
                         pred,row.names = NULL)
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

# Gam Fig.5 - Ephem FW relations ####
#Water type must be either 1-Total, 2-Ephemeral, 3-Temporary, 4-Permanent 
EU_relat_Ephem <- Plot_Relation_EU(Water_Type_List = Ephem_FW,WaterType =2,
                                  MeanMed_or_CV = "MeanMed",Centrality_data = centr_data,Environmental_data = env_data)
colnames(EU_relat_Ephem[[1]])[12:15] <- c("Betw_CV","Betw_Med","OutDeg_CV","OutDeg_Med")

EU_relat_Ephem[[1]] <- EU_relat_Ephem[[1]]%>%left_join(read.csv2("data/NoComp_ECOR.csv"),by=c("EcoR"="Number_EcoR"),relationship = "many-to-many") %>%
  filter(Name!="Unassigned") %>%  filter(X!="no complet") %>% 
  dplyr::select(colnames(EU_relat_Ephem[[1]]))
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
                                          dplyr::select(-c(precAn,elev,WaterSystem)) %>% 
                                          mutate(EcoRSize=log(EcoRSize+1),
                                                 Betw_CV=log(Betw_CV+1),
                                                 Betw_Med=log(Betw_Med+1)) %>%  
                                          dplyr::select(-c(Long,Lat,EcoR,disp,disp_plot,Metric))
    # We scale data to homogenize its strength on the patterns detected (strong differenced between response values)
    data_to_gam_scaled <- scale(data_to_gam[,3:ncol(data_to_gam)],center = T,scale = T)
    # We detect highly correlated variables and eliminate them from the run
    hc = findCorrelation(cor(data_to_gam_scaled, use="complete.obs", method="spearman"),
                         cutoff=0.8,names = F, exact = T)
    
    print(colnames(data_to_gam_scaled)[hc])
    
    # Final plot
    as.data.frame(data_to_gam_scaled) %>% 
      rename("OutDeg_Mean"="OutDeg_Med",
             "Betw_Mean"="Betw_Med") %>%  
      ggpairs(upper = list(continuous = my_cor),
              lower = list(continuous = lower_density),
              diag = list(continuous = diag_density))+
      labs(title="Ephemeral FW")
    
    data_to_gam<- data_to_gam %>% dplyr::select(-c("Betw_Med","OutDeg_Med","EcoRSize","WatCov_CV"))
    data_to_gam_scaled <- data_to_gam_scaled[,colnames(data_to_gam)[3:ncol(data_to_gam)]]
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
               s(OutDeg_CV,k=3, bs="cr")+
               #s(OutDeg_Med,k=3, bs="cr")+
               s(WatCov_Mean,k=3, bs="cr"),
               #s(WatCov_CV,k=3, bs="cr")+
               #s(EcoRSize,k=3, bs="cr"),
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
                                         dplyr::select(-c(Long,Lat,Betw_Med,OutDeg_Med,precAn,elev)) %>% 
                                         mutate(EcoRSize=log(EcoRSize+1),
                                                Betw_CV=log(Betw_CV+1)) %>% 
                                         dplyr::select(c("disp_plot","coef_b","coef_q",colnames(data_to_gam_scaled))) %>% 
                                         pivot_longer(cols=colnames(data_to_gam_scaled)) %>% 
                                         mutate(Sign_value=rep(sign_values,nrow(.)/ncol(data_to_gam_scaled)))
    
    # Model prediction to later plot the relationships and the trends
    pred_Out <- data.frame()
    # We run the prediction for eahc significant variable
    for (col_noms in 1:length(colnames(data_to_gam_scaled)[which(out$s.pv<0.05)])) {
      data_to_gam_scaled_depur <- data_to_gam_scaled[,colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]]
      
      if(length(which(out$s.pv<0.05))==1){
        data_to_gam_scaled_depur <- as.matrix(data_to_gam_scaled_depur)
        colnames(data_to_gam_scaled_depur) <- colnames(data_to_gam_scaled)[which(out$s.pv<0.05)]
      }
      
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
      pred <- data.frame("Var"=Main_variable,"disp"=unique(EU_relat_TOTAL[[1]]$disp)[disper],"disp_plot"=unique(EU_relat_TOTAL[[1]]$disp_plot)[disper],
                         pred,row.names = NULL)
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


# # Figure 5 OUTPUT managing and gam values/tables extraction ####
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


# Total freshwater significant values (manual selection)
# for b 
select_variables <- c("Betw_CV","OutDeg_CV","WatCov_Mean","WatCov_CV")
name_select_variables <- c("Betweenness CV","Out degree CV","Water Cover Mean","Water Cover CV")
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
                                 arrangeGrob(select_plotList[[4]]+theme(legend.position = "none")),
                                 ncol=4)

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
                                 arrangeGrob(select_plotList[[4]]+theme(legend.position = "none")),
                                 ncol=4)

Total_Fresh<- arrangeGrob(b_plot,q_plot,nrow=2,
                          top=grid::textGrob("D) Total freshwaters",
                                            gp=grid::gpar(fontsize=20,font=2)))

# Permanent freshwater significant values (manual selection)
# for b 
select_variables <- c("Betw_CV","OutDeg_CV","WatCov_Mean","WatCov_CV")
name_select_variables <- c("Betweenness CV","Out degree CV","Water Cover Mean","Water Cover CV")
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
                                 arrangeGrob(select_plotList[[4]]+theme(legend.position = "none")),
                                 ncol=4)

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
                                 arrangeGrob(select_plotList[[4]]+theme(legend.position = "none")),
                                 ncol=4)

Perm_Fresh<- arrangeGrob(b_plot,q_plot,nrow=2,
                         top=grid::textGrob("A) Permanent freshwaters",
                                            gp=grid::gpar(fontsize=20,font=2)))


# Temporary freshwater significant values (manual selection)
# for b 
select_variables <- c("OutDeg_CV")
name_select_variables <- c("Out degree CV")
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
b_plot <- gridExtra::arrangeGrob(blank,
                                 arrangeGrob(select_plotList[[1]]+theme(legend.position = "none")),
                                 blank,
                                 blank,
                                 ncol=4)

select_variables <- c("Betw_CV","OutDeg_CV")
name_select_variables <- c("Betweenness CV","Out degree CV")
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
                                 blank,
                                 ncol=4)

Temp_Fresh<- arrangeGrob(b_plot,q_plot,nrow=2,
                         top=grid::textGrob("B) Temporary freshwaters",
                                            gp=grid::gpar(fontsize=20,font=2)))

# Ephemeral freshwater significant values (manual selection)
# for b 
select_variables <- c("Betw_CV","OutDeg_CV")
name_select_variables <- c("Betweenness CV","Out degree CV")
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
                                 blank,
                                 ncol=4)
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
                                 blank,
                                 ncol=4)

Ephem_Fresh<- arrangeGrob(b_plot,q_plot,nrow=2,
                          top=grid::textGrob("C) Ephemeral freshwaters",
                                             gp=grid::gpar(fontsize=20,font=2)))

png(filename ="GAM_Figure4.png",width =550*5 ,height =1050*7 ,units ="px",res = 300)
grid.arrange(Perm_Fresh,Temp_Fresh,Ephem_Fresh,Total_Fresh,leg_plot,nrow=5)
dev.off()


Total_list_RESULTS <- list("Total Freshwaters"=TOTAL_Out_Results_GAM,
                           "Permanent Freshwaters"=Perm_Out_Results_GAM,
                           "Temporal Freshwaters"=Tempo_Out_Results_GAM,
                           "Ephemeral Freshwaters"=Ephem_Out_Results_GAM)


# Supplementary material 9 ####
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


Gam_RESULTS_TOTAL_Table_R2 <- data.frame()
for (typ_fre in 1:length(Total_list_RESULTS)) {
  disp_Output <- data.frame()
  for (disp in 1:7) {
    tempo_out <- bind_rows(
      data.frame(
        "Freshwater type"=names(Total_list_RESULTS)[[typ_fre]],
        "Diversity decay parameter"="Proportional decay rate (b)",
        "Dispersal"=names(Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result)[[disp]],
        "Descriptors"=c("R-sq.(adj)","Dev. exp."),
        "Value"=rbind(as.numeric(Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result[[disp]]$r.sq),
              as.numeric(Total_list_RESULTS[[typ_fre]]$coef_b$GAM_Result[[disp]]$dev.expl))),
      data.frame(
        "Freshwater type"=names(Total_list_RESULTS)[[typ_fre]],
        "Diversity decay parameter"="Collapsing rate (q)",
        "Dispersal"=names(Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result)[[disp]],
        "Descriptors"=c("R-sq.(adj)","Dev. exp."),
        "Value"=rbind(as.numeric(Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result[[disp]]$r.sq),
              as.numeric(Total_list_RESULTS[[typ_fre]]$coef_q$GAM_Result[[disp]]$dev.expl))),
    )
    disp_Output <- bind_rows(disp_Output,tempo_out)
  }
  
  Gam_RESULTS_TOTAL_Table_R2 <- bind_rows(Gam_RESULTS_TOTAL_Table_R2,disp_Output)
}

write.table(Gam_RESULTS_TOTAL_Table,file = "Gam_RESULTS_TOTAL_Table.txt",row.names = F)
write.table(Gam_RESULTS_TOTAL_Table_R2,file = "Gam_RESULTS_TOTAL_R2_Table.txt",row.names = F)
save(list = "Total_list_RESULTS",file="GAM_outputs.RData")

