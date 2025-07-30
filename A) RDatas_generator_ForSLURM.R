
# We set the working direcory 
setwd("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation")

# Charghe and depurate the original EU database from ZENODO https://doi.org/10.5281/zenodo.15105286
source("BDD_PanEU/Upload_Depur_PanEU.R")

# Charge the function to run the coalescent model found in functions
source("functions/H2020_Lattice_expKernel_Jenv_TempMeta.R")

# Both datasets must have the same length
nrow(Output_GRIDvsLAKES_TvsM_dep)
nrow(Centroides_coordenadas_dep)

# Plot to check the whole dataset
ggplot(data.frame(cbind(Centroides_coordenadas_dep$CENTROID_X,
                        Centroides_coordenadas_dep$CENTROID_Y)), aes(x=X1, y=X2))+geom_point(shape=0)

EcoR_length <- c()
EcoReg_ID <- unique(Centroides_coordenadas_dep$EcoR_ID)
for (EcoRegion in 1:length(unique(Centroides_coordenadas_dep$EcoR_ID))) {
  EcoR_length[EcoRegion] <- nrow(Centroides_coordenadas_dep %>% filter(EcoR_ID==EcoReg_ID[EcoRegion]))
}


EcoR_size <- data.frame(EcoReg_ID,EcoR_length) %>%
                          filter(EcoReg_ID %in%EcoReg_ID[-c((length(EcoReg_ID)-2):length(EcoReg_ID))])%>% 
                          arrange(EcoR_length) 


for (ID_ecor in 1:length(EcoR_size$EcoReg_ID)) {

Coord_EcoReg <- Centroides_coordenadas_dep %>% filter(EcoR_ID==EcoR_size$EcoReg_ID[ID_ecor])

GRID_EcoReg <- Output_GRIDvsLAKES_TvsM_dep%>%filter(PAGENAME%in%Coord_EcoReg$PageName)

# Distance matrix
library(geosphere)
ptxy <- cbind(Coord_EcoReg$CENTROID_X,Coord_EcoReg$CENTROID_Y)
Dist_Matrix <- distm(ptxy, fun = distGeo)/1000

# Area total
# J calculation
FW_area <- apply(GRID_EcoReg[,103:ncol(GRID_EcoReg)], 1, sum)

# We define parameters of FW area to generate communities J
area.max.europa<-max(FW_area)
Jmin <- 400*0.02
J.max<-400+Jmin
b.ef<-0.5 

# Permanent ________________________________________________#
# J calculation
FW_area_perm <- apply(GRID_EcoReg[,192:ncol(GRID_EcoReg)], 1, sum)

# We define parameters of FW area to generate communities J
area.max.europa_perm<-max(FW_area_perm)
Jmin <- 400*0.02
J.max<-400+Jmin
b.ef<-0.5 

# Temporary ________________________________________________#
# J calculation
FW_area_temp <- apply(GRID_EcoReg[,113:191], 1, sum)

# We define parameters of FW area to generate communities J
area.max.europa_temp<-max(FW_area_temp)
Jmin <- 400*0.02
J.max<-400+Jmin
b.ef<-0.5 

# Ephemeral ________________________________________________#
# J calculation
FW_area_ephe <- apply(GRID_EcoReg[,103:112], 1, sum)

# We define parameters of FW area to generate communities J
area.max.europa_ephe<-max(FW_area_ephe)
Jmin <- 400*0.02
J.max<-400+Jmin
b.ef<-0.5 


id_NOmodule <- rep(1,nrow(Coord_EcoReg))
filter_NOfilter <- matrix(nrow = 200, ncol=nrow(Coord_EcoReg), data = 1)
pool_200 <- rlnorm(n = 200,5,1)
Meta_t0 <- matrix(nrow = length(pool_200), ncol =nrow(Coord_EcoReg), 1)

# Distances
dispersal_distances <- c(0.001,0.1,0.5,1,2,5,10) 

# Gradient of loss
habitat_lost <- c(0,0.1,0.3,0.5,0.7,0.85,0.9,0.95,0.99)

save.image(paste("RDatas_for_SLURM/","test_SLURM_", EcoR_size$EcoReg_ID[ID_ecor],".RData", sep=""))
}













