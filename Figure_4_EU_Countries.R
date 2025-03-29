
# Extra figure just in case for the review after the GCB

#You need to: 

library(viridis);library(tidyverse)

source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/Functions_to_plot.R")
setwd("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation")

# We charge TOTAL freshwater Data
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/SLURM_outputs_treat_Total.R")

# And then charge the following
load("treated_data.RData")

Water_colors <- c("white","#9191F0","#5E92F2","#6CB8A0")

EU_Countries <- c("Ireland","Portugal","Spain","France","Luxembourg","Belgium","Netherlands",
                  "Germany","Denmark","Sweden","Finland","Latvia","Estonia","Lithuania",
                  "Slovenia","Poland","Czechia","Slovakia","Hungary","Austria",
                  "Romania","Czechia","Greece","Croatia","Italy")
Centroides_coordenadas_dep_EU <- Centroides_coordenadas_dep%>% filter(NAME_ENGL%in%EU_Countries)


Perc_Countries <- read.csv2("Percentage_per_Countries.csv",dec = ".") %>%
                  filter(!Etiquetes.de.fila%in%c("Total general","Unassigned","Bullshit")) %>% 
                  rename("EcoR_Name"=Etiquetes.de.fila) %>% 
                  pivot_longer(cols=2:ncol(.),names_to = "NAME_ENGL",values_to = "perc") %>% 
                  filter(NAME_ENGL!="Total.general",perc!=0) 

Total_FW_DDcoef <- Total_FW$Curv_Coef %>% mutate(EcoR_ID=as.character(EcoR))

left_join(Centroides_coordenadas_dep_EU,
          Total_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL !="") %>% 
  left_join(Perc_Countries,by=c("EcoR_Name","NAME_ENGL")) %>% 
  filter(NAME_ENGL=="Spain",disp_plot=="0.001 km") %>% 
  group_by(NAME_ENGL,EcoR_Name, disp_plot,perc) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
  group_by(NAME_ENGL, disp_plot) %>% 
  summarise(Mean_b=weighted.mean(x =Mean_b,w =perc, na.rm=T))

left_join(Centroides_coordenadas_dep_EU,
          Total_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL !="") %>% group_by(NAME_ENGL, disp_plot) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
  filter(NAME_ENGL=="Spain")


plot_list <- list()
# Total water 
Total_FW_DDcoef <- Total_FW$Curv_Coef %>% mutate(EcoR_ID=as.character(EcoR))

#Mean_Values <- left_join(Centroides_coordenadas_dep_EU,
#          Total_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
#  filter(NAME_ENGL !="") %>% group_by(NAME_ENGL) %>% 
#  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T))

plot_list[[1]] <- left_join(Centroides_coordenadas_dep_EU,
                            Total_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL !="") %>%
  left_join(Perc_Countries,by=c("EcoR_Name","NAME_ENGL")) %>% 
  group_by(NAME_ENGL,EcoR_Name, disp_plot,perc) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
  group_by(NAME_ENGL, disp_plot) %>% 
  summarise(Mean_b=weighted.mean(x =Mean_b,w =perc, na.rm=T),Mean_q=weighted.mean(x =Mean_q,w =perc, na.rm=T)) %>% 
  ggplot()+
  geom_vline(aes(xintercept=NAME_ENGL ),colour="grey60",alpha=0.3)+
  geom_hline(yintercept = mean(Total_FW$Curv_Coef$b_Coef,na.rm = T), linewidth=2,alpha=0.5, colour="black")+
  #geom_errorbar(aes(x=NAME_ENGL ,ymax=Mean_b+SD_b,ymin=Mean_b-SD_b))+
  geom_violin(aes(x=NAME_ENGL ,y=Mean_b),alpha=0.6,fill="#b30000",colour=0)+
  #geom_point(data=Mean_Values, aes(x=NAME_ENGL ,y=Mean_b), fill="black",shape=21, size=3, alpha=0.8)+
  geom_jitter(aes(x=NAME_ENGL ,y=Mean_b, fill=disp_plot),shape=21, size=2.5, alpha=0.8)+
  scale_fill_viridis(discrete = T)+
  #scale_fill_gradient(low ="#b30000",high ="#fff2e6" ,limits=c(-1.3,0))+
  scale_y_continuous(limits = c(-0.79,-0.2))+
  geom_text(x="Czechia",y=-0.2,label="Flatter diversity decay", colour="darkblue",size=3)+
  geom_text(x="Czechia",y=-0.79,label="Steeper diversity decay", colour="darkred",size=3)+
  guides(color="none", alpha="none", fill=guide_colourbar("b"))+
  labs(x="",y="Mean Proportional decay rate",subtitle="Proportional decay rate",title="Total FW")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=15,face = "bold",color="black"),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1))

plot_list[[2]] <- left_join(Centroides_coordenadas_dep_EU,Total_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL !="") %>% 
  left_join(Perc_Countries,by=c("EcoR_Name","NAME_ENGL")) %>% 
  group_by(NAME_ENGL,EcoR_Name, disp_plot,perc) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
  group_by(NAME_ENGL, disp_plot) %>% 
  summarise(Mean_b=weighted.mean(x =Mean_b,w =perc, na.rm=T),Mean_q=weighted.mean(x =Mean_q,w =perc, na.rm=T)) %>% 
  ggplot()+
  geom_vline(aes(xintercept=NAME_ENGL ),colour="grey60",alpha=0.3)+
  geom_hline(yintercept = mean(Total_FW$Curv_Coef$q_Coef,na.rm = T), linewidth=2,alpha=0.5, colour="black")+
  #geom_errorbar(aes(x=NAME_ENGL ,ymax=Mean_b+SD_b,ymin=Mean_b-SD_b))+
  geom_violin(aes(x=NAME_ENGL ,y=Mean_q),alpha=0.6,fill="#8A2BE2",colour=0)+
  #geom_point(data=Mean_Values, aes(x=NAME_ENGL ,y=Mean_b), fill="black",shape=21, size=3, alpha=0.8)+
  geom_jitter(aes(x=NAME_ENGL ,y=Mean_q, fill=disp_plot),shape=21, size=2.5, alpha=0.8)+
  scale_fill_viridis(discrete = T)+
  #scale_fill_gradient(low ="#b30000",high ="#fff2e6" ,limits=c(-1.3,0))+
  scale_y_reverse(limits = c(0.355,0.305))+
  geom_text(x="Czechia",y=-0.305,label="Flatter diversity decay", colour="darkblue",size=3)+
  geom_text(x="Czechia",y=-0.355,label="Steeper diversity decay", colour="darkred",size=3)+
  guides(color="none", alpha="none", fill=guide_colourbar("b"))+
  labs(x="",y="Mean Collapsing rate",subtitle="Collapsing rate",title="Total FW")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=15,face = "bold",color="black"),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1))


# Permanent
Perm_FW_DDcoef <- Perm_FW$Curv_Coef %>% mutate(EcoR_ID=as.character(EcoR))

plot_list[[3]] <- left_join(Centroides_coordenadas_dep_EU,Perm_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL !="") %>% 
  left_join(Perc_Countries,by=c("EcoR_Name","NAME_ENGL")) %>% 
  group_by(NAME_ENGL,EcoR_Name, disp_plot,perc) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
  group_by(NAME_ENGL, disp_plot) %>% 
  summarise(Mean_b=weighted.mean(x =Mean_b,w =perc, na.rm=T),Mean_q=weighted.mean(x =Mean_q,w =perc, na.rm=T)) %>% 
  ggplot()+
  geom_vline(aes(xintercept=NAME_ENGL ),colour="grey60",alpha=0.3)+
  geom_hline(yintercept = mean(Perm_FW$Curv_Coef$b_Coef,na.rm = T), linewidth=2,alpha=0.5, colour="black")+
  #geom_errorbar(aes(x=NAME_ENGL ,ymax=Mean_b+SD_b,ymin=Mean_b-SD_b))+
  geom_violin(aes(x=NAME_ENGL ,y=Mean_b),alpha=0.6,fill="#b30000",colour=0)+
  #geom_point(data=Mean_Values, aes(x=NAME_ENGL ,y=Mean_b), fill="black",shape=21, size=3, alpha=0.8)+
  geom_jitter(aes(x=NAME_ENGL ,y=Mean_b, fill=disp_plot),shape=21, size=2.5, alpha=0.8)+
  scale_fill_viridis(discrete = T)+
  #scale_fill_gradient(low ="#b30000",high ="#fff2e6" ,limits=c(-1.3,0))+
  scale_y_continuous(limits = c(-0.79,-0.2))+
  geom_text(x="Czechia",y=-0.2,label="Flatter diversity decay", colour="darkblue",size=3)+
  geom_text(x="Czechia",y=-0.79,label="Steeper diversity decay", colour="darkred",size=3)+
  guides(color="none", alpha="none", fill=guide_colourbar("b"))+
  labs(x="",y="Mean Proportional decay rate",subtitle="Proportional decay rate",title="Permanent FW")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=15,face = "bold",color=Water_colors[2]),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1))

plot_list[[4]] <- left_join(Centroides_coordenadas_dep_EU,Perm_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL !="") %>%
  left_join(Perc_Countries,by=c("EcoR_Name","NAME_ENGL")) %>% 
  group_by(NAME_ENGL,EcoR_Name, disp_plot,perc) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
  group_by(NAME_ENGL, disp_plot) %>% 
  summarise(Mean_b=weighted.mean(x =Mean_b,w =perc, na.rm=T),Mean_q=weighted.mean(x =Mean_q,w =perc, na.rm=T)) %>% 
  ggplot()+
  geom_vline(aes(xintercept=NAME_ENGL ),colour="grey60",alpha=0.3)+
  geom_hline(yintercept = mean(Perm_FW$Curv_Coef$q_Coef,na.rm = T), linewidth=2,alpha=0.5, colour="black")+
  #geom_errorbar(aes(x=NAME_ENGL ,ymax=Mean_b+SD_b,ymin=Mean_b-SD_b))+
  geom_violin(aes(x=NAME_ENGL ,y=Mean_q),alpha=0.6,fill="#8A2BE2",colour=0)+
  #geom_point(data=Mean_Values, aes(x=NAME_ENGL ,y=Mean_b), fill="black",shape=21, size=3, alpha=0.8)+
  geom_jitter(aes(x=NAME_ENGL ,y=Mean_q, fill=disp_plot),shape=21, size=2.5, alpha=0.8)+
  scale_fill_viridis(discrete = T)+
  #scale_fill_gradient(low ="#b30000",high ="#fff2e6" ,limits=c(-1.3,0))+
  scale_y_reverse(limits = c(0.355,0.305))+
  geom_text(x="Czechia",y=-0.305,label="Flatter diversity decay", colour="darkblue",size=3)+
  geom_text(x="Czechia",y=-0.355,label="Steeper diversity decay", colour="darkred",size=3)+
  guides(color="none", alpha="none", fill=guide_colourbar("b"))+
  labs(x="",y="Mean Collapsing rate",subtitle="Collapsing rate",title="Permanent FW")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=15,face = "bold",color=Water_colors[2]),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1))

# Temporary

Tempo_FW_DDcoef <- Tempo_FW$Curv_Coef %>% mutate(EcoR_ID=as.character(EcoR))

plot_list[[5]] <- left_join(Centroides_coordenadas_dep_EU,Tempo_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL !="") %>% 
  left_join(Perc_Countries,by=c("EcoR_Name","NAME_ENGL")) %>% 
  group_by(NAME_ENGL,EcoR_Name, disp_plot,perc) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
  group_by(NAME_ENGL, disp_plot) %>% 
  summarise(Mean_b=weighted.mean(x =Mean_b,w =perc, na.rm=T),Mean_q=weighted.mean(x =Mean_q,w =perc, na.rm=T)) %>%  
  ggplot()+
  geom_vline(aes(xintercept=NAME_ENGL ),colour="grey60",alpha=0.3)+
  geom_hline(yintercept = mean(Tempo_FW$Curv_Coef$b_Coef,na.rm = T), linewidth=2,alpha=0.5, colour="black")+
  #geom_errorbar(aes(x=NAME_ENGL ,ymax=Mean_b+SD_b,ymin=Mean_b-SD_b))+
  geom_violin(aes(x=NAME_ENGL ,y=Mean_b),alpha=0.6,fill="#b30000",colour=0)+
  #geom_point(data=Mean_Values, aes(x=NAME_ENGL ,y=Mean_b), fill="black",shape=21, size=3, alpha=0.8)+
  geom_jitter(aes(x=NAME_ENGL ,y=Mean_b, fill=disp_plot),shape=21, size=2.5, alpha=0.8)+
  scale_fill_viridis(discrete = T)+
  #scale_fill_gradient(low ="#b30000",high ="#fff2e6" ,limits=c(-1.3,0))+
  scale_y_continuous(limits = c(-0.79,-0.2))+
  guides(color="none", alpha="none", fill=guide_colourbar("b"))+
  geom_text(x="Czechia",y=-0.2,label="Flatter diversity decay", colour="darkblue",size=3)+
  geom_text(x="Czechia",y=-0.79,label="Steeper diversity decay", colour="darkred",size=3)+
  labs(x="",y="Mean Proportional decay rate",subtitle="Proportional decay rate",title="Temporary FW")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=15,face = "bold",color=Water_colors[3]),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1))

plot_list[[6]] <- left_join(Centroides_coordenadas_dep_EU,Tempo_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL !="") %>% 
  left_join(Perc_Countries,by=c("EcoR_Name","NAME_ENGL")) %>% 
  group_by(NAME_ENGL,EcoR_Name, disp_plot,perc) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
  group_by(NAME_ENGL, disp_plot) %>% 
  summarise(Mean_b=weighted.mean(x =Mean_b,w =perc, na.rm=T),Mean_q=weighted.mean(x =Mean_q,w =perc, na.rm=T)) %>%  
  ggplot()+
  geom_vline(aes(xintercept=NAME_ENGL ),colour="grey60",alpha=0.3)+
  geom_hline(yintercept = mean(Tempo_FW$Curv_Coef$q_Coef,na.rm = T), linewidth=2,alpha=0.5, colour="black")+
  #geom_errorbar(aes(x=NAME_ENGL ,ymax=Mean_b+SD_b,ymin=Mean_b-SD_b))+
  geom_violin(aes(x=NAME_ENGL ,y=Mean_q),alpha=0.6,fill="#8A2BE2",colour=0)+
  #geom_point(data=Mean_Values, aes(x=NAME_ENGL ,y=Mean_b), fill="black",shape=21, size=3, alpha=0.8)+
  geom_jitter(aes(x=NAME_ENGL ,y=Mean_q, fill=disp_plot),shape=21, size=2.5, alpha=0.8)+
  scale_fill_viridis(discrete = T)+
  #scale_fill_gradient(low ="#b30000",high ="#fff2e6" ,limits=c(-1.3,0))+
  scale_y_reverse(limits = c(0.355,0.305))+
  geom_text(x="Czechia",y=-0.305,label="Flatter diversity decay", colour="darkblue",size=3)+
  geom_text(x="Czechia",y=-0.355,label="Steeper diversity decay", colour="darkred",size=3)+
  guides(color="none", alpha="none", fill=guide_colourbar("b"))+
  labs(x="",y="Mean Collapsing rate",subtitle="Collapsing rate",title="Temporary FW")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=15,face = "bold",color=Water_colors[3]),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1))

# Ephemeral 
Ephem_FW_DDcoef <- Ephem_FW$Curv_Coef %>% mutate(EcoR_ID=as.character(EcoR))

plot_list[[7]] <- left_join(Centroides_coordenadas_dep_EU,Ephem_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL !="") %>% 
  left_join(Perc_Countries,by=c("EcoR_Name","NAME_ENGL")) %>% 
  group_by(NAME_ENGL,EcoR_Name, disp_plot,perc) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
  group_by(NAME_ENGL, disp_plot) %>% 
  summarise(Mean_b=weighted.mean(x =Mean_b,w =perc, na.rm=T),Mean_q=weighted.mean(x =Mean_q,w =perc, na.rm=T)) %>%  
  ggplot()+
  geom_vline(aes(xintercept=NAME_ENGL ),colour="grey60",alpha=0.3)+
  geom_hline(yintercept = mean(Ephem_FW$Curv_Coef$b_Coef,na.rm = T), linewidth=2,alpha=0.5, colour="black")+
  #geom_errorbar(aes(x=NAME_ENGL ,ymax=Mean_b+SD_b,ymin=Mean_b-SD_b))+
  geom_violin(aes(x=NAME_ENGL ,y=Mean_b),alpha=0.6,fill="#b30000",colour=0)+
  #geom_point(data=Mean_Values, aes(x=NAME_ENGL ,y=Mean_b), fill="black",shape=21, size=3, alpha=0.8)+
  geom_jitter(aes(x=NAME_ENGL ,y=Mean_b, fill=disp_plot),shape=21, size=2.5, alpha=0.8)+
  scale_fill_viridis(discrete = T)+
  #scale_fill_gradient(low ="#b30000",high ="#fff2e6" ,limits=c(-1.3,0))+
  scale_y_continuous(limits = c(-0.65,-0.1))+
  geom_text(x="Czechia",y=-0.1,label="Flatter diversity decay", colour="darkblue",size=3)+
  geom_text(x="Czechia",y=-0.65,label="Steeper diversity decay", colour="darkred",size=3)+
  guides(color="none", alpha="none", fill=guide_colourbar("b"))+
  labs(x="",y="Mean Proportional decay rate",subtitle="Proportional decay rate",title="Ephemeral FW")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=15,face = "bold",color=Water_colors[4]),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1))

plot_list[[8]] <- left_join(Centroides_coordenadas_dep_EU,Ephem_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL !="") %>% 
  left_join(Perc_Countries,by=c("EcoR_Name","NAME_ENGL")) %>% 
  group_by(NAME_ENGL,EcoR_Name, disp_plot,perc) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
  group_by(NAME_ENGL, disp_plot) %>% 
  summarise(Mean_b=weighted.mean(x =Mean_b,w =perc, na.rm=T),Mean_q=weighted.mean(x =Mean_q,w =perc, na.rm=T)) %>%  
  ggplot()+
  geom_vline(aes(xintercept=NAME_ENGL ),colour="grey60",alpha=0.3)+
  geom_hline(yintercept = mean(Ephem_FW$Curv_Coef$q_Coef,na.rm = T), linewidth=2,alpha=0.5, colour="black")+
  #geom_errorbar(aes(x=NAME_ENGL ,ymax=Mean_b+SD_b,ymin=Mean_b-SD_b))+
  geom_violin(aes(x=NAME_ENGL ,y=Mean_q),alpha=0.6,fill="#8A2BE2",colour=0)+
  #geom_point(data=Mean_Values, aes(x=NAME_ENGL ,y=Mean_b), fill="black",shape=21, size=3, alpha=0.8)+
  geom_jitter(aes(x=NAME_ENGL ,y=Mean_q, fill=disp_plot),shape=21, size=2.5, alpha=0.8)+
  scale_fill_viridis(discrete = T)+
  #scale_fill_gradient(low ="#b30000",high ="#fff2e6" ,limits=c(-1.3,0))+
  scale_y_reverse(limits = c(0.345,0.28))+
  geom_text(x="Czechia",y=-0.28,label="Flatter diversity decay", colour="darkblue",size=3)+
  geom_text(x="Czechia",y=-0.345,label="Steeper diversity decay", colour="darkred",size=3)+
  guides(color="none", alpha="none", fill=guide_colourbar("b"))+
  labs(x="",y="Mean Collapsing rate",subtitle="Collapsing rate",title="Ephemeral FW")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=15,face = "bold",color=Water_colors[4]),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1))


Legend_1<- cowplot::get_legend(
  left_join(Centroides_coordenadas_dep_EU,Ephem_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
    filter(NAME_ENGL !="") %>% 
    left_join(Perc_Countries,by=c("EcoR_Name","NAME_ENGL")) %>% 
    group_by(NAME_ENGL,EcoR_Name, disp_plot,perc) %>% 
    summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T)) %>%drop_na() %>% 
    group_by(NAME_ENGL, disp_plot) %>% 
    summarise(Mean_b=weighted.mean(x =Mean_b,w =perc, na.rm=T),Mean_q=weighted.mean(x =Mean_q,w =perc, na.rm=T)) %>%  
    ggplot()+
    geom_jitter(aes(x=NAME_ENGL ,y=Mean_q, fill=disp_plot),shape=21, size=2.5, alpha=0.8)+
    scale_fill_viridis(discrete = T)+
    #scale_fill_gradient(low ="#b30000",high ="#fff2e6" ,limits=c(-1.3,0))+
    guides(color="none", alpha="none")+
    labs(x="",y="Mean Collapsing rate",fill="Dispersal ability",title="Ephemeral FW")+
    theme_classic()+
    theme(legend.position = "bottom")+
    theme(plot.title = element_text(size=15,face = "bold",color=Water_colors[4]),
          axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1))
)

Legend_2<- cowplot::get_legend(left_join(Centroides_coordenadas_dep_EU,Ephem_FW_DDcoef,by="EcoR_ID",relationship="many-to-many") %>% 
  filter(NAME_ENGL!="") %>% group_by(NAME_ENGL) %>% 
  summarise(Mean_b=mean(b_Coef,na.rm = T),Mean_q=mean(q_Coef,na.rm = T),
            SD_b=sd(b_Coef,na.rm = T),SD_q=sd(q_Coef,na.rm = T)) %>%drop_na() %>% 
  ggplot()+
  geom_hline(yintercept = mean(Ephem_FW$Curv_Coef$q_Coef,na.rm = T), linewidth=2,alpha=0.5, colour="black")+
  geom_errorbar(aes(x=NAME_ENGL,ymax=Mean_q+SD_q,ymin=Mean_q-SD_q))+
  geom_point(aes(x=NAME_ENGL,y=Mean_q, fill=Mean_q),shape=21, size=3, alpha=0.8)+
  scale_fill_gradient(low ="#f2e6ff",high ="#8A2BE2" ,limits=c(0.30,0.347))+
  scale_y_reverse(limits = c(0.36,0.28))+
  guides(color="none", alpha="none", fill=guide_colourbar("Mean q"))+
  labs(x="",y="Mean Collapsing rate",subtitle="Collapsing rate",title="Ephemeral FW")+
  theme_classic()+
  theme(legend.direction = "vertical")+
  theme(plot.title = element_text(size=15,face = "bold",color=Water_colors[4]),
        axis.text.x = element_text(size=10, angle = 50,vjust = 1,hjust = 1))
)



png(filename = "Eu_Countries.png",width = 1200*5,height = 400*5, units = "px",res = 300)
gridExtra::grid.arrange(
  plot_list[[3]],plot_list[[5]],plot_list[[7]],plot_list[[1]],
  plot_list[[4]],plot_list[[6]],plot_list[[8]],plot_list[[2]],
nrow=2)
dev.off()

