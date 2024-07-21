setwd("Q:/1 GIS/json/Adjust DEM 20240423/GEE table/")
library(readxl)
library(car)
library(MuMIn)
library(lme4)
library(nlme)
d <- read_excel("ForestLoss_driver_Data_ALL.xls")
d_transform=d
d_transform$ForestLoss=scale(d$ForestLoss)
d_transform$HFP=scale(log(d$HFP+1.00001))
d_transform$Slope=scale(d$Slope)
d_transform$MAP=scale(log(d$MAP+1.00001))
d_transform$WaterDeficit=scale(exp(d$WaterDeficit/2413))

# Calculating VIF
lmForestLoss_transform = lm(ForestLoss~HFP+Slope+MAP+WaterDeficit, data = d_transform) #Create the linear regression
vif_values <- vif(lmForestLoss_transform)



#Model selection for the factors of Band-level forest loss rate
options(na.action = "na.fail")# Required for dredge to run
ForestLoss_mixed_transform_full = lmer(ForestLoss~HFP+Slope+MAP+WaterDeficit+(1|Mountain_ID),data = d_transform)
dd <- dredge(ForestLoss_mixed_transform_full,evaluate=T,rank = AIC)
summary(model.avg(dd))
options(na.action = "na.omit") # set back to default

###########Fig.5a

##Function of inverse transformation
ForestLoss_trans<-function(v){
  v=v*sd(d$ForestLoss)+mean(d$ForestLoss)
  return(v)
} 
HFP_trans<-function(v){
  v=v*sd(log(d$HFP+1.00001))+mean(log(d$HFP+1.00001))
  v=exp(v)-1.00001
  return(v)
} 
MAP_trans<-function(v){
  v=v*sd(log(d$MAP+1.00001))+mean(log(d$MAP+1.00001))
  v=exp(v)-1.00001
  return(v)
} 
Slope_trans<-function(v){
  v=v*sd(d$Slope)+mean(d$Slope)
  return(v)
} 

lme.predict<-function(mod, newdat, se.mult){
  pred <- as.vector(predict(mod, newdat, level = 0))
  Designmat <- model.matrix(formula(mod)[-2], newdat)
  predvar <- diag(Designmat %*% vcov(mod,full = TRUE) %*% t(Designmat)) 
  #vcov(mod,full = TRUE) Full = True, otherwise no upper and lower CI
  #https://www.rdocumentation.org/packages/merDeriv/versions/0.2-4/topics/vcov.lmerMod
  #https://stat.ethz.ch/pipermail/r-sig-mixed-models/2013q3/020867.html
  SE <- sqrt(predvar) 
  upper<- pred + (se.mult*SE)
  lower<- pred - (se.mult*SE) 
  return(data.frame(pred, upper, lower))
}	

###plot_code
ForestLoss_mixed_transform = lme(ForestLoss~HFP+Slope+MAP,random = ~1|Mountain_ID,data = d_transform)
summary(ForestLoss_mixed_transform) 
confint(ForestLoss_mixed_transform)
x_for_plot<-seq(min(d_transform$HFP),max(d_transform$HFP), length.out=100)
predictions_to_plot <-lme.predict(mod=ForestLoss_mixed_transform,
                                  newdat=data.frame(HFP= x_for_plot,
                                                    MAP = mean(d_transform$MAP),
                                                    Slope = mean(d_transform$Slope)),
                                  se.mult = 1.96)

##Fig.5a_HFP
predict_data<-data.frame(x_HFP=HFP_trans(x_for_plot),
                         HFP_y=ForestLoss_trans(predictions_to_plot$pred)*100,
                         lwr=ForestLoss_trans(predictions_to_plot$lower)*100,
                         upr=ForestLoss_trans(predictions_to_plot$upper)*100)

library("ggplot2")
library("ggtext")
text_type<-element_text(colour='black',size=13.5)
ggplot(data = predict_data, aes(x=x_HFP,y=HFP_y))+geom_ribbon(aes(ymin = lwr,ymax = upr),fill="#3B95C2",alpha=0.4)+geom_point(data=d,aes(x=HFP,y=ForestLoss*100),color='#797979',size=0.3,alpha=0.8,shape=16)+geom_line(linewidth=1,color='#0E60A5')+
  coord_cartesian(ylim=c(0,100),xlim=c(0,max(d$HFP)))+scale_y_continuous(name='Forest loss rate (%)')+xlab('HFP')+theme(
    axis.line = element_line(linewidth=0.6),panel.background = element_blank(),panel.grid = element_blank(),axis.ticks.length=unit(.1, "cm"),
    panel.border = element_blank(),axis.text = text_type,axis.title = text_type)


##Fig.5a_MAP
library(nlme)
ForestLoss_mixed_transform = lme(ForestLoss~HFP+Slope+MAP,random = ~1|Mountain_ID,data = d_transform)

x_for_plot<-seq(min(d_transform$MAP),max(d_transform$MAP), length.out=100)
predictions_to_plot <-lme.predict(mod=ForestLoss_mixed_transform,
                                  newdat=data.frame(HFP= mean(d_transform$HFP),
                                                    MAP = x_for_plot,
                                                    Slope = mean(d_transform$Slope)),
                                  se.mult = 1.96)

predict_data<-data.frame(x_MAP=MAP_trans(x_for_plot),
                         MAP_y=ForestLoss_trans(predictions_to_plot$pred)*100,
                         lwr=ForestLoss_trans(predictions_to_plot$lower)*100,
                         upr=ForestLoss_trans(predictions_to_plot$upper)*100)
library("ggplot2")
library("ggtext")
text_type<-element_text(colour='black',size=13.5)
ggplot(data = predict_data, aes(x=x_MAP,y=MAP_y))+geom_ribbon(aes(ymin = lwr,ymax = upr),fill="#3B95C2",alpha=0.4)+geom_point(data=d,aes(x=MAP,y=ForestLoss*100),color='#797979',size=0.3,alpha=0.8,shape=16)+geom_line(linewidth=1,color='#0E60A5')+
  coord_cartesian(ylim=c(0,100),xlim=c(min(d$MAP),max(d$MAP)))+scale_x_continuous(breaks = c(0,2000,4000),labels = c("0","2,000","4,000"))+theme(
    axis.line = element_line(linewidth=0.6),panel.background = element_blank(),panel.grid = element_blank(),axis.ticks.length=unit(.1, "cm"),
    panel.border = element_blank(),axis.text = text_type,axis.title = text_type)


##Fig.5a_Slope
x_for_plot<-seq(min(d_transform$Slope),max(d_transform$Slope), length.out=100)
predictions_to_plot <-lme.predict(mod=ForestLoss_mixed_transform,
                                  newdat=data.frame(HFP= mean(d_transform$HFP),
                                                    MAP = mean(d_transform$MAP),
                                                    Slope = x_for_plot),
                                  se.mult = 1.96)


predict_data<-data.frame(x_Slope=Slope_trans(x_for_plot),
                         Slope_y=ForestLoss_trans(predictions_to_plot$pred)*100,
                         lwr=ForestLoss_trans(predictions_to_plot$lower)*100,
                         upr=ForestLoss_trans(predictions_to_plot$upper)*100)
library("ggplot2")
library("ggtext")
text_type<-element_text(colour='black',size=13.5)
ggplot(data = predict_data, aes(x=x_Slope,y=Slope_y))+geom_ribbon(aes(ymin = lwr,ymax = upr),fill="#3B95C2",alpha=0.4)+geom_point(data=d,aes(x=Slope,y=ForestLoss*100),color='#797979',size=0.3,alpha=0.8,shape=16)+geom_line(linewidth=1,color='#0E60A5')+
  coord_cartesian(ylim=c(0,100),xlim=c(0,max(d$Slope)))+scale_y_continuous(name='Forest loss rate (%)')+xlab('Slope')+theme(
    axis.line = element_line(linewidth=0.6),panel.background = element_blank(),panel.grid = element_blank(),axis.ticks.length=unit(.1, "cm"),
    panel.border = element_blank(),axis.text = text_type,axis.title = text_type)


##Import FLER scores
setwd("Q:/1 GIS/json/Adjust DEM 20240423/GEE table")
Va <- read.csv(file = "Mountain_Index_F30.csv", header=T)
Va_subset=data.frame()
ID<-unique(d$Mountain_ID)
for (value in ID){
  data_subset<-subset(Va, ID == value);
  Va_subset<-rbind(Va_subset,data_subset)
};

##function Factor_ER_calculate
Factor_ER_calculation<-function(data){
  ID=unique(d$Mountain_ID);
  Factor_ER_further=data.frame()
  for (value in ID){
    data_subset<-subset(d, Mountain_ID == value);
    Mountain_ID_1 = data_subset$Mountain_ID[1]
    ##HFP
    if (data_subset$HFP_MWide[1]==0){
      HFP_ER<-0.5
    }else {
      data_subset <-data_subset[order(data_subset$HFP,decreasing = TRUE),]
      HFP_ER<-(sum(data_subset$Elevation*seq(10, 1))-220)/(385-220)
    }
    ##Slope
      data_subset <-data_subset[order(data_subset$Slope,decreasing = TRUE),]
      Slope_ER<-(sum(data_subset$Elevation*seq(10,1))-220)/(385-220)
    ##MAP
      data_subset <-data_subset[order(data_subset$MAP,decreasing = TRUE),]
      MAP_ER<-(sum(data_subset$Elevation*seq(10,1))-220)/(385-220)
      b_table = data.frame(
        Mountain_ID_1=c(Mountain_ID_1),
        HFP_ER = c(HFP_ER),
        Slope_ER = c(Slope_ER),
        MAP_ER = c(MAP_ER), 
        HFP_MWide = c(data_subset$HFP_MWide[1]),
        Slope_MWide = c(data_subset$Slope_MWide[1]),
        MAP_MWide = c(data_subset$MAP_MWide[1]))
      Factor_ER_further<-rbind(DER_further,b_table)
  }
  return(Factor_ER_further)
}

##Calculate the elevational rank indices of factors
Table_coeffcient=Factor_ER_calculation(d);
Table_coeffcient$FLER<-Va_subset$FLER
Table_coeffcient$country<-Va_subset$Country
name_order<-c("Mountain_ID_1","country","FLER","HFP_ER","MAP_ER","Slope_ER","HFP_MWide","Slope_MWide","MAP_MWide")
Table_coeffcient <-Table_coeffcient[,name_order]
df <-Table_coeffcient


##Data transformation
df$HFP_MWide_scaled <- scale(log(df$HFP_MWide+1.00001))
df$Slope_MWide_scaled <- scale(df$Slope_MWide)
df$MAP_MWide_scaled <- scale(log(df$MAP_MWide+1.00001))

##Calculating VIF
ForestLoss_lm_vif <-lm(FLER~HFP_MWide_scaled+HFP_ER+MAP_MWide_scaled+MAP_ER+Slope_MWide_scaled+Slope_ER,data = df)
vif_values <- vif(ForestLoss_lm_vif)
vif_values

##Model selection
a<-cor(df[, c('HFP_ER','Slope_ER','MAP_ER','HFP_MWide_scaled','Slope_MWide_scaled','MAP_MWide_scaled')])
library(MuMIn)
options(na.action = "na.fail")# Required for dredge to run
ForestLoss_lm = lm(FLER~HFP_MWide_scaled*HFP_ER+MAP_MWide_scaled*MAP_ER+Slope_MWide_scaled*Slope_ER,data = df)
dd <- dredge(ForestLoss_lm,evaluate=T,rank = AIC)
options(na.action = "na.omit") # set back to default
summary(ForestLoss_lm)
confint(ForestLoss_lm)

###########Fig.5c
Model_result_FLER<-summary(ForestLoss_lm)[["coefficients"]]
Model_result_FLER<- Model_result_FLER[-1,]
colnames(Model_result_FLER)<-c('coef','se','t_value','P') 
new_row <- as.data.frame(Model_result_FLER[1:4,])
new_row[,1:4]<-NA
rownames(new_row)<- c('1','2','3','4')
Model_result_FLER<-rbind(as.data.frame(Model_result_FLER),new_row)
name_order<-c("HFP_ER","HFP_MWide_scaled","HFP_MWide_scaled:HFP_ER","1","2","MAP_ER","MAP_MWide_scaled","MAP_MWide_scaled:MAP_ER","3","4","Slope_ER","Slope_MWide_scaled","Slope_MWide_scaled:Slope_ER")
Model_result_FLER <-Model_result_FLER[name_order,]

library(ggplot2)
limits <- aes(xmax = coef + 1.96*se, xmin=coef - 1.96*se)
ggplot(data = Model_result_FLER, aes(coef,seq(1,13)))+
  geom_errorbar(limits, linewidth=0.6,width=0.25,alpha=0.9,color="#0E60A5")+geom_point(size=1,alpha=0.9,shape=16,colour='black') +scale_y_continuous(name=NULL,breaks=c(1,2,3,6,7,8,11,12,13),labels=c("HFP_ER","HFP_MWide","HFP_ER*HFP_MWide","MAP_ER","MAP_MWide","MAP_ER*MAP_MWide","Slope_ER","Slope_MWide","Slope_ER*Slope_MWide"),trans="reverse")+scale_x_continuous("Standardized coefficient",breaks=c(-0.5,-0.25,0,0.25,0.5),limits = c(-0.55,+0.55),labels=c("-0.5","-0.25","0","0.25","0.5"))+theme(
    axis.line = element_line(linewidth=0.4),panel.background = element_blank(),panel.grid = element_blank(),axis.ticks.length=unit(.1, "cm"),
    panel.border = element_blank(),axis.text.y = element_text(colour='black'),axis.text.x = element_text(colour='black'))+geom_vline(aes(xintercept=0),linetype="dashed",linewidth=0.6)