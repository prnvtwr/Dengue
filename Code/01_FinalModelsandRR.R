# Set working directory
setwd("/Users/pranav/Documents/Dengue vs Covid")

#Import Packages
library(tidyverse)
library(ggplot2)
library(stargazer)
library(MASS)
library(fastshap)
library(patchwork)
library(shapviz)
library(scales)
library(mgcv)
library(sf)

#Import Datasets
HDB_w_airpoll <- read.csv('/Users/pranav/Documents/Dengue vs Covid/HDB_w_airpoll_new.csv')
HDB_wo_airpoll <- read.csv('/Users/pranav/Documents/Dengue vs Covid/HDB_wo_airpoll_new.csv')
Landed_wo_airpoll <- read.csv("~/Documents/Dengue vs Covid/Landed_wo_airpoll_new.csv")
Landed_w_airpoll <- read.csv('/Users/pranav/Documents/Dengue vs Covid/Landed_w_airpoll_new.csv')
dwell_HDB<-read.csv('/Users/pranav/Documents/Dengue vs Covid/dwell_HDB.csv')
dwell_landed <- read.csv('/Users/pranav/Documents/Dengue vs Covid/dwell_landed.csv')

#Merge Datasets with Population number
HDB_w_airpoll <- inner_join(HDB_w_airpoll,dwell_HDB, by = 'Sector_ID', relationship = 'many-to-one') %>%
  mutate( pop  = Total_dwelling * 4) %>%
  mutate (logp = log(pop)) %>%
  mutate (PrevYear_Inc = (PrevYear_count/ pop) * 1000) %>%
  filter (Wolb_Q1 != 1) %>%
  filter (Wolb_Q2 != 1)

Landed_w_airpoll <- inner_join(Landed_w_airpoll,dwell_landed, by = 'Sector_ID', relationship = 'many-to-one') %>%
  mutate (pop = Total_dwelling * 3) %>%
  mutate (logp = log(pop)) %>%
  mutate (PrevYear_Inc = (PrevYear_count / pop) * 1000) %>%
  filter (Wolb_Q1 != 1) %>%
  filter (Wolb_Q2 != 1)


dataset2_1 <- (HDB_w_airpoll[c('X','PrevYear_Inc','Year','NDVI_A','V_Density','Forest_P', 'Grass_P', 'MVege_P',
                               'Building_P','HDB_RU', 'A_HDB_H','A_HDB_A','D_To_Drain','length_D','X300m.P','X500m_P',
                               'Drt_mean','H60Min_mean','MeanT_mean','MeanWS_mean','pm10','o3', 'no2', 'so2','co',
                               'logp','CurrentYear_count')])


dataset4_1 <- (Landed_w_airpoll[c('X','PrevYear_Inc','Year','NDVI_A','V_Density', 'Forest_P','Grass_P',
                                  'MVege_P','Building_P','Condo_n','Landed_n','HDB_RU','D_To_Drain','length_D','X300m.P',
                                  'X500m_P','Drt_mean','H60Min_mean','MeanT_mean','MeanWS_mean','pm10', 'o3', 'no2', 'so2', 'co','logp',
                                  'CurrentYear_count')])


#GLMS
glm1 <- glm.nb(CurrentYear_count ~ . -logp-X  + offset(logp), data = dataset2_1)
glm2 <-glm.nb(CurrentYear_count ~.-logp-X + offset(logp), data = dataset4_1)


#GAMs
fm_2_1 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =11)+s(Year, k = 6) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                s(Grass_P) + s(MVege_P, k = 4)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A, k =10) +
                s(D_To_Drain) + s(length_D) +s(X300m.P) + s(X500m_P)+ s(Drt_mean)+ s(H60Min_mean)+ s(MeanT_mean)+ s(MeanWS_mean) +
                s(pm10, k=12) + s(o3,k=11) + s(no2,k=12) + s(so2) + s(co) + offset(logp) ,data = dataset2_1,
              family = nb(), method = 'REML')

fm_4_1 <- gam( CurrentYear_count ~ s(PrevYear_Inc, k =12)+s(Year, k =7) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                 s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(HDB_RU) + s(D_To_Drain) + s(length_D) +
                 s(X300m.P) + s(X500m_P) +s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(pm10) + s(o3) + s(no2)+ 
                 s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')


# Public Housing RR

mean_dataset2_1 <- colMeans(dataset2_1)
names(mean_dataset2_1)<- colnames(dataset2_1)
mean_dataset2_1 <-data.frame(t(mean_dataset2_1))
quantile_grid <- seq(0,1000)/1000
j<-1
store_rr_HDB <-list()

for (i in 1:25){
  pred <- quantile(dataset2_1[,i],quantile_grid, na.rm =T)
  pred_df <- data.frame(mean_dataset2_1[seq(1,i-1)], pred, mean_dataset2_1[seq(i+1,length(mean_dataset2_1))])
  names(pred_df)[i] = names(mean_dataset2_1)[i]
  mean_pred <- as.vector(exp(predict(fm_2_1,mean_dataset2_1))) #mean prediction given that variable of interest is at 50th quantile
  pred_rest <- predict(fm_2_1,pred_df,se.fit=T)   #prediction for all other values of variable of interest
  mean <- exp(pred_rest$fit)
  lo <- exp(pred_rest$fit - (1.96 * pred_rest$se.fit))
  up <- exp(pred_rest$fit + (1.96* pred_rest$se.fit))
  out <- cbind(mean/mean_pred,lo/mean_pred,up/mean_pred,pred)
  
  colnames(out) <- c("mean_rr","lo_rr","up_rr", "x")
  store_rr_HDB[[j]] <- as.data.frame(out)
  
  j <-j +1
}

# Private Housing RR
mean_dataset4_1 <- colMeans(dataset4_1)
names(mean_dataset4_1)<- colnames(dataset4_1)
mean_dataset4_1 <-data.frame(t(mean_dataset4_1))
quantile_grid <- seq(0,1000)/1000
j<-1
store_rr_Landed <-list()

for (i in 1:25){
  pred <- quantile(dataset4_1[,i],quantile_grid, na.rm =T)
  pred_df <- data.frame(mean_dataset4_1[seq(1,i-1)], pred, mean_dataset4_1[seq(i+1,length(mean_dataset4_1))])
  names(pred_df)[i] = names(mean_dataset4_1)[i]
  mean_pred <- as.vector(exp(predict(fm_4_1,mean_dataset4_1))) #mean prediction given that variable of interest is at 50th quantile
  pred_rest <- predict(fm_4_1,pred_df,se.fit=T)   #prediction for all other values of variable of interest
  mean <- exp(pred_rest$fit)
  lo <- exp(pred_rest$fit - (1.96 * pred_rest$se.fit))
  up <- exp(pred_rest$fit + (1.96* pred_rest$se.fit))
  out <- cbind(mean/mean_pred,lo/mean_pred,up/mean_pred,pred)
  
  colnames(out) <- c("mean_rr","lo_rr","up_rr", "x")
  store_rr_Landed[[j]] <- as.data.frame(out)
  
  j <-j +1
}