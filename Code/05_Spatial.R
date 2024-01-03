library(tidyverse)
library(ggplot2)
library(patchwork)
library(sf)
library(spdep)
library(broom)
library(knitr)
library(stargazer)
spatial <- st_read('/Users/pranav/Documents/Spatial/Data/Sectorboundary_7Nov2022/Sectorboundary_7Nov2022.shp')

#Load datasets

#Merge Datasets with Population number, remove Wolbachia spatial units
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

# HDB w Airpoll Spatial Autocorrelation 
HDB2014spat <- merge(spatial, HDB_w_airpoll, by = 'Sector_ID') %>%
  dplyr::select(Sector_ID,PrevYear_Inc,Year,NDVI_A,V_Density,Forest_P, Grass_P, MVege_P,
          Building_P,HDB_RU, A_HDB_H,A_HDB_A,D_To_Drain,length_D,X300m.P,X500m_P,
          Drt_mean, H60Min_mean, MeanT_mean, MeanWS_mean, pm10,o3, no2, so2,co,
          pop,CurrentYear_count) %>%
  mutate( CurrentYear_Inc = (CurrentYear_count/pop) * 1000 )%>%
  mutate ( logp = log(pop))

#Create list of HDB dataframes for each year
HDB2014_spat_list <- split(HDB2014spat, HDB2014spat$Year)

# Test dengue incidence rates for spatial autocorrelation (Public Housing)
moran_results_HDB <- list()
for (i in 1:7) {
  nb <- poly2nb(HDB2014_spat_list[[i]], queen = TRUE)
  lw <- nb2listw(nb,style = 'W', zero.policy = TRUE)
  moran_results_HDB[[i]] <- tidy(moran.test(HDB2014_spat_list[[i]]$CurrentYear_Inc,lw,zero.policy = TRUE))
}

combined_results_HDB <- do.call(rbind,moran_results_HDB)

# Landed w Airpoll Spatial Autocoreelation
Landed2014spat <- merge(spatial, Landed_w_airpoll, by = 'Sector_ID') %>%
  select(Sector_ID,PrevYear_count,Year,NDVI_A,V_Density,Forest_P, Grass_P, MVege_P,
         Building_P,HDB_RU,Condo_n,Landed_n,D_To_Drain,length_D,X300m.P,X500m_P,
         Drt_mean, H60Min_mean, MeanT_mean, MeanWS_mean, pm10,o3, no2, so2,co,
         pop,CurrentYear_count) %>%
  mutate(CurrentYearInc = (CurrentYear_count/pop) * 1000 )

#Create list of Landed dataframes
Landed2014_spat_list <- split(Landed2014spat, Landed2014spat$Year)

#Test dengue incidence rates for spatial autocorrelation (Private Housing)
moran_results_Landed <- list()
for (i in 1:7) {
  nb2 <- poly2nb(Landed2014_spat_list[[i]], queen = TRUE)
  lw2 <- nb2listw(nb2, style = 'W', zero.policy = TRUE) 
  moran_results_Landed[[i]] <- tidy(moran.test(Landed2014_spat_list[[i]]$CurrentYearInc,lw2,zero.policy = TRUE))
}
combined_results_Landed <- do.call(rbind, moran_results_Landed)


#Test model residuals for SAC (Public)
HDB_resid <- HDB2014spat %>%
  mutate(resid = resid(fm_2_1)) %>%
  dplyr::select(Sector_ID,Year, geometry, resid)

HDB_resid_list <- split(HDB_resid, HDB_resid$Year)

for (i in 1:7) {
  nb3 <- poly2nb(HDB_resid_list[[i]], queen = TRUE)
  lw3 <- nb2listw(nb3, style = 'W', zero.policy = TRUE)
  resid_results_HDB[[i]] <- tidy(moran.test(HDB_resid_list[[i]]$resid,lw3,zero.policy = TRUE))
}

HDB_resid_results <- do.call(rbind, resid_results_HDB)

#Test model residuals for SAC (Private)
Landed_resid <- Landed2014spat %>%
  mutate(resid = resid(fm_4_1)) %>%
  dplyr::select(Sector_ID, Year, geometry,resid)

Landed_resid_list <- split(Landed_resid,Landed_resid$Year)

for (i in 1:7) {
  nb4 <- poly2nb(Landed_resid_list[[i]],queen = TRUE)
  lw4 <- nb2listw(nb4, style = 'W', zero.policy = TRUE)
  resid_results_Landed[[i]] <- tidy(moran.test(Landed_resid_list[[i]]$resid, lw4, zero.policy = TRUE))
}

Landed_resid_results <- do.call(rbind, resid_results_Landed)