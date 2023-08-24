library(stargazer)
library(jtools)
library(MASS)

#HDB2014 Stepwise
HDB2014dataset <- dataset2_1 %>%
  select(-X)
HDB2014intonly <- glm.nb(CurrentYear_count ~ 1 + offset(logp), data = HDB2014dataset)
HDB2014all <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                                            Forest_P + Grass_P + MVege_P + Building_P + HDB_RU + A_HDB_H + 
                                            A_HDB_A + D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + 
                                            H60Min_mean + MeanT_mean + MeanWS_mean + pm10 + o3 + no2 + 
                                            so2 + co  + offset(logp), data = HDB2014dataset)

HDB2014forward <- step(HDB2014intonly, direction = 'forward', scope = formula(HDB2014all), trace = 0)
HDB2014backward <- step(HDB2014all, direction ='backward')

#HDB2014 GAMs
HDB2014s1 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =11)+s(Year, k = 6) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                   s(Grass_P) + s(MVege_P, k = 4)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A, k =10) +
                   s(D_To_Drain) + s(length_D) +s(X300m.P) + s(X500m_P)+ s(Drt_mean)+ s(H60Min_mean)+ s(MeanT_mean)+ s(MeanWS_mean) +
                   s(pm10, k=12) + s(o3,k=11) + s(no2,k=12) + s(so2) + s(co) + offset(logp) ,data = HDB2014dataset,
                 family = nb(), method = 'REML')
HDB2014s2 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =11)+s(Year, k = 6) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                   s(Grass_P) + s(MVege_P, k = 4)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A, k =10) +
                   s(D_To_Drain) + s(length_D) + s(X500m_P)+ s(Drt_mean)+ s(H60Min_mean)+ s(MeanT_mean)+ s(MeanWS_mean) +
                   s(pm10, k=12) + s(o3,k=11) + s(no2,k=12) + s(so2) + s(co) + offset(logp) ,data = HDB2014dataset,
                 family = nb(), method = 'REML')
HDB2014s3 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =11)+s(Year, k = 6) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                   s(Grass_P) + s(MVege_P, k = 4)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A, k =10) +
                   s(D_To_Drain) + s(length_D)+ s(Drt_mean)+ s(H60Min_mean)+ s(MeanT_mean)+ s(MeanWS_mean) +
                   s(pm10, k=12) + s(o3,k=11) + s(no2,k=12) + s(so2) + s(co) + offset(logp) ,data = HDB2014dataset,
                 family = nb(), method = 'REML')
HDB2014s4 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =11)+s(Year, k = 6) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                   s(MVege_P, k = 4)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A, k =10) +
                   s(D_To_Drain) + s(length_D)+ s(Drt_mean)+ s(H60Min_mean)+ s(MeanT_mean)+ s(MeanWS_mean) +
                   s(pm10, k=12) + s(o3,k=11) + s(no2,k=12) + s(so2) + s(co) + offset(logp) ,data = HDB2014dataset,
                 family = nb(), method = 'REML')
HDB2014s5 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =11)+s(Year, k = 6) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                   s(MVege_P, k = 4)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A, k =10) +
                   s(D_To_Drain) + s(length_D)+ s(Drt_mean)+ s(H60Min_mean)+ s(MeanT_mean)+ s(MeanWS_mean) +
                   s(pm10, k=12) + s(o3,k=11) + s(no2,k=12) + s(so2) + offset(logp) ,data = HDB2014dataset,
                 family = nb(), method = 'REML')
HDB2014s6 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =11)+s(Year, k = 6) + s(NDVI_A) + s(Forest_P) +
                   s(MVege_P, k = 4)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A, k =10) +
                   s(D_To_Drain) + s(length_D)+ s(Drt_mean)+ s(H60Min_mean)+ s(MeanT_mean)+ s(MeanWS_mean) +
                   s(pm10, k=12) + s(o3,k=11) + s(no2,k=12) + s(so2) + offset(logp) ,data = HDB2014dataset,
                 family = nb(), method = 'REML')
HDB2014s7 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =11)+s(Year, k = 6) + s(NDVI_A) + s(Forest_P) +
                   s(MVege_P, k = 4)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A, k =10) +
                   s(D_To_Drain) + s(Drt_mean)+ s(H60Min_mean)+ s(MeanT_mean)+ s(MeanWS_mean) +
                   s(pm10, k=12) + s(o3,k=11) + s(no2,k=12) + s(so2) + offset(logp) ,data = HDB2014dataset,
                 family = nb(), method = 'REML')
#HDB2014 GLMs
HDB2014glms1 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + Grass_P + MVege_P + Building_P + HDB_RU + A_HDB_H + 
                         A_HDB_A + D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + 
                         H60Min_mean + MeanT_mean + MeanWS_mean + pm10 + o3 + no2 + 
                         so2 + co + offset(logp), data = HDB2014dataset)
HDB2014glms2 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + Grass_P + MVege_P + Building_P + HDB_RU + A_HDB_H + 
                         A_HDB_A + D_To_Drain + length_D + X500m_P + Drt_mean + H60Min_mean + 
                         MeanT_mean + MeanWS_mean + pm10 + o3 + no2 + so2 + co + offset(logp), data = HDB2014dataset)

HDB2014glms3 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + Grass_P + MVege_P + Building_P + HDB_RU + A_HDB_H + 
                         A_HDB_A + D_To_Drain + length_D + Drt_mean + H60Min_mean + 
                         MeanT_mean + MeanWS_mean + pm10 + o3 + no2 + so2 + co + offset(logp), data = HDB2014dataset)
HDB2014glms4 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + MVege_P + Building_P + HDB_RU + A_HDB_H + A_HDB_A + 
                         D_To_Drain + length_D + Drt_mean + H60Min_mean + MeanT_mean + 
                         MeanWS_mean + pm10 + o3 + no2 + so2 + co + offset(logp), data = HDB2014dataset)
HDB2014glms5 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + MVege_P + Building_P + HDB_RU + A_HDB_H + A_HDB_A + 
                         D_To_Drain + length_D + Drt_mean + H60Min_mean + MeanT_mean + 
                         MeanWS_mean + pm10 + o3 + no2 + so2 + offset(logp), data = HDB2014dataset)
HDB2014glms6 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + Forest_P + 
                         MVege_P + Building_P + HDB_RU + A_HDB_H + A_HDB_A + D_To_Drain + 
                         length_D + Drt_mean + H60Min_mean + MeanT_mean + MeanWS_mean + 
                         pm10 + o3 + no2 + so2 + offset(logp), data = HDB2014dataset)
HDB2014glms7 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + Forest_P + 
                         MVege_P + Building_P + HDB_RU + A_HDB_H + A_HDB_A + D_To_Drain + 
                         Drt_mean + H60Min_mean + MeanT_mean + MeanWS_mean + pm10 + 
                         o3 + no2 + so2 + offset(logp), data = HDB2014dataset)



#Landed 2014 Stepwise
Landed2014dataset <- dataset4_1 %>%
  dplyr::select(-X)

Landed2014intonly <- glm.nb(CurrentYear_count ~ 1 + offset(logp), data = Landed2014dataset)
Landed2014all <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                       Forest_P + Grass_P + MVege_P + Building_P + Condo_n + Landed_n + 
                       HDB_RU + D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + 
                       H60Min_mean + MeanT_mean + MeanWS_mean + pm10 + o3 + no2 + 
                       so2 + co  + offset(logp), data = Landed2014dataset)
Landed2014backward <- step(Landed2014all, direction = 'backward')

#Landed 2014 GAM

Landed2014s1 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =12)+s(Year, k =7) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                       s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(HDB_RU) + s(D_To_Drain) + s(length_D) +
                       s(X300m.P) + s(X500m_P) +s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(pm10) + s(o3) + s(no2)+ 
                       s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')
Landed2014s2 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =12)+s(Year, k =7) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                       s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(D_To_Drain) + s(length_D) +
                       s(X300m.P) + s(X500m_P) +s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(pm10) + s(o3) + s(no2)+ 
                       s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')
Landed2014s3 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =12)+s(Year, k =7)+ s(V_Density) + s(Forest_P) +
                      s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(D_To_Drain) + s(length_D) +
                      s(X300m.P) + s(X500m_P) +s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(o3) + s(no2)+ 
                      s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')
Landed2014s4 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =12)+s(Year, k =7)+ s(V_Density) + s(Forest_P) +
                          s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(D_To_Drain) + s(length_D) +
                          s(X300m.P)+s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(o3) + s(no2)+ 
                          s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')
Landed2014s5 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =12)+s(Year, k =7)+ s(V_Density) +
                      s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(D_To_Drain) + s(length_D) +
                      s(X300m.P)+s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(o3) + s(no2)+ 
                      s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')
Landed2014s6 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =12)+s(Year, k =7)+ s(V_Density) +
                      s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(D_To_Drain) + s(length_D) +
                      s(X300m.P)+s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(o3) + s(no2)+ 
                      s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')
Landed2014s7 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =12)+s(Year, k =7) +
                      s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(D_To_Drain) + s(length_D) +
                      s(X300m.P)+s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(o3) + s(no2)+ 
                      s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')
Landed2014s8 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =12)+s(Year, k =7) +
                      s(Grass_P) + s(MVege_P)+ s(Building_P)+ s(Landed_n)+ s(D_To_Drain) + s(length_D) +
                      s(X300m.P)+s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(o3) + s(no2)+ 
                      s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')
Landed2014s9 <- gam(CurrentYear_count ~ s(PrevYear_Inc, k =12)+s(Year, k =7) +
                          s(Grass_P) + s(MVege_P)+ s(Building_P)+ s(Landed_n) + s(length_D) +
                          s(X300m.P)+s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(o3) + s(no2)+ 
                          s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')

#Landed 2014 GLM
Landed2014glms1 <- glm.nb (CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                             Forest_P + Grass_P + MVege_P + Building_P + Condo_n + Landed_n + 
                             HDB_RU + D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + 
                             H60Min_mean + MeanT_mean + MeanWS_mean + pm10 + o3 + no2 + 
                             so2 + co + offset(logp), data = Landed2014dataset)
Landed2014glms2 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                            Forest_P + Grass_P + MVege_P + Building_P + Condo_n + Landed_n + 
                            D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + H60Min_mean + 
                            MeanT_mean + MeanWS_mean + pm10 + o3 + no2 + so2 + co + offset(logp), data = Landed2014dataset)
Landed2014glms3 <- glm.nb (CurrentYear_count ~ PrevYear_Inc + Year + V_Density + Forest_P + 
                             Grass_P + MVege_P + Building_P + Condo_n + Landed_n + D_To_Drain + 
                             length_D + X300m.P + X500m_P + Drt_mean + H60Min_mean + MeanT_mean + 
                             MeanWS_mean + pm10 + o3 + no2 + so2 + co + offset(logp), data = Landed2014dataset)
Landed2014glms4 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + V_Density + Forest_P + 
                            Grass_P + MVege_P + Building_P + Condo_n + Landed_n + D_To_Drain + 
                            length_D + X300m.P + X500m_P + Drt_mean + H60Min_mean + MeanT_mean + 
                            MeanWS_mean + o3 + no2 + so2 + co + offset(logp), data = Landed2014dataset)
Landed2014glms5 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + V_Density + Forest_P + 
                            Grass_P + MVege_P + Building_P + Condo_n + Landed_n + D_To_Drain + 
                            length_D + X300m.P + Drt_mean + H60Min_mean + MeanT_mean + 
                            MeanWS_mean + o3 + no2 + so2 + co + offset(logp), data = Landed2014dataset)
Landed2014glms6 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + V_Density + Grass_P + 
                            MVege_P + Building_P + Condo_n + Landed_n + D_To_Drain + 
                            length_D + X300m.P + Drt_mean + H60Min_mean + MeanT_mean + 
                            MeanWS_mean + o3 + no2 + so2 + co + offset(logp), data = Landed2014dataset)
Landed2014glms7 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + Grass_P + MVege_P + 
                            Building_P + Condo_n + Landed_n + D_To_Drain + length_D + 
                            X300m.P + Drt_mean + H60Min_mean + MeanT_mean + MeanWS_mean + 
                            o3 + no2 + so2 + co + offset(logp), data = Landed2014dataset)
Landed2014glms8 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + Grass_P + MVege_P + 
                            Building_P + Landed_n + D_To_Drain + length_D + X300m.P + 
                            Drt_mean + H60Min_mean + MeanT_mean + MeanWS_mean + o3 + 
                            no2 + so2 + co + offset(logp), data = Landed2014dataset)
Landed2014glms9 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + Grass_P + MVege_P + 
                            Building_P + Landed_n + length_D + X300m.P + Drt_mean + H60Min_mean + 
                            MeanT_mean + MeanWS_mean + o3 + no2 + so2 + co + offset(logp), data = Landed2014dataset)
                  

#HDB 2008 Stepwise (dataset1)
HDB2008all <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + Building_P + HDB_RU +
                       A_HDB_H + A_HDB_A + D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + MeanT_mean + MeanWS_mean+
                       offset(logp), data = dataset1)
HDB2008backward <- step(HDB2008all, direction = 'backward')

#HDB 2008 GAMs

HDB2008s1 <- gam( CurrentYear_count ~ s(PrevYear_Inc)+s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + s(Grass_P) + 
                                 s(MVege_P)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A) +s(D_To_Drain) + s(length_D) +
                                 s(X300m.P) + s(X500m_P) + s(Drt_mean) + s(MeanT_mean) + s(MeanWS_mean) + offset(logp),
                               data = dataset1, family = nb(), method = 'REML') 

HDB2008s2 <- gam(CurrentYear_count ~ s(PrevYear_Inc)+s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + s(Grass_P) + 
                         s(MVege_P)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A) + s(length_D) +
                         s(X300m.P) + s(X500m_P) + s(Drt_mean) + s(MeanT_mean) + s(MeanWS_mean) + offset(logp),
                       data = dataset1, family = nb(), method = 'REML')
HDB2008s3 <- gam(CurrentYear_count ~ s(PrevYear_Inc)+s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + 
                    s(MVege_P)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A) + s(length_D) +
                    s(X300m.P) + s(X500m_P) + s(Drt_mean) + s(MeanT_mean) + s(MeanWS_mean) + offset(logp),
                  data = dataset1, family = nb(), method = 'REML')
HDB2008s4 <- gam(CurrentYear_count ~ s(PrevYear_Inc)+s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + 
                   s(MVege_P)+ s(Building_P) + s(A_HDB_H, k = 12)  + s(A_HDB_A) + s(length_D) +
                   s(X300m.P) + s(X500m_P) + s(Drt_mean) + s(MeanT_mean) + s(MeanWS_mean) + offset(logp),
                 data = dataset1, family = nb(), method = 'REML')
HDB2008s5 <- gam(CurrentYear_count ~ s(PrevYear_Inc)+s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + 
                   s(Building_P) + s(A_HDB_H, k = 12)  + s(A_HDB_A) + s(length_D) +
                   s(X300m.P) + s(X500m_P) + s(Drt_mean) + s(MeanT_mean) + s(MeanWS_mean) + offset(logp),
                 data = dataset1, family = nb(), method = 'REML')
HDB2008s6 <- gam(CurrentYear_count ~ s(PrevYear_Inc)+s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + 
                   s(Building_P) + s(A_HDB_H, k = 12)  + s(A_HDB_A) + s(length_D) +
                  s(X500m_P) + s(Drt_mean) + s(MeanT_mean) + s(MeanWS_mean) + offset(logp),
                 data = dataset1, family = nb(), method = 'REML')
HDB2008s7 <- gam(CurrentYear_count ~ s(PrevYear_Inc)+s(Year, k =12)+ s(V_Density) + s(Forest_P) + 
                   s(Building_P) + s(A_HDB_H, k = 12)  + s(A_HDB_A) + s(length_D) +
                   s(X500m_P) + s(Drt_mean) + s(MeanT_mean) + s(MeanWS_mean) + offset(logp),
                 data = dataset1, family = nb(), method = 'REML')
#HDB 2008 GLMs

HDB2008glms1 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + Grass_P + MVege_P + Building_P + HDB_RU + A_HDB_H + 
                         A_HDB_A + D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + 
                         MeanT_mean + MeanWS_mean + offset(logp), data = dataset1)
HDB2008glms2 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + Grass_P + MVege_P + Building_P + HDB_RU + A_HDB_H + 
                         A_HDB_A + length_D + X300m.P + X500m_P + Drt_mean + MeanT_mean + 
                         MeanWS_mean + offset(logp), data = dataset1)
HDB2008glms3 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + MVege_P + Building_P + HDB_RU + A_HDB_H + A_HDB_A + 
                         length_D + X300m.P + X500m_P + Drt_mean + MeanT_mean + MeanWS_mean + 
                         offset(logp), data = dataset1)
HDB2008glms4 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + MVege_P + Building_P + A_HDB_H + A_HDB_A + length_D + 
                         X300m.P + X500m_P + Drt_mean + MeanT_mean + MeanWS_mean + 
                         offset(logp), data = dataset1)
HDB2008glms5 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + Building_P + A_HDB_H + A_HDB_A + length_D + X300m.P + 
                         X500m_P + Drt_mean + MeanT_mean + MeanWS_mean + offset(logp), data = dataset1)
HDB2008glms6 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                         Forest_P + Building_P + A_HDB_H + A_HDB_A + length_D + X500m_P + 
                         Drt_mean + MeanT_mean + MeanWS_mean + offset(logp), data = dataset1)
HDB2008glms7 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + V_Density + Forest_P + 
                         Building_P + A_HDB_H + A_HDB_A + length_D + X500m_P + Drt_mean + 
                         MeanT_mean + MeanWS_mean + offset(logp), data = dataset1)

#Landed 2008 Stepwise (dataset 3)

Landed2008all <- glm.nb(CurrentYear_count ~ PrevYear_Inc +Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + Building_P + 
                                 Condo_n + Landed_n+ HDB_RU + D_To_Drain + length_D + X300m.P + X500m_P+ Drt_mean + MeanT_mean + MeanWS_mean+ offset(logp), data = dataset3)

Landed2008backward <- step(Landed2008all, direction = 'backward')

#Landed 2008 GAMs

Landed2008s1 <- gam( CurrentYear_count ~ s(PrevYear_Inc) + s(Year, k =7) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                       s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(HDB_RU) + s(D_To_Drain) + s(length_D) +
                       s(X300m.P) + s(X500m_P) +s(Drt_mean) +s(MeanT_mean)+  s(MeanWS_mean)+ offset(logp),
                     data = dataset3, family = nb(), method = 'REML')
Landed2008s2 <- gam( CurrentYear_count ~ s(PrevYear_Inc) + s(Year, k =7) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                       s(Grass_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(HDB_RU) + s(D_To_Drain) + s(length_D) +
                       s(X300m.P) + s(X500m_P) +s(Drt_mean) +s(MeanT_mean)+  s(MeanWS_mean)+ offset(logp),
                     data = dataset3, family = nb(), method = 'REML')
Landed2008s3 <- gam( CurrentYear_count ~ s(PrevYear_Inc) + s(Year, k =7) + s(NDVI_A)+ s(V_Density) +
                       s(Grass_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(HDB_RU) + s(D_To_Drain) + s(length_D) +
                       s(X300m.P) + s(X500m_P) +s(Drt_mean) +s(MeanT_mean)+  s(MeanWS_mean)+ offset(logp),
                     data = dataset3, family = nb(), method = 'REML')
Landed2008s4 <- gam( CurrentYear_count ~ s(PrevYear_Inc) + s(Year, k =7) + s(NDVI_A) +
                       s(Grass_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(HDB_RU) + s(D_To_Drain) + s(length_D) +
                       s(X300m.P) + s(X500m_P) +s(Drt_mean) +s(MeanT_mean)+  s(MeanWS_mean)+ offset(logp),
                     data = dataset3, family = nb(), method = 'REML')
Landed2008s5 <- gam( CurrentYear_count ~ s(PrevYear_Inc) + s(Year, k =7) + s(NDVI_A) +
                      s(Grass_P)+ s(Building_P) + s(Condo_n) + s(Landed_n) + s(D_To_Drain) + s(length_D) +
                      s(X300m.P) + s(X500m_P) +s(Drt_mean) +s(MeanT_mean)+  s(MeanWS_mean)+ offset(logp),
                    data = dataset3, family = nb(), method = 'REML')
#Landed 2008 GLMs

Landed2008glms1 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                            Forest_P + Grass_P + MVege_P + Building_P + Condo_n + Landed_n + 
                            HDB_RU + D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + 
                            MeanT_mean + MeanWS_mean + offset(logp), data = dataset3)
Landed2008glms2 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                            Forest_P + Grass_P + Building_P + Condo_n + Landed_n + HDB_RU + 
                            D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + MeanT_mean + 
                            MeanWS_mean + offset(logp), data = dataset3)
Landed2008glms3 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + V_Density + 
                            Grass_P + Building_P + Condo_n + Landed_n + HDB_RU + D_To_Drain + 
                            length_D + X300m.P + X500m_P + Drt_mean + MeanT_mean + MeanWS_mean + 
                            offset(logp), data = dataset3)
Landed2008glms4 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + Grass_P + 
                            Building_P + Condo_n + Landed_n + HDB_RU + D_To_Drain + length_D + 
                            X300m.P + X500m_P + Drt_mean + MeanT_mean + MeanWS_mean + 
                            offset(logp), data = dataset3)
Landed2008glms5 <- glm.nb(CurrentYear_count ~ PrevYear_Inc + Year + NDVI_A + Grass_P + 
                            Building_P + Condo_n + Landed_n + D_To_Drain + length_D + 
                            X300m.P + X500m_P + Drt_mean + MeanT_mean + MeanWS_mean + 
                            offset(logp), data = dataset3)

#Regression Coefficients
export_summs(HDB2014glms1, HDB2014glms2, HDB2014glms3, HDB2014glms4, HDB2014glms5, HDB2014glms6, HDB2014glms7, exp = TRUE,
             to.file = 'docx', file.name = 'HDB2014step.docx',error_format = "[{conf.low}, {conf.high}]")

export_summs(Landed2014glms1, Landed2014glms2, Landed2014glms3, Landed2014glms4, Landed2014glms5, Landed2014glms6,
             Landed2014glms7, Landed2014glms8, Landed2014glms9, exp = TRUE, to.file = 'html', file.name = 'Landed2014step.html',
             error_format = "[{conf.low}, {conf.high}]")

export_summs(HDB2008glms1, HDB2008glms2, HDB2008glms3, HDB2008glms4, HDB2008glms5, HDB2008glms6, HDB2008glms7, exp = TRUE,
             to.file = 'html', file.name = 'HDB2008step.html',error_format = "[{conf.low}, {conf.high}]")

export_summs(Landed2008glms1, Landed2008glms2, Landed2008glms3, Landed2008glms4, Landed2008glms5, exp = TRUE, to.file = 'html',
             file.name = 'Landed2008step.html',error_format = "[{conf.low}, {conf.high}]")
