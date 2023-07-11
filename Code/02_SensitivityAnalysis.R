#HDB 2014 Sensitivity
HDB2014s1 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + offset(logp), data = dataset2_1)
HDB2014s2 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + Building_P + HDB_RU +
                      A_HDB_H + A_HDB_A + D_To_Drain + length_D + X300m.P + X500m_P + offset(logp), data = dataset2_1)
HDB2014s3 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + Building_P + HDB_RU +
                      A_HDB_H + A_HDB_A + D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + H60Min_mean + MeanT_mean +
                      MeanWS_mean + offset(logp), data = dataset2_1)
HDB2014s4 <- glm.nb(CurrentYear_count ~ . -logp-X  + offset(logp), data = dataset2_1)

HDB2014s5 <- gam(CurrentYear_count ~  s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                   s(Grass_P) + s(MVege_P, k = 4)+ offset(logp) ,data = dataset2_1,
                 family = nb(), method = 'REML')
HDB2014s6 <- gam(CurrentYear_count ~ s(Year, k = 6) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                   s(Grass_P) + s(MVege_P, k = 4)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A, k =10) +
                   s(D_To_Drain) + s(length_D) +s(X300m.P) + s(X500m_P)+ offset(logp) ,data = dataset2_1,
                 family = nb(), method = 'REML')
HDB2014s7 <- gam(CurrentYear_count ~ s(Year, k = 6) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                   s(Grass_P) + s(MVege_P, k = 4)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A, k =10) +
                   s(D_To_Drain) + s(length_D) +s(X300m.P) + s(X500m_P)+ s(Drt_mean)+ s(H60Min_mean)+ s(MeanT_mean)+ s(MeanWS_mean) +
                   offset(logp) ,data = dataset2_1,
                 family = nb(), method = 'REML')

#Landed 2014 Sensitivity

Landed2014s1 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + offset(logp), data = dataset4_1)
Landed2014s2 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + Building_P + 
                         Condo_n + Landed_n+ HDB_RU + D_To_Drain + length_D + X300m.P + X500m_P +
                         offset(logp), data = dataset4_1)
Landed2014s3 <-  glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + Building_P + 
                          Condo_n + Landed_n+ HDB_RU + D_To_Drain + length_D + X300m.P + X500m_P + Drt_mean + H60Min_mean+
                          MeanT_mean + MeanWS_mean+offset(logp), data = dataset4_1)
Landed2014s4 <- glm.nb(CurrentYear_count ~.-logp-X + offset(logp), data = dataset4_1)

Landed2014s5 <- gam( CurrentYear_count ~ +s(Year, k =7) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                       s(Grass_P) + s(MVege_P)+ offset(logp), data = dataset4_1, family = nb(), method = 'REML')
Landed2014s6 <- gam( CurrentYear_count ~ +s(Year, k =7) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                       s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(HDB_RU) + s(D_To_Drain) + s(length_D) +
                       s(X300m.P) + s(X500m_P) +s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean) + s(pm10) + s(o3) + s(no2)+ 
                       s(so2, k =12) + s(co) + offset(logp), data = dataset4_1, family = nb(), method = 'REML')
Landed2014s7 <- gam( CurrentYear_count ~s(Year, k =7) + s(NDVI_A)+ s(V_Density) + s(Forest_P) +
                       s(Grass_P) + s(MVege_P)+ s(Building_P) + s(Condo_n) + s(Landed_n)+ s(HDB_RU) + s(D_To_Drain) + s(length_D) +
                       s(X300m.P) + s(X500m_P) +s(Drt_mean) + s(H60Min_mean)+s(MeanT_mean)+  s(MeanWS_mean)+ offset(logp),
                     data = dataset4_1, family = nb(), method = 'REML')
col_lab <- c('Preceding Year Incidence', 'Year', 'NDVI', 'Vegetation Density', 'Forest Cover (%)',
             'Grass Cover (%)', 'Total Vegetation Area (%)', 'Bulding Area (%)', 'Number of Public Housing Units',
             'Average Public Housing Building Height (m)', 'Average Public Housing Building Age (years)',
             'Distance of Centroid to Drainage Network (m)', 'Length of Drainage Network in Spatial Unit (m)',
             'Area Within 300m of a Waterbody (%)', 'Area Within 500m of a Waterbody', 'Daily Rainfall Total (mm)',
             "Highest 60-Minute Rainfall (mm)",'Mean Temperature (°C)','Mean Wind Speed (km/h)')
col_lab_Landed <- c('Preceding Year Incidence', 'Year', 'NDVI', 'Vegetation Density', 'Forest Cover (%)',
                    'Grass Cover (%)', 'Total Vegetation Area (%)', 'Bulding Area (%)','Number of Condominium Units',
                    'Number of Landed Housing Units', 'Number of Public Housing Units','Distance of Centroid to Drainage Network (m)', 
                    'Length of Drainage Network in Spatial Unit (m)','Area Within 300m of a Waterbody (%)', 'Area Within 500m of a Waterbody',
                    'Daily Rainfall Total (mm)',"Highest 60-Minute Rainfall (mm)",'Mean Temperature (°C)','Mean Wind Speed (km/h)')

stargazer(HDB2014s1,HDB2014s2,HDB2014s3,HDB2014s4, dep.var.labels = ' Annual Dengue Incidence Rate (Cases per 1,000 person-years)',
          title = 'Regression Results of Public Housing Study Setting (2014-2020)',digits = 2 ,apply.coef = exp,
          covariate.labels = col_lab, type = "html", out = 'HDB2014table.html', align = TRUE)

stargazer(Landed2014s1,Landed2014s2,Landed2014s3,Landed2014s4,
          dep.var.labels = ' Annual Dengue Incidence Rate (Cases per 1,000 person-years)',
          title = 'Regression Results of Private Housing Study Setting (2014-2020)',digits = 2 ,apply.coef = exp,
          covariate.labels = col_lab_Landed, type = "html", out = 'Landed2014table.html', align = TRUE)


HDB 2008 Sensitivity
HDB_wo_airpoll <- inner_join(HDB_wo_airpoll,dwell_HDB, by = 'Sector_ID', relationship = 'many-to-one') %>%
  mutate( pop  = Total_dwelling * 4) %>%
  mutate (logp = log(pop)) %>%
  mutate (PrevYear_Inc = (PrevYear_count/ pop) * 1000) %>%
  filter (Wolb_Q1 != 1) %>%
  filter (Wolb_Q2 != 1)

dataset1 <- (HDB_wo_airpoll[c('PrevYear_Inc','Year','NDVI_A','V_Density','Forest_P', 'Grass_P', 'MVege_P',
                              'Building_P','HDB_RU','A_HDB_H','A_HDB_A','D_To_Drain','length_D','X300m.P','X500m_P',
                              'Drt_mean','MeanT_mean','MeanWS_mean','CurrentYear_count','logp')])

HDB2008s1 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + offset(logp), data = dataset1)
HDB2008s2 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + Building_P + HDB_RU +
                      A_HDB_H + A_HDB_A + offset(logp), data = dataset1)
HDB2008s3 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + Building_P + HDB_RU +
                      A_HDB_H + A_HDB_A + D_To_Drain + length_D + X300m.P + X500m_P + offset(logp), data = dataset1)
HDB2008s4 <- glm.nb (CurrentYear_count ~ . -logp + offset(logp), data = dataset1)

HDB2008s5 <- gam( CurrentYear_count ~ s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + s(Grass_P) + 
                    s(MVege_P) + offset(logp),data = dataset1, family = nb(), method = 'REML')
HDB2008s6 <- gam( CurrentYear_count ~ s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + s(Grass_P) + 
                    s(MVege_P)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A) + offset(logp),
                  data = dataset1, family = nb(), method = 'REML')
HDB2008s7 <- gam( CurrentYear_count ~ s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + s(Grass_P) + 
                    s(MVege_P)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A) +s(D_To_Drain) + s(length_D) +
                    s(X300m.P) + s(X500m_P) + offset(logp),data = dataset1, family = nb(), method = 'REML') 
HDB2008s8 <- gam( CurrentYear_count ~ s(PrevYear_Inc)+s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + s(Grass_P) + 
                    s(MVege_P)+ s(Building_P) + s(HDB_RU) + s(A_HDB_H, k = 12)  + s(A_HDB_A) +s(D_To_Drain) + s(length_D) +
                    s(X300m.P) + s(X500m_P) + s(Drt_mean) + s(MeanT_mean) + s(MeanWS_mean) + offset(logp),
                  data = dataset1, family = nb(), method = 'REML') 


col_lab_HDB <- c('Preceding Year Incidence', 'Year', 'NDVI', 'Vegetation Density', 'Forest Cover (%)','Grass Cover (%)',
                 'Total Vegetation Area (%)', 'Bulding Area (%)','Number of Condominium Units','Number of Landed Housing Units',
                 'Number of Public Housing Units','Distance of Centroid to Drainage Network (m)', 
                 'Length of Drainage Network in Spatial Unit (m)','Area Within 300m of a Waterbody (%)',
                 'Area Within 500m of a Waterbody','Daily Rainfall Total (mm)','Mean Temperature (°C)','Mean Wind Speed (km/h)')

stargazer(HDB2008s1,HDB2008s2,HDB2008s3,HDB2008s4, dep.var.labels = ' Annual Dengue Incidence Rate (Cases per 1,000 person-years)',
          title = 'Regression Results of Public Housing Study Setting (2008-2020)',digits = 2 ,apply.coef = exp,
          covariate.labels = col_lab_HDB, type = "html", out = 'HDB2008table.html', align = TRUE)

# Landed 2008 Sensitivity
Landed_wo_airpoll <- inner_join(Landed_wo_airpoll,dwell_landed, by = 'Sector_ID', relationship = 'many-to-one') %>%
  mutate (pop = Total_dwelling * 3) %>%
  mutate (logp = log(pop)) %>%
  mutate (PrevYear_Inc = (PrevYear_count / pop) * 1000) %>%
  filter (Wolb_Q1 != 1) %>%
  filter (Wolb_Q2 != 1)

dataset3 <-(Landed_wo_airpoll[c('PrevYear_Inc','Year','NDVI_A','V_Density','Forest_P', 'Grass_P', 'MVege_P',
                                'Building_P','HDB_RU','Condo_n','Landed_n','D_To_Drain','length_D','X300m.P','X500m_P',
                                'Drt_mean','MeanT_mean','MeanWS_mean','CurrentYear_count','logp')])

Landed2008s1 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + offset(logp), data = dataset3)
Landed2008s2 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + Building_P + 
                         Condo_n + Landed_n+ HDB_RU + offset(logp), data = dataset3)
Landed2008s3 <- glm.nb(CurrentYear_count ~ Year + NDVI_A + V_Density + Forest_P + Grass_P + MVege_P + Building_P + 
                         Condo_n + Landed_n+ HDB_RU + D_To_Drain + length_D + X300m.P + X500m_P+ offset(logp), data = dataset3)

Landed2008s4 <- glm.nb(CurrentYear_count ~.-logp + offset(logp), data = dataset3)
Landed2008s5 <- gam(CurrentYear_count ~ s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + s(Grass_P) + 
                      s(MVege_P) + offset(logp),data = dataset3, family = nb(), method = 'REML')
Landed2008s6 <- gam(CurrentYear_count ~ s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + s(Grass_P) + s(MVege_P) + s(Building_P) +
                      s(Condo_n) + s(Landed_n)+ s(HDB_RU) + offset(logp), data = dataset3, family = nb(),method = 'REML')
Landed2008s7 <- gam(CurrentYear_count ~ s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + s(Grass_P) + s(MVege_P) + s(Building_P) + 
                      s(Condo_n) + s(Landed_n)+ s(HDB_RU) + s(D_To_Drain)+ s(length_D)+ s(X300m.P)+s(X500m_P)+offset(logp),
                    data = dataset3, family = nb(),method = 'REML')
Landed2008s8 <- gam(CurrentYear_count ~ s(Year, k =12) + s(NDVI_A)+ s(V_Density) + s(Forest_P) + s(Grass_P) + 
                      s(MVege_P) + s(Building_P) + s(Condo_n) + s(Landed_n)+ s(HDB_RU) + s(D_To_Drain)+ s(length_D)+ s(X300m.P)+
                      s(X500m_P)+ s(Drt_mean) + s(MeanT_mean) + s(MeanWS_mean)+ offset(logp), data = dataset3, family = nb(),
                    method = 'REML')

export_summs(Landed2008s1, Landed2008s2, Landed2008s3,Landed2008s4, exp = TRUE, to.file = 'docx', file.name = 'Landed2008Reg.docx',
             error_format = "[{conf.low}, {conf.high}]")
export_summs(HDB2008s1,HDB2008s2,HDB2008s3,HDB2008s4, exp = TRUE, to.file = 'docx', file.name = 'HDB2008Reg.docx',
             error_format = "[{conf.low}, {conf.high}]")
export_summs(Landed2014s1,Landed2014s2,Landed2014s3,Landed2014s4, exp = TRUE, to.file = 'html', file.name = 'Landed2014Reg.html',
             error_format = "[{conf.low}, {conf.high}]")
export_summs(HDB2014s1,HDB2014s2,HDB2014s3,HDB2014s4, exp = TRUE, to.file = 'html', file.name = 'HDB2014Reg.html',
             error_format = "[{conf.low}, {conf.high}]")