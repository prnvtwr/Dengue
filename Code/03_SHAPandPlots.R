#Fast SHAP
data_list <- list(dataset1,dataset2,dataset3,dataset4)
model_list <- list(fm1,fm2,fm3,fm4)
shap_list2 <- list()
for (i in 1:4){
  X<- subset(data_list[[i]], select = -CurrentYear_count)
  shap2 <- explain(model_list[[i]], X = X, pred_wrapper = predict, nsim = 100)
  shap_list2[[i]] <- shap2
}

S_HDB<- dataset2_1 %>%
  dplyr::select(-X,-CurrentYear_count)

S_landed <- dataset4_1 %>%
  dplyr ::select(-CurrentYear_count,-X)

#Final SHAP values
shap_HDB <- fastshap::explain(fm_2_1, X = S_HDB, pred_wrapper = predict, nsim = 100)
shap_Landed <- fastshap::explain(fm_4_1, X = S_landed, pred_wrapper = predict, nsim = 100)

#SHAP Imp Plot
shap_imp2 <- abs(shap_HDB) %>%
  summarize_all(base::mean) %>%
  add_column(Type = 'Public Housing')
shap_imp4 <- abs(shap_Landed) %>%
  summarize_all(base::mean) %>%
  add_column(Type = 'Private Housing')
shap_imp_plot <- bind_rows(shap_imp2,shap_imp4) %>%
  pivot_longer(!Type, names_to = 'Variable', values_to = 'Mean_Absolute_Shapley_Value')


#IRR Plots

label_names <- c('Preceding Year Incidence Rate \n(Cases per 1,000 person-years)','NDVI', 'Vegetation Density', 'Forest Cover (%)',
                 'Grass Cover (%)','Total Vegetation Area (%)', 'Bulding Area (%)','Number of Public Housing Units',
                 'Average Public Housing Building Height (m)', 'Average Public Housing Building Age (years)',
                 'Number of Condominium Units','Number of Landed Units','Distance of Centroid to Drainage Network (m)',
                 'Length of Drainage Network in Spatial Unit (m)','Area Within 300m of a Waterbody (%)',
                 'Area Within 500m of a Waterbody (%)', 'Total Daily Rainfall (mm)','Highest 60-minute Rainfall (mm)',
                 'Mean Temperature (°C)','Mean Wind Speed (km/h)',expression("Mean Annual PM"[10]*" Concentration ("*mu*"g/"*"m"^3*")"),
                 expression("Mean Annual O"[3]*" Concentration (ppm)"),expression("Mean Annual NO"[2]*" Concentration (ppb)"),
                 expression("Mean Annual SO"[2]*" Concentration (ppb)"), 'Mean Annual CO Concentration (ppm)')

variables <- c('PrevYear_Inc', 'NDVI_A', 'V_Density', 'Forest_P', 'Grass_P', 'MVege_P','Building_P', 'HDB_RU', 'A_HDB_H',
               'A_HDB_A', 'Condo_n', 'Landed_n','D_To_Drain','length_D', 'X300m.P', 'X500m_P', 'Drt_mean','H60Min_mean','MeanT_mean',
               'MeanWS_mean','pm10','o3','no2','so2','co')


# Figure 1
ggplot(shap_imp_plot, aes(x=Variable, y = Mean_Absolute_Shapley_Value, fill = Type)) +
  geom_col(position ='dodge', stat='identity', alpha =0.4)+ coord_flip()+
  scale_x_discrete(limits = rev(variables), labels = rev(label_names))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c('grey60','dodgerblue3'))+
  labs(x = NULL, y = 'Mean Absolute Shapley Value', fill = 'Sector Type') + 
  theme_classic()+
  theme (
    axis.title.y.left = element_text(size =15),
    axis.text.y.left = element_text(size = 21),
    axis.title.x = element_text(size = 27),
    axis.text.x = element_text(size = 25),
    legend.position = c(.8, .9),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 27)
  )


# Plotting Functions

plotter2 <- function(rr_store,xlabel,xcol, shapval,ylimit=NULL, xlimit = NULL, title, k=12){
  df <- as.data.frame(rr_store) %>%
    mutate(sig = if_else(lo_rr <= 1 & up_rr >= 1,'No','Yes'))
  
  scalefactor <- max(df$up_rr)/max(shapval)
  
  ggplot(df, aes(x= x, y = mean_rr)) + theme_classic() + labs(x = xlabel)+ ggtitle(title)+
    geom_line()+
    geom_ribbon(data = df[df$sig == 'No' | c(FALSE, head(df$sig == 'No', -1)), ], aes(x = x, ymax = up_rr, ymin = lo_rr),
                fill = 'steelblue3', alpha = 0.10) +
    geom_ribbon (data = df[df$sig == 'Yes' | c(FALSE, head(df$sig == 'Yes', -1)), ], aes(x = x, ymax = up_rr, ymin = lo_rr),
                 fill = 'steelblue3', alpha = 0.45) +
    geom_vline(xintercept = mean(xcol), colour = 'goldenrod1')+
    geom_hline(yintercept = 1, linetype = 'dashed', colour = 'cadetblue4')+
    geom_smooth(data = data.frame(x = xcol,y=shapval), aes(x=x,y=y * scalefactor),
                method = 'gam', formula = y~s(x,bs = 'cs', k = k), colour = 'tomato3',se = FALSE)+
    scale_y_continuous(name = 'Incidence Rate Ratio', limits = ylimit, sec.axis = sec_axis(~./scalefactor, name = 'SHAP'))+
    scale_x_continuous(limits = xlimit)+
    theme (
      axis.title.y.left = element_text(color = 'steelblue3', size =19),
      axis.text.y.left = element_text(color = 'steelblue3', size = 17),
      axis.title.y.right = element_text(color = 'tomato3', size = 19),
      axis.text.y.right = element_text(color = 'tomato3',size= 17),
      axis.title.x = element_text(size = 19),
      axis.text.x = element_text(size = 17))
  
}

plotter_mult <- function(rr_store,xlabel,xcol, shapval, title,k=12){
  df <- as.data.frame(rr_store) %>%
    mutate(sig = if_else(lo_rr <= 1 & up_rr >= 1,'No','Yes'))
  
  scalefactor <- max(df$up_rr)/max(shapval)
  
  ggplot(df, aes(x= x, y = mean_rr)) + theme_classic() + labs(x = xlabel)+ ggtitle(title)+
    geom_line()+
    geom_ribbon(data = df, aes(x = x, ymax = up_rr, ymin = lo_rr), alpha = 0.10, fill = 'steelblue3') +
    geom_vline(xintercept = mean(xcol), colour = 'goldenrod1')+
    geom_hline(yintercept = 1, linetype = 'dashed', colour = 'cadetblue4')+
    geom_smooth(data = data.frame(x = xcol,y=shapval), aes(x=x,y=y * scalefactor),
                method = 'gam', formula = y~s(x,bs = 'cs', k = k), colour = 'tomato3',se = FALSE)+
    scale_y_continuous(name = 'Incidence Rate Ratio',sec.axis = sec_axis(~./scalefactor, name = 'SHAP'))+
    theme (
      axis.title.y.left = element_text(color = 'steelblue3', size =19),
      axis.text.y.left = element_text(color = 'steelblue3', size = 17),
      axis.title.y.right = element_text(color = 'tomato3', size = 19),
      axis.text.y.right = element_text(color = 'tomato3',size= 17),
      axis.title.x = element_text(size = 19),
      axis.text.x = element_text(size = 17))
  
  
}

plotter_year<- function(data){
  df <- as.data.frame(data) %>%
    mutate(sig = if_else(lo_rr <= 1 & up_rr >= 1,'Non-Significant','Significant')) %>%
    filter (x%%1 == 0) %>%
    distinct(x, .keep_all = TRUE)
  
  
  ggplot(df, aes(y=x, x = mean_rr,colour = sig))+ theme_classic()+ labs(y='Year', colour ='95% CI')+ ggtitle('A2')+
    geom_errorbarh(aes(xmin=lo_rr, xmax=up_rr), height=.15)+
    geom_point(alpha = 0.8) +
    scale_x_continuous(name = 'Incidence Rate Ratio' )+
    scale_y_continuous(name = 'Year', breaks = c(2014,2015,2016,2017,2018,2019,2020))+
    scale_color_manual(values = c('azure3','tomato3'))+
    geom_vline(xintercept = 1, linetype = 'dashed', colour = 'cadetblue4') +
    theme (
      axis.title.y.left = element_text(size =19),
      axis.text.y.left = element_text(size = 17),
      axis.title.x = element_text(size = 19),
      axis.text.x = element_text(size = 17),
      legend.position = c(.85, .2),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 15)
    )
}



# Public Housing Plots/ Figure 2

df <- as.data.frame(store_rr_Landed[[]]) %>%
  mutate(sig = if_else(lo_rr <= 1 & up_rr >= 1, 'No', 'Yes'))

plot1 <- plotter2(store_rr[[2]], 'Preceding Year Case Count', dataset2$PrevYear_count, shap_HDB$PrevYear_count,title = 'A1') +
  coord_cartesian(xlim = c(0,200), ylim = c(-1,5))
plot2 <- plotter_year(store_rr[[3]])
plot3 <- plotter2(store_rr[[4]], 'NDVI', dataset2$NDVI_A,shap2$NDVI_A,title = 'A3')
plot4 <- plotter2(store_rr[[5]], 'Total Vegetation Area (%)', dataset2$MVege_P,shap2$MVege_P,title = 'A4')
plot5 <- plotter_mult(store_rr[[6]],'Forest Cover (%)',dataset2$Forest_P,shap2$Forest_P, title = 'B1',k=10)+
  geom_polygon(data= data.frame(x=c(0.105,0.11,0.11,0.105),y =c(1.00,0.6139,0.0165,0.0285)), aes(x=x,y=y),
               alpha=0.4,fill='steelblue3',inherit.aes=F)+
  geom_ribbon(data = df[df$x == 0.11 |df$x== 0.14,],aes(x=x,ymax=up_rr,ymin=lo_rr), fill ='steelblue3',alpha=0.4)

plot6 <- plotter2(store_rr[[7]], 'Grass Cover (%)', dataset2$Grass_P, shap2$Grass_P, title = 'B2')
plot7 <- plotter2(store_rr[[8]],'Manged Vegetation Cover (%)', dataset2$MVege_P,shap2$MVege_P, title = 'B3')
plot8 <- plotter2(store_rr[[9]],'Building Area (%)', dataset2$Building_P,shap2$Building_P, title = 'B4')
plot9 <- plotter2(store_rr[[10]],'Number of Public Housing Units', dataset2$HDB_RU,shap2$HDB_RU, title = 'C1')
plot10 <- plotter2(store_rr[[11]],'Average Public Housing Building Height (m)', dataset2$A_HDB_H,shap2$A_HDB_H, title = 'C2')
plot11 <- plotter2(store_rr[[12]],'Average Public Housing Building Age (years)', dataset2$A_HDB_A,shap2$A_HDB_A, title = 'C3',
                   xlimit = c(0,55))
plot12 <- plotter2(store_rr[[13]],'Distance of Centroid to Drainage Network (m)', dataset2$D_To_Drain,shap2$D_To_Drain, title = 'C4')
plot13 <- plotter2(store_rr[[14]],'Length of Drainage Network in Spatial Unit (m)', dataset2$length_D,shap2$length_D, title = 'D1')
plot14 <- plotter2(store_rr[[15]],'Area Within 300m of a Waterbody (%)', dataset2$X300m.P,shap2$X300m.P, title = 'D2')
plot15 <- plotter2(store_rr[[16]],'Area Within 500m of a Waterbody (%)', dataset2$X500m_P,shap2$X500m_P, title = 'D3')
plot16 <- plotter2(store_rr[[17]],'Total Daily Rainfall(mm)', dataset2$Drt_mean,shap2$Drt_mean, title = 'D4')
plot17 <- plotter2(store_rr[[18]], 'Highest 60-Minute Rainfall (mm)', dataset2$H60Min_mean,shap_HDB$H60Min_mean, title ='E1')
plot18 <- plotter_mult(store_rr[[19]], 'Mean Temperature (°C)', dataset2$MeanT_mean,shap_HDB$MeanT_mean, title ='E2')+
  geom_ribbon(data = df[df$sig == 'Yes' & df$x <= 28,], aes(x = x, ymin = lo_rr, ymax = up_rr), fill = 'steelblue3', alpha = 0.45)+
  geom_ribbon(data = df[df$sig == 'Yes' & df$x >= 28,], aes(x = x, ymin = lo_rr, ymax = up_rr), fill = 'steelblue3', alpha = 0.45) +
  coord_cartesian(ylim = c(-5,6))
plot19 <- plotter2(store_rr[[20]], 'Mean Wind Speed (km/h)', dataset2$MeanWS_mean,shap_HDB$MeanWS_mean, title ='E3')
plot20 <- plotter2(store_rr[[21]], expression("Mean Annual PM"[10]*" Concentration ("*mu*"g/"*"m"^3*")"), dataset2$pm10,
                   shap_HDB$pm10, title = 'E4')
plot21 <- plotter2(store_rr[[22]], expression("Mean Annual O"[3]*" Concentration (ppm)"), dataset2$o3,shap_HDB$o3, title = 'F1')
plot22 <- plotter2(store_rr[[23]], expression("Mean Annual NO"[2]*" Concentration (ppb)"), dataset2$no2,shap_HDB$no2, title = 'F2')+
  coord_cartesian(xlim = c(7,24))
plot23 <- plotter_mult(store_rr[[24]],expression("Mean Annual SO"[2]*" Concentration (ppb)"), dataset2$so2,shap_HDB$so2, title = 'F3')+
  geom_ribbon(data = df[df$sig == 'Yes' & df$x <= 2,], aes (x = x, ymin = lo_rr, ymax = up_rr), alpha = 0.45, fill = 'steelblue3')+
  geom_ribbon(data = df[df$sig == 'Yes' & df$x <= 3 & df$x >=2,], aes (x = x, ymin = lo_rr, ymax = up_rr),
              alpha = 0.45, fill = 'steelblue3')+
  geom_ribbon(data = df[df$sig == 'Yes' & df$x >= 3,], aes (x = x, ymin = lo_rr, ymax = up_rr), alpha = 0.45, fill = 'steelblue3')
plot24 <- plotter2(store_rr[[25]], 'Mean Annual CO Concentration (ppm)', dataset2$co,shap_HDB$co, title = 'F4')

plot1+plot2+plot3+plot4+plot5+plot6+plot7+plot8+plot9+plot10+plot11+plot12+plot13+plot14+plot15+plot16+plot17+plot18+plot19+plot20+
  plot21+plot22+plot23+plot24+plot_layout(ncol = 4)


#Private Housing Plots/ Figure 3

land_plot1 <- plotter2(store_rr_Landed[[2]], 'Preceding Year Incidence Rate \n (Cases per 1,000 person-years)',
                       dataset4_1$PrevYear_Inc, shap_Landed$PrevYear_Inc,title = 'A1')+
  coord_cartesian(xlim=c(0,200), ylim = c(-5,35))
land_plot2 <- plotter_year(store_rr_Landed[[3]])
land_plot3 <- plotter2(store_rr_Landed[[4]],'NDVI', dataset4_1$NDVI_A,shap_Landed$NDVI_A,title ='A3')
land_plot4 <- plotter2(store_rr_Landed[[5]],'Total Vegetation Area (%)', dataset4_1$V_Density,shap_Landed$V_Density,title = 'A4')+
  coord_cartesian(ylim=c(-2,40))
land_plot5 <- plotter2(store_rr_Landed[[6]],'Forest Cover(%)', dataset4_1$Forest_P,shap_Landed$Forest_P, title = 'B1')
land_plot6 <- plotter2(store_rr_Landed[[7]],'Grass Cover (%)', dataset4_1$Grass_P,shap_Landed$Grass_P,title = 'B2')+
  coord_cartesian(ylim=c(-100,80))
land_plot7 <- plotter2(store_rr_Landed[[8]],'Managed Vegetation Cover (%)', dataset4_1$MVege_P,shap_Landed$MVege_P,title = 'B3')
land_plot8 <- plotter2(store_rr_Landed[[9]],'Building Area (%)',dataset4_1$Building_P,shap_Landed$Building_P,title ='B4')
land_plot9 <- plotter2(store_rr_Landed[[10]],'Number of Condominium Units',dataset4_1$Condo_n,shap_Landed$Condo_n,title = 'C1')
land_plot10 <- plotter2(store_rr_Landed[[11]],'Number of Landed Housing Units', dataset4_1$Landed_n,shap_Landed$Landed_n,title = 'C2')
land_plot11 <- plotter2(store_rr_Landed[[12]],' Number of Public Housing Units', dataset4_1$HDB_RU,shap_Landed$HDB_RU,title = 'C3') + 
  coord_cartesian(ylim = c(-1,5))
land_plot12 <- plotter2(store_rr_Landed[[13]], 'Distance of Centroid to Drainage Network (m)', dataset4$D_To_Drain,
                        shap_Landed$D_To_Drain,title = 'C4') + coord_cartesian(ylim = c(-4,3))
land_plot13 <- plotter2(store_rr_Landed[[14]],'Length of Drainage Network in Spatial Unit (m)',dataset4_1$length_D,
                        shap_Landed$length_D,title = 'D1')
land_plot14 <- plotter2(store_rr_Landed[[15]],'Area Within 300m of a Waterbody (%)',dataset4_1$X300m.P,shap_Landed$X300m.P,
                        title ='D2')
land_plot15 <- plotter2(store_rr_Landed[[16]],'Area Within 500m of a Waterbody (%)',dataset4_1$X500m_P,shap_Landed$X500m_P,
                        title ='D3')
land_plot16 <- plotter_mult(store_rr_Landed[[17]],'Total Daily Rainfall (mm)', dataset4_1$Drt_mean,shap_Landed$Drt_mean,title = 'D4')+
  geom_ribbon(data=df[df$sig=='Yes' & df$x <= 5.0,], aes(x=x,ymin=lo_rr,ymax=up_rr), fill = 'steelblue3',alpha =0.45)+
  geom_ribbon(data=df[df$sig=='Yes' & df$x >= 7.0,], aes(x=x,ymin=lo_rr,ymax=up_rr), fill = 'steelblue3',alpha =0.45)+
  coord_cartesian(ylim=c(-115,110))
land_plot17 <- plotter_mult(store_rr_Landed[[18]], 'Highest 60-Minute Rainfall (mm)', dataset4_1$H60Min_mean,
                            shap_Landed$H60Min_mean,title = 'E1')+
  geom_ribbon(data=df[df$sig=='Yes' & df$x <= 4,], aes(x=x,ymin=lo_rr,ymax=up_rr), fill = 'steelblue3',alpha =0.45)+
  geom_ribbon(data=df[df$sig=='Yes' & df$x >= 4,], aes(x=x,ymin=lo_rr,ymax=up_rr), fill = 'steelblue3',alpha =0.45)+
  coord_cartesian(ylim = c(-75,75))
land_plot18 <- plotter2(store_rr_Landed[[19]],'Mean Temperature (°C)',dataset4_1$MeanT_mean,shap_Landed$MeanT_mean,title = 'E2')+
  coord_cartesian(ylim=c(-10,20))
land_plot19 <- plotter_mult(store_rr_Landed[[20]],'Mean Wind Speed (km/h)',dataset4_1$MeanWS_mean,shap_Landed$MeanWS_mean,title = 'E3')+
  geom_ribbon(data=df[df$sig=='Yes' & df$x >= 9,], aes(x=x,ymin=lo_rr,ymax=up_rr), fill = 'steelblue3',alpha =0.4)
land_plot20<- plotter_mult(store_rr_Landed[[21]],expression("Mean Annual PM"[10]*" Concentration ("*mu*"g/"*"m"^3*")"),dataset4_1$pm10,
                           shap_Landed$pm10,title = 'E4') + 
  geom_polygon(data= data.frame(x=c(21.03,21.8,21.8,21.03),y =c(0.782,1,0.18,0.12)), aes(x=x,y=y),
               alpha=0.4,fill='steelblue3',inherit.aes=F)+
  coord_cartesian(ylim = c(-3.5,5.5))
land_plot21 <- plotter2(store_rr_Landed[[22]],expression("Mean Annual O"[3]*" Concentration (ppm)"),dataset4_1$o3,
                        shap_Landed$o3,title = 'F1')
land_plot22 <- plotter2(store_rr_Landed[[23]],expression("Mean Annual NO"[2]*" Concentration (ppb)"),dataset4_1$no2,shap_Landed$no2,
                        title ='F2')
land_plot23 <- plotter2(store_rr_Landed[[24]],expression("Mean Annual SO"[2]*" Concentration (ppb)"),dataset4_1$so2,shap_Landed$so2,
                        title = 'F3')+ coord_cartesian(ylim = c(-30,20),xlim = c(1.3,6.2))
land_plot24 <-plotter2(store_rr_Landed[[25]],'Mean Annual CO Concentration (ppm)', dataset4_1$co,shap_Landed$co,title = 'F4')

land_plot1+land_plot2+land_plot3+land_plot4+land_plot5+land_plot6+land_plot7+land_plot8+land_plot9+land_plot10+land_plot11+land_plot12+
  land_plot13+land_plot14+land_plot15+land_plot16+land_plot17+land_plot18+land_plot19+land_plot20+land_plot21+land_plot22+land_plot23+
  land_plot24+plot_layout(ncol=4)

