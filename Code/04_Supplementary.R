#Supplementary Figure 1
HDB_zeroinf <- HDB_w_airpoll %>%
  dplyr::select(CurrentYear_count,pop, PremType) %>%
  mutate( Inc = (CurrentYear_count/pop)*1000)

Landed_zeroinf <- Landed_w_airpoll %>%
  dplyr::select(CurrentYear_count,pop, PremType) %>%
  mutate (Inc = (CurrentYear_count/pop) *1000)


combinedzeroinf <- bind_rows(HDB_zeroinf, Landed_zeroinf)
new_labels <- c('HDB' = 'A', 'Landed' = 'B')
ggplot(combinedzeroinf, aes(Inc, fill = PremType, col = PremType)) + geom_density(alpha = 0.4) +
  scale_fill_manual(values = c('steelblue3','tomato3')) +
  scale_color_manual(values = c('steelblue3', 'tomato3'))+
  facet_wrap(~PremType, nrow = 2, scales = 'free', labeller = labeller(PremType = new_labels)) + theme_apa() + xlim(0,80)+
  labs ( x = 'Annual Dengue Incidence Rate\n (Cases per 1,000 person-years)', y = 'Density')+
  theme( strip.text = element_text(hjust = 0, vjust =1),
         strip.text.x = element_text(size= 25),
         axis.title.x = element_text(size = 30),
         axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 30),
         axis.text.y = element_text(size = 20),
         legend.position = 'none')

HDBzeroinfplot <- ggplot(HDB_zeroinf, aes(Inc))+ geom_density(alpha = 0.4, colour = 'steelblue3', fill = 'steelblue3') +
  scale_y_continuous(name = 'Density')+ scale_x_continuous(name = 'Annual Dengue Incidence Rate\n (Cases per 1,000 person-years)')+
  theme_classic() + theme (
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 25),
    plot.title = element_text(size = 35)) + ggtitle('A')

Landedzeroinfplot<- ggplot(Landed_zeroinf, aes(Inc))+ geom_density(alpha = 0.4, colour = 'tomato3', fill = 'tomato3') +
  scale_y_continuous(name = 'Density')+ scale_x_continuous(name = 'Annual Dengue Incidence Rate\n (Cases per 1,000 person-years)')+
  theme_classic() + theme (
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 25),
    plot.title = element_text(size = 35))+ ggtitle('B')

HDBzeroinfplot + Landedzeroinfplot + plot_layout(ncol = 1,widths = c(0.2,2)) 


# Supplementary Figure 2

#Total Dengue Cases 
HDB_cases <- HDB_w_airpoll %>%
  dplyr::select(Sector_ID,PremType,Year,CurrentYear_count)

Landed_cases <- Landed_w_airpoll %>%
  dplyr ::select(Sector_ID,PremType,Year,CurrentYear_count)

Dengue_cases <- bind_rows(HDB_cases, Landed_cases) %>%
  group_by(Year) %>%
  summarize(total_cases = sum(CurrentYear_count)) 


ggplot(Dengue_cases,aes(x=Year, y = total_cases))+
  geom_bar(stat = 'identity', fill = 'steelblue3',width = 0.4,alpha = 0.8) + theme_apa() +
  geom_text(aes(label = total_cases), position = position_dodge(width = 1), vjust = -0.5 ,size = 8) +
  scale_x_continuous(breaks = seq(2014,2020,1), name ='Year')+
  scale_y_continuous(limits = c(0,16300),expand = c(0,0),name = 'Cases') +
  theme (
    axis.title.y = element_text(size =45, vjust = 0.75),
    axis.text.y = element_text(size = 35),
    axis.title.x = element_text(size = 45, vjust = -0.25),
    axis.text.x = element_text(size = 35), plot.title = element_text(size = 35, hjust = 0.5))


# Summary Stats

#HDB 2014 Summary Stats
summdataset2 <- (HDB_w_airpoll[c('CurrentYear_count', 'pop', 'PrevYear_Inc','Year','NDVI_A','V_Density','Forest_P', 'Grass_P', 'MVege_P',
                                 'Building_P','HDB_RU', 'A_HDB_H','A_HDB_A','D_To_Drain','length_D','X300m.P','X500m_P',
                                 'Drt_mean','H60Min_mean','MeanT_mean','MeanWS_mean','pm10','o3', 'no2', 'so2','co')]) %>%
  mutate(CurrentYear_count = (CurrentYear_count/pop) * 1000)

stargazer(summdataset2, type = 'html', nobs = FALSE, mean.sd= TRUE, median = TRUE, iqr = TRUE, summary.logical = FALSE, digits = 2,
          out = 'HDB2014Summ.html')

#Landed 2014 Summary Stats
summdataset4 <- (Landed_w_airpoll[c('CurrentYear_count', 'pop', 'PrevYear_Inc','Year','NDVI_A','V_Density', 'Forest_P','Grass_P',
                                    'MVege_P','Building_P','Condo_n','Landed_n','HDB_RU','D_To_Drain','length_D','X300m.P',
                                    'X500m_P','Drt_mean','H60Min_mean','MeanT_mean','MeanWS_mean','pm10', 'o3', 'no2', 'so2', 'co'
)]) %>%
  mutate ( CurrentYear_count = (CurrentYear_count/pop) * 1000)

stargazer(summdataset4, type = 'html', nobs = FALSE, mean.sd = TRUE, median = TRUE, iqr = TRUE, summary.logical = FALSE,digits = 2,
          out = 'Landed2014Summ.html')

#Paper Summary Format
sample_dataset <-(HDB_w_airpoll[c('CurrentYear_count', 'pop', 'PrevYear_Inc','Year','NDVI_A','V_Density','Forest_P', 'Grass_P', 'MVege_P',
                                  'Building_P','HDB_RU', 'A_HDB_H','A_HDB_A','D_To_Drain','length_D','X300m.P','X500m_P',
                                  'Drt_mean','H60Min_mean','MeanT_mean','MeanWS_mean','pm10','o3', 'no2', 'so2','co','X','H30Min_max')])
sumform <- glm.nb(CurrentYear_count ~., data = sample_dataset)
stargazer(Landed2014s1,sumform,
          dep.var.labels = ' Annual Dengue Incidence Rate (Cases per 1,000 person-years)',
          title = 'Regression Results of Private Housing Study Setting (2014-2020)',
          covariate.labels = col_lab_Landed, type = "html", out = 'statsform.html')


# 2008 Summary Stats
Landed2008summ <-(Landed_wo_airpoll[c('CurrentYear_count','pop','PrevYear_Inc','Year','NDVI_A','V_Density','Forest_P', 'Grass_P', 'MVege_P',
                                      'Building_P','HDB_RU','Condo_n','Landed_n','D_To_Drain','length_D','X300m.P','X500m_P',
                                      'Drt_mean','MeanT_mean','MeanWS_mean')]) %>%
  mutate(CurrentYear_count = (CurrentYear_count/pop) * 1000)
stargazer(Landed2008summ, type = 'html', nobs = FALSE, mean.sd = TRUE, median = TRUE, iqr = TRUE, summary.logical = FALSE,digits = 2,
          out = 'Landed2008Summ.html')

HDB2008sum <-(HDB_wo_airpoll[c('CurrentYear_count','pop','PrevYear_Inc','Year','NDVI_A','V_Density','Forest_P', 'Grass_P', 'MVege_P',
                               'Building_P','HDB_RU','A_HDB_H','A_HDB_A','D_To_Drain','length_D','X300m.P','X500m_P',
                               'Drt_mean','MeanT_mean','MeanWS_mean')]) %>%
  mutate(CurrentYear_count = (CurrentYear_count/pop) * 1000)
stargazer(HDB2008sum, type = 'html', nobs = FALSE, mean.sd = TRUE, median = TRUE, iqr = TRUE, summary.logical = FALSE,digits = 2,
          out = 'HDB2008Sum.html')