copy <- data
copy$id <- NULL
str(copy)

if (! require ("PerformanceAnalytics" )){
  install.packages ("PerformanceAnalytics")
  library (PerformanceAnalytics)
}
if (! require ("ggplot2" )){
  install.packages ("ggplot2")
  library (ggplot2)
}
if (! require ("GGally" )){
  install.packages ("GGally")
  library (GGally)
}
if (! require ("ggpubr" )){
  install.packages ("ggpubr")
  library (ggpubr)
}

#analyze correlation
#a) mean
chart.Correlation(copy[,c(2:11)],histogram=TRUE,method = "pearson", col="blue", pch=1, main="Mean")
?chart.Correlation

ggpairs(copy[,c(2:11,1)], aes(color=copy$diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

#ggcor plot
ggcorr(copy[,c(2:11)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.8,size=10))

#b) SE
chart.Correlation(copy[,c(12:21)],histogram=TRUE,method = "pearson", col="blue", pch=1, main="SE")
ggpairs(copy[,c(12:21,1)], aes(color=copy$diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="SE")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
#ggcor plot
ggcorr(copy[,c(12:21)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="SE")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.8,size=10))

#c) Worst
chart.Correlation(copy[,c(22:31)],histogram=TRUE,method = "pearson", col="blue", pch=1, main="Worst")
ggpairs(copy[,c(22:31,1)], aes(color=copy$diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
#ggcor plot
ggcorr(copy[,c(22:31)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.8,size=10))


chart.Correlation(copy[,c(2:31)],histogram=TRUE,method = "pearson", col="blue", pch=1, main="all")


s1_train %>%
  filter(diagnosis == "M") %>%
  dplyr::select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

dat <- copy
str(dat)
dat$diagnosis <- as.factor(copy2$diagnosis)
dat %>% 
  gather(diagnosis, id) %>% 
  ggplot(aes(id,  group=diagnosis, fill=diagnosis, color = diagnosis)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ diagnosis, scales = "free")

copy %>% 
  gather(diagnosis, value) %>% 
  ggplot(aes(value,  group=diagnosis, fill=diagnosis,)) + 
  geom_density(colour = diagnosis, show.legend = FALSE) + 
  facet_wrap(~ diagnosis, scales = "free")

ggplot(df, aes(x=weight, color=sex)) +
  geom_density()
ggplot(train, aes(x=weight, color=diagnosis)) +
  geom_density()

p <- ggdensity(copy, x = "id", 
                      fill = "diagnosis", palette = "jco")

#featurePlot(x=x, y=y, plot="density", scales=scales)
