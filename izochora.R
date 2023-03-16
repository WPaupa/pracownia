setwd('C:/Users/wojciech/Documents/pracownia')
library(ggplot2)
library(dplyr)
library(tikzDevice)

df = read.csv('izochora.csv')
colnames(df) <- c('T1', 'p1', 'T2', 'p2')

deltap <- 2
deltaT <- 0.5

pr1 <- rma(df$p1 ~ df$T1, sei=deltap, method="FE")
print(summary(pr1))
print(vcov(pr1))

pr2 <- rma(df$p2 ~ df$T2, sei=deltap, method="FE")
print(summary(pr2))
print(vcov(pr2))

ggplot(df) + 
  geom_point(aes(x = T1, y = p1)) + 
  geom_errorbar(aes(ymin = p1 - deltap, ymax = p1 + deltap, xmin = T1 - deltaT, xmax = T1 + deltaT, x = T1, y = p1)) + 
  stat_smooth(aes(x = T1, y = p1), method = "lm") + 
  geom_smooth(method = "lm", mapping=aes(x = T1, y=predict(pr1,df$T1)$pred)) + 
  labs(x = 'T [°C]', y = 'p [kPa]', caption = 'Wykres 1. Zależność ciśnienia od temperatury przy ogrzewaniu izochorycznym') +
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(angle=90, 
                                 hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )

ggplot(df) + 
  geom_point(aes(x = T2, y = p2)) + 
  geom_errorbar(aes(ymin = p2 - deltap, ymax = p2 + deltap, xmin = T2 - deltaT, xmax = T2 + deltaT, x = T2, y = p2)) + 
  stat_smooth(aes(x = T2, y = p2), method = "lm") + 
  geom_smooth(method = "lm", mapping=aes(x = T2, y=predict(pr2,df$T2)$pred)) + 
  labs(x = 'T [°C]', y = 'p [kPa]', caption = 'Wykres 2. Zależność ciśnienia od temperatury przy schładzaniu izochorycznym') +
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(angle=90, 
                                 hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )

df2 <- read.csv('izoterma.csv')

ggplot(df2) + 
  geom_point(aes(x = p1, y = pVT1)) + 
  geom_errorbar(aes(ymin = pVT1 - DeltapVT1, ymax = pVT1 + DeltapVT1, xmin = p1 - 2, xmax = p1 + 2, x = p1, y = pVT1)) +
  labs(x = 'p [kPa]', y = 'pV/T [J/K]', caption = 'Wykres 3. Zależność pV/T od ciśnienia, pierwsza próba') + 
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(angle=90, 
                                 hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )

ggplot(df2) + 
  geom_point(aes(x = p2, y = pVT2)) + 
  geom_errorbar(aes(ymin = pVT2 - DeltapVT1, ymax = pVT2 + DeltapVT1, xmin = p2 - 2, xmax = p2 + 2, x = p2, y = pVT2)) +
  labs(x = 'p [kPa]', y = 'pV/T [J/K]', caption = 'Wykres 4. Zależność pV/T od ciśnienia, druga próba') + 
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(angle=90, 
                                 hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )

ggplot(df2) + 
  geom_point(aes(x = p3, y = pVT3)) + 
  geom_errorbar(aes(ymin = pVT3 - DeltapVT3, ymax = pVT3 + DeltapVT3, xmin = p3 - 2, xmax = p3 + 2, x = p3, y = pVT3)) +
  labs(x = 'p [kPa]', y = 'pV/T [J/K]', caption = 'Wykres 5. Zależność pV/T od ciśnienia, trzecia próba') + 
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(angle=90, 
                                 hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )
