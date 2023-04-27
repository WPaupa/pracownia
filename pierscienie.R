setwd('C:/Users/wojciech/Documents/pracownia')
library(ggplot2)
library(dplyr)
library(tikzDevice)
library(chemCal)

df1 <- read.csv('s1.csv')
mid1 <- df1$ciemne[6]
df1 <- data.frame(sapply(df1, function(col) as.numeric(gsub(",", ".", col))))[1:5,]
df1$lenc = (df1$ciemne-df1$ciemne.) / 2
df1$lenj = (df1$jasne-df1$jasne.) / 2

plot1 <- data.frame(x = sqrt((10:1) / 2), y = c(rbind(df1$lenc, df1$lenj)))
ggplot(plot1, aes(x = x, y = y)) + geom_point() + geom_smooth(method = "lm")

df2 <- read.csv('s2.csv')
mid2 <- df2$ciemne[6]
df2 <- data.frame(sapply(df2, function(col) as.numeric(gsub(",", ".", col))))[1:5,]
df2$lenc = (df2$ciemne-df2$ciemne. ) /2
df2$lenj = (df2$jasne-df2$jasne. ) / 2

plot2 <- data.frame(x = sqrt((10:1) / 2), y = c(rbind(df2$lenc, df2$lenj)))
ggplot(plot2, aes(x = x, y = y)) + geom_point() + geom_smooth(method = "lm")

df3 <- read.csv('s3.csv')
mid3 <- df3$ciemne[6]
df3 <- data.frame(sapply(df3, function(col) as.numeric(gsub(",", ".", col))))[1:5,]
df3$lenc = (df3$ciemne-df3$ciemne.) / 2
df3$lenj = (df3$jasne-df3$jasne. ) / 2

plot3 <- data.frame(x = sqrt((10:1) / 2), y = c(rbind(df3$lenc, df3$lenj)))
ggplot(plot3, aes(x = x, y = y)) + geom_point() + geom_smooth(method = "lm")

ggplot() + geom_point(plot1, mapping=aes(x = x, y = y), color="black", shape="circle", size=5) + 
  geom_smooth(plot1, mapping=aes(x = x, y = y), color="black", method="lm", se=FALSE) +
  geom_point(plot2, mapping=aes(x = x, y = y), color="green", shape="triangle", size=5) + 
  geom_smooth(plot2, mapping=aes(x = x, y = y), color="green", method="lm", se=FALSE) +
  geom_point(plot3, mapping=aes(x = x, y = y), color="orange", shape="square", size=5) + 
  geom_smooth(plot3, mapping=aes(x = x, y = y), color="orange", method="lm", se=FALSE) +
  labs(x = '√q', y = 'r [mm]') +
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


woda <- read.csv('woda.csv')
midw <- woda$ciemne[6]
woda <- data.frame(sapply(woda, function(col) as.numeric(gsub(",", ".", col))))[1:5,]
woda$lenc = (woda$ciemne-woda$ciemne. ) / 2
woda$lenj = (woda$jasne-woda$jasne. ) / 2

plotw <- data.frame(x = sqrt((10:1) / 2), y = c(rbind(woda$lenc, woda$lenj)))
ggplot(plotw, aes(x = x, y = y)) + geom_point(size=4) + geom_smooth(method = "lm", se=FALSE, color="black") +
  labs(x = '√q', y = 'r [mm]') +
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


kolory <- data.frame(x = sqrt((10:1) / 2))

żółta <- read.csv('zolta.csv')
midc <- żółta$ciemne[6]
żółta <- data.frame(sapply(żółta, function(col) as.numeric(gsub(",", ".", col))))[1:5,]
żółta$lenc = ( żółta$ciemne-żółta$ciemne. ) / 2
żółta$lenj = ( żółta$jasne-żółta$jasne. ) / 2

kolory$żółta = c(rbind(żółta$lenc, żółta$lenj))
ggplot(kolory, aes(x = x, y = żółta)) + geom_point() + geom_smooth(method = "lm", color='yellow')

czerwona <- read.csv('czerwona.csv')
midc <- czerwona$ciemne[6]
czerwona <- data.frame(sapply(czerwona, function(col) as.numeric(gsub(",", ".", col))))[1:5,]
czerwona$lenc = ( czerwona$ciemne-czerwona$ciemne. ) /2
czerwona$lenj = ( czerwona$jasne-czerwona$jasne. ) / 2

kolory$czerwona = c(rbind(czerwona$lenc, czerwona$lenj))
ggplot(kolory, aes(x = x, y = czerwona)) + geom_point() + geom_smooth(method = "lm", color='red')

niebieska <- read.csv('niebieska.csv')
midn <- niebieska$ciemne[6]
niebieska <- data.frame(sapply(niebieska, function(col) as.numeric(gsub(",", ".", col))))[1:5,]
niebieska$lenc = ( niebieska$ciemne-niebieska$ciemne. ) / 2
niebieska$lenj = ( niebieska$jasne-niebieska$jasne. ) / 2

kolory$niebieska <- c(rbind(niebieska$lenc, niebieska$lenj))
ggplot(kolory, aes(x = x, y = niebieska)) + geom_point() + geom_smooth(method = "lm", color='blue')

ggplot(kolory) + geom_point(aes(x = x, y = niebieska), color='blue', size=4) + 
  geom_point(aes(x = x, y = czerwona), color='red', size=4) + geom_point(aes(x = x, y = żółta), color='yellow', size=4) + 
  geom_smooth(aes(x = x, y = niebieska), method = "lm", color = 'blue', se=FALSE) +
  geom_smooth(aes(x = x, y = czerwona), method = "lm", color = 'red', se=FALSE) + 
  geom_smooth(aes(x = x, y = żółta), method = "lm", color = 'yellow', se=FALSE) + 
  labs(x = '√q', y = 'r [mm]') +
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

biala <- read.csv('biala.csv')
midb <- biala$różowe[6]
biala <- data.frame(sapply(biala, function(col) as.numeric(gsub(",", ".", col))))[1:3,]
biala$lenc = ( biala$różowe-biala$różowe. ) / 2
biala$lenj = ( biala$niebieskie-biala$niebieskie. ) / 2

plota <- data.frame(x = sqrt((6:1) / 2), y = c(rbind(biala$lenc, biala$lenj)))
ggplot(plota, aes(x = x, y = y)) + geom_point(size=4) + geom_smooth(method = "lm", color="black") +
  labs(x = '√q', y = 'r [mm]') +
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

plotb <- data.frame(x = sqrt((5:0) / 2), y = c(rbind(biala$lenc, biala$lenj)))
ggplot(plotb, aes(x = x, y = y)) + geom_point(size=4) + geom_smooth(method = "lm", color="black") +
  labs(x = '√q', y = 'r [mm]') +
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