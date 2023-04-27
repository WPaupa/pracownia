setwd('C:/Users/wojciech/Documents/pracownia')
library(ggplot2)
library(dplyr)
library(tikzDevice)
library(chemCal)
library(metafor)

df = data.frame(x = c(20, 40, 60, 80, 85))
df$y = c(9, 158, 243, 323, 342)
df$z = c(95, 333, 492, 646, 714)

df$ymin = df$y - 8
df$ymax = df$y + 8
df$zmin = df$z - 8
df$zmax = df$z + 8
ggplot(df) + 
  geom_point(aes(x = x, y = y), color="blue") + 
  geom_point(aes(x = x, y = z), color="red") + 
  geom_errorbar(aes(x = x, y = y, ymin = ymin, ymax = ymax, width=0.5), color = "blue") +
  geom_errorbar(aes(x = x, y = z, ymin = zmin, ymax = zmax, width=0.5), color = "red") +
  geom_smooth(aes(x = x, y = y), method="lm", color="blue") + 
  geom_smooth(aes(x = x, y = z), method="lm", color="red") +
  labs(x = 'l [m]', y = 't [ns]') +
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

rma(y ~ x, sei = 8, data=df)
rma(z ~ x, sei = 8, data=df)

df2 = data.frame(x = c(25, 50, 75, 100, 125))
df2$y = c(125, 260, 390, 520, 656)
df2$z = c(261, 524, 667, 1044, 1316)

df2$ymin = df2$y - 8
df2$ymax = df2$y + 8
df2$zmin = df2$z - 8
df2$zmax = df2$z + 8
ggplot(df2) + 
  geom_point(aes(x = x, y = y), color="blue") + 
  geom_point(aes(x = x, y = z), color="red") + 
  geom_errorbar(aes(x = x, y = y, ymin = ymin, ymax = ymax, width=0.5), color = "blue") +
  geom_errorbar(aes(x = x, y = z, ymin = zmin, ymax = zmax, width=0.5), color = "red") +
  geom_smooth(aes(x = x, y = y), method="lm", color="blue") + 
  geom_smooth(aes(x = x, y = z), method="lm", color="red") +
  labs(x = 'l [m]', y = 't [ns]') +
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

df3 = data.frame(x = c(300.5, 255.8, 211.5, 184.3, 158.6, 119, 88.8, 52.8, 36.9, 9.1))
df3$v <- c(1.44, 1.38, 1.26, 1.22, 1.08, 0.82, 0.56, 0.04, -0.31, -1.49)

df4 <- data.frame(x = c(9.2, 36.5, 53.4, 82.2, 110.1, 137.7, 168, 198.7, 254, 305))
df4$v = c(-0.94, -0.54, -0.32, -0.08, 0.14, 0.24, 0.34, 0.48, 0.58, 0.66)

U0 <- 

ggplot() + 
  geom_point(df4, mapping=aes(x = x, y = v), color="black", shape="square", size=5) + 
  geom_smooth(df4, mapping=aes(x = x, y = v),color="black", method="nls", formula=y~ U0 * (a - x) / (a + x), 
              method.args = list(start = list(a = 50), algorithm="plinear"),
              se=FALSE) +
  geom_point(df3, mapping=aes(x = x, y = v), color="blue", shape="triangle", size=5) + 
  geom_smooth(df3, mapping=aes(x = x, y = v),color="blue", method="nls", formula=y~ U0 * (a - x) / (a + x), 
              method.args = list(start = list(a = 50), algorithm="plinear"),
              se=FALSE) +
  labs(x = 'R [Ω]', y = 'U [V]') +
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
