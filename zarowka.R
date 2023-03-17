setwd('C:/Users/wojciech/Documents/pracownia')
library(ggplot2)
library(dplyr)
library(tikzDevice)
library(chemCal)

df = read.csv('A.csv')
colnames(df) <- c('UB', 'UR', 'U0', 'UZ', 'I', 'Ps', 'Rw')
df <- data.frame(sapply(df, function(col) as.numeric(gsub(",", ".", col))))

cutoff = 3

y <- df[-1:-(length(df$Ps)-cutoff),]$Ps
x <- df[-1:-(length(df$Ps)-cutoff),]$Rw
pr1 <- lm(y ~ x)

print(summary(pr1))
print(vcov(pr1))
print(inverse.predict(pr1, rep(0,5000000)))
R0 <- inverse.predict(pr1, rep(0,5000000))$Prediction
R0e <- inverse.predict(pr1, rep(0,5000000))$`Standard Error`
alphaR <- 4.5 * 10 ^ -3
T0 <- 295
df$T <- ((df$Rw / R0) - 1) / alphaR + T0 
df$DeltaT <- df$Rw / alphaR * R0e / (R0 ^ 2)

y <- df[-1:-(length(df$Ps)-cutoff),]$Ps
x <- df[-1:-(length(df$Ps)-cutoff),]$T
pr2 <- lm(y ~ x)

ggplot() + 
  geom_point(df, mapping=aes(x = Rw, y = Ps)) +
  stat_smooth(df[-1:-(length(df$Ps)-cutoff),], mapping=aes(x = Rw, y = Ps), method = "lm") + 
  labs(x = 'R [Ω]', y = 'p [W]', caption = 'Wykres 1. Zależność mocy wydzielanej przez żarówkę od jej oporu wewnętrznego') +
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

ggplot() + 
  geom_point(df, mapping=aes(x = T, y = Ps)) +
  geom_errorbar(df, mapping=aes(xmin = T - DeltaT, xmax = T + DeltaT, ymin = Ps, ymax = Ps, x = T, y = Ps)) + 
  labs(x = 'T [K]', y = 'p [W]', caption = 'Wykres 2. Zależność mocy wydzielanej przez żarówkę od jej temperatury') +
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

df2 <- read.csv('B.csv')
colnames(df2) <- c('U0', 'U00', 'UR0', 'U0t', 'URt', 't', 'Rw0', 'Rwt', 'Tt', 'Tt2', 'p')
df2 <- data.frame(sapply(df2, function(col) as.numeric(gsub(",", ".", col))))
df2$Tt = 1 / (7.1 * 0.0045) * (df2$Rwt - 7.1) / df2$t
df2$T1 <- ((df2$Rwt / R0) - 1) / alphaR + T0 
df2$DeltaT1 <- df2$Rwt / alphaR * R0e / (R0 ^ 2)

ggplot() + 
  geom_point(df2, mapping=aes(x = T1, y = p), color='red') +
  geom_errorbar(df2, mapping=aes(xmin = T1 - DeltaT1, xmax = T1 + DeltaT1, ymin = p, ymax = p, x = T1, y = p), color='red') + 
  geom_point(df, mapping=aes(x = T, y = Ps)) +
  geom_errorbar(df, mapping=aes(xmin = T - DeltaT, xmax = T + DeltaT, ymin = Ps, ymax = Ps, x = T, y = Ps)) +
  labs(x = 'T [K]', y = 'P [W]', caption = 'Wykres 3. Zależność mocy pobieranej i wydzielanej przez żarówkę od jej temperatury') +
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

df2$pmps <- df2$p - predict(pr2, data.frame(x = df2$T1))
ggplot(df2, mapping=aes(x = Tt, y = pmps)) + 
  geom_point() +
  stat_smooth(method="lm") + 
  labs(x = 'ΔT/Δt [K/ms]', y = 'P-Ps [W]', caption = 'Wykres 4. Zależność mocy ogrzewającej żarówkę od przyrostu temperatury w czasie') +
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

pred <- lm(df2$pmps ~ df2$Tt)
