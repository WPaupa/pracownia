setwd('C:/Users/wojciech/Documents/pracownia')
library(ggplot2)
library(dplyr)
library(tikzDevice)
library(chemCal)
library(metafor)

df <- read.csv('radon.csv')
df <- data.frame(sapply(df, function(col) as.numeric(gsub(",", ".", col))))
colnames(df) <- c('n1', 'n2', 'eta', 't')

lA <- log(2) / 3.05 / 60
lB <- log(2) / 26.8 / 60 
lC <- log(2) / 19.8 / 60 

df$ea = exp(-lA * df$t)
df$eb = exp(-lB * df$t)
df$ec = exp(-lC * df$t)

linm <- rma(df$n1 ~ df$ea + df$eb + df$ec, sei = sqrt(df$n1), method="FE", intercept=FALSE)
a <- linm$beta['df$ea',]
b <- linm$beta['df$eb',]
c <- linm$beta['df$ec',]
int <- linm$beta['intrcpt',]

ca <- a/184
cb <- (b-139 * ca) / 1084
cc <- (c + 143 * ca + 1060 * cb) / 275

df$nmin = df$n1 - sqrt(df$n1)
df$nmax = df$n1 + sqrt(df$n1)
df$pred = a * df$ea + b * df$eb + c * df$ec + int
df$diff = df$pred - df$n1
df$drel = df$diff / df$n1

ggplot(df) + geom_point(aes(x = t, y = drel)) + 
  geom_hline(yintercept=0) + 
  labs(x = 't [s]', y = 'Δn') +
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

counts = c(25 + 25 + 22, 8 + 10 + 13, 14 + 10 + 21, 8 + 4 + 6, 4 + 8 + 4, 6 + 5 + 3, 1 + 4 + 7, 0 + 2 + 1, 1 + 3 + 1, 0 + 0 + 1, 0 + 1 + 1, 0 + 0 + 1)
lens = 0.2 + 0.5 * (1 : length(counts))
cls <- data.frame(counts, lens)
cls$cmin = cls$counts - 3 * sqrt(cls$counts)
cls$cmax = cls$counts + 3 * sqrt(cls$counts)
ggplot(cls, aes(y = counts, x = lens)) + 
  geom_point() + geom_errorbar(aes(ymin = cmin, ymax = cmax)) + 
  geom_hline(yintercept=1.33)