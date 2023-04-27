setwd('C:/Users/wojciech/Documents/pracownia/rentgen')
library(ggplot2)
library(dplyr)
library(tikzDevice)
library(chemCal)
library(grid)
library(gridExtra)
library(lattice)

offset1 <- read.delim2("~/pracownia/rentgen/offset1.tsv")
offset2 <- read.delim2("~/pracownia/rentgen/offset2.tsv")
offset3 <- read.delim2("~/pracownia/rentgen/offset3.tsv")
offset4 <- read.delim2("~/pracownia/rentgen/offset4.tsv")
padajaca <- read.delim2("~/pracownia/rentgen/padajaca.tsv")
coupled1 <- read.delim2("~/pracownia/rentgen/1coupled.tsv")
coupled2 <- read.delim2("~/pracownia/rentgen/2coupled.tsv")

plot1 <- ggplot(offset1, aes(x = b, y = R)) + geom_line() + 
  labs(x = 'ω [°]', y = 'R') +
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )
plot2 <- ggplot(offset2, aes(x = b, y = R)) + geom_line() + 
  labs(x = '2θ [°]', y = 'R') +
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )
plot3 <- ggplot(offset3, aes(x = b, y = R)) + geom_line() + 
  labs(x = 'ω [°]', y = 'R') +
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )
plot4 <- ggplot(offset4, aes(x = b, y = R)) + geom_line() + 
  labs(x = '2θ [°]', y = 'R') +
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

plot1 <- ggplot(coupled1, aes(x = b, y = R)) + geom_line() + 
  labs(x = 'ω [°]', y = 'R') +
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )
plot2 <- ggplot(coupled2, aes(x = b, y = R)) + geom_line() + 
  labs(x = 'ω [°]', y = 'R') +
  #textsize
  theme_bw()+
  theme(legend.position="right",
        legend.key.height=unit(2,"lines"),
        legend.title=element_text(size=rel(0.8)),
        legend.text=element_text(size=rel(1.3)),
        axis.text.y=element_text(hjust=0.5),
        axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.5)),
        plot.caption = element_text(hjust=0.5, size=rel(1.2))
  )

grid.arrange(plot1, plot2, ncol=2)