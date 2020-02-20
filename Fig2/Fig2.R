library(tidyverse)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

dat <- read.csv("Fig2.csv")

p1 <- ggplot(data=dat, aes(x=time, y=emergence_low)) +
  geom_line(color="darkorchid2", size = 0.4668623442372146, linetype = "dashed") +
  theme(axis.line.y = element_line(size = 0.3734899)) +
  theme(axis.line.x = element_line(size = 0.3734899)) +
  labs(y='density of cells \n containing both plasmids',
    x = 'time') +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  theme(axis.title=element_text(size=8)) +
  scale_y_continuous(limits = c(0,1000)) +
  scale_x_continuous(limits = c(0,4)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))
  
p1

save_plot("CartoonEmergence_Low.pdf", plot = p1, base_width = 1.4, base_height = 1.6)

#############################

p2 <- ggplot(data=dat, aes(x=time, y=emergence_high)) +
  geom_line(color="darkorchid2", size = 0.4668623442372146) +
  theme(axis.line.y = element_line(size = 0.3734899)) +
  theme(axis.line.x = element_line(size = 0.3734899)) +
  labs(y='density of cells \n containing both plasmids',
       x = 'time') +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  theme(axis.title=element_text(size=8)) +
  scale_y_continuous(limits = c(0,1000)) +
  scale_x_continuous(limits = c(0,4)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))

p2

save_plot("CartoonEmergence_High.pdf", plot = p2, base_width = 1.4, base_height = 1.6)

