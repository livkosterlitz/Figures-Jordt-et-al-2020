library(ggplot2)
library(Rmisc)
library(lattice)
library(plyr)
library(cowplot)
library(gridExtra)
library(grid)
library(egg)
theme_set(theme_cowplot())

#########################
#FigureS5########
#####################
Data_KP28s <- as.data.frame(read.csv("FigS5_KP28s.csv", header = TRUE))
Data_KP29s <- as.data.frame(read.csv("FigS5_KP29s.csv", header = TRUE))
Data_HevoPancs <- as.data.frame(read.csv("FigS5_HevoPancs.csv", header = TRUE))

sum_dat_KP28s <- summarySE(Data_KP28s, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_KP29s <- summarySE(Data_KP29s, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_HevoPancs<- summarySE(Data_HevoPancs, measurevar="Fraction_P", groupvars=c("Strain","Day"))

###5b###
sum_dat_KP28 <- sum_dat_KP28s[which(sum_dat_KP28s$Strain == 'ancestor'),]
sum_dat_KP28 <- rbind(sum_dat_KP28, sum_dat_HevoPancs[which(sum_dat_HevoPancs$Strain == 'K1(p1)'),])
sum_dat_KP28 <- rbind(sum_dat_KP28, sum_dat_KP28s[which(sum_dat_KP28s$Strain == 'evolved 4'),])

FigS5b <- 
  ggplot(sum_dat_KP28, aes(x=Day, y=Fraction_P)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146, color="blue2") +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146, color="blue2") +
  geom_point(shape=24, fill="blue2", color="blue2", size=1)+
  scale_linetype_manual(values=c("dashed", "solid", "longdash")) +
  theme(legend.position="none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  theme(axis.line.y = element_line(size = 0.3734899)) +
  theme(axis.line.x = element_line(size = 0.3734899)) +
  theme(axis.ticks = element_line(size = 0.3734899))+
  theme(axis.text.x = element_text(margin=margin(1,0,0,0,"pt")),
        axis.text.y = element_text(margin=margin(0,1,0,0,"pt")))+
  theme(axis.ticks.length=unit(.025, "in"))+
  theme(plot.margin = margin(.2, 0, 0, .05, "in"))+
  expand_limits(x = 0, y = 0) +
  theme(axis.text = element_text(size = 8)) +
  scale_x_continuous(breaks=c(0,3,6,9))+
  scale_y_continuous(breaks=c(0.0, 0.5, 1.0))


FigS5b

###S5a###
sum_dat_KP29 <- sum_dat_KP29s[which(sum_dat_KP29s$Strain == 'ancestor'),]
sum_dat_KP29 <- rbind(sum_dat_KP29, sum_dat_HevoPancs[which(sum_dat_HevoPancs$Strain == 'K2(p2)'),])
sum_dat_KP29 <- rbind(sum_dat_KP29, sum_dat_KP29s[which(sum_dat_KP29s$Strain == 'evolved 2'),])

FigS5a <-
  ggplot(sum_dat_KP29, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se),  width=0, size = 0.4668623442372146, color="firebrick1") +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146, color="firebrick1") +
  geom_point(shape=25, fill="firebrick1", color="firebrick1", size=1)+
  scale_linetype_manual(values=c("dashed", "solid", "longdash")) +
  theme(legend.position="none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  theme(axis.line.y = element_line(size = 0.3734899)) +
  theme(axis.line.x = element_line(size = 0.3734899)) +
  theme(axis.ticks = element_line(size = 0.3734899))+
  theme(axis.text.x = element_text(margin=margin(1,0,0,0,"pt")),
        axis.text.y = element_text(margin=margin(0,1,0,0,"pt")))+
  theme(axis.ticks.length=unit(.025, "in"))+
  theme(plot.margin = margin(.2, 0, 0, .05, "in"))+
  expand_limits(x = 0, y = 0) +
  theme(axis.text = element_text(size = 8)) +
  scale_x_continuous(breaks=c(0,3,6,9))+
  scale_y_continuous(breaks=c(0.0, 0.5, 1.0))


FigS5a

FigS5a_fixed <- set_panel_size(FigS5a, width  = unit(1.35, "in"), height = unit(1.35, "in"))
FigS5b_fixed <- set_panel_size(FigS5b, width  = unit(1.35, "in"), height = unit(1.35, "in"))

####Fit together###

FigureS5_main <- plot_grid(FigS5a_fixed, FigS5b_fixed, ncol = 2, align = "v", 
                           labels = c('a','b'), label_size = 10)

FigureS5_main

#create common x and y labels

y.grob <- textGrob("Proportion of plasmid-containing cells", 
                   gp=gpar(fontsize=10), rot=90)

x.grob <- textGrob("Transfer", 
                   gp=gpar(fontsize=10))

#add common axis to plot

FigureS5 <- grid.arrange(arrangeGrob(FigureS5_main, left = y.grob, bottom = x.grob))

save_plot("FigureS5.pdf", plot = FigureS5, base_width = 3.45, base_height = 1.85)


