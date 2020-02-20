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
#FigureS12########
#####################

Data_KP28s <- as.data.frame(read.csv("FigS12_KP28s.csv", header = TRUE))
Data_EC29s <- as.data.frame(read.csv("FigS12_EC29s.csv", header = TRUE))
Data_EC28s <- as.data.frame(read.csv("FigS12_EC28s.csv", header = TRUE))
Data_KP29s <- as.data.frame(read.csv("FigS12_KP29s.csv", header = TRUE))

sum_dat_KP28s <- summarySE(Data_KP28s, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_EC29s <- summarySE(Data_EC29s, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_EC28s <- summarySE(Data_EC28s, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_KP29s <- summarySE(Data_KP29s, measurevar="Fraction_P", groupvars=c("Strain","Day"))


###S12a###
sum_dat_EC28evo <- sum_dat_EC28s[which(sum_dat_EC28s$Strain != 'ancestor'),]

FigS12a <-
  ggplot(sum_dat_EC28evo, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(shape=24, size=1)+
  scale_linetype_manual(values=c("solid", "solid")) +
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
  theme(axis.text = element_text(size = 8), legend.text = element_text(size = 8), legend.title = element_blank()) +
  scale_x_continuous(breaks=c(0,3,6,9))+
  scale_y_continuous(breaks=c(0.0, 0.5, 1.0))

FigS12a

###S12b###
sum_dat_KP29evo <- sum_dat_KP29s[which(sum_dat_KP29s$Strain != 'ancestor'),]


FigS12b <-
  ggplot(sum_dat_KP29evo, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se),  width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(shape=25, size=1)+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid","solid")) +
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
  theme(axis.text = element_text(size = 8), legend.text = element_text(size = 8), legend.title = element_blank()) +
  scale_x_continuous(breaks=c(0,3,6,9))+
  scale_y_continuous(breaks=c(0.0, 0.5, 1.0))

FigS12b

###S12d###
sum_dat_KP28evo <- sum_dat_KP28s[which(sum_dat_KP28s$Strain != 'ancestor'),]

FigS12d <-
  ggplot(sum_dat_KP28evo, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se),  width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(shape=24, size=1)+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid","solid")) +
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
  theme(axis.text = element_text(size = 8), legend.text = element_text(size = 8), legend.title = element_blank()) +
  scale_x_continuous(breaks=c(0,3,6,9))+
  scale_y_continuous(breaks=c(0.0, 0.5, 1.0))


FigS12d


###S12c###
sum_dat_EC29evo <- sum_dat_EC29s[which(sum_dat_EC29s$Strain != 'ancestor'),]

FigS12c <-
  ggplot(sum_dat_EC29evo, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se),  width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(shape=25, size=1)+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid","solid")) +
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
  theme(axis.text = element_text(size = 8), legend.text = element_text(size = 8), legend.title = element_blank()) +
  scale_x_continuous(breaks=c(0,3,6,9))+
  scale_y_continuous(breaks=c(0.0, 0.5, 1.0))

FigS12c


FigS12a_fixed <- set_panel_size(FigS12a, width  = unit(1.55, "in"), height = unit(1.35, "in"))
FigS12b_fixed <- set_panel_size(FigS12b, width  = unit(1.55, "in"), height = unit(1.35, "in"))
FigS12c_fixed <- set_panel_size(FigS12c, width  = unit(1.55, "in"), height = unit(1.35, "in"))
FigS12d_fixed <- set_panel_size(FigS12d, width  = unit(1.55, "in"), height = unit(1.35, "in"))

####Fit together###


FigureS12_main <- plot_grid(FigS12a_fixed, FigS12c_fixed, FigS12b_fixed, 
                          FigS12d_fixed, ncol = 2, align = "v", 
                          labels = c('a','c', 'b', 'd'), label_size = 10)

FigureS12_main

#create common x and y labels

y.grob <- textGrob("Proportion of plasmid-containing cells", 
                   gp=gpar(fontsize=10), rot=90)

x.grob <- textGrob("Transfer", 
                   gp=gpar(fontsize=10))

#add common axis to plot

FigureS12 <- grid.arrange(arrangeGrob(FigureS12_main, left = y.grob, bottom = x.grob))

save_plot("FigureS12.pdf", plot = FigureS12, base_width = 6, base_height = 4)


