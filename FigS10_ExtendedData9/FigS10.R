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
#FigureS10########
#####################

Data_S10b <- as.data.frame(read.csv("FigS10b.csv", header = TRUE))
Data_S10c <- as.data.frame(read.csv("FigS10c.csv", header = TRUE))
Data_S10d <- as.data.frame(read.csv("FigS10d.csv", header = TRUE))
Data_S10e <- as.data.frame(read.csv("FigS10e.csv", header = TRUE))

sum_dat_S10b <- summarySE(Data_S10b, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_S10c <- summarySE(Data_S10c, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_S10d <- summarySE(Data_S10d, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_S10e <- summarySE(Data_S10e, measurevar="Fraction_P", groupvars=c("Strain","Day"))

#####FigS10b####

colors_S10b <- c('blue2', 'blue2')

FigS10b <- 
  ggplot(sum_dat_S10b, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(aes(shape=Strain, fill=Strain, color=Strain, size=Strain)) +
  scale_color_manual(values = colors_S10b) +
  scale_color_manual(values=colors_S10b) +
  scale_fill_manual(values=colors_S10b)+
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_shape_manual(values=c(24,24)) +
  scale_size_manual(values=c(1.5,1.5))+
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
FigS10b

FigS10b_fixed <- set_panel_size(FigS10b, width  = unit(1.35, "in"), height = unit(1.35, "in"))

###S10c###

colors_S10c <- c('firebrick1', 'firebrick1')

FigS10c <- 
  ggplot(sum_dat_S10c, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(aes(shape=Strain, fill=Strain, color=Strain, size=Strain)) +
  scale_color_manual(values = colors_S10c) +
  scale_color_manual(values=colors_S10c) +
  scale_fill_manual(values=colors_S10c)+
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_shape_manual(values=c(25,25)) +
  scale_size_manual(values=c(1.5,1.5))+
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
FigS10c

FigS10c_fixed <- set_panel_size(FigS10c, width  = unit(1.35, "in"), height = unit(1.35, "in"))

###FigS10d#####

colors_S10d <- c('darkorchid2', 'darkorchid2')

FigS10d <- 
  ggplot(sum_dat_S10d, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(aes(shape=Strain, fill=Strain, color=Strain, size=Strain)) +
  scale_color_manual(values = colors_S10d) +
  scale_color_manual(values=colors_S10d) +
  scale_fill_manual(values=colors_S10d)+
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_shape_manual(values=c(18,18)) +
  scale_size_manual(values=c(3,3))+
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
  scale_y_continuous(breaks=c(0.0, 0.5, 1.0))+
  theme(panel.background = element_rect(fill = "lightsteelblue1",
                                        colour = "lightsteelblue1", 
                                        linetype = "solid"))

FigS10d

FigS10d_fixed <- set_panel_size(FigS10d, width  = unit(1.35, "in"), height = unit(1.35, "in"))

#####S10e#####

colors_S10e <- c('darkorchid2', 'darkorchid2')

FigS10e <- 
  ggplot(sum_dat_S10e, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(aes(shape=Strain, fill=Strain, color=Strain, size=Strain)) +
  scale_color_manual(values = colors_S10e) +
  scale_color_manual(values=colors_S10e) +
  scale_fill_manual(values=colors_S10e)+
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_shape_manual(values=c(18,18)) +
  scale_size_manual(values=c(3,3))+
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
  scale_y_continuous(breaks=c(0.0, 0.5, 1.0))+
  theme(panel.background = element_rect(fill = "mistyrose",
                                        colour = "mistyrose",
                                        linetype = "solid"))
FigS10e

FigS10e_fixed <- set_panel_size(FigS10e, width  = unit(1.35, "in"), height = unit(1.35, "in"))

####Fit together###


FigureS10_main <- plot_grid(FigS10e_fixed, FigS10d_fixed, FigS10b_fixed, FigS10c_fixed, 
                           ncol = 2, align = "v", 
                          labels = c('b','c', 'd', 'e'), label_size = 10)

FigureS10_main

#create common x and y labels

y.grob <- textGrob("Proportion of plasmid-containing cells", 
                   gp=gpar(fontsize=10), rot=90)

x.grob <- textGrob("Transfer", 
                   gp=gpar(fontsize=10))

#add common axis to plot

FigureS10 <- grid.arrange(arrangeGrob(FigureS10_main, left = y.grob, bottom = x.grob))

save_plot("FigureS10.pdf", plot = FigureS10, base_width = 3.45, base_height = 3.7)


