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
#FigureS11########
#####################

Data_S11a <- as.data.frame(read.csv("FigS11a.csv", header = TRUE))
Data_S11b <- as.data.frame(read.csv("FigS11b.csv", header = TRUE))

sum_dat_S11a <- summarySE(Data_S11a, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_S11b <- summarySE(Data_S11b, measurevar="Fraction_P", groupvars=c("Strain","Day"))

###FigS11a#####

colors_S11a <- c('darkorchid2', 'darkorchid2')

FigS11a <- 
  ggplot(sum_dat_S11a, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(aes(shape=Strain, fill=Strain, color=Strain, size=Strain)) +
  scale_color_manual(values = colors_S11a) +
  scale_color_manual(values=colors_S11a) +
  scale_fill_manual(values=colors_S11a)+
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
  scale_y_continuous(breaks=c(0.0, 0.5, 1.0))

FigS11a

FigS11a_fixed <- set_panel_size(FigS11a, width  = unit(1.35, "in"), height = unit(1.35, "in"))

#####S11b#####

colors_S11b <- c('darkorchid2', 'darkorchid2')

FigS11b <- 
  ggplot(sum_dat_S11b, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(aes(shape=Strain, fill=Strain, color=Strain, size=Strain)) +
  scale_color_manual(values = colors_S11b) +
  scale_color_manual(values=colors_S11b) +
  scale_fill_manual(values=colors_S11b)+
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
  scale_y_continuous(breaks=c(0.0, 0.5, 1.0))

FigS11b

FigS11b_fixed <- set_panel_size(FigS11b, width  = unit(1.35, "in"), height = unit(1.35, "in"))

####Fit together###


FigureS11_main <- plot_grid(FigS11a_fixed, FigS11b_fixed, ncol = 2, align = "v", 
                          labels = c('a','b'), label_size = 10)

FigureS11_main

#create common x and y labels

y.grob <- textGrob("Proportion of plasmid-containing cells", 
                   gp=gpar(fontsize=10), rot=90)

x.grob <- textGrob("Transfer", 
                   gp=gpar(fontsize=10))

#add common axis to plot

FigureS11 <- grid.arrange(arrangeGrob(FigureS11_main, left = y.grob, bottom = x.grob))

save_plot("FigureS11.pdf", plot = FigureS11, base_width = 3.45, base_height = 1.85)


