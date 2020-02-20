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
#Figure5########
#####################

Data_5b <- as.data.frame(read.csv("Fig5b.csv", header = TRUE))
Data_5c <- as.data.frame(read.csv("Fig5c.csv", header = TRUE))
Data_5d <- as.data.frame(read.csv("Fig5d.csv", header = TRUE))
Data_5e <- as.data.frame(read.csv("Fig5e.csv", header = TRUE))

sum_dat_5b <- summarySE(Data_5b, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_5c <- summarySE(Data_5c, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_5d <- summarySE(Data_5d, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_5e <- summarySE(Data_5e, measurevar="Fraction_P", groupvars=c("Strain","Day"))

#####Fig5b####

colors_5b <- c('firebrick1', 'firebrick1')

Fig5b <- 
  ggplot(sum_dat_5b, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(aes(shape=Strain, fill=Strain, color=Strain, size=Strain)) +
  scale_color_manual(values = colors_5b) +
  scale_color_manual(values=colors_5b) +
  scale_fill_manual(values=colors_5b)+
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
Fig5b

Fig5b_fixed <- set_panel_size(Fig5b, width  = unit(1.35, "in"), height = unit(1.35, "in"))

###5c###

colors_5c <- c('blue2', 'blue2')

Fig5c <- 
  ggplot(sum_dat_5c, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(aes(shape=Strain, fill=Strain, color=Strain, size=Strain)) +
  scale_color_manual(values = colors_5c) +
  scale_color_manual(values=colors_5c) +
  scale_fill_manual(values=colors_5c)+
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
Fig5c

Fig5c_fixed <- set_panel_size(Fig5c, width  = unit(1.35, "in"), height = unit(1.35, "in"))

###Fig5d#####

colors_5d <- c('darkorchid2', 'darkorchid2')

Fig5d <- 
  ggplot(sum_dat_5d, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(aes(shape=Strain, fill=Strain, color=Strain, size=Strain)) +
  scale_color_manual(values = colors_5d) +
  scale_color_manual(values=colors_5d) +
  scale_fill_manual(values=colors_5d)+
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

Fig5d

Fig5d_fixed <- set_panel_size(Fig5d, width  = unit(1.35, "in"), height = unit(1.35, "in"))

#####5e#####

colors_5e <- c('darkorchid2', 'darkorchid2')

Fig5e <- 
  ggplot(sum_dat_5e, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146) +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146) +
  geom_point(aes(shape=Strain, fill=Strain, color=Strain, size=Strain)) +
  scale_color_manual(values = colors_5e) +
  scale_color_manual(values=colors_5e) +
  scale_fill_manual(values=colors_5e)+
  scale_linetype_manual(values=c("solid", "dashed")) +
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
Fig5e

Fig5e_fixed <- set_panel_size(Fig5e, width  = unit(1.35, "in"), height = unit(1.35, "in"))

####Fit together###

Figure5_main <- plot_grid(Fig5d_fixed, Fig5e_fixed, Fig5b_fixed, Fig5c_fixed,  
                           ncol = 2, align = "v", 
                          labels = c('b','c', 'd', 'e'), label_size = 10)
Figure5_main

#create common x and y labels

y.grob <- textGrob("Proportion of focal plasmid-containing cells", 
                   gp=gpar(fontsize=10), rot=90)

x.grob <- textGrob("Transfer", 
                   gp=gpar(fontsize=10))

#add common axis to plot

Figure5 <- grid.arrange(arrangeGrob(Figure5_main, left = y.grob, bottom = x.grob))

save_plot("Figure5.pdf", plot = Figure5, base_width = 3.45, base_height = 3.7)


