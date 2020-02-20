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
#FigureS4########
#####################
Data_KP28s <- as.data.frame(read.csv("FigS4KP28s.csv", header = TRUE))
Data_EC29s <- as.data.frame(read.csv("FigS4_EC29s.csv", header = TRUE))

sum_dat_KP28s <- summarySE(Data_KP28s, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_EC29s <- summarySE(Data_EC29s, measurevar="Fraction_P", groupvars=c("Strain","Day"))

###S4b###
sum_dat_KP28anc <- sum_dat_KP28s[which(sum_dat_KP28s$Strain == 'ancestor'),]

FigS4b <- 
  ggplot(sum_dat_KP28anc, aes(x=Day, y=Fraction_P)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146, color="blue2") +
  geom_line(linetype = "dashed", size = 0.4668623442372146, color="blue2") +
  geom_point(shape=24, fill="blue2", color="blue2", size=1)+
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


FigS4b

###S4d###
sum_dat_KP28evo <- sum_dat_KP28s[which(sum_dat_KP28s$Strain != 'ancestor'),]

FigS4d <-
  ggplot(sum_dat_KP28evo, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se),  width=0, size = 0.4668623442372146, color="blue2") +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146, color="blue2") +
  geom_point(shape=24, fill="blue2", color="blue2", size=1)+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid","solid")) +
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


FigS4d

###S4a###
sum_dat_EC29anc <- sum_dat_EC29s[which(sum_dat_EC29s$Strain == 'ancestor'),]

FigS4a <- 
  ggplot(sum_dat_EC29anc, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146, color="firebrick1") +
  geom_line(linetype = "dashed", size = 0.4668623442372146, color="firebrick1") +
  geom_point(shape=25, fill="firebrick1", color="firebrick1", size=1)+
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

FigS4a

###S4c###
sum_dat_EC29evo <- sum_dat_EC29s[which(sum_dat_EC29s$Strain != 'ancestor'),]


FigS4c <-
  ggplot(sum_dat_EC29evo, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se),  width=0, size = 0.4668623442372146, color="firebrick1") +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146, color="firebrick1") +
  geom_point(shape=25, fill="firebrick1", color="firebrick1", size=1)+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid","solid")) +
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

FigS4c


FigS4a_fixed <- set_panel_size(FigS4a, width  = unit(1.35, "in"), height = unit(1.35, "in"))
FigS4b_fixed <- set_panel_size(FigS4b, width  = unit(1.35, "in"), height = unit(1.35, "in"))
FigS4c_fixed <- set_panel_size(FigS4c, width  = unit(1.35, "in"), height = unit(1.35, "in"))
FigS4d_fixed <- set_panel_size(FigS4d, width  = unit(1.35, "in"), height = unit(1.35, "in"))

####Fit together###

FigureS4_main <- plot_grid(FigS4a_fixed, FigS4c_fixed, FigS4b_fixed, 
                          FigS4d_fixed, ncol = 2, align = "v", 
                          labels = c('a','c', 'b', 'd'), label_size = 10)

FigureS4_main

#create common x and y labels

y.grob <- textGrob("Proportion of plasmid-containing cells", 
                   gp=gpar(fontsize=10), rot=90)

x.grob <- textGrob("Transfer", 
                   gp=gpar(fontsize=10))

#add common axis to plot

FigureS4 <- grid.arrange(arrangeGrob(FigureS4_main, left = y.grob, bottom = x.grob))

save_plot("FigureS4.pdf", plot = FigureS4, base_width = 3.45, base_height = 4)


