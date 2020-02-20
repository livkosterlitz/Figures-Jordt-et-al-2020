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
#Figure3########
#####################
Data_EC28s <- as.data.frame(read.csv("Fig3_EC28s.csv", header = TRUE))
Data_KP29s <- as.data.frame(read.csv("Fig3_KP29s.csv", header = TRUE))

sum_dat_EC28s <- summarySE(Data_EC28s, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_KP29s <- summarySE(Data_KP29s, measurevar="Fraction_P", groupvars=c("Strain","Day"))

###3a###
sum_dat_EC28anc <- sum_dat_EC28s[which(sum_dat_EC28s$Strain == 'ancestor'),]

Fig3a <- 
  ggplot(sum_dat_EC28anc, aes(x=Day, y=Fraction_P)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146, color="blue2") +
  geom_line(linetype = "dashed", size = 0.4668623442372146, color="blue2") +
  geom_point(shape=24, fill="blue2", color="blue2", size=1.5)+
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


Fig3a

###3c###
sum_dat_EC28evo <- sum_dat_EC28s[which(sum_dat_EC28s$Strain != 'ancestor'),]
sum_dat_EC28evo

Fig3c <-
  ggplot(sum_dat_EC28evo, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146, color="blue2") +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146, color="blue2") +
  geom_point(shape=24, fill="blue2", color="blue2", size=1.5)+
  scale_linetype_manual(values=c("solid", "solid")) +
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

Fig3c

###3b###
sum_dat_KP29anc <- sum_dat_KP29s[which(sum_dat_KP29s$Strain == 'ancestor'),]

Fig3b <- 
  ggplot(sum_dat_KP29anc, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146, color="firebrick1") +
  geom_line(linetype = "dashed", size = 0.4668623442372146, color="firebrick1") +
  geom_point(shape=25, fill="firebrick1", color="firebrick1", size=1.5)+
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

Fig3b

###3d###
sum_dat_KP29evo <- sum_dat_KP29s[which(sum_dat_KP29s$Strain != 'ancestor'),]

Fig3d <-
  ggplot(sum_dat_KP29evo, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se),  width=0, size = 0.4668623442372146, color="firebrick1") +
  geom_line(aes(linetype = Strain), size = 0.4668623442372146, color="firebrick1") +
  geom_point(shape=25, fill="firebrick1", color="firebrick1", size=1.5)+
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

Fig3d

Fig3a_fixed <- set_panel_size(Fig3a, width  = unit(1.35, "in"), height = unit(1.35, "in"))
Fig3b_fixed <- set_panel_size(Fig3b, width  = unit(1.35, "in"), height = unit(1.35, "in"))
Fig3c_fixed <- set_panel_size(Fig3c, width  = unit(1.35, "in"), height = unit(1.35, "in"))
Fig3d_fixed <- set_panel_size(Fig3d, width  = unit(1.35, "in"), height = unit(1.35, "in"))

####Fit together###

Figure3_main <- plot_grid(Fig3a_fixed, Fig3c_fixed, Fig3b_fixed, 
                          Fig3d_fixed, ncol = 2, align = "v", 
                          labels = c('a','c', 'b', 'd'), label_size = 10)

Figure3_main

#create common x and y labels

y.grob <- textGrob("Proportion of plasmid-containing cells", 
                   gp=gpar(fontsize=10), rot=90)

x.grob <- textGrob("Transfer", 
                   gp=gpar(fontsize=10))

#add common axis to plot

Figure3 <- grid.arrange(arrangeGrob(Figure3_main, left = y.grob, bottom = x.grob))

save_plot("Figure3.pdf", plot = Figure3, base_width = 3.45, base_height = 4)


