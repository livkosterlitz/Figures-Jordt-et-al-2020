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
#FigureS1########
#####################
Data_low <- as.data.frame(read.csv("FigS1_low.csv", header = TRUE))
Data_high <- as.data.frame(read.csv("FigS1_high.csv", header = TRUE))

sum_dat_low <- summarySE(Data_low, measurevar="Fraction_P", groupvars=c("Strain","Day"))
sum_dat_high <- summarySE(Data_high, measurevar="Fraction_P", groupvars=c("Strain","Day"))

###S1a###
sum_data_low <- sum_dat_low[which(sum_dat_low$Strain == 'Ancestor'),]

FigS1a <- 
  ggplot(sum_data_low, aes(x=Day, y=Fraction_P)) + 
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146, color="blue2") +
  geom_line(linetype = "dashed", size = 0.4668623442372146, color="blue2") +
  geom_point(fill="blue2", color="blue2", shape = 24, size = 1.5)+
  theme_cowplot(12)+
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


FigS1a

###S1b###
sum_data_high <- sum_dat_high[which(sum_dat_high$Strain == 'Evolved'),]

FigS1b <- 
  ggplot(sum_data_high, aes(x=Day, y=Fraction_P, colour=Strain)) + 
  theme_cowplot(12)+
  geom_errorbar(aes(ymin=Fraction_P-se, ymax=Fraction_P+se), width=0, size = 0.4668623442372146, color="blue2") +
  geom_line(size = 0.4668623442372146, color="blue2") +
  geom_point(fill="blue2", color="blue2", shape = 24, size = 1.5)+
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

FigS1b

FigS1a_fixed <- set_panel_size(FigS1a, width  = unit(1.35, "in"), height = unit(1.35, "in"))
FigS1b_fixed <- set_panel_size(FigS1b, width  = unit(1.35, "in"), height = unit(1.35, "in"))

####Fit together###

FigureS1_main <- plot_grid(FigS1a_fixed, FigS1b_fixed, 
                          ncol = 2, align = "v", 
                          labels = c('a','b'), label_size = 10)

FigureS1_main

#create common x and y labels

y.grob <- textGrob("Proportion of plasmid-containing cells", 
                   gp=gpar(fontsize=10), rot=90)

x.grob <- textGrob("Transfer", 
                   gp=gpar(fontsize=10))

#add common axis to plot

FigureS1 <- grid.arrange(arrangeGrob(FigureS1_main, left = y.grob, bottom = x.grob))

save_plot("FigureS1.pdf", plot = FigureS1, base_width = 3.45, base_height = 1.85)


