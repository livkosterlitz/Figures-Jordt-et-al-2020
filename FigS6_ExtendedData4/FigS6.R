library(tidyverse)
library(cowplot)
library(lattice)
library(gridExtra)
library(grid)
library(egg)
theme_set(theme_cowplot())

#########################
#FigureS6########
#####################

###Ancestor###

dat <- read.csv("FigS6_Mix2wCm75.csv")

dat <- dat %>%
  select(-Mixture, -CFUs) %>%
  group_by(Host, Antibiotic, Day) %>%
  summarise(N = n(),
            CFUs = mean(CFUs_sub),
            SD = sd(CFUs_sub),
            SE = SD/sqrt(N))
levels(dat$Antibiotic)

colors_light <- c('darkgrey', 'firebrick1', 'blue2', 'darkorchid2')

p1 <- ggplot(dat %>% filter(Host=='Kleb'),
             aes(x=Day, y=CFUs, color=Antibiotic)) + 
  geom_line(linetype = "dashed", size = 0.4668623442372146) +
  geom_errorbar(aes(ymin=CFUs-SE, ymax=CFUs+SE), width=0, size = 0.4668623442372146) +
  geom_point(aes(shape=Antibiotic, color=Antibiotic, fill=Antibiotic, size=Antibiotic)) +
  scale_color_manual(values=colors_light) +
  scale_fill_manual(values=colors_light)+
  scale_size_manual(values=c(2,1.5,1.5,3))+
  scale_shape_manual(values=c(16,25,24,18)) +
  scale_y_log10(limits=c(1e0, 1e10),
                breaks=10^seq(0, 9, by=3),
                labels=seq(0, 9, by=3)) +
  scale_x_continuous(limits = c(0,4.2)) +
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
  theme(axis.text = element_text(size = 8))

p1

p2 <- ggplot(dat %>% filter(Host=='Ecoli'),
             aes(x=Day, y=CFUs, color=Antibiotic)) + 
  geom_line(linetype = "dashed", size = 0.4668623442372146) +
  geom_errorbar(aes(ymin=CFUs-SE, ymax=CFUs+SE), width=0, size = 0.4668623442372146) +
  geom_point(aes(shape=Antibiotic, color=Antibiotic, fill=Antibiotic, size=Antibiotic)) +
  scale_color_manual(values=colors_light) +
  scale_fill_manual(values=colors_light)+
  scale_size_manual(values=c(2,1.5,1.5,3))+
  scale_shape_manual(values=c(16,25,24,18)) +
  scale_y_log10(limits=c(1e0, 1e10),
                breaks=10^seq(0, 9, by=3),
                labels=seq(0, 9, by=3)) +
  scale_x_continuous(limits = c(0,4.2)) +
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
  theme(axis.text = element_text(size = 8))

p2

###Evolved###

dat1 <- read.csv("FigS6_Mix4.csv")

dat1 <- dat1 %>%
  select(-Mixture, -CFUs) %>%
  group_by(Host, Antibiotic, Day) %>%
  summarise(N = n(),
            CFUs = mean(CFUs_sub),
            SD = sd(CFUs_sub),
            SE = SD/sqrt(N))

colors_dark <- c('darkgrey', 'firebrick1', 'blue2', 'darkorchid2')

p3 <- ggplot(dat1 %>% filter(Host=='Kleb'),
             aes(x=Day, y=CFUs, color=Antibiotic)) + 
  geom_line(size = 0.4668623442372146) +
  geom_errorbar(aes(ymin=CFUs-SE, ymax=CFUs+SE), width=0, size = 0.4668623442372146) +
  geom_point(aes(shape=Antibiotic, color=Antibiotic, fill=Antibiotic, size=Antibiotic)) +
  scale_color_manual(values=colors_light) +
  scale_fill_manual(values=colors_light)+
  scale_size_manual(values=c(2,1.5,1.5,3))+
  scale_shape_manual(values=c(16,25,24,18)) +
  scale_y_log10(limits=c(1e0, 1e10),
                breaks=10^seq(0, 9, by=3),
                labels=seq(0, 9, by=3)) +
  scale_x_continuous(limits = c(0,4.2)) +
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
  theme(axis.text = element_text(size = 8))

p3

p4 <- ggplot(dat1 %>% filter(Host=='Ecoli'),
             aes(x=Day, y=CFUs, color=Antibiotic)) + 
  geom_line(size = 0.4668623442372146) +
  geom_errorbar(aes(ymin=CFUs-SE, ymax=CFUs+SE), width=0, size = 0.4668623442372146) +
  geom_point(aes(shape=Antibiotic, color=Antibiotic, fill=Antibiotic, size=Antibiotic)) +
  scale_color_manual(values=colors_light) +
  scale_fill_manual(values=colors_light)+
  scale_size_manual(values=c(2,1.5,1.5,3))+
  scale_shape_manual(values=c(16,25,24,18)) +
  scale_y_log10(limits=c(1e0, 1e10),
                breaks=10^seq(0, 9, by=3),
                labels=seq(0, 9, by=3)) +
  scale_x_continuous(limits = c(0,4.2)) +
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
  theme(axis.text = element_text(size = 8))

p4

Figp1_fixed <- set_panel_size(p1, width  = unit(1.35, "in"), height = unit(1.35, "in"))
Figp2_fixed <- set_panel_size(p2, width  = unit(1.35, "in"), height = unit(1.35, "in"))
Figp3_fixed <- set_panel_size(p3, width  = unit(1.35, "in"), height = unit(1.35, "in"))
Figp4_fixed <- set_panel_size(p4, width  = unit(1.35, "in"), height = unit(1.35, "in"))

FigureS6_main <- plot_grid(Figp2_fixed, Figp4_fixed, Figp1_fixed, Figp3_fixed,
                          ncol = 2, align = "v")

FigureS6_main

#create common x and y labels

y.grob <- textGrob("cell density [log [(CFUs/mL)+1)]]",
                   gp=gpar(fontsize=10), rot=90)

x.grob <- textGrob("Transfer", 
                   gp=gpar(fontsize=10))

#add common axis to plot

FigureS6 <- grid.arrange(arrangeGrob(FigureS6_main, left = y.grob, bottom = x.grob))

save_plot("FigureS6_parts.pdf", plot = FigureS6, base_width = 3.45, base_height = 3.7)

