## CENOGRAM AND HISTOGRAMS ##

## A.V. Demers-Potvin* & H.C.E. Larsson ##
## *script author ##
## date created: 2024/07/29 ##
## last date modified: 2024/08/15 ##

setwd("")

# install.packages("tidyverse")
# install.packages("ggpubr")
# install.packages("data.table")
# install.packages("car")
library(tidyverse)
library(ggpubr)
library(scales)
library(data.table)
library(car)




### Cenograms for DPP, Serengeti and Komodo
cenogram <- read.csv("Data S3 - Cenograms and histograms/Data S3A - Cenogram.csv")


ggplot(cenogram, aes(x = Rank.M, y = M.kg, shape = Fauna)) + 
  theme_test() +
  scale_y_log10('Body mass (kg)', labels = label_number(accuracy = 1), limits = c(1,7500)) + 
  scale_x_continuous('Rank body mass', limits = c(1,67), breaks = seq(1,67,3)) + annotation_logticks(sides = 'rl') +
  geom_line(col = 'gray30') +
  geom_point(aes(fill = Group), size = 2.5, stroke = .5) +
  
  
  scale_fill_manual('Guild', breaks = c('C dinosaur','C lizard','C mammal','C inv mammal','C pterosaur',
                               'H dinosaur','H mammal','H turtle','O mammal','O dinosaur','O meat mammal',
                               'O plant mammal','O plant dinosaur'),
                    values = c('#a50f15','#de2d26','#fb6a4a','orange','pink',
                               '#006d2c','#31a354','#bae4b3','white','#cccccc','#969696','#636363','black')) +
  scale_shape_manual('Fauna',breaks = c('Dinosaur Park Fm MAZ-1b','Serengeti National Park','Komodo National Park'),
                     values = c(24,21,22)) +
  
  theme(axis.text = element_text(size = 12)) + theme(axis.title = element_text(size = 15)) + 
  theme(axis.title.y = element_text(margin = margin(r = -2.5))) +
  theme(legend.position = c(0.825,0.7)) + theme(legend.title = element_text(size = 10)) +
  theme(legend.text = element_text(size = 8)) + theme(legend.key.size = unit(.35, 'cm')) +
  guides(fill = guide_legend(override.aes=list(shape = 21), nrow = 7))

ggsave("Cenogram.png", width = 8, height = 4)




# body size distribution histograms
cenogram <- cenogram %>% mutate(Log2M = log2(M.kg))

ceno_stats <- cenogram %>% group_by(Fauna) %>% summarise(Mean = mean(Log2M), SD = sd(Log2M))

ggplot(cenogram, aes(x = Log2M)) + 
  geom_histogram(bins = 13, boundary = 0, binwidth = 1, closed = 'left', 
                 col = 'black', fill = 'ivory', position = 'identity') +
  facet_wrap(~Fauna) + 
  
  geom_vline(data = ceno_stats, aes(xintercept = Mean), col = 'red', linetype = 'dashed') +
  geom_vline(data = ceno_stats, aes(xintercept = Mean+SD), col = 'red', linetype = 'dotted', linewidth = .5) +
  geom_vline(data = ceno_stats, aes(xintercept = Mean-SD), col = 'red', linetype = 'dotted', linewidth = .5) +
  
  scale_x_continuous(breaks = seq(0,12,2)) + labs(x = expression(Log[2]*~body~mass)) +
  theme_test() + scale_y_continuous('Frequency', breaks = seq(0,12,4)) +
  theme(axis.text = element_text(size = 12)) + theme(axis.title = element_text(size = 15)) +
  theme(strip.text = element_text(size = 12))

ggsave("Histogram.png", width = 8, height = 2.25)




# calculate descriptive stats for body mass in each community
cenogram <- cenogram %>% filter(!Ont.stage %in% c('Juvenile')) # use this line only to calculate stats without juveniles

setDT(cenogram)
# mean and standard deviation
cenogram[ ,list(Mean = mean(Log2M), SD = sd(Log2M)), by = Fauna]

# test assumptions of variance homogeneity and normal distributions
leveneTest(data = cenogram, Log2M ~ Fauna, center = mean) # when juveniles are retained, F=0.4962, p-value = 0.61 >0.05 thus null hypothesis of equal variances not rejected

cenogram[ ,list(shapiro = shapiro.test(cenogram$Log2M)), by = Fauna]
shapiro.test(cenogram$Log2M) # when juveniles are retained, p-value = 0.002045 <0.05 thus null hypothesis of normal distribution is rejected

# compare means with non-parametric test
kruskal.test(Log2M ~ Fauna, data =cenogram) # when juveniles are retained, p-value = 2.89e-05 <0.05 therefore some means are significantly different

pairwise.wilcox.test(cenogram$Log2M, cenogram$Fauna, p.adjust.method = "bonferroni")
# p-values with juveniles: DPF-Komodo = 0.008; DPF-Serengeti = 9.7e-05; Komodo-Serengeti = 0.854
# p-values without juveniles: DPF-Komodo = 0.1288; DPF-Serengeti = 0.0012; Komodo-Serengeti = 1.000






#### Histograms of trophic level distributions for food webs ####
TL <- read.csv("Data S3 - Cenograms and histograms/Data S3B - NodeTLf.csv")

TL2 <- TL %>% group_by(Food.web) %>% summarise(Mean = mean(PATrophicLevel), SD = sd(PATrophicLevel))

# DPF main
MAZ1a <- TL %>% filter(Food.web %in% c('MAZ1a'))
MAZ1aTL <- TL2 %>% filter(Food.web %in% c('MAZ1a'))
ggplot(MAZ1a, aes(x = PATrophicLevel)) + geom_histogram(bins = 8, boundary = 0, binwidth = .5, closed = 'left',
                                                        col = 'black', fill = 'palegreen', position = 'identity') +
  scale_x_continuous('PATL') + theme_classic() +
  theme(panel.background = element_rect(fill='transparent'),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.line.x.bottom = element_line(), axis.ticks = element_blank()) +
  geom_vline(data = MAZ1aTL, aes(xintercept = Mean), col = 'red', linetype = 'dashed', linewidth = 1) +
  geom_vline(data = MAZ1aTL, aes(xintercept = Mean+SD), col = 'red', linetype = 'dotted', linewidth = 1) +
  geom_vline(data = MAZ1aTL, aes(xintercept = Mean-SD), col = 'red', linetype = 'dotted', linewidth = 1)

ggsave("MAZ1a.png", width = 4, height = 2.5, bg = 'transparent')


# DPF terrestrial pack-hunting version
MAZ1ap <- TL %>% filter(Food.web %in% c('MAZ1apack'))
MAZ1apTL <- TL2 %>% filter(Food.web %in% c('MAZ1apack'))
ggplot(MAZ1ap, aes(x = PATrophicLevel)) + geom_histogram(bins = 8, boundary = 0, binwidth = .5, closed = 'left',
                                                         col = 'black', fill = 'palegreen', position = 'identity') +
  scale_x_continuous('PATL') + theme_classic() +
  theme(panel.background = element_rect(fill='transparent'),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.line.x.bottom = element_line(), axis.ticks = element_blank()) +
  geom_vline(data = MAZ1apTL, aes(xintercept = Mean), col = 'red', linetype = 'dashed', linewidth = 1) +
  geom_vline(data = MAZ1apTL, aes(xintercept = Mean+SD), col = 'red', linetype = 'dotted', linewidth = 1) +
  geom_vline(data = MAZ1apTL, aes(xintercept = Mean-SD), col = 'red', linetype = 'dotted', linewidth = 1)

ggsave("MAZ1apack.png", width = 4, height = 2.5, bg = 'transparent')


# Serengeti
Ser <- TL %>% filter(Food.web %in% c('Serengeti'))
SerTL <- TL2 %>% filter(Food.web %in% c('Serengeti'))
ggplot(Ser, aes(x = PATrophicLevel)) + geom_histogram(bins = 8, boundary = 0, binwidth = .5, closed = 'left',
                                                      col = 'black', fill = 'palegreen', position = 'identity') +
  scale_x_continuous('PATL') + theme_classic() +
  theme(panel.background = element_rect(fill='transparent'),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.line.x.bottom = element_line(), axis.ticks = element_blank()) +
  geom_vline(data = SerTL, aes(xintercept = Mean), col = 'red', linetype = 'dashed', linewidth = 1) +
  geom_vline(data = SerTL, aes(xintercept = Mean+SD), col = 'red', linetype = 'dotted', linewidth = 1) +
  geom_vline(data = SerTL, aes(xintercept = Mean-SD), col = 'red', linetype = 'dotted', linewidth = 1)

ggsave("Serengeti.png", width = 4, height = 2.5, bg = 'transparent')


# Komodo
Komodo <- TL %>% filter(Food.web %in% c('Komodo'))
KomodoTL <- TL2 %>% filter(Food.web %in% c('Komodo'))
ggplot(Komodo, aes(x = PATrophicLevel)) + geom_histogram(bins = 8, boundary = 0, binwidth = .5, closed = 'left',
                                                         col = 'black', fill = 'palegreen', position = 'identity') +
  scale_x_continuous('PATL') + theme_classic() +
  theme(panel.background = element_rect(fill='transparent'),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.line.x.bottom = element_line(), axis.ticks = element_blank()) +
  geom_vline(data = KomodoTL, aes(xintercept = Mean), col = 'red', linetype = 'dashed', linewidth = 1) +
  geom_vline(data = KomodoTL, aes(xintercept = Mean+SD), col = 'red', linetype = 'dotted', linewidth = 1) +
  geom_vline(data = KomodoTL, aes(xintercept = Mean-SD), col = 'red', linetype = 'dotted', linewidth = 1)

ggsave("Komodo.png", width = 4, height = 2.5, bg = 'transparent')



### END OF SCRIPT ###


