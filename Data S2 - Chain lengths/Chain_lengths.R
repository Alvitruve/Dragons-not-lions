## TROPHIC LEVEL AND CHAIN LENGTH ESTIMATES ##

## A.V. Demers-Potvin* & H.C.E. Larsson ##
## *script author ##
## date created: 2024/07/29 ##
## last date modified: 2024/08/15 ##

setwd("")

# install.packages("tidyverse")
# install.packages("cheddar")
# install.packages("reshape2")
# install.packages("scales)
# install.packages("ggpubr")
library(tidyverse)
library(cheddar)
library(reshape2)
library(scales)
library(ggpubr)



#### calculate trophic chain stats ####

# DPF trophic species food web with juveniles (files labeled .pack include pack hunting)
Nodes <- read.csv("Data S2 - Chain lengths/DPF Tsp/DPF.Tsp.terr.nodes.csv")
Links <- read.csv("Data S2 - Chain lengths/DPF Tsp/")

title = 'DPP'
M.units = 'kg'
N.units = 'n.individuals'
properties = list('title'=title, 'M.units'=M.units)
DPFtrophsp <- Community(Nodes, properties, Links)

DPFchains <- TrophicChains(DPFtrophsp) # Returns file with every food chain element
PreyAveragedTrophicLevel(DPFtrophsp) # Obtain prey-averaged trophic levels (PATL)

# getOption("max.print")
options(max.print=1000000)
options(cheddarMaxQueue=0) 

DPFchain.stats <- TrophicChainsStats(DPFtrophsp)
DPFchain.stats

chain.data <- do.call(rbind.data.frame, DPFchain.stats)
chain.data <- chain.data[-c(1), ]

write.csv(chain.data, "", row.names = F) # Remove second column (with header = 1) since that includes frequencies at PATL = 1 (producers);
# only steps between each level (i.e. chains) are required here
#



# DPF trophic species food web without juveniles; choose version (files labeled .pack include pack hunting)
Nodes <- read.csv("Data S2 - Chain lengths/DPF Tsp/DPF.Tsp.terr.nojuv.nodes.csv")
Links <- read.csv("Data S2 - Chain lengths/DPF Tsp/")

title = 'DPP'
M.units = 'kg'
N.units = 'n.individuals'
properties = list('title'=title, 'M.units'=M.units)
DPFtrophsp <- Community(Nodes, properties, Links)

DPFchains <- TrophicChains(DPFtrophsp) # Returns file with every food chain element
PreyAveragedTrophicLevel(DPFtrophsp) # Obtain PATL

# getOption("max.print")
options(max.print=1000000)
options(cheddarMaxQueue=0) 

DPFchain.stats <- TrophicChainsStats(DPFtrophsp)
DPFchain.stats

chain.data <- do.call(rbind.data.frame, DPFchain.stats)
chain.data <- chain.data[-c(1), ]

write.csv(chain.data, "", row.names = F) # Remove second column (with header = 1) since that includes frequencies at PATL = 1 (producers);
# only steps between each level (i.e. chains) are required here
#



# Komodo trophic species food web; choose version (with / without cannibals (C))
Nodes <- read.csv("Data S2 - Chain lengths/Komodo Tsp/Komodo.Tsp.nodes.csv")
Links <- read.csv("Data S2 - Chain lengths/Komodo Tsp/")

title = 'Komodo'
M.units = 'kg'
N.units = 'n.individuals'
properties = list('title'=title, 'M.units'=M.units)
Komodo <- Community(Nodes, properties, Links)

Kchains <- TrophicChains(Komodo) # Returns file with every food chain element
PreyAveragedTrophicLevel(Komodo) # Obtain PATL

Kchain.stats <- TrophicChainsStats(Komodo) # the most useful code to get chain length frequency for each node
Kchain.stats

chain.data <- do.call(rbind.data.frame, Kchain.stats)
chain.data <- chain.data[-c(1), ]

write.csv(chain.data, "", row.names = F) # Remove second column (with header = 1) since that includes frequencies at PATL = 1 (producers);
# only steps between each level (i.e. chains) are required here
#



# Komodo fully resolved food web; choose version (with / without cannibals (C))
Nodes <- read.csv("Data S2 - Chain lengths/Komodo fully resolved/Komodo.nodes.csv")
Links <- read.csv("Data S2 - Chain lengths/Komodo Tsp/")

title = 'Komodo'
M.units = 'kg'
N.units = 'n.individuals'
properties = list('title'=title, 'M.units'=M.units)
Komodo <- Community(Nodes, properties, Links)

Kchains <- TrophicChains(Komodo) # Returns file with every food chain element
PreyAveragedTrophicLevel(Komodo) # Obtain PATL

Kchain.stats <- TrophicChainsStats(Komodo) # the most useful code to get chain length frequency for each node
Kchain.stats

chain.data <- do.call(rbind.data.frame, Kchain.stats)
chain.data <- chain.data[-c(1), ]

write.csv(chain.data, "", row.names = F) # Remove second column (with header = 1) since that includes frequencies at PATL = 1 (producers);
# only steps between each level (i.e. chains) are required here
#



### Serengeti fully resolved food web; choose version ###
Nodes <- read.csv("Data S2 - Chain lengths/Serengeti fully resolved/Serengeti.nodes.csv")
Links <- read.csv("Data S2 - Chain lengths/Serengeti fully resolved/")

# write properties vector
title = 'Serengeti'
M.units = 'kg'
properties = list('title'=title, 'M.units'=M.units)
Serengeti <- Community(Nodes, properties, Links)

Schains <- TrophicChains(Serengeti) # Returns file with every food chain element
PreyAveragedTrophicLevel(Serengeti) # Obtain PATL

# getOption("max.print")
options(max.print=1000000)

Schain.stats <- TrophicChainsStats(Serengeti)
Schain.stats

chain.data <- do.call(rbind.data.frame, Schain.stats)
chain.data <- chain.data[-c(1), ]

write.csv(chain.data, "", row.names = F) # Remove second column (with header = 1) since that includes frequencies at PATL = 1 (producers);
# only steps between each level (i.e. chains) are required here
#



### Serengeti trophic species food web; choose version ###
Nodes <- read.csv("Data S2 - Chain lengths/Serengeti Tsp/Serengeti.Tsp.nodes.csv")
Links <- read.csv("Data S2 - Chain lengths/Serengeti fully resolved/")

# write properties vector
title = 'Serengeti'
M.units = 'kg'
properties = list('title'=title, 'M.units'=M.units)
Serengeti <- Community(Nodes, properties, Links)

Schains <- TrophicChains(Serengeti) # Returns file with every food chain element
PreyAveragedTrophicLevel(Serengeti) # Obtain PATL

# getOption("max.print")
options(max.print=1000000)

Schain.stats <- TrophicChainsStats(Serengeti)
Schain.stats

chain.data <- do.call(rbind.data.frame, Schain.stats)
chain.data <- chain.data[-c(1), ]

write.csv(chain.data, "", row.names = F) # Remove second column (with header = 1) since that includes frequencies at PATL = 1 (producers);
# only steps between each level (i.e. chains) are required here
#



#### Measure mean chain lengths ####
Chains <- read.csv("Data S2 - Chain lengths/Data S2A - Predator.chainF.csv")

chain.stats <- function(x) {
  c(mean = mean(x), median = median(x), var = var(x), sd = sd(x), fq = summary(x)[2], tq = summary(x)[5])
} 



# Serengeti food web, choose appropriate version
# adult lion
Lion <- Chains %>% filter(Node %in% c('Panthera leo'), Version %in% c('Ser Trophsp alt'))

Lion <- rep(Lion$Chain.length, Lion$Frequency)
chain.stats(Lion)

# juvenile lion
Lion <- Chains %>% filter(Node %in% c('Panthera leo J'), Version %in% c('Ser Trophsp alt'))

Lion <- rep(Lion$Chain.length, Lion$Frequency)
chain.stats(Lion)

# young juvenile (weaned) lion
Lion <- Chains %>% filter(Node %in% c('Panthera leo Y'), Version %in% c('Ser Trophsp'))

Lion <- rep(Lion$Chain.length, Lion$Frequency)
chain.stats(Lion)





### Komodo food web, choose appropriate version
# young juvenile Komodo dragon
DragonY <- Chains %>% filter(Node %in% c('Varanus komodoensis Y'), Version %in% c('Kom Trophsp cann'))

DragonY <- rep(DragonY$Chain.length, DragonY$Frequency)
chain.stats(DragonY)

# juvenile Komodo dragon
DragonJ <- Chains %>% filter(Node %in% c('Varanus komodoensis J'), Version %in% c('Kom Trophsp cann')) 

DragonJ <- rep(DragonJ$Chain.length, DragonJ$Frequency)
chain.stats(DragonJ)

# subadult Komodo dragon
DragonS <- Chains %>% filter(Node %in% c('Varanus komodoensis S'), Version %in% c('Kom Trophsp'))

DragonS <- rep(DragonS$Chain.length, DragonS$Frequency)
chain.stats(DragonS)

# Adult Komodo dragon
DragonA <- Chains %>% filter(Node %in% c('Varanus komodoensis A'), Version %in% c('Kom Trophsp cann'))

DragonA <- rep(DragonA$Chain.length, DragonA$Frequency)
chain.stats(DragonA)






# DPP food web, choose appropriate version
# young juvenile Gorgosaurus
GorgosaurusY <- Chains %>% filter(Node %in% c('Gorgosaurus libratus Y'), Version %in% c('DPF Trophsp pack')) 

GorgosaurusY <- rep(GorgosaurusY$Chain.length, GorgosaurusY$Frequency)
chain.stats(GorgosaurusY)

# juvenile Gorgosaurus
GorgosaurusJ <- Chains %>% filter(Node %in% c('Gorgosaurus libratus J'), Version %in% c('DPF Trophsp pack-juv')) 

GorgosaurusJ <- rep(GorgosaurusJ$Chain.length, GorgosaurusJ$Frequency)
chain.stats(GorgosaurusJ)

# adult Gorgosaurus
Gorgosaurus <- Chains %>% filter(Node %in% c('Gorgosaurus libratus'), Version %in% c('DPF Trophsp pack-juv')) 

Gorgosaurus <- rep(Gorgosaurus$Chain.length, Gorgosaurus$Frequency)
chain.stats(Gorgosaurus)




#### Mean chain lengths for other animals ####

# Araneidae
Araneidae <- Chains %>% filter(Node %in% c('Araneidae'), Version %in% c('Ser Trophsp')) 

Araneidae <- rep(Araneidae$Chain.length, Araneidae$Frequency)
chain.stats(Araneidae)


# Odonata
Odonata <- Chains %>% filter(Node %in% c('Odonata Komodo'), Version %in% c('Trophsp')) 

Odonata <- rep(Odonata$Chain.length, Odonata$Frequency)
chain.stats(Odonata)


# Coleoptera
Coleoptera <- Chains %>% filter(Node %in% c('Coleoptera'), Version %in% c('Ser Trophsp')) 

Coleoptera <- rep(Coleoptera$Chain.length, Coleoptera$Frequency)
chain.stats(Coleoptera)


# Hymenoptera
Hymenoptera <- Chains %>% filter(Node %in% c('Hymenoptera'), Version %in% c('Kom Trophsp')) 

Hymenoptera <- rep(Hymenoptera$Chain.length, Hymenoptera$Frequency)
chain.stats(Hymenoptera)


# Anura
Anura <- Chains %>% filter(Node %in% c('Anura Komodo'), Version %in% c('Trophsp')) 

Anura <- rep(Anura$Chain.length, Anura$Frequency)
chain.stats(Anura)


# Gekkos
Gekko <- Chains %>% filter(Node %in% c('Lygodactylus capensis'), Version %in% c('Trophsp')) 

Gekko <- rep(Gekko$Chain.length, Gekko$Frequency)
chain.stats(Gekko)


# Anguid
Anguid <- Chains %>% filter(Node %in% c('Odaxosaurus priscus'), Version %in% c('DPF Trophsp')) 

Anguid <- rep(Anguid$Chain.length, Anguid$Frequency)
chain.stats(Anguid)


# Scincid
Scincid <- Chains %>% filter(Node %in% c('Orthrioscincus mixtus'), Version %in% c('DPF Trophsp')) 

Scincid <- rep(Scincid$Chain.length, Scincid$Frequency)
chain.stats(Scincid)


# Rats
Rats <- Chains %>% filter(Node %in% c('Rats'), Version %in% c('Kom Trophsp')) 

Rats <- rep(Rats$Chain.length, Rats$Frequency)
chain.stats(Rats)


# Shrew
Shrew <- Chains %>% filter(Node %in% c('Eutheria (other)'), Version %in% c('DPF Trophsp')) 

Shrew <- rep(Shrew$Chain.length, Shrew$Frequency)
chain.stats(Shrew)


# Stagodontidae
Stagodontidae <- Chains %>% filter(Node %in% c('Didelphodon sp.'), Version %in% c('DPF MAZ-1a')) 

Stagodontidae <- rep(Stagodontidae$Chain.length, Stagodontidae$Frequency)
chain.stats(Stagodontidae)


# Palm civet
Civet <- Chains %>% filter(Node %in% c('Paradoxurus musangus'), Version %in% c('Kom Trophsp')) 

Civet <- rep(Civet$Chain.length, Civet$Frequency)
chain.stats(Civet)


# Mongoose
Mongoose <- Chains %>% filter(Node %in% c('Helogale parvula'), Version %in% c('Ser Trophsp')) 

Mongoose <- rep(Mongoose$Chain.length, Mongoose$Frequency)
chain.stats(Mongoose)


# Palaeosaniwa, Nile monitor
Varanoidea <- Chains %>% filter(Node %in% c('Labrodioctes'), Version %in% c('DPF MAZ-1a')) 

Varanoidea <- rep(Varanoidea$Chain.length, Varanoidea$Frequency)
chain.stats(Varanoidea)
Palaeosaniwadf <- as.data.frame(Varanoidea)
write.csv(Palaeosaniwadf, "Chains/Palaeosaniwa.csv", row.names = F)






#### plot predator trophic level against ontogenetic stage ####
# PATL new code
TL <- read.csv("Data S2 - Chain lengths/Data S2B - Predator.chain.stats.csv") 

TL$Ont.stage <- factor(TL$Ont.stage, levels = c('Young juvenile','Juvenile','Subadult','Adult'))

PATL <- TL %>% filter(Trophic.res %in% c('Full'))

ggplot(PATL, aes(x = Ont.stage, y = PATL, col = Food.web.version, shape = Food.web.version)) + theme_test() + 
  geom_point(size = 2.5, stroke = 1) +
  scale_colour_manual(breaks = c('Gorgo MAZ-1a main','Gorgo MAZ-1a main-juv','Gorgo MAZ-1a pack','Gorgo MAZ-1b main','Gorgo MAZ-1b pack',
                                 'Gorgo MAZ-1b pack-juv',
                                 'P. leo main','P. leo alt','V. komodo main','V. komodo cannibal'), 
                      values = c('red4','red','red4','orange3','orange3','orangered',
                                 'black','black','#78c679','#78c679')) +
  scale_shape_manual(breaks = c('Gorgo MAZ-1a main','Gorgo MAZ-1a main-juv','Gorgo MAZ-1a pack','Gorgo MAZ-1b main','Gorgo MAZ-1b pack',
                                'Gorgo MAZ-1b pack-juv',
                                'P. leo main','P. leo alt','V. komodo main','V. komodo cannibal'), 
                     values = c(21,21,24,21,24,24,21,24,21,24)) +
  scale_y_continuous('Prey-averaged trophic level', limits = c(2.9,4.5), breaks = seq(3,4.5, by = 0.25)) +
  theme(axis.text = element_text(size = 12)) + theme(axis.title.y = element_text(size = 15)) +
  theme(axis.title.x = element_blank()) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = 'bottom') + theme(legend.title = element_blank()) +
  theme(legend.key.size = unit(.5, 'cm')) +
  guides(shape = guide_legend(nrow = 4), col = guide_legend(nrow = 4))

ggsave('', width = 6, height = 6)





# PATL for trophic species version
PATL2 <- TL %>% filter(Trophic.res %in% c('Trophic.sp'), Predator %in% c('Varanus komodoensis','Panthera leo','Gorgosaurus libratus'))

ggplot(PATL2, aes(x = Ont.stage, y = PATL, col = Food.web.version, shape = Food.web.version)) + theme_test() + 
  geom_point(size = 2.5, stroke = 1) +
  scale_colour_manual(breaks = c('Gorgo main','Gorgo main-juv','Gorgo pack','Gorgo pack-juv',
                                 'P. leo main','P. leo alt','V. komodo main','V. komodo cannibal'), 
                      values = c('darkred','orangered','darkred','orangered',
                                 'black','black','#78c679','#78c679')) +
  scale_shape_manual(breaks = c('Gorgo main','Gorgo main-juv','Gorgo pack','Gorgo pack-juv',
                                'P. leo main','P. leo alt','V. komodo main','V. komodo cannibal'), 
                     values = c(21,21,24,24,21,24,21,24)) +
  scale_y_continuous('Prey-averaged trophic level', limits = c(2.9,4.25), breaks = seq(3,4.5, by = 0.25)) +
  theme(axis.text = element_text(size = 15)) + theme(axis.title.y = element_text(size = 20)) +
  theme(axis.title.x = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = 'bottom') + theme(legend.title = element_blank()) +
  theme(legend.key.size = unit(.5, 'cm')) +
  guides(shape = guide_legend(nrow = 2), col = guide_legend(nrow = 2))

ggsave('', width = 7, height = 7)



# alternate version with variance shown in error bars
ggplot(PATL2, aes(x = Ont.stage, y = PATL, col = Food.web.version, shape = Food.web.version)) + theme_test() + 
  geom_point(size = 2.5, stroke = 1, position = position_dodge(width = 0.5)) +
  scale_colour_manual(breaks = c('Gorgo main','Gorgo main-juv','Gorgo pack','Gorgo pack-juv',
                                 'P. leo main','P. leo alt','V. komodo main','V. komodo cannibal'), 
                      values = c('darkred','orangered','darkred','orangered',
                                 'black','black','#78c679','#78c679')) +
  scale_shape_manual(breaks = c('Gorgo main','Gorgo main-juv','Gorgo pack','Gorgo pack-juv',
                                'P. leo main','P. leo alt','V. komodo main','V. komodo cannibal'), 
                     values = c(21,21,24,24,21,24,21,24)) +
  scale_y_continuous('Prey-averaged trophic level', limits = c(2.6,4.75), breaks = seq(2.5,5, by = 0.5)) +
  geom_errorbar(aes(ymin = PATL-(PATL-PATL.CI.L), ymax = PATL+(PATL.CI.H-PATL)), width = 0.25, position = position_dodge(width = 0.5)) +
  theme(axis.text = element_text(size = 15)) + theme(axis.title.y = element_text(size = 20)) +
  theme(axis.title.x = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = 'bottom') + theme(legend.title = element_blank()) +
  theme(legend.key.size = unit(.5, 'cm')) +
  guides(shape = guide_legend(nrow = 2), col = guide_legend(nrow = 2))

ggsave('', width = 6, height = 6)





# Chain length plot
Chainstats <- TL %>% filter(Trophic.res %in% c('Trophic.sp'), Predator %in% c('Varanus komodoensis','Panthera leo','Gorgosaurus libratus'))


Chainstats$Ont.stage <- factor(Chainstats$Ont.stage, levels = c('Young juvenile','Juvenile','Subadult','Adult'))


ggplot(Chainstats, aes(x = Ont.stage, y = Mean.CL, col = Food.web.version, shape = Food.web.version)) + theme_test() + 
  geom_point(size = 2.5, stroke = 1, position = position_dodge(width = 0.5)) +
  scale_colour_manual(breaks = c('Gorgo main','Gorgo main-juv','Gorgo pack','Gorgo pack-juv',
                                 'P. leo main','P. leo alt','V. komodo main','V. komodo cannibal'), 
                      values = c('darkred','orangered','darkred','orangered',
                                 'black','black','#78c679','#78c679')) +
  scale_shape_manual(breaks = c('Gorgo main','Gorgo main-juv','Gorgo pack','Gorgo pack-juv',
                                'P. leo main','P. leo alt','V. komodo main','V. komodo cannibal'), 
                     values = c(21,21,24,24,21,24,21,24)) +
  scale_y_continuous('Mean chain length', limits = c(1.5,11), breaks = seq(1,11, by = 1)) +
  geom_errorbar(aes(ymin = Mean.CL-SD.CL, ymax = Mean.CL+SD.CL), width = 0.25, position = position_dodge(width = 0.5)) +
  theme(axis.text = element_text(size = 15)) + theme(axis.title.y = element_text(size = 20)) +
  theme(axis.title.x = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = 'bottom') + theme(legend.title = element_blank()) +
  theme(legend.key.size = unit(.5, 'cm')) +
  guides(shape = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))

ggsave('', width = 6, height = 6)





# Trophic analogue mean chain lengths
Chainstats2 <- read.csv("Data S2 - Chain lengths/Data S2C - Other.chain.stats.csv") %>% filter(!Group %in% c('Frogs','Rats','Anguid'))

Chainstats2$Group <- factor(Chainstats2$Group, levels = c('Spiders','Dragonflies','Beetles','Wasps','Gekkos','Skinks','Varanoids','I mammals','C mammals'))

ggplot(Chainstats2, aes(x = Group, y = Mean.CL, col = Food.web)) + theme_bw() +
  geom_point(size = 2.5, stroke = 1, position = position_dodge(width = 0.5)) +
  scale_color_manual('Food web', breaks = c('DPF Tsp','DPF fulltax', 'Serengeti Tsp', 'Komodo Tsp'), 
                     values = c('orange3','red4','black','#78c679')) +
  scale_y_continuous('Mean chain length', breaks = seq(2,10,1)) +
  geom_errorbar(aes(ymin = Mean.CL-SD.CL, ymax = Mean.CL+SD.CL), width = 0.05,
                position = position_dodge(width = 0.5)) +
  theme(axis.text = element_text(size = 12)) + theme(axis.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.position = c(0.8,0.15)) + theme(legend.title = element_text(size = 10)) + 
  guides(col = guide_legend(nrow = 4)) + coord_flip()

ggsave("", width = 5, height = 6)



# Mean, confidence intervals of PATL
library(data.table)
Links <- read.csv("Data S2 - Chain lengths/Data S2D - PATL.csv")

# calculate 95% CI for each predator
Predator <- Links %>% filter(consumer %in% c(''))
t.test(Predator$PATL, conf.level = 0.95)$conf.int



### END OF SCRIPT ###