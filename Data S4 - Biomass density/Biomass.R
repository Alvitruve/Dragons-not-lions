## BIOMASS CORRECTIONS AND PREDATOR-PREY BIOMASS DISTRIBUTIONS ##

## A.V. Demers-Potvin* & H.C.E. Larsson ##
## *script author ##
## date created: 2024/07/29 ##
## last date modified: 2024/08/15 ##

setwd("")

# install.packages("tidyverse")
# install.packages("stringr)
# install.packages("ggpubr")
# install.packages("scales")
library(tidyverse)
library(stringr)
library(ggpubr)
library(scales)


#### Abundance data from Dinosaur Provincial Park ####
# Call Specimens, Localities and Abundance datasets, probably join to biomass clean R script
Localities <- read.csv("Data S4 - Biomass density/Data S4A - Localities master file.csv")
Specimens <- read.csv("Data S4 - Biomass density/Data S4B - Specimens master file.csv")




# Join relevant Localities columns to Specimens dataset, remove surface-collected specimens
Specimen_strat <- left_join(Specimens, Localities %>% select(Locality, Height_Oldman_DPF, Within_DPP_vicinity), by = "Locality")


# Only plot skeletons
Specimen_strat <- Specimen_strat %>% filter(!In_ex_situ %in% c('ex situ'), !Preservation %in% c('IE'), Loc_known %in% c('Y'), !OTU %in% c('Mosasauridae'))

Specimen_strat$OTU2 <- factor(Specimen_strat$OTU2, levels = c('Tyrannosaurids','Hadrosaurids','Ceratopsids','Ankylosaurs','Small theropods','Small ornithischians','Crocodilians',
                                                              'Turtles','Other reptiles','Fishes'))

ggplot(Specimen_strat, aes(x = OTU2, y = Height_Oldman_DPF)) + 
  geom_rect(xmin = 0, xmax = 11, ymin = 0, ymax = 30, fill = 'gray95') +
  geom_rect(xmin = 0, xmax = 11, ymin = 30, ymax = 60, fill = 'gray90') +
  geom_rect(xmin = 0, xmax = 11, ymin = 60, ymax = 90, fill = 'gray80') +
  geom_hline(yintercept = 0, col = 'black', linetype = 'dashed', size = .75) +
  geom_rect(xmin = 0, xmax = 11, ymin = 0, ymax = 10, col = 'firebrick', alpha = 0, linetype = 'dashed') +
  geom_jitter(width = 0.1, aes(fill = OTU2), shape = 21, alpha = 0.25) +
  theme_test() + scale_y_continuous(name = 'Height above OF-DPF contact', breaks = seq(-20,80,10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) + scale_x_discrete('Dinosaur Provincial Park skeletons') +
  theme(axis.text.y = element_text(size = 12)) + theme(axis.title = element_text(size  =15)) +
  theme(legend.position = 'none')
ggsave("DPPall.png", width = 10, height = 6)





#### Estimate absolute abundance ####
# obtain raw counts for each terrestrial and semiaquatic animal; nonadjusted counts, only include isolated elements for animals not preserved

Specimen_strat <- left_join(Specimens, Localities %>% select(Locality, Height_Oldman_DPF), by = 'Locality') %>%
  filter(!In_ex_situ %in% c('ex situ'))

# Include all fossils: as skeletons, microsite elements included only for species completely unrepresented by any skeleton or isolated element (make exception for mammals?)
Specimens.count <- Specimens %>% group_by(Family, OTU, Preservation) %>% count()



# Include only MAZ-1 specimens in DPF lowermost 5 m
Specimens.MAZ1a <- Specimen_strat %>% filter(between(Height_Oldman_DPF, 0, 5))
Specimens.MAZ1a.count <- Specimens.MAZ1a %>% group_by(Family, OTU, Preservation) %>% count()

# Include only MAZ-1 specimens in DPF lowermost 10 m (MAZ-1a, max palaeochannel depth)
Specimens.MAZ1a <- Specimen_strat %>% filter(between(Height_Oldman_DPF, 0, 10))
Specimens.MAZ1a.count <- Specimens.MAZ1a %>% group_by(Family, OTU, Preservation) %>% count()
Abundance.MAZ1a.count <- Abundance_strat %>% filter(Strat_unit %in% c('DPF1')) %>% group_by(OTU) %>% mutate(Sum.Ab = sum (Abundance)) %>% select(OTU, Sum.Ab) %>% unique()

# Repeat for MAZ-1b specimens
Specimens.MAZ1b <- Specimen_strat %>% filter(between(Height_Oldman_DPF, 10, 30))
Specimens.MAZ1b.count <- Specimens.MAZ1b %>% group_by(Family, OTU, Preservation) %>% count()

# Repeat for lower MAZ-2 specimens
Specimens.MAZ2 <- Specimen_strat %>% filter(between(Height_Oldman_DPF, 25, 50))
Specimens.MAZ2.count <- Specimens.MAZ2 %>% group_by(Family, OTU, Preservation) %>% count()





#### Body mass-density regressions ####
# make regression between body mass and density combining mammal vertebrate consumers and reptiles
# (Damuth 1987, corrected for metabolic rate)



# Carnivore/omnivore mass-density dataset (including omnivores)
MD <- read.csv("Data S4 - Biomass density/Data S4D - DamuthMD.csv") %>% 
  mutate(Log.M = log10(Mass), Log.D = log10(Density), Log.D2 = log10(Density.corr))

Carnivores <- MD %>% filter(Taxon %in% c('C_Mammal'))
Theropods <- MD %>% filter(Taxon %in% c('C_Dinosaur'))

ggplot(Carnivores, aes(x = Log.M, y = Log.D, shape = Taxon)) + theme_test() + geom_point(size = 3) +
  geom_point(data = Theropods, size = 3) + 
  scale_shape_manual(values = c(19,21), labels = c('Dinosaur','Mammal')) +
  scale_x_continuous('Log predator mass', limits = c(1.75,6.5), breaks = c(1.5:6.5)) +
  scale_y_continuous('Log predator density', limits = c(-2.5,2.5), breaks = c(-3:2)) +
  geom_smooth(method = lm, se=TRUE, aes(group = 1), col = 'black', size = 0.75) +
  theme(axis.text = element_text(size = 15)) + theme(legend.title = element_blank()) + 
  theme(legend.key.size = unit(7.5, 'mm')) + theme(legend.position = c(.15,.1)) + theme(legend.text = element_text(size = 15))
ggsave("Carnivoran M-D.png", width = 6, height = 5)
# this plot shows how observed tyrannosaur density in DPP MAZ1a is much higher than expected compared with carnivoran mammal mass-density regression
# while small theropod densities are slightly lower than expected



# regression stats for carnivores
cor.test(Carnivores$Log.D, Carnivores$Log.M)
M.D.regression1 <- lm(Log.D ~ Log.M, data = Carnivores)
summary(M.D.regression1)

# predict Gorgosaurus density
predict(M.D.regression1, data.frame(Log.M = log10(2513000)), se.fit = TRUE, interval = 'confidence')
10^(-2.414455-0.291289)
10^(-2.414455+0.291289)
# predicted Gorgosaurus densities are almost 10 times lower than observed ones



# Try dataset with better fit for tyrannosaurs: version with mammal densities multiplied by 10 and reptile densities divided by 20
Predators2 <- MD %>% filter(Taxon %in% c('C_Mammal','Reptile'))
cor.test(Predators2$Log.D2, Predators2$Log.M)
M.D.regression2 <- lm(Log.D2 ~ Log.M, data = Predators2)
summary(M.D.regression2)
confint(M.D.regression2)

ggplot(Predators2, aes(x = Log.M, y = Log.D2, shape = Taxon)) + theme_test() + geom_point(size = 3) +
  geom_point(data = Theropods, size = 3) + 
  scale_shape_manual(values = c(19,21,23), labels = c('Dinosaur','Mammal','Reptile')) +
  scale_x_continuous('Log predator mass', limits = c(-0.1,6.5), breaks = c(0:6)) +
  scale_y_continuous('Log predator density', limits = c(-2.5,4.5), breaks = c(-2:4)) +
  geom_smooth(method = lm, se=TRUE, aes(group=1), col = 'black', size = 0.75) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title = element_blank()) + theme(legend.key.size = unit(7.5, 'mm')) +
  theme(legend.position = c(.15,.1)) + theme(legend.text = element_text(size = 15))

# predict Gorgosaurus density
predict(M.D.regression2, data.frame(Log.M = log10(2513000)), se.fit = TRUE, interval = 'confidence')
10^(-0.9312962)
10^(-0.9312962-0.2123318)
10^(-0.9312962+0.2123318)
# lower bound of standard error is closest to observed Gorgosaurus densities so use that bound for other theropod density estimates

predict(M.D.regression2, data.frame(Log.M = log10(96000)), se.fit = TRUE, interval = 'confidence')# Caenagnathus density

predict(M.D.regression2, data.frame(Log.M = log10(63872)), se.fit = TRUE, interval = 'confidence')# Chirostenotes density

predict(M.D.regression2, data.frame(Log.M = log10(19800)), se.fit = TRUE, interval = 'confidence')# Citipes density

predict(M.D.regression2, data.frame(Log.M = log10(57447)), se.fit = TRUE, interval = 'confidence')# Stenonychosaurus density

predict(M.D.regression2, data.frame(Log.M = log10(21789)), se.fit = TRUE, interval = 'confidence')# Saurornitholestes density

predict(M.D.regression2, data.frame(Log.M = log10(15060)), se.fit = TRUE, interval = 'confidence')# Dromaeosaurus density

predict(M.D.regression2, data.frame(Log.M = log10(2330)), se.fit = TRUE, interval = 'confidence')# Hesperonychus density



# Now produce same plot showing corrected small theropod densities
ggplot(Predators2, aes(x = Log.M, y = Log.D2, shape = Taxon)) + theme_test() + geom_point(size = 3) +
  geom_point(data = Theropods, size = 3) + 
  scale_shape_manual(values = c(19,21,23), labels = c('Dinosaur','Mammal','Reptile')) +
  scale_x_continuous('Log predator mass', limits = c(-0.1,6.5), breaks = c(0:6)) +
  scale_y_continuous('Log predator density', limits = c(-2.5,4.5), breaks = c(-2:4)) +
  geom_smooth(method = lm, se=TRUE, aes(group = 1), col = 'black', size = 0.5) +
  theme(axis.text = element_text(size = 15)) + theme(legend.title = element_text(size = 12)) +
  theme(legend.title = element_blank()) + theme(legend.key.size = unit(7.5, 'mm')) +
  theme(legend.position = c(.15,.125)) + theme(legend.text = element_text(size = 15)) +
  geom_segment(x=4.982271, y=-1.836143, xend=4.982271, yend=-0.2771673, linetype=3, size=.1) + #Caenagnathus
  geom_segment(x=-0.42, y=-0.2771673, xend=4.982271, yend=-0.2771673, linetype=3, size=.1) +
  geom_segment(x=4.805311, y=-1.836143, xend=4.805311, yend=-0.1443689, linetype=3, size=.1) + #Chirostenotes
  geom_segment(x=-0.42, y=-0.1443689, xend=4.805311, yend=-0.1443689, linetype=3, size=.1) +
  geom_segment(x=4.759267, y=-1.301030, xend=4.759267, yend=-0.1099087, linetype=3, size=.1) + #Stenonychosaurus
  geom_segment(x=-0.42, y=-0.1099087, xend=4.759267, yend=-0.1099087, linetype=3, size=.1) +
  geom_segment(x=4.296665, y=-1.535113, xend=4.296665, yend=0.2336565, linetype=3, size=.1) + #Citipes
  geom_segment(x=-0.42, y=0.2336565, xend=4.296665, yend=0.2336565, linetype=3, size=.1) +
  geom_segment(x=4.177825, y=-1.42596873227228, xend=4.177825, yend=0.3209559, linetype=3, size=.1) + #Dromaeosaurus
  geom_segment(x=-0.42, y=0.3209559, xend=4.177825, yend=0.3209559, linetype=3, size=.1) +
  geom_segment(x=4.338237, y=-1.42596873227228, xend=4.338237, yend=0.2030122, linetype=3, size=.1) + #Saurornitholestes
  geom_segment(x=-0.42, y=0.2030122, xend=4.338237, yend=0.2030122, linetype=3, size=.1)
ggsave("Predator M-D.png", width = 6, height = 5.5)




# Herbivore mass-density dataset
MHerb <- MD %>% filter(Taxon %in% c('H_Mammal'))
DHerb <- MD %>% filter(Taxon %in% c('H_Dinosaur'))
cor.test(MHerb$Log.D, MHerb$Log.M)
M.D.regression3 <- lm(Log.D ~ Log.M, data = MHerb)
summary(M.D.regression3)
confint(M.D.regression3)

predict(M.D.regression3, data.frame(Log.M = log10(13000)), se.fit = TRUE, interval = 'confidence')# Orodrominae and Foraminacephale densities

predict(M.D.regression3, data.frame(Log.M = log10(25135)), se.fit = TRUE, interval = 'confidence')# Stegoceras

predict(M.D.regression3, data.frame(Log.M = log10(130000)), se.fit = TRUE, interval = 'confidence')# Unescoceratops

predict(M.D.regression3, data.frame(Log.M = log10(178508)), se.fit = TRUE, interval = 'confidence')# Ornithomimus and Struthiomimus

ggplot(MHerb, aes(x = Log.M, y = Log.D, shape = Taxon)) + theme_test() + geom_point(size = 3) +
  geom_point(data = DHerb, size = 3) + 
  scale_shape_manual(values = c(19,21), labels = c('Dinosaur','Mammal')) +
  scale_x_continuous('Log herbivore mass') +
  scale_y_continuous('Log herbivore density') +
  geom_smooth(method = lm, se=TRUE, aes(group=1), col = 'black', size = 0.75) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title = element_blank()) + theme(legend.key.size = unit(7.5, 'mm')) +
  theme(legend.position = c(.15,.1)) + theme(legend.text = element_text(size = 15)) +
  geom_segment(x=4.113943, y=-1.65321251377534, xend=4.113943, yend=0.9295777, linetype=3, size=.1) + # Orodrominae and Foraminacephale
  geom_segment(x=1.9, y=0.9295777, xend=4.113943, yend=0.9295777, linetype=3, size=.1) +
  geom_segment(x=4.400279, y=-1.35218251811136, xend=4.400279, yend=0.7439881, linetype=3, size=.1) + # Stegoceras
  geom_segment(x=1.9, y=0.7439881, xend=4.400279, yend=0.7439881, linetype=3, size=.1) +
  geom_segment(x=5.113943, y=-2.0791813, xend=5.113943, yend=0.2814234, linetype=3, size=.1) + # Unescoceratops
  geom_segment(x=1.9, y=0.2814234, xend=5.113943, yend=0.2814234, linetype=3, size=.1) +
  geom_segment(x=5.251658, y=-1.0982045, xend=5.251658, yend=0.1921632, linetype=3, size=.1) + # Ornithomimids
  geom_segment(x=1.9, y=0.1921632, xend=5.251658, yend=0.1921632, linetype=3, size=.1)
ggsave("Herbivore M-D.png", width = 6, height = 5.5)







#### Gorgosaurus survivorship curve ####
TyrOnt <- read.csv("Data S4 - Biomass density/Data S4E - Tyrannosaur ontogeny.csv") %>% filter(Taxon %in% c('Gorgosaurus')) %>%
  mutate(B.proportion.perc = B.proportion*100)
sum(TyrOnt$B.proportion.perc)

# Biomass proportions at every age
B <- ggplot(TyrOnt, aes(x = Age, y = B.proportion.perc)) + geom_bar(stat = 'identity', col = 'black', fill = 'grey95', width = 0.75) + theme_test() +
  scale_y_continuous('Biomass proportion (%)', lim = c(0,15)) + scale_x_continuous(breaks = seq(0,26,2)) +
  geom_vline(xintercept = 4.5, linetype = 'dashed', size = 1) + geom_vline(xintercept = 11.5, linetype = 'dashed', size = 1) +
  geom_vline(xintercept = 5.5, linetype = 'dotted', size = 1, col = 'grey50') + geom_vline(xintercept = 16.5, linetype = 'dotted', size = 1, col = 'grey50') +
  theme(axis.text = element_text(size = 15)) + theme(axis.title = element_text(size = 15)) + theme(axis.title = element_text(size = 15))
# Body mass at every age
options(scipen=999)
M <- ggplot(TyrOnt, aes(x = Age, y = M.Erickson2)) + geom_bar(stat = 'identity', col = 'black', fill = 'grey95', width = 0.75) + theme_test() +
  scale_y_log10('Body mass (kg)', lim = c(1,10000)) + scale_x_continuous(breaks = seq(0,26,2)) +
  geom_vline(xintercept = 4.5, linetype = 'dashed', size = 1) + geom_vline(xintercept = 11.5, linetype = 'dashed', size = 1) +
  geom_vline(xintercept = 5.5, linetype = 'dotted', size = 1, col = 'grey50') + geom_vline(xintercept = 16.5, linetype = 'dotted', size = 1, col = 'grey50') +
  theme(axis.text = element_text(size = 15)) + theme(axis.title = element_text(size = 15)) + theme(axis.title = element_text(size = 15))
M
ggarrange(B, M, ncol = 1, nrow = 2)
ggsave("Tyrannosaur ontogeny.png", width = 10, height = 7)






#### Biomass plots ####
Biomass <- read.csv("Data S4 - Biomass density/Data S4F - Predator-prey B.csv")

# version with DPP MAZ-1a, African and Indian predators
Biomass1 <- Biomass %>% filter(Group %in% c('All','Komodo','Wolf'), !Biostrat %in% c('MAZ-2','MAZ-1b'))
options(scipen=999)
ggplot(Biomass1, aes(x = Prey.Biomass.kg.km2, y = Predator.Biomass.kg.km2, shape = Fauna, fill = Fauna)) + theme_test() +
  scale_x_log10(limits = c(50,14000)) + scale_y_log10(labels = label_number(accuracy = 1)) + 
  labs(x = bquote('Total prey biomass'~(kg/km^2)), y = bquote('Total predator biomass'~(kg/km^2))) + 
  annotation_logticks(sides = 'trbl') + geom_point(size = 5) +
  
  scale_shape_manual('Fauna', breaks = c('DPP time averaged','DPP time averaged megafauna','DPF MAZ-1a','DPF MAZ-1a with juveniles','DPF MAZ-1a with Caen+Troo as prey','DPF MAZ-1a with Caen+Troo as predators',
                                'Africa predators','India predators','Gray wolf (North America)',
                                'Komodo (Jessop et al., 2020)','Komodo (Auffenberg, 1981)'), 
                     values = c(21,21,21,21,21,21,1,2,5,0,22),
                     labels = c('DPP time averaged','DPP time averaged\nmegafauna','DPF MAZ-1a','DPF MAZ-1a\nwith juveniles','DPF MAZ-1a with\nCaen+Troo as prey','DPF MAZ-1a with\nCaen+Troo as predators',
                              'Africa predators','India predators','Gray wolf\n(North America)',
                              'Komodo\n(Jessop et al., 2020)','Komodo\n(Auffenberg, 1981)')) +
  
  scale_fill_manual('Fauna', breaks = c('DPP time averaged','DPP time averaged megafauna','DPF MAZ-1a','DPF MAZ-1a with juveniles','DPF MAZ-1a with Caen+Troo as prey','DPF MAZ-1a with Caen+Troo as predators',
                               'Africa predators','India predators','Gray wolf (North America)',
                               'Komodo (Jessop et al., 2020)','Komodo (Auffenberg, 1981)'),
                    values = c('black','gray','#0868ac','#43a2ca','#7bccc4','#bae4bc',
                               'black','black','black','black','gray'), 
                    labels = c('DPP time averaged','DPP time averaged\nmegafauna','DPF MAZ-1a','DPF MAZ-1a\nwith juveniles','DPF MAZ-1a with\nCaen+Troo as prey','DPF MAZ-1a with\nCaen+Troo as predators',
                             'Africa predators','India predators','Gray wolf\n(North America)',
                             'Komodo\n(Jessop et al., 2020)','Komodo\n(Auffenberg, 1981)')) +
  
  theme(axis.text = element_text(size = 15)) + theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 15)) + theme(legend.title = element_text(size = 20)) +
  theme(legend.position = 'right') + theme(legend.key.size = unit(14, 'mm')) +
  guides(shape = guide_legend(nrow = 11), fill = guide_legend(nrow = 11))

ggsave("Biomass_MAZ-1a.png", width = 11, height = 7.5)



# version with DPF MAZ-1b, MAZ-2
Biomass2 <- Biomass %>% filter(Group %in% c('All','Komodo','Wolf'), !Biostrat %in% c('MAZ-1a'))
options(scipen=999)
ggplot(Biomass2, aes(x = Prey.Biomass.kg.km2, y = Predator.Biomass.kg.km2, shape = Fauna, fill = Fauna)) + theme_test() +
  scale_x_log10(limits = c(50,14000)) + scale_y_log10(labels = label_number(accuracy = 1)) + 
  labs(x = bquote('Total prey biomass'~(kg/km^2)), y = bquote('Total predator biomass'~(kg/km^2))) + 
  annotation_logticks(sides = 'trbl') + geom_point(size = 5) +
  
  scale_shape_manual('Fauna', breaks = c('DPP time averaged','DPP time averaged megafauna',
                                         'DPF MAZ-1b','DPF MAZ-1b with juveniles','DPF MAZ-1b with Caen+Troo as prey','DPF MAZ-1b with Caen+Troo as predators',
                                         'DPF MAZ-2','DPF MAZ-2 with juveniles','DPF MAZ-2 with Caen+Troo as prey','DPF MAZ-2 with Caen+Troo as predators',
                                         'Africa predators','India predators','Gray wolf (North America)',
                                         'Komodo (Jessop et al., 2020)','Komodo (Auffenberg, 1981)'), 
                     values = c(21,21,21,21,21,21,21,21,21,21,1,2,5,0,22),
                     labels = c('DPP time averaged','DPP time averaged\nmegafauna',
                                'DPF MAZ-1b','DPF MAZ-1b\nwith juveniles','DPF MAZ-1b with\nCaen+Troo as prey','DPF MAZ-1b with\nCaen+Troo as predators',
                                'DPF MAZ-2','DPF MAZ-2\nwith juveniles','DPF MAZ-2 with\nCaen+Troo as prey','DPF MAZ-2 with\nCaen+Troo as predators',
                                'Africa predators','India predators','Gray wolf\n(North America)',
                                'Komodo\n(Jessop et al., 2020)','Komodo\n(Auffenberg, 1981)')) +
  
  scale_fill_manual('Fauna', breaks = c('DPP time averaged','DPP time averaged megafauna',
                                        'DPF MAZ-1b','DPF MAZ-1b with juveniles','DPF MAZ-1b with Caen+Troo as prey','DPF MAZ-1b with Caen+Troo as predators',
                                        'DPF MAZ-2','DPF MAZ-2 with juveniles','DPF MAZ-2 with Caen+Troo as prey','DPF MAZ-2 with Caen+Troo as predators',
                                        'Africa predators','India predators','Gray wolf (North America)',
                                        'Komodo (Jessop et al., 2020)','Komodo (Auffenberg, 1981)'),
                    values = c('black','gray','#54278f','#756bb1','#9e9ac8','#cbc9e2',
                               '#980043','#dd1c77','#df65b0','#d7b5d8','black','black','black','black','gray'), 
                    labels = c('DPP time averaged','DPP time averaged\nmegafauna',
                               'DPF MAZ-1b','DPF MAZ-1b\nwith juveniles','DPF MAZ-1b with\nCaen+Troo as prey','DPF MAZ-1b with\nCaen+Troo as predators',
                               'DPF MAZ-2','DPF MAZ-2\nwith juveniles','DPF MAZ-2 with\nCaen+Troo as prey','DPF MAZ-2 with\nCaen+Troo as predators',
                               'Africa predators','India predators','Gray wolf\n(North America)',
                               'Komodo\n(Jessop et al., 2020)','Komodo\n(Auffenberg, 1981)')) +
  
  theme(axis.text = element_text(size = 15)) + theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 12)) + theme(legend.title = element_text(size = 15)) +
  theme(legend.position = 'right') + theme(legend.key.size = unit(12, 'mm')) +
  guides(shape = guide_legend(nrow = 15), fill = guide_legend(nrow = 15))

ggsave("Biomass_MAZ-1b.png", width = 11, height = 8.25)



# version with Gorgosaurus, lion and tiger only
Biomass3 <- Biomass %>% filter(Group %in% c('Select','Komodo','Wolf','Lion only','Tiger only'))
options(scipen=999)

ggplot(Biomass3, aes(x = Prey.Biomass.kg.km2, y = Predator.Biomass.kg.km2, shape = Fauna, fill = Fauna)) + theme_test() +
  scale_x_log10(limits = c(50,14000)) + scale_y_log10(labels = label_number(accuracy = 1)) + 
  labs(x = bquote('Total prey biomass'~(kg/km^2)), y = bquote('Total predator biomass'~(kg/km^2))) + 
  annotation_logticks(sides = 'trbl') + geom_point(size = 5) +
  
  scale_shape_manual('Fauna', breaks = c('DPP time averaged','DPP time averaged megafauna','DPF MAZ-1a','DPF MAZ-1a with juveniles','DPF MAZ-1a with Caen+Troo as prey','DPF MAZ-1a with Caen+Troo as predators',
                                         'DPF MAZ-1b','DPF MAZ-1b with juveniles','DPF MAZ-1b with Caen+Troo as prey','DPF MAZ-1b with Caen+Troo as predators',
                                         'DPF MAZ-2','DPF MAZ-2 with juveniles','DPF MAZ-2 with Caen+Troo as prey','DPF MAZ-2 with Caen+Troo as predators',
                                         'Lion only','Tiger only','Gray wolf (North America)',
                                         'Komodo (Jessop et al., 2020)','Komodo (Auffenberg, 1981)'), 
                     values = c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,1,2,5,0,22),
                     labels = c('DPP time averaged','DPP time averaged\nmegafauna','DPF MAZ-1a','DPF MAZ-1a\nwith juveniles','DPF MAZ-1a with\nCaen+Troo as prey','DPF MAZ-1a with\nCaen+Troo as predators',
                                'DPF MAZ-1b','DPF MAZ-1b\nwith juveniles','DPF MAZ-1b with\nCaen+Troo as prey','DPF MAZ-1b with\nCaen+Troo as predators',
                                'DPF MAZ-2','DPF MAZ-2\nwith juveniles','DPF MAZ-2 with\nCaen+Troo as prey','DPF MAZ-2 with\nCaen+Troo as predators',
                                'Lion only','Tiger only','Gray wolf\n(North America)',
                                'Komodo\n(Jessop et al., 2020)','Komodo\n(Auffenberg, 1981)')) +
  
  scale_fill_manual('Fauna', breaks = c('DPP time averaged','DPP time averaged megafauna','DPF MAZ-1a','DPF MAZ-1a with juveniles','DPF MAZ-1a with Caen+Troo as prey','DPF MAZ-1a with Caen+Troo as predators',
                                        'DPF MAZ-1b','DPF MAZ-1b with juveniles','DPF MAZ-1b with Caen+Troo as prey','DPF MAZ-1b with Caen+Troo as predators',
                                        'DPF MAZ-2','DPF MAZ-2 with juveniles','DPF MAZ-2 with Caen+Troo as prey','DPF MAZ-2 with Caen+Troo as predators',
                                        'Lion only','Tiger only','Gray wolf (North America)',
                                        'Komodo (Jessop et al., 2020)','Komodo (Auffenberg, 1981)'),
                    values = c('black','gray','#0868ac','#43a2ca','#7bccc4','#bae4bc','#54278f','#756bb1','#9e9ac8','#cbc9e2',
                               '#980043','#dd1c77','#df65b0','#d7b5d8','black','black','black','black','gray'), 
                    labels = c('DPP time averaged','DPP time averaged\nmegafauna','DPF MAZ-1a','DPF MAZ-1a\nwith juveniles','DPF MAZ-1a with\nCaen+Troo as prey','DPF MAZ-1a with\nCaen+Troo as predators',
                               'DPF MAZ-1b','DPF MAZ-1b\nwith juveniles','DPF MAZ-1b with\nCaen+Troo as prey','DPF MAZ-1b with\nCaen+Troo as predators',
                               'DPF MAZ-2','DPF MAZ-2\nwith juveniles','DPF MAZ-2 with\nCaen+Troo as prey','DPF MAZ-2 with\nCaen+Troo as predators',
                               'Lion only','Tiger only','Gray wolf\n(North America)',
                               'Komodo\n(Jessop et al., 2020)','Komodo\n(Auffenberg, 1981)')) +
  
  theme(axis.text = element_text(size = 15)) + theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 12)) + theme(legend.title = element_blank()) +
  theme(legend.position = 'bottom') + theme(legend.key.size = unit(10, 'mm')) +
  guides(shape = guide_legend(nrow = 5), fill = guide_legend(nrow = 5))

ggsave("Biomass_Gorgo_NEW.png", width = 10, height = 11)




# Individual apex predator biomasses: DPF
PB <- read.csv("Data S4 - Biomass density/Data S4G - Predator biomass.csv")

DPF <- PB %>% filter(Ecosystem %in% c("DPF MAZ1a"))
DPF$Predator <- factor(DPF$Predator, levels = c('G. libratus','Daspleto','Caena','Stenony','Saurorni','Dromaeo','Other'))
DPF$Group <- factor(DPF$Group, levels = c('Uncorrected','Corrected'))

DPFplot <- ggplot(DPF, aes(x = Predator, y = B, fill = Group)) + geom_bar(stat = 'identity', position = 'dodge', col = 'black') + theme_test() +
  scale_fill_manual(breaks = c('Uncorrected','Corrected'), values = c('black','grey'), name = '') +
  labs(y = bquote('Biomass density'~(kg/km^2))) + scale_y_continuous(lim = c(0,220)) +
  scale_x_discrete('DPF predator') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15)) + 
  theme(axis.text.y = element_text(size = 15)) + theme(legend.position = 'top') + theme(legend.key.size = unit(5, 'mm')) +
  theme(legend.text = element_text(size = 15)) + theme(axis.title = element_text(size = 20))
DPFplot



# Individual apex predator biomasses: Africa 
Africa <- PB %>% filter(Ecosystem %in% c("Africa")) %>% group_by(Name,Predator) %>% select(!Group) %>% 
  mutate(MeanB = mean(B), SD = sd(B), Variance = var(B)) %>% select(!B) %>% select(!Proportion) %>% unique()
Africa$Predator <- factor(Africa$Predator, levels = c('P. leo','C. crocuta','P. pardus','H. hyaena','A. jubatus','L. pictus'))

Africaplot <- ggplot(Africa, aes(x = Predator, y = MeanB)) + geom_bar(stat = 'identity', position = 'dodge', col = 'black', fill = 'gray', width = .5) + theme_test() +
  labs(y = bquote('Biomass density'~(kg/km^2))) + scale_y_continuous(lim = c(0,30), breaks = seq(0,30,by =5)) +
  scale_x_discrete('Africa predator') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15)) + 
  theme(axis.text.y = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) + theme(axis.title = element_text(size = 20)) +
  geom_errorbar(aes(ymax = MeanB+SD, ymin = MeanB), width = .5)
Africaplot


# combine DPF and Africa plots
ggarrange(DPFplot, Africaplot, ncol = 2, nrow = 1)
ggsave("DPF+Africa predators.png", width = 8, height = 6)



### END OF SCRIPT ###