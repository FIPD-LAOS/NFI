((pi*(100*((dead_trees$dist*(tan(atan((dead_trees$slope.b+(dead_trees$slope.t*-1))/100)))
))))/12)


tan(dead_trees$slope.b*pi/180) *10 + tan(dead_trees$slope.t*pi/180) *10


# V = (pi * Height/ 12) * Dbase^2 + (Dbase + Dtop) + Dtop^1
#(pi * Height/ 12)
(pi*tan(dead_trees$slope.b*pi/180) *10 + tan(dead_trees$slope.t*pi/180) *dead_trees$dist)/12

(tan(dead_trees$slope.b*pi/180) *10 + tan(dead_trees$slope.t*pi/180) *dead_trees$dist)
# Dbase^2 + (Dbase + Dtop) + Dtop^2
## Dbase
dead_trees$DB.t

## Dtop = Dbase - (Height*(Dbase - DBH/130*100))
## DBH
dead_trees$DBH.t


### -> (Dbase^2 + (Dbase + Dbase - (Height*(Dbase - DBH/130*100))) + Dbase - (Height*(Dbase - DBH/130*100))^2)
(pi*tan(dead_trees$slope.b*pi/180) *10 + tan(dead_trees$slope.t*pi/180) *dead_trees$dist)/12 *
(dead_trees$DB.t^2 + 
    (dead_trees$DB.t + 
       (dead_trees$DB.t - ((tan(dead_trees$slope.b*pi/180) *10 + tan(dead_trees$slope.t*pi/180) *dead_trees$dist)*(dead_trees$DB.t - dead_trees$DBH.t/130*100)))
     ) + 
    (dead_trees$DB.t - ((tan(dead_trees$slope.b*pi/180) *10 + tan(dead_trees$slope.t*pi/180) *dead_trees$dist)*(dead_trees$DB.t - dead_trees$DBH.t/130*100)))^2
  ) *0.6*0.001

#######################
# NFI II
# Download the data from Ona server ####
NFI <- onaDownload('NFI_20162017_Inventory_v1',
                   account = 'fipdlaos',
                   uname = 'fipdlaos',
                   pass = 'fipdlaos1')
NFI <- data.frame(NFI)
NFI$parent_index <- c(1:length(NFI$end))

# Create index variable ####
index.new <- as.data.frame(cbind(as.character(NFI$plot_info.plot_code_nmbr),
                                 as.character(NFI$plot_info.sub_plot),
                                 as.character(NFI$lc_class.lc_class),
                                 as.character(NFI$lc_data.lc_type),
                                 NFI$parent_index))
names(index.new) <- c("plot", "subplot", "type","otype", "parent_index")
index.new$type[is.na(index.new$type)] <- "OTH"
prov.code <- read.csv("data/province_code.csv")
prov.code <- unique(prov.code)

index.new$province <- ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Khammouan")]), "Khammouan",
                             ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Savannakhet")]), "Savannakhet",      
                                    ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Salavan")]), "Salavan",  
                                           ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Xekong")]), "Xekong",   
                                                  ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Champasak")]), "Champasak",
                                                         ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Attapeu")]), "Attapeu",  
                                                                ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Xaignabouly")]), "Xaignabouly",        
                                                                       ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Xiengkhouang")]), "Xiengkhouang",       
                                                                              ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Vientiane")]), "Vientiane",
                                                                                     ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Xaisomboun")]), "Xaisomboun",         
                                                                                            ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Bolikhamxai")]), "Bolikhamxai",        
                                                                                                   ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Louangnamtha")]), "Louangnamtha",       
                                                                                                          ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Oudomxai")]), "Oudomxai", 
                                                                                                                 ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Bokeo")]), "Bokeo",    
                                                                                                                        ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Houaphan")]), "Houaphan", 
                                                                                                                               ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Louangphabang")]), "Louangphabang",      
                                                                                                                                      ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Phongsaly")]), "Phongsaly",
                                                                                                                                             "Vientiane Capital")))))))))))))))))

# Apply rules for inclusion/ exclusion ####
index.NFI.II <- index.new
index.newII <- index.new
index.NFI.II <- index.newII

index.NFI.II <- index.NFI.II[c(1:20),]
# remove plots with n(otype) < 3 
index.NFI.II <- index.NFI.II %>% group_by(plot) %>% do(mutate(.,otype.new = ifelse(length(plot[otype %in% c("f_nat")]) < 2,
                                                                                   NA,
                                                                                   as.character(otype))))
###
index.NFI.II <- index.NFI.II %>% group_by(plot) %>% do(mutate(.,newtype.2 = ifelse(!length(plot) == 1,
                                                                                   c(as.character(type[c(2:length(type))]),as.character(type[2])),NA)))
# for first +2 type
index.NFI.II <- index.NFI.II %>% group_by(plot) %>% do(mutate(.,newtype.3 = ifelse(length(plot) == 3, 
                                                                                   c(as.character(type[c(3:length(type))]),NA,NA),
                                                                                   (as.character(type[c(3)])))))
# for first +3 type
index.NFI.II <- index.NFI.II %>% group_by(plot) %>% do(mutate(.,newtype.4 = ifelse(length(plot) == 4, 
                                                                                   c(as.character(type[c(4:length(type))]),NA,NA,NA),
                                                                                   NA)))
index.NFI.II <- group_by(index.NFI.II, plot) %>% 
  do(mutate(.,newtype = ifelse(length(plot) == 2, 
                               ifelse(type == newtype.2, as.character(type), 
                                      NA),
                               ifelse(length(plot) == 3, 
                                      ifelse(type == newtype.2 | type == newtype.3,as.character(type),
                                             ifelse(newtype.2 == newtype.3,as.character(newtype.2),
                                                    NA)),
                                      ifelse(length(plot) == 4,
                                             ifelse(type == newtype.2 && newtype.3 == newtype.4 && type != newtype.3 |
                                                      type == newtype.3 && newtype.2 == newtype.4 && type != newtype.2 |
                                                      type == newtype.4 && newtype.2 == newtype.3 && type != newtype.2, NA,
                                                    ifelse(type == newtype.2 | type == newtype.3 | type == newtype.4, as.character(type), 
                                                           ifelse(newtype.2 == newtype.3 | newtype.2 == newtype.4, as.character(newtype.2),
                                                                  ifelse(newtype.3 == newtype.4,as.character(newtype.3),
                                                                         NA)))),NA)
                               ))))

#####################################
# NFI II
NFI <- onaDownload('NFI3',
                   account = 'fipdlaos',
                   uname = 'fipdlaos',
                   pass = 'fipdlaos1')
NFI <- data.frame(NFI)
NFI$parent_index <- c(1:length(NFI$end))

# Create index variable ####
index.new <- as.data.frame(cbind(as.character(NFI$plot_info.plot_code_nmbr),
                                 as.character(NFI$plot_info.sub_plot),
                                 as.character(NFI$lc_class.lc_class),
                                 as.character(NFI$lc_data.lc_type),
                                 NFI$parent_index))
names(index.new) <- c("plot", "subplot", "type","otype", "parent_index")
index.new$type[is.na(index.new$type)] <- "OTH"
setwd("~/Documents/Dropbox/FC Projects - Active/0134-KKC NFI III/Data/Dashboard")
prov.code <- read.csv("data/province_code.csv")
prov.code <- unique(prov.code)

index.new$province <- ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Khammouan")]), "Khammouan",
                             ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Savannakhet")]), "Savannakhet",      
                                    ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Salavan")]), "Salavan",  
                                           ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Xekong")]), "Xekong",   
                                                  ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Champasak")]), "Champasak",
                                                         ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Attapeu")]), "Attapeu",  
                                                                ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Xaignabouly")]), "Xaignabouly",        
                                                                       ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Xiengkhouang")]), "Xiengkhouang",       
                                                                              ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Vientiane")]), "Vientiane",
                                                                                     ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Xaisomboun")]), "Xaisomboun",         
                                                                                            ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Bolikhamxai")]), "Bolikhamxai",        
                                                                                                   ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Louangnamtha")]), "Louangnamtha",       
                                                                                                          ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Oudomxai")]), "Oudomxai", 
                                                                                                                 ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Bokeo")]), "Bokeo",    
                                                                                                                        ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Houaphan")]), "Houaphan", 
                                                                                                                               ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Louangphabang")]), "Louangphabang",      
                                                                                                                                      ifelse(index.new$plot %in% c(prov.code$plot[prov.code$Province_Name %in% ("Phongsaly")]), "Phongsaly",
                                                                                                                                             "Vientiane Capital")))))))))))))))))

# Apply rules for inclusion/ exclusion ####
index.NFI.III <- index.new
# remove plots with n(otype) < 3 
index.NFI.III <- index.NFI.III %>% group_by(plot) %>% do(mutate(.,otype.new = ifelse(length(plot[otype %in% c("f_nat")]) < 2,
                                                                                     NA,
                                                                                     as.character(otype))))
###
index.NFI.III <- index.NFI.III %>% group_by(plot) %>% do(mutate(.,newtype.2 = ifelse(!length(plot) == 1,
                                                                                     c(as.character(type[c(2:length(type))]),as.character(type[2])),NA)))
# for first +2 type
index.NFI.III <- index.NFI.III %>% group_by(plot) %>% do(mutate(.,newtype.3 = ifelse(length(plot) == 3, 
                                                                                     c(as.character(type[c(3:length(type))]),NA,NA),
                                                                                     (as.character(type[c(3)])))))
# for first +3 type
index.NFI.III <- index.NFI.III %>% group_by(plot) %>% do(mutate(.,newtype.4 = ifelse(length(plot) == 4, 
                                                                                     c(as.character(type[c(4:length(type))]),NA,NA,NA),
                                                                                     NA)))
index.NFI.III <- group_by(index.NFI.III, plot) %>% 
  do(mutate(.,newtype = ifelse(length(plot) == 2, 
                               ifelse(type == newtype.2, as.character(type), 
                                      NA),
                               ifelse(length(plot) == 3, 
                                      ifelse(type == newtype.2 | type == newtype.3,as.character(type),
                                             ifelse(newtype.2 == newtype.3,as.character(newtype.2),
                                                    NA)),
                                      ifelse(length(plot) == 4,
                                             ifelse(type == newtype.2 && newtype.3 == newtype.4 && type != newtype.3 |
                                                      type == newtype.3 && newtype.2 == newtype.4 && type != newtype.2 |
                                                      type == newtype.4 && newtype.2 == newtype.3 && type != newtype.2, NA,
                                                    ifelse(type == newtype.2 | type == newtype.3 | type == newtype.4, as.character(type), 
                                                           ifelse(newtype.2 == newtype.3 | newtype.2 == newtype.4, as.character(newtype.2),
                                                                  ifelse(newtype.3 == newtype.4,as.character(newtype.3),
                                                                         NA)))),NA)
                               ))))
summarySE(index.NFI.III, measurevar = "subplot", groupvars = c("plot","otype", "type"))

index.test.1 <- summarySE(index.NFI.II, measurevar = "subplot", groupvars = c("plot","otype"))
index.NFI.II[index.NFI.II$plot %in% c(as.numeric(as.character(index.test.1$plot[index.test.1$N == 4]))),]

index.test.2 <- summarySE(index.NFI.III, measurevar = "subplot", groupvars = c("plot","otype"))
index.NFI.III[!index.NFI.III$plot %in% c(as.numeric(as.character(index.test.2$plot[index.test.2$N == 4]))),]

index.test.1 <- summarySE(index.new, measurevar = "subplot", groupvars = c("plot","otype"))
index.ex.less4 <- index.new[!index.new$plot %in% c(as.numeric(as.character(index.test.1$plot[index.test.1$N == 4]))),]
index.ex.noNat<- index.new[!index.new$otype %in% c("f_nat"),]

index.new <- index.new[index.new$plot %in% c(as.numeric(as.character(index.test.1$plot[index.test.1$N == 4]))),]

index.ex. <- index.new[!index.new$otype %in% c("f_nat"),]

# insert v
index.new[,4] <- as.character(index.new[,4])
index.new[,3] <- as.character(index.new[,3])
index.new[,1] <- as.numeric(as.character(index.new[,1]))
index.new[10,] <- c(999, "sub_plotA","DD", "nf", 9, "Vientiane Capital")
index.new[11,] <- c(167, "sub_plotE","DD", "nf", 8, "Xaisomboun")

index.new[12,] <- c(168, "sub_plotA","DD", "f_nat", 8, "Xaisomboun")
index.new[13,] <- c(168, "sub_plotB","DD", "f_nat", 8, "Xaisomboun")
index.new[14,] <- c(168, "sub_plotC","OTH", "f_nat", 8, "Xaisomboun")
index.new[15,] <- c(168, "sub_plotE","OTH", "f_nat", 8, "Xaisomboun")

##########################
# Run script until 350 (wiht the above inserted at 311) check if update index 1 (non forest and conflicting forest types) still applies
# Update index 1 ####

index.new$type <- index.new$newtype
index.new <- index.new[,c(1:3,5,6)]
index.check.a <- summarySE(data = index.new, measurevar = "parent_index", groupvars = c("plot", "type"))

index.check.b <- index.check.a[index.check.a$N >= 2,]
index.check <- index.check.b[!index.check.b$type %in% c(NA),]

excluded.plots <- index.old[index.old$plot %in% index.check.a$plot[index.check.a$type %in% c(NA)] |
                              index.old$plot %in% index.check.a$plot[index.check.a$type %in% c("")]|
                              index.old$plot %in% index.check.a$plot[index.check.a$type %in% c("OTH")],]


excluded.plots.a <- cbind.data.frame(plot = ifelse(length(excluded.plots$plot) == 0,
                                                   0,
                                                   as.numeric(as.character(summarySE(excluded.plots, measurevar = "plot", groupvars = c("plot"))[,c(1)]))
),
N_subplots = ifelse(length(excluded.plots$plot) == 0,
                    0,
                    summarySE(excluded.plots, measurevar = "plot", groupvars = c("plot"))[,c(2)]
)
)

excluded.plots.a$reason <- ifelse(excluded.plots.a$N_subplots > 1, "conflicting forest type",
                                  ifelse(excluded.plots.a$N_subplots == 0, "no exluded plots",
                                         "non-forest")
)

#### graph
ggplot(dat.graph1, aes(factor(dat.graph1[,3]), dat.graph1[,4]))  + 
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot(aes(fill = dat.graph1[,3])) + scale_fill_manual(values = c(colfunc(length(levels(as.factor(dat.graph1[,3])))))) +
  ylab(paste0(names(dat.graph1[4])," carbon t/ha")) +
  xlab(paste0(names(dat.graph1[3]))) +
  theme_bw() +
  theme(legend.position = "none",
        #axis.ticks = element_blank(),
        axis.title = element_text(size=14, family="Lato", face = "bold"),
        axis.text = element_text(size=10, family="Lato"),
        panel.grid.major = element_line(colour = "grey85"), 
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(colour = "transparent")
  )

# images
head(NFI)
NFI$plot_GPS.photo[1]

substr("abcdef", 2, 4)
substr(NFI$plot_GPS.photo, 36, length(NFI$plot_GPS.photo))

### get rid of warnings:
index.new$plot <- as.numeric(as.character(index.new$plot))
index.check.a <- summarySUM(data = index.new, measurevar = "plot", groupvars = c("plot", "type"))


### get non uniqoe subplots
explot.2 <- index.old[index.old$plot %in% index.new.test$plot[duplicated(index.new.test)],] 
explot.2$plot <- as.numeric(as.character(explot.2$plot))
duplicated(explot.2$subplot)

####### bulk download images #####

  my_table <- cbind(NFI[,c(6,8,28,3)])
  my_table <- my_table[my_table$today == today()-1,]
  names(my_table) <- c("Plot Number", "Sub_Plot", "Land Use Class", "Date")
  my_table$Tree_Plot <- NFI$plot_GPS.photo[NFI$today == today()-1 & 
                                             NFI$plot_info.plot_code_nmbr %in% my_table$`Plot Number` &
                                             NFI$plot_info.sub_plot %in% my_table$Sub_Plot]
  
  my_table$Canopy <- NFI$canopy.photo_can[NFI$today == today()-1 & 
                                            NFI$plot_info.plot_code_nmbr %in% my_table$`Plot Number` &
                                            NFI$plot_info.sub_plot %in% my_table$Sub_Plot]
  
  tablePhotoDaily.pre <- my_table
  tablePhotoDaily.pre$Tree_Plot[1]
  
  
### subplot table
  carbon.new.sub <- cbind.data.frame(
    biomass = c(rep("AGB", length(tree_carbon2.sub$plot)),
                rep("Dead_Tree", length(dead_tree_carbon.sub$plot)),
                rep("Stump", length(stump_carbon.sub$plot))
    ), 
    rbind.data.frame(tree_carbon2.sub,
                     dead_tree_carbon.sub,
                     stump_carbon.sub)
  )
  carbon.new.sub <- carbon.new.sub[carbon.new.sub$type %in% c("EF", "MCB", "CF", "MDF", "DD"),]
  carbon.new.sub$Forest_Type <- carbon.new.sub$type

  ### adding date to subplot table
  stump.sub <- ddply(stump, .(plot, subplot, type, parent_index), function(stump) sum(stump$biomass_per_Mg_ha)) # change .() to include parent_index in dead_trees
  stump.sub <- rename(stump.sub, biomass = V1)
  stump.sub$biomass <- as.numeric(as.character(stump.sub$biomass))
  
  #carbon
  stump_carbon.sub <- stump.sub
  stump_carbon.sub$carbon <- stump_carbon.sub$biomass * 0.47
  
  stump_carbon.sub$plotsubplot <- paste0(stump_carbon.sub$plot, stump_carbon.sub$subplot)
  stump_carbon2 <- tree_carbon2.sub16[!tree_carbon2.sub16$plotsubplot %in% stump_carbon.sub$plotsubplot,]
  stump_carbon2$carbon <- 0
  stump_carbon2$biomass <- 0
  stump_carbon.sub <- rbind.data.frame(stump_carbon.sub[,c(1:6)], stump_carbon2[,c(1:6)]) #change 5 to 6
  
  stump_carbon.sub$date <- NFI$today[NFI$parent_index %in% stump_carbon.sub$parent_index]
  
  summarySE(
  summarySUMse(carbon.new.sub, measurevar = "carbon",sdvar = "sd", groupvars = c("plot", "subplot", "type")),
  measurevar = "sum",
  groupvars = c("plot", "type")
  )
  
  
  ##########
  summarySE(summarySUMse(carbon.new, measurevar = "carbon", sdvar = "sd",groupvars = c("Forest_Type", "plot")),
            measurevar = "sum", groupvars = c("Forest_Type")
  )[,2]
sum(summarySE(summarySUMse(carbon.new, measurevar = "carbon", sdvar = "sd",groupvars = c("Forest_Type", "plot")),
              measurevar = "sum", groupvars = c("Forest_Type")
)[,2])


tree_carbon2.plotProv[tree_carbon2.plotProv$Province_Name %in% c("Vientiane Capital"),]

living_trees[living_trees$province %in% c("Vientiane Capital"),]

tree_carbon2.subProv[tree_carbon2.subProv$Province_Name %in% c("Vientiane Capital"),]
tree_carbon2.sub
tree_carbon2.sub[tree_carbon2.sub$parent_index %in% c(167),]
tree_carbon2.subProv[tree_carbon2.subProv$parent_index %in% c(167),]
