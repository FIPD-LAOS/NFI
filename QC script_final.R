# QC data ####
# Please run the NFI-Calc-III.R script first in order to:
#   1) load the required packages
#   2) define the customized functions
#   3) get the data frame "prov.new" and the data frames of the C pools at the subplot level from the "normal" field data
# Please also note that no NTV was collected in the QC process, thus this C pool is not included in the analysis

# download the data from formhub ####
NFI <- onaDownload('NFI3QC',
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

# Progress list (by province/ total) ####
setwd("~/Documents/Dropbox/FC Projects - Active/0134-KKC NFI III/Data/Dashboard")
prov.code.ini <- read.csv("data/415plots_province.csv")
prov.code <- read.csv("data/province_code.csv")
prov.code <- unique(prov.code)
prov.summary <- summarySE(prov.code.ini[prov.code.ini$plot < 416,], measurevar = "plot", groupvars = c("Province_Name"))

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


N.prov <- summarySE(index.new, measurevar = "parent_index", groupvars = c("province", "plot"))
N.prov$n_plots_finished <- c(rep(1, length(N.prov$N)))
N.prov <- summarySE(N.prov, measurevar = "n_plots_finished", groupvars = "province")

N.prov.new <- merge(prov.summary, N.prov,  by.x = "Province_Name", by.y = "province", all = TRUE) 
N.prov.new <- N.prov.new[,c(1,2,7)]
names(N.prov.new) <- c("Province_Name", "N_Plots_initial", "N_Plots_actual")
N.prov.new[is.na(N.prov.new)] <- 0
prov.new <- prov.code

N.prov.new$Plots_remaining <- N.prov.new$N_Plots_initial - N.prov.new$N_Plots_actual
N.prov.new$Plots_remaining <- ifelse(N.prov.new$Plots_remaining < 0, 0, N.prov.new$Plots_remaining)
N.prov.new$Plots_remaining_perCent <- round((N.prov.new$Plots_remaining/N.prov.new$N_Plots_initial) * 100,2)

province.total <- cbind.data.frame(
  Province_Name = c("TOTAL"),
  N_Plots_initial = sum(N.prov.new$N_Plots_initial),
  N_Plots_actual = sum(N.prov.new$N_Plots_actual),
  Plots_remaining = sum(N.prov.new$Plots_remaining),
  Plots_remaining_perCent = round((sum(N.prov.new$Plots_remaining) / sum(N.prov.new$N_Plots_initial)) *100, 2)
)
N.prov.new <- rbind.data.frame(N.prov.new, province.total)

# Manually remove certain sub-plots ####

# Apply rules for inclusion/ exclusion ####
index.old <- index.new
index.new <- index.old
# remove plots with n(otype) < 4
index.test.1 <- summarySE(index.new, measurevar = "subplot", groupvars = c("plot","otype"))

index.ex.less4 <- index.new[!index.new$plot %in% c(as.numeric(as.character(index.test.1$plot[index.test.1$N >= 3]))),]

index.ex.less4$plot <- as.numeric(as.character(index.ex.less4$plot))
index.ex.less4 <- cbind.data.frame(plot = summarySUM(index.ex.less4, measurevar = "plot", groupvars = c("plot"))[,1],
                                   reason = c(rep("less than 3 natural forest subplots"))
)

index.ex.noNat <- index.new[!index.new$otype %in% c("f_nat"),]
index.ex.noNat$reason_slope <- NFI$access.access_reason.slope[NFI$parent_index %in% index.ex.noNat$parent_index]
index.ex.noNat$reason_danger <- NFI$access.access_reason.danger[NFI$parent_index %in% index.ex.noNat$parent_index]
index.ex.noNat$reason_distance <- NFI$access.access_reason.distance[NFI$parent_index %in% index.ex.noNat$parent_index]
index.ex.noNat$reason_prohibited <- NFI$access.access_reason.prohibited[NFI$parent_index %in% index.ex.noNat$parent_index]
index.ex.noNat$reason_other <- NFI$access.access_reason.other[NFI$parent_index %in% index.ex.noNat$parent_index]
index.ex.noNat$reason_other_otype <- NFI$lc_data.lc_type[NFI$parent_index %in% index.ex.noNat$parent_index]

index.new <- index.new[index.new$plot %in% c(as.numeric(as.character(index.test.1$plot[index.test.1$N >= 3]))) &
                         index.new$otype %in% c("f_nat"),]

#exclude plots with noNat subplots that that have 4 f_nat plots
index.ex.noNat <- index.ex.noNat[!index.ex.noNat$plot %in% index.new$plot,]

# create reason description as ifelse for each reason
index.ex.noNat$reason <- ifelse(index.ex.noNat$reason_slope == T, "Slope was too steep",
                                ifelse(index.ex.noNat$reason_danger == T, "Accessing area was too dangerous, i.e. UXO, or other safety reason",
                                       ifelse(index.ex.noNat$reason_distance == T, "Distance was too far, not enough supplies",
                                              ifelse(index.ex.noNat$reason_prohibited == T, "Ground access was restricted or denied",
                                                     "Other reasons (Check ONA for more information)")
                                       )
                                )
)

index.ex.noNat$reason <-  ifelse(!is.na(index.ex.noNat$reason_other_otype) == T, as.character(index.ex.noNat$reason_other_otype), index.ex.noNat$reason)

index.new <- index.new %>% group_by(plot) %>% do(mutate(.,newtype.2 = ifelse(!length(plot) == 1,
                                                                             c(as.character(type[c(2:length(type))]),
                                                                               as.character(type[2])),NA)))
# for first +2 type
index.new <- index.new %>% group_by(plot) %>% do(mutate(.,newtype.3 = ifelse(length(plot) == 3, 
                                                                             c(as.character(type[c(3:length(type))]),NA,NA),
                                                                             (as.character(type[c(3)])))))
# for first +3 type
index.new <- index.new %>% group_by(plot) %>% do(mutate(.,newtype.4 = ifelse(length(plot) == 4, 
                                                                             c(as.character(type[c(4:length(type))]),NA,NA,NA),
                                                                             NA)))

# for first +4 type
index.new <- index.new %>% group_by(plot) %>% do(mutate(.,newtype.5 = ifelse(length(plot) == 5, 
                                                                             c(as.character(type[c(5:length(type))]),NA,NA,NA,NA),
                                                                             NA)))

index.new <- group_by(index.new, plot) %>% 
  do(mutate(.,newtype = ifelse(length(plot) == 2, 
                               ifelse(type == newtype.2, as.character(type), 
                                      NA),
                               ifelse(length(plot) == 3, 
                                      ifelse(type == newtype.2 | type == newtype.3, as.character(type),
                                             ifelse(newtype.2 == newtype.3, as.character(newtype.2),
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

index.new$newtype[!is.na(index.new$newtype.5)] <- index.new$newtype.5[!is.na(index.new$newtype.5)]
# Update index 1 ####
index.new$type <- index.new$newtype
index.new <- index.new[,c(1:3,5,6)]

index.a <- index.new
index.a$plot <- as.numeric(as.character(index.a$plot))
index.check.a <- summarySUM(data = index.a, measurevar = "plot", groupvars = c("plot", "type"))


excluded.plots <- index.old[index.old$plot %in% index.check.a$plot[index.check.a$type %in% c(NA)] |
                              index.old$plot %in% index.check.a$plot[index.check.a$type %in% c("")]|
                              index.old$plot %in% index.check.a$plot[index.check.a$type %in% c("OTH")],]


excluded.plots.a <- cbind.data.frame(plot = ifelse(length(excluded.plots$plot) == 0,
                                                   0,
                                                   as.numeric(as.character(summarySE(excluded.plots, measurevar = "plot", groupvars = c("plot"))[,c(1)]))
)
)

excluded.plots.a$reason <- ifelse(excluded.plots.a$plot == 0, "no conflicting forest type",
                                  "conflicting forest type")


index.new.test <- index.a[,c(1,2)]            #for plot with conflicting subplot (e.g. 2 sub_plotA)

explot.2 <- index.old[index.old$plot %in% index.new.test$plot[duplicated(index.new.test)],] 
explot.2$plot <- as.numeric(as.character(explot.2$plot))

excluded.plots.b <- cbind.data.frame(plot = ifelse(length(explot.2$plot) == 0,
                                                   0,
                                                   as.numeric(as.character(summarySUM(explot.2, measurevar = "plot", groupvars = c("plot"))[,c(1)]))
)
)

excluded.plots.b$reason <- ifelse(excluded.plots.b$plot == 0, "no conflicting subplot description",
                                  c(rep("conflicting subplot description", length(excluded.plots.b$plot)))
)

excluded.plots.end2 <- rbind.data.frame(excluded.plots.a, excluded.plots.b, index.ex.less4, unique(index.ex.noNat[,c(1,13)]))
excluded.plots.end2 <- excluded.plots.end2[!excluded.plots.end2$plot == 0,]
ex.plot.prov <- merge(excluded.plots.end2, prov.new, by = "plot")
ex.plot.prov$plot <- as.numeric(as.character(ex.plot.prov$plot))
ex.plots.end <- summarySUM(ex.plot.prov, measurevar = "plot", groupvars = c("reason", "plot"))
ex.plots.end <- ex.plots.end[!duplicated(ex.plots.end$plot) == T,]
# Update index 2 ####
index.new <-  index.new[!index.new$plot %in% excluded.plots.end2$plot,]

index.new <- index.new[!index.new$type %in% c(NA) |
                         !index.new$type %in% c("") |
                         !index.new$type %in% c("SHBB") | 
                         !index.new$type %in% c("SC"),]

index.new <- index.new[index.new$type %in% c("EF", "DD", "MCB", "CF", "MDF"),]

# Plot progress after application of rules ####
N.prov <- summarySE(index.new, measurevar = "parent_index", groupvars = c("province", "plot"))
N.prov$n_plots_finished <- c(rep(1, length(N.prov$N)))
N.prov <- summarySE(N.prov, measurevar = "n_plots_finished", groupvars = "province")

N.prov.calc <- merge(prov.summary, N.prov,  by.x = "Province_Name", by.y = "province", all = TRUE) 
N.prov.calc <- N.prov.calc[,c(1,7)]
N.prov.calc[is.na(N.prov.calc)] <- 0


N.prov.calc <- 
  rbind(N.prov.calc,
        cbind.data.frame(
          Province_Name = c("TOTAL"),
          N.y = sum(N.prov.calc$N.y))
  )

names(N.prov.calc) <- c("Province_Name", "N_plots_calculated")

N.prov.new <- cbind.data.frame(N.prov.new[,c(1:3)], 
                               N_plots_calculated = N.prov.calc$N_plots_calculated, 
                               N.prov.new[,c(4,5)])

# Living trees ####
# Building the database from all nests 2, 3, 4 (only live/dead status, DBH, parent_index)
# nest 2
trees_nest_2.a <- NFI[, names(NFI)[grep("t_dbh_nest1|parent_index", names(NFI))]]
trees_nest_2.b <- NFI[, names(NFI)[grep("livedead_nest1|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]

trees_nest_2.c <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
trees_nest_2.b <- if(length(trees_nest_2.b) == length(trees_nest_2.a)) {
  trees_nest_2.b
} else {trees_nest_2.c}

test_nest_2.a.1 <- melt(trees_nest_2.a, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.a.1) <- c("parent_index", "DBH")
test_nest_2.b.1 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.1) <- c("parent_index", "status")

trees_nest_2.end <- cbind(test_nest_2.a.1, status = test_nest_2.b.1[,c(2)])
trees_nest_2.end <- trees_nest_2.end[!trees_nest_2.end$DBH %in% c(NA),]

#nest 3
trees_nest_3.a <- NFI[, names(NFI)[grep("t_dbh_nest2|parent_index", names(NFI))]]
trees_nest_3.b <- NFI[, names(NFI)[grep("livedead_nest2|parent_index", names(NFI))]]
trees_nest_3.b <- trees_nest_3.b[,c(1:(length(trees_nest_3.b)-2), length(trees_nest_3.b))]

trees_nest_3.c <- trees_nest_3.b[,c(1:(length(trees_nest_3.b)-2), length(trees_nest_3.b))]
trees_nest_3.b <- if(length(trees_nest_3.b) == length(trees_nest_3.a)) {
  trees_nest_3.b
} else {trees_nest_3.c}

test_nest_3.a.1 <- melt(trees_nest_3.a, id=c("parent_index"))[,c(1,3)]
names(test_nest_3.a.1) <- c("parent_index", "DBH")
test_nest_3.b.1 <- melt(trees_nest_3.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_3.b.1) <- c("parent_index", "status")

trees_nest_3.end <- cbind(test_nest_3.a.1, status = test_nest_3.b.1[,c(2)])
trees_nest_3.end <- trees_nest_3.end[!trees_nest_3.end$DBH %in% c(NA),]
#nest 4
trees_nest_4.a <- NFI[, names(NFI)[grep("t_dbh_nest3|parent_index", names(NFI))]]
trees_nest_4.b <- NFI[, names(NFI)[grep("livedead_nest3|parent_index", names(NFI))]]
trees_nest_4.b <- trees_nest_4.b[,c(1:(length(trees_nest_4.b)-2), length(trees_nest_4.b))]

trees_nest_4.c <- trees_nest_4.b[,c(1:(length(trees_nest_4.b)-2), length(trees_nest_4.b))]
trees_nest_4.b <- if(length(trees_nest_4.b) == length(trees_nest_4.a)) {
  trees_nest_4.b
} else {trees_nest_4.c}

test_nest_4.a.1 <- melt(trees_nest_4.a, id=c("parent_index"))[,c(1,3)]
names(test_nest_4.a.1) <- c("parent_index", "DBH")
test_nest_4.b.1 <- melt(trees_nest_4.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_4.b.1) <- c("parent_index", "status")

trees_nest_4.end <- cbind(test_nest_4.a.1, status = test_nest_4.b.1[,c(2)])
trees_nest_4.end <- trees_nest_4.end[!trees_nest_4.end$DBH %in% c(NA),]

trees <- rbind(trees_nest_2.end[,c(1,2,3)],
               trees_nest_3.end[,c(1,2,3)],
               trees_nest_4.end[,c(1,2,3)]
)

# getting forest type, plot and subplot into the dataset
trees <- merge(index.new, trees,   by.x = "parent_index") 

living_trees <- trees[trees$status %in% c(1),] # only living trees 

living_trees$DBH[living_trees$DBH == 199] <- 204 # change DBH from restricted max DBH (199) to DBH = 204 (not possible in ONA directly)

living_trees$biomass2_per_kg_tree <- ifelse(living_trees$type %in% c("EF"), 0.3112*living_trees$DBH^2.2331,
                                            ifelse(living_trees$type %in% c("MDF"), 0.523081*living_trees$DBH^2,
                                                   ifelse(living_trees$type %in% c("DD"), 0.2137*living_trees$DBH^2.2575,
                                                          0.1277*living_trees$DBH^2.3944)
                                            )
)

living_trees$biomass2_per_Mg_ha <- ifelse(living_trees$DBH < 30, living_trees$biomass2_per_kg_tree*((10000/(pi*6^2))*0.001),
                                          ifelse(living_trees$DBH >= 30 & living_trees$DBH < 50 , living_trees$biomass2_per_kg_tree*((10000/(pi*15^2))*0.001),
                                                 living_trees$biomass2_per_kg_tree*((10000/(pi*20^2))*0.001)))

# biomass
tree_biomass2.sub <- ddply(living_trees, .(plot, subplot, type, parent_index), function(living_trees) sum(living_trees$biomass2_per_Mg_ha))
tree_biomass2.sub <- rename(tree_biomass2.sub, biomass = V1)
tree_biomass2.sub$biomass <- as.numeric(as.character(tree_biomass2.sub$biomass))

#carbon
tree_carbon2.sub <- tree_biomass2.sub
tree_carbon2.sub$carbon <- tree_carbon2.sub$biomass * 0.47

tree_carbon2.sub16 <- tree_carbon2.sub
tree_carbon2.sub16$plotsubplot <- paste0(tree_carbon2.sub16$plot, tree_carbon2.sub16$subplot)
# attache date
tree_carbon2.sub16$date <- NFI$today[NFI$parent_index %in% tree_carbon2.sub16$parent_index]

# Deadwood standing trees ####
# daed tree class 1 (standing)
trees_nest_2.a <- NFI[, names(NFI)[grep("t_dbh_nest1|parent_index", names(NFI))]]

trees_nest_2.b <- NFI[, names(NFI)[grep("livedead_nest1|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
trees_nest_2.c <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
trees_nest_2.b <- if(length(trees_nest_2.b) == length(trees_nest_2.a)) {
  trees_nest_2.b
} else {trees_nest_2.c}

test_nest_2.b.1 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.1) <- c("parent_index", "status")
test_nest_2.a.1 <- melt(trees_nest_2.a, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.a.1) <- c("parent_index", "DBH")

dead_trees_class1.2 <- cbind.data.frame(test_nest_2.b.1, 
                                        "class" = c(rep(1, length(test_nest_2.b.1$parent_index))),
                                        "DBH" = test_nest_2.a.1[,2])

dead_trees_class1.2 <- dead_trees_class1.2[dead_trees_class1.2$status %in% c(2),]
dead_trees_class1.2 <- dead_trees_class1.2[!dead_trees_class1.2$DBH %in% c(NA),]

###
trees_nest_2.a <- NFI[, names(NFI)[grep("t_dbh_nest2|parent_index", names(NFI))]]

trees_nest_2.b <- NFI[, names(NFI)[grep("livedead_nest2|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
trees_nest_2.c <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
trees_nest_2.b <- if(length(trees_nest_2.b) == length(trees_nest_2.a)) {
  trees_nest_2.b
} else {trees_nest_2.c}

test_nest_2.b.1 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.1) <- c("parent_index", "status")
test_nest_2.a.1 <- melt(trees_nest_2.a, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.a.1) <- c("parent_index", "DBH")

dead_trees_class1.3 <- cbind.data.frame(test_nest_2.b.1, 
                                        "class" = c(rep(1, length(test_nest_2.b.1$parent_index))),
                                        "DBH" = test_nest_2.a.1[,2])
dead_trees_class1.3 <- dead_trees_class1.3[dead_trees_class1.3$status %in% c(2),]
dead_trees_class1.3 <- dead_trees_class1.3[!dead_trees_class1.3$DBH %in% c(NA),]
###
trees_nest_2.a <- NFI[, names(NFI)[grep("t_dbh_nest3|parent_index", names(NFI))]]

trees_nest_2.b <- NFI[, names(NFI)[grep("livedead_nest3|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
trees_nest_2.c <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
trees_nest_2.b <- if(length(trees_nest_2.b) == length(trees_nest_2.a)) {
  trees_nest_2.b
} else {trees_nest_2.c}

test_nest_2.b.1 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.1) <- c("parent_index", "status")
test_nest_2.a.1 <- melt(trees_nest_2.a, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.a.1) <- c("parent_index", "DBH")

dead_trees_class1.4 <- cbind.data.frame(test_nest_2.b.1,
                                        "class" = c(rep(1, length(test_nest_2.b.1$parent_index))),
                                        "DBH" = test_nest_2.a.1[,2])
dead_trees_class1.4 <- dead_trees_class1.4[dead_trees_class1.4$status %in% c(2),]
dead_trees_class1.4 <- dead_trees_class1.4[!dead_trees_class1.4$DBH %in% c(NA),]

###
trees_nest_2.a <- NFI[, names(NFI)[grep("t_dbh_nest4|parent_index", names(NFI))]]

dead_trees_class1.5 <- cbind.data.frame("parent_index" = trees_nest_2.a[,2],
                                        "status" = rep(2, length(trees_nest_2.a[,2])),
                                        "class" = rep(1, length(trees_nest_2.a[,2])),
                                        "DBH" = trees_nest_2.a[,1]
)
dead_trees_class1.5 <- dead_trees_class1.5[!dead_trees_class1.5$DBH %in% c(NA),]

dead_trees_class1 <- rbind.data.frame(dead_trees_class1.2, dead_trees_class1.3, dead_trees_class1.4, dead_trees_class1.5)
dead_trees_class1 <- cbind.data.frame(dead_trees_class1,
                                      "subclass" = c(rep(NA, length(dead_trees_class1$parent_index))),
                                      "DB.s" = c(rep(NA, length(dead_trees_class1$parent_index))),
                                      "DBH.s" = c(rep(NA, length(dead_trees_class1$parent_index))),
                                      "DT.s" = c(rep(NA, length(dead_trees_class1$parent_index))),
                                      "height.s" = c(rep(NA, length(dead_trees_class1$parent_index))),
                                      "DB.t" = c(rep(NA, length(dead_trees_class1$parent_index))),
                                      "DBH.t" = c(rep(NA, length(dead_trees_class1$parent_index))),
                                      "dist" = c(rep(NA, length(dead_trees_class1$parent_index))),
                                      "slope.t" = c(rep(NA, length(dead_trees_class1$parent_index))),
                                      "slope.b" = c(rep(NA, length(dead_trees_class1$parent_index)))
)

# class 2 short 
trees_nest_2.b <- NFI[, names(NFI)[grep("deadcl2_nest1_tallshort|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.3 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.3) <- c("parent_index", "subclass")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest1_DB_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.4 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.4) <- c("parent_index", "DB.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest1_DBH_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.5 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.5) <- c("parent_index", "DBH.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest1_DT_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.6 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.6) <- c("parent_index", "DT.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest1_height_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.7 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.7) <- c("parent_index", "height.s")

dead_trees_class2.short.nest_2 <- cbind.data.frame("parent_index" = test_nest_2.b.4[,1],
                                                   "status" = c(rep(2, length(test_nest_2.b.4$parent_index))),
                                                   "class" = c(rep(2, length(test_nest_2.b.4$parent_index))),
                                                   "DBH" = c(rep(NA, length(test_nest_2.b.4$parent_index))),
                                                   "subclass" = c(rep(1, length(test_nest_2.b.4$parent_index))),
                                                   "DB.s" = test_nest_2.b.4[,2],
                                                   "DBH.s" = test_nest_2.b.5[,2],
                                                   "DT.s" = test_nest_2.b.6[,2],
                                                   "height.s" = test_nest_2.b.7[,2]
)
dead_trees_class2.short.nest_2 <- dead_trees_class2.short.nest_2[!dead_trees_class2.short.nest_2$DBH.s %in% c(NA),]
###
trees_nest_2.b <- NFI[, names(NFI)[grep("deadcl2_nest2_tallshort|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.3 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.3) <- c("parent_index", "subclass")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest2_DB_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.4 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.4) <- c("parent_index", "DB.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest2_DBH_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.5 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.5) <- c("parent_index", "DBH.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest2_DT_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.6 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.6) <- c("parent_index", "DT.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest2_height_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.7 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.7) <- c("parent_index", "height.s")

dead_trees_class2.short.nest_3 <- cbind.data.frame("parent_index" = test_nest_2.b.4[,1],
                                                   "status" = c(rep(2, length(test_nest_2.b.4$parent_index))),
                                                   "class" = c(rep(2, length(test_nest_2.b.4$parent_index))),
                                                   "DBH" = c(rep(NA, length(test_nest_2.b.4$parent_index))),
                                                   "subclass" = c(rep(1, length(test_nest_2.b.4$parent_index))),
                                                   "DB.s" = test_nest_2.b.4[,2],
                                                   "DBH.s" = test_nest_2.b.5[,2],
                                                   "DT.s" = test_nest_2.b.6[,2],
                                                   "height.s" = test_nest_2.b.7[,2]
)
dead_trees_class2.short.nest_3 <- dead_trees_class2.short.nest_3[!dead_trees_class2.short.nest_3$DBH.s %in% c(NA),]
###
trees_nest_2.b <- NFI[, names(NFI)[grep("deadcl2_nest3_tallshort|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.3 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.3) <- c("parent_index", "subclass")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest3_DB_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.4 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.4) <- c("parent_index", "DB.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest3_DBH_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.5 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.5) <- c("parent_index", "DBH.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest3_DT_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.6 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.6) <- c("parent_index", "DT.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest3_height_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.7 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.7) <- c("parent_index", "height.s")

dead_trees_class2.short.nest_4 <- cbind.data.frame("parent_index" = test_nest_2.b.4[,1],
                                                   "status" = c(rep(2, length(test_nest_2.b.4$parent_index))),
                                                   "class" = c(rep(2, length(test_nest_2.b.4$parent_index))),
                                                   "DBH" = c(rep(NA, length(test_nest_2.b.4$parent_index))),
                                                   "subclass" = c(rep(1, length(test_nest_2.b.4$parent_index))),
                                                   "DB.s" = test_nest_2.b.4[,2],
                                                   "DBH.s" = test_nest_2.b.5[,2],
                                                   "DT.s" = test_nest_2.b.6[,2],
                                                   "height.s" = test_nest_2.b.7[,2]
)
dead_trees_class2.short.nest_4 <- dead_trees_class2.short.nest_4[!dead_trees_class2.short.nest_4$DBH.s %in% c(NA),]
###
trees_nest_2.b <- NFI[, names(NFI)[grep("deadcl2_nest4_tallshort|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.3 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.3) <- c("parent_index", "subclass")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest4_DB_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.4 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.4) <- c("parent_index", "DB.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest4_DBH_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.5 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.5) <- c("parent_index", "DBH.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest4_DT_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.6 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.6) <- c("parent_index", "DT.s")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest4_height_short|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.7 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.7) <- c("parent_index", "height.s")

dead_trees_class2.short.nest_5 <- cbind.data.frame("parent_index" = test_nest_2.b.4[,1],
                                                   "status" = c(rep(2, length(test_nest_2.b.4$parent_index))),
                                                   "class" = c(rep(2, length(test_nest_2.b.4$parent_index))),
                                                   "DBH" = c(rep(NA, length(test_nest_2.b.4$parent_index))),
                                                   "subclass" = c(rep(1, length(test_nest_2.b.4$parent_index))),
                                                   "DB.s" = test_nest_2.b.4[,2],
                                                   "DBH.s" = test_nest_2.b.5[,2],
                                                   "DT.s" = test_nest_2.b.6[,2],
                                                   "height.s" = test_nest_2.b.7[,2]
)
dead_trees_class2.short.nest_5 <- dead_trees_class2.short.nest_5[!dead_trees_class2.short.nest_5$DBH.s %in% c(NA),]


dead_trees_class2.short <- rbind.data.frame(dead_trees_class2.short.nest_2,
                                            dead_trees_class2.short.nest_3,
                                            dead_trees_class2.short.nest_4,
                                            dead_trees_class2.short.nest_4
)

dead_trees_class2.short <- cbind.data.frame(dead_trees_class2.short,
                                            "DB.t" = c(rep(NA, length(dead_trees_class2.short$parent_index))),
                                            "DBH.t" = c(rep(NA, length(dead_trees_class2.short$parent_index))),
                                            "dist" = c(rep(NA, length(dead_trees_class2.short$parent_index))),
                                            "slope.t" = c(rep(NA, length(dead_trees_class2.short$parent_index))),
                                            "slope.b" = c(rep(NA, length(dead_trees_class2.short$parent_index)))
)

# class 2 tall
trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest1_DB_tall|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.8 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.8) <- c("parent_index", "DB.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest1_DBH_tall|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.9 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.9) <- c("parent_index", "DBH.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest1_dist_t|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.10 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.10) <- c("parent_index", "dist")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest1_slope_t|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.11 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.11) <- c("parent_index", "slope.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest1_slope_b|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.12 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.12) <- c("parent_index", "slope.b")

dead_trees_class2.tall.nest_2 <- cbind.data.frame("parent_index" = test_nest_2.b.8[,1],
                                                  "status" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "class" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "DBH" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "subclass" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "DB.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DBH.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DT.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "height.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DB.t" = test_nest_2.b.8[,2],
                                                  "DBH.t" = test_nest_2.b.9[,2],
                                                  "dist" = test_nest_2.b.10[,2],
                                                  "slope.t" = test_nest_2.b.11[,2],
                                                  "slope.b" = test_nest_2.b.12[,2]
)

dead_trees_class2.tall.nest_2 <- dead_trees_class2.tall.nest_2[!dead_trees_class2.tall.nest_2$DBH.t %in% c(NA),]

###
trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest2_DB_tall|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.8 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.8) <- c("parent_index", "DB.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest2_DBH_tall|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.9 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.9) <- c("parent_index", "DBH.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest2_dist_t|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.10 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.10) <- c("parent_index", "dist")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest2_slope_t|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.11 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.11) <- c("parent_index", "slope.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest2_slope_b|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.12 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.12) <- c("parent_index", "slope.b")

dead_trees_class2.tall.nest_3 <- cbind.data.frame("parent_index" = test_nest_2.b.8[,1],
                                                  "status" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "class" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "DBH" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "subclass" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "DB.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DBH.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DT.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "height.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DB.t" = test_nest_2.b.8[,2],
                                                  "DBH.t" = test_nest_2.b.9[,2],
                                                  "dist" = test_nest_2.b.10[,2],
                                                  "slope.t" = test_nest_2.b.11[,2],
                                                  "slope.b" = test_nest_2.b.12[,2]
)

dead_trees_class2.tall.nest_3 <- dead_trees_class2.tall.nest_3[!dead_trees_class2.tall.nest_3$DBH.t %in% c(NA),]
###
trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest3_Db_tall|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.8 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.8) <- c("parent_index", "DB.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest3_DBH_tall|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.9 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.9) <- c("parent_index", "DBH.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest3_dist_t|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.10 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.10) <- c("parent_index", "dist")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest3_slope_t|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.11 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.11) <- c("parent_index", "slope.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest3_slope_b|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.12 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.12) <- c("parent_index", "slope.b")

dead_trees_class2.tall.nest_4 <- cbind.data.frame("parent_index" = test_nest_2.b.8[,1],
                                                  "status" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "class" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "DBH" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "subclass" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "DB.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DBH.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DT.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "height.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DB.t" = test_nest_2.b.8[,2],
                                                  "DBH.t" = test_nest_2.b.9[,2],
                                                  "dist" = test_nest_2.b.10[,2],
                                                  "slope.t" = test_nest_2.b.11[,2],
                                                  "slope.b" = test_nest_2.b.12[,2]
)

dead_trees_class2.tall.nest_4 <- dead_trees_class2.tall.nest_4[!dead_trees_class2.tall.nest_4$DBH.t %in% c(NA),]
###
trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest4_Db_tall|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.8 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.8) <- c("parent_index", "DB.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest4_DBH_tall|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.9 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.9) <- c("parent_index", "DBH.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest4_dist_t|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.10 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.10) <- c("parent_index", "dist")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest4_slope_t|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.11 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.11) <- c("parent_index", "slope.t")

trees_nest_2.b <- NFI[, names(NFI)[grep("dead_nest4_slope_b|parent_index", names(NFI))]]
trees_nest_2.b <- trees_nest_2.b[,c(1:(length(trees_nest_2.b)-2), length(trees_nest_2.b))]
test_nest_2.b.12 <- melt(trees_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(test_nest_2.b.12) <- c("parent_index", "slope.b")

dead_trees_class2.tall.nest_5 <- cbind.data.frame("parent_index" = test_nest_2.b.8[,1],
                                                  "status" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "class" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "DBH" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "subclass" = c(rep(2, length(test_nest_2.b.8$parent_index))),
                                                  "DB.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DBH.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DT.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "height.s" = c(rep(NA, length(test_nest_2.b.8$parent_index))),
                                                  "DB.t" = test_nest_2.b.8[,2],
                                                  "DBH.t" = test_nest_2.b.9[,2],
                                                  "dist" = test_nest_2.b.10[,2],
                                                  "slope.t" = test_nest_2.b.11[,2],
                                                  "slope.b" = test_nest_2.b.12[,2]
)

dead_trees_class2.tall.nest_5 <- dead_trees_class2.tall.nest_5[!dead_trees_class2.tall.nest_5$DBH.t %in% c(NA),]

dead_trees_class2.tall <- rbind.data.frame(dead_trees_class2.tall.nest_2,
                                           dead_trees_class2.tall.nest_3,
                                           dead_trees_class2.tall.nest_4,
                                           dead_trees_class2.tall.nest_5)

dead_trees <- rbind.data.frame(dead_trees_class1,
                               dead_trees_class2.short,
                               dead_trees_class2.tall)

dead_trees <- merge(dead_trees, index.new, by.x = "parent_index", by.y = "parent_index")

# biomass
dead_trees$biomass_per_kg_tree <- ifelse(dead_trees$class == 2  & dead_trees$subclass == 2,
                                         (pi*((tan(dead_trees$slope.b*pi/180) *dead_trees$dist + tan(dead_trees$slope.t*pi/180) *dead_trees$dist)*100)/12) *
                                           (dead_trees$DB.t^2 +
                                              (dead_trees$DB.t *
                                                 (dead_trees$DB.t - 
                                                    ((tan(dead_trees$slope.b*pi/180) *dead_trees$dist + tan(dead_trees$slope.t*pi/180) *dead_trees$dist)
                                                     *((dead_trees$DB.t - dead_trees$DBH.t)/130*100)
                                                    ))
                                              ) +
                                              ((dead_trees$DB.t - 
                                                  ((tan(dead_trees$slope.b*pi/180) *dead_trees$dist + tan(dead_trees$slope.t*pi/180) *dead_trees$dist)
                                                   *((dead_trees$DB.t - dead_trees$DBH.t)/130*100)
                                                  )))^2
                                           ) * 0.6 *0.001,
                                         
                                         
                                         ifelse(dead_trees$class == 2 & dead_trees$subclass %in% c(1), #short dead trees class 2
                                                (((pi*dead_trees$height.s*100)/12)*(dead_trees$DB.s^2+(dead_trees$DB.s*dead_trees$DT.s)+dead_trees$DT.s^2))*0.6*0.001,
                                                
                                                0.6 * exp (-1.499 + 2.148 * log(dead_trees$DBH) + 0.207 * log(dead_trees$DBH)^2 - 0.0281 * log(dead_trees$DBH)^3) #dead trees class 1
                                         ))

dead_trees$biomass_per_Mg_ha <- ifelse(dead_trees$class == 1 &dead_trees$DBH < 30, dead_trees$biomass_per_kg_tree*((10000/(pi*6^2))*0.001), 
                                       ifelse(dead_trees$class == 2 & dead_trees$subclass == 1 & dead_trees$DBH.s < 30, dead_trees$biomass_per_kg_tree*((10000/(pi*6^2))*0.001),
                                              ifelse(dead_trees$class == 2 & dead_trees$subclass == 2 & dead_trees$DBH.t < 30, dead_trees$biomass_per_kg_tree*((10000/(pi*6^2))*0.001),
                                                     
                                                     ifelse(dead_trees$DBH < 30 & dead_trees$class == 1 , dead_trees$biomass_per_kg_tree*((10000/(pi*25^2))*0.001),  
                                                            ifelse(dead_trees$DBH.s < 30 & dead_trees$class == 2 & dead_trees$subclass == 1, dead_trees$biomass_per_kg_tree*((10000/(pi*25^2))*0.001),
                                                                   dead_trees$biomass_per_kg_tree*((10000/(pi*25^2))*0.001))
                                                     ))))




dead_trees <- dead_trees[!dead_trees$biomass_per_Mg_ha %in% c(NA),]
# biomass
dead_tree_biomass.sub <- ddply(dead_trees, .(plot, subplot, type, parent_index), function(dead_trees) sum(dead_trees$biomass_per_Mg_ha))
dead_tree_biomass.sub <- rename(dead_tree_biomass.sub, biomass = V1)

dead_tree_biomass.sub$biomass <- as.numeric(as.character(dead_tree_biomass.sub$biomass))

#carbon
dead_tree_carbon.sub <- dead_tree_biomass.sub
dead_tree_carbon.sub$carbon <- dead_tree_carbon.sub$biomass * 0.47

# next 3 line include plots without dead_tree measured, but this should be included to account for no-dead_tree plot
dead_tree_carbon.sub$plotsubplot <- paste0(dead_tree_carbon.sub$plot, dead_tree_carbon.sub$subplot)
dead_tree2 <- tree_carbon2.sub16[!tree_carbon2.sub16$plotsubplot %in% dead_tree_carbon.sub$plotsubplot,]
dead_tree2$carbon <- 0
dead_tree2$biomass <- 0
dead_tree_carbon.sub <- rbind.data.frame(dead_tree_carbon.sub[,c(1:6)], dead_tree2[,c(1:6)])

# attach date
dead_tree_carbon.sub$date <- NFI$today[NFI$parent_index %in% dead_tree_carbon.sub$parent_index]

# Deadwood stumps ####
# nest 1
stumps_nest_2.b <- NFI[, names(NFI)[grep("stump_nest1.diameter1_nest1|parent_index", names(NFI))]]
stumps_nest_2.b <- stumps_nest_2.b[,c(1:(length(stumps_nest_2.b)-2), length(stumps_nest_2.b))]
stumps_nest_2.1 <- melt(stumps_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_2.1) <- c("parent_index", "Diam1")

stumps_nest_2.b <- NFI[, names(NFI)[grep("stump_nest1.diameter2_nest1|parent_index", names(NFI))]]
stumps_nest_2.b <- stumps_nest_2.b[,c(1:(length(stumps_nest_2.b)-2), length(stumps_nest_2.b))]
stumps_nest_2.2 <- melt(stumps_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_2.2) <- c("parent_index", "Diam2")

stumps_nest_2.b <- NFI[, names(NFI)[grep("stump_nest1.height_st_nest1|parent_index", names(NFI))]]
stumps_nest_2.b <- stumps_nest_2.b[,c(1:(length(stumps_nest_2.b)-2), length(stumps_nest_2.b))]
stumps_nest_2.3 <- melt(stumps_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_2.3) <- c("parent_index", "height")

stumps_nest_2.b <- NFI[, names(NFI)[grep("stump_nest1.stump_cut_cl_nest1|parent_index", names(NFI))]]
stumps_nest_2.b <- stumps_nest_2.b[,c(1:(length(stumps_nest_2.b)-2), length(stumps_nest_2.b))]
stumps_nest_2.4 <- melt(stumps_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_2.4) <- c("parent_index", "reason")

stumps_nest_1 <- cbind.data.frame(stumps_nest_2.1,
                                  "Diam2" = stumps_nest_2.2[,2],
                                  "height" = stumps_nest_2.3[,2],
                                  "reason" = stumps_nest_2.4[,2]
)
stumps_nest_1 <- stumps_nest_1[!stumps_nest_1$Diam1 %in% c(NA),]

# nest 2
stumps_nest_2.b <- NFI[, names(NFI)[grep("stump_nest2.diameter1_nest2|parent_index", names(NFI))]]
stumps_nest_2.b <- stumps_nest_2.b[,c(1:(length(stumps_nest_2.b)-2), length(stumps_nest_2.b))]
stumps_nest_2.1 <- melt(stumps_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_2.1) <- c("parent_index", "Diam1")

stumps_nest_2.b <- NFI[, names(NFI)[grep("stump_nest2.diameter2_nest2|parent_index", names(NFI))]]
stumps_nest_2.b <- stumps_nest_2.b[,c(1:(length(stumps_nest_2.b)-2), length(stumps_nest_2.b))]
stumps_nest_2.2 <- melt(stumps_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_2.2) <- c("parent_index", "Diam2")

stumps_nest_2.b <- NFI[, names(NFI)[grep("stump_nest2.height_st_nest2|parent_index", names(NFI))]]
stumps_nest_2.b <- stumps_nest_2.b[,c(1:(length(stumps_nest_2.b)-2), length(stumps_nest_2.b))]
stumps_nest_2.3 <- melt(stumps_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_2.3) <- c("parent_index", "height")

stumps_nest_2.b <- NFI[, names(NFI)[grep("stump_nest2.stump_cut_cl_nest2|parent_index", names(NFI))]]
stumps_nest_2.b <- stumps_nest_2.b[,c(1:(length(stumps_nest_2.b)-2), length(stumps_nest_2.b))]
stumps_nest_2.4 <- melt(stumps_nest_2.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_2.4) <- c("parent_index", "reason")

stumps_nest_2 <- cbind.data.frame(stumps_nest_2.1,
                                  "Diam2" = stumps_nest_2.2[,2],
                                  "height" = stumps_nest_2.3[,2],
                                  "reason" = stumps_nest_2.4[,2]
)
stumps_nest_2 <- stumps_nest_2[!stumps_nest_2$Diam1 %in% c(NA),]

### nest 3
stumps_nest_3.b <- NFI[, names(NFI)[grep("stump_nest3.diameter1_nest3|parent_index", names(NFI))]]
stumps_nest_3.b <- stumps_nest_3.b[,c(1:(length(stumps_nest_3.b)-2), length(stumps_nest_3.b))]
stumps_nest_3.1 <- melt(stumps_nest_3.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_3.1) <- c("parent_index", "Diam1")

stumps_nest_3.b <- NFI[, names(NFI)[grep("stump_nest3.diameter2_nest3|parent_index", names(NFI))]]
stumps_nest_3.b <- stumps_nest_3.b[,c(1:(length(stumps_nest_3.b)-2), length(stumps_nest_3.b))]
stumps_nest_3.2 <- melt(stumps_nest_3.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_3.2) <- c("parent_index", "Diam2")

stumps_nest_3.b <- NFI[, names(NFI)[grep("stump_nest3.height_st_nest3|parent_index", names(NFI))]]
stumps_nest_3.b <- stumps_nest_3.b[,c(1:(length(stumps_nest_3.b)-2), length(stumps_nest_3.b))]
stumps_nest_3.3 <- melt(stumps_nest_3.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_3.3) <- c("parent_index", "height")

stumps_nest_3.b <- NFI[, names(NFI)[grep("stump_nest3.stump_cut_cl_nest3|parent_index", names(NFI))]]
stumps_nest_3.b <- stumps_nest_3.b[,c(1:(length(stumps_nest_3.b)-2), length(stumps_nest_3.b))]
stumps_nest_3.4 <- melt(stumps_nest_3.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_3.4) <- c("parent_index", "reason")

stumps_nest_3 <- cbind.data.frame(stumps_nest_3.1,
                                  "Diam2" = stumps_nest_3.2[,2],
                                  "height" = stumps_nest_3.3[,2],
                                  "reason" = stumps_nest_3.4[,2]
)
stumps_nest_3 <- stumps_nest_3[!stumps_nest_3$Diam1 %in% c(NA),]

### nest 4
stumps_nest_3.b <- NFI[, names(NFI)[grep("stump_nest4.diameter1_nest4|parent_index", names(NFI))]]
stumps_nest_3.b <- stumps_nest_3.b[,c(1:(length(stumps_nest_3.b)-2), length(stumps_nest_3.b))]
stumps_nest_3.1 <- melt(stumps_nest_3.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_3.1) <- c("parent_index", "Diam1")

stumps_nest_3.b <- NFI[, names(NFI)[grep("stump_nest4.diameter2_nest4|parent_index", names(NFI))]]
stumps_nest_3.b <- stumps_nest_3.b[,c(1:(length(stumps_nest_3.b)-2), length(stumps_nest_3.b))]
stumps_nest_3.2 <- melt(stumps_nest_3.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_3.2) <- c("parent_index", "Diam2")

stumps_nest_3.b <- NFI[, names(NFI)[grep("stump_nest4.height_st_nest4|parent_index", names(NFI))]]
stumps_nest_3.b <- stumps_nest_3.b[,c(1:(length(stumps_nest_3.b)-2), length(stumps_nest_3.b))]
stumps_nest_3.3 <- melt(stumps_nest_3.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_3.3) <- c("parent_index", "height")

stumps_nest_3.b <- NFI[, names(NFI)[grep("stump_nest4.stump_cut_cl_nest4|parent_index", names(NFI))]]
stumps_nest_3.b <- stumps_nest_3.b[,c(1:(length(stumps_nest_3.b)-2), length(stumps_nest_3.b))]
stumps_nest_3.4 <- melt(stumps_nest_3.b, id=c("parent_index"))[,c(1,3)]
names(stumps_nest_3.4) <- c("parent_index", "reason")

stumps_nest_4 <- cbind.data.frame(stumps_nest_3.1,
                                  "Diam2" = stumps_nest_3.2[,2],
                                  "height" = stumps_nest_3.3[,2],
                                  "reason" = stumps_nest_3.4[,2]
)
stumps_nest_4 <- stumps_nest_4[!stumps_nest_4$Diam1 %in% c(NA),]

stump <- rbind(stumps_nest_1,
               stumps_nest_2,
               stumps_nest_3,
               stumps_nest_4)

stump <- merge(stump, index.new, by.x = "parent_index", by.y = "parent_index")

stump$mean_Diam <- rowMeans(stump[,c(2,3)])
stump$biomass_per_kg_tree <- (((stump$mean_Diam/2)^2*pi)*stump$height)*0.57*0.001

stump$biomass_per_Mg_ha <- ifelse(stump$mean_Diam < 30, stump$biomass_per_kg_tree*((10000/(pi*6^2))*0.001),
                                  stump$biomass_per_kg_tree*((10000/(pi*25^2))*0.001))

stump.sub <- ddply(stump, .(plot, subplot, type, parent_index), function(stump) sum(stump$biomass_per_Mg_ha))
stump.sub <- rename(stump.sub, biomass = V1)
stump.sub$biomass <- as.numeric(as.character(stump.sub$biomass))

#carbon
stump_carbon.sub <- stump.sub
stump_carbon.sub$carbon <- stump_carbon.sub$biomass * 0.47

stump_carbon.sub$plotsubplot <- paste0(stump_carbon.sub$plot, stump_carbon.sub$subplot)
stump_carbon2 <- tree_carbon2.sub16[!tree_carbon2.sub16$plotsubplot %in% stump_carbon.sub$plotsubplot,]
stump_carbon2$carbon <- 0
stump_carbon2$biomass <- 0
stump_carbon.sub <- rbind.data.frame(stump_carbon.sub[,c(1:6)], stump_carbon2[,c(1:6)])


# by province ####
tree_carbon2.subProvQC <- merge(tree_carbon2.sub, prov.new, by.x = "plot")
dead_tree_carbon.subProvQC <- merge(dead_tree_carbon.sub, prov.new, by.x = "plot")
stump_carbon.subProvQC <- merge(stump_carbon.sub, prov.new, by.x = "plot")


tree_carbon2.subProvQC$roots <- ifelse(tree_carbon2.subProvQC$type %in% c("CF") & tree_carbon2.subProvQC$biomass < 50, tree_carbon2.subProvQC$carbon * 0.46,
                                       ifelse(tree_carbon2.subProvQC$type %in% c("CF") & tree_carbon2.subProvQC$biomass <= 150, tree_carbon2.subProvQC$carbon * 0.32,
                                              ifelse(tree_carbon2.subProvQC$type %in% c("CF") & tree_carbon2.subProvQC$biomass > 150, tree_carbon2.subProvQC$carbon * 0.23,
                                                     ifelse(!tree_carbon2.subProvQC$type %in% c("CF") & tree_carbon2.subProvQC$biomass < 125, tree_carbon2.subProvQC$carbon * 0.20,
                                                            tree_carbon2.subProvQC$carbon * 0.24)
                                              )
                                       )
)

# Create data frame of the QC data for the significance test
carbon.new.sub.qc <- cbind.data.frame(
  biomass = c(rep("AGB", length(tree_carbon2.subProvQC$plot)),
              rep("BGB", length(tree_carbon2.subProvQC$plot)),
              rep("Dead_Tree", length(dead_tree_carbon.subProvQC$plot)),
              rep("Stump", length(stump_carbon.subProvQC$plot))
  ), 
  rbind.data.frame(tree_carbon2.subProvQC[,c(1:3,6,7)], 
                   cbind.data.frame( #this "cbind.data.frame"-function creates the rows for the BGB
                     plot = tree_carbon2.subProvQC[,1],
                     subplot = tree_carbon2.subProvQC[,2],
                     type = tree_carbon2.subProvQC[,3],
                     carbon = tree_carbon2.subProvQC[,8],
                     Province_Name = tree_carbon2.subProvQC[,7]
                   ),
                   dead_tree_carbon.subProvQC[,c(1:3,6,8)],
                   stump_carbon.subProvQC[,c(1:3,6,7)]
  )
)
carbon.new.sub.qc.sum <- summarySUM(carbon.new.sub.qc, measurevar = "carbon", groupvars = c("plot", "subplot","Province_Name", "type"))
carbon.new.sub.qc.sum$plot <- as.numeric(as.character(carbon.new.sub.qc.sum$plot))

write.csv(carbon.new.sub.qc.sum, "QC_data_subplot.csv", row.names = F)

carbon.new.sub.qc.sum.plot <- summarySE(carbon.new.sub.qc.sum, measurevar = "sum", groupvars = c("plot", "type", "Province_Name"))
write.csv(carbon.new.sub.qc.sum.plot, "QC_data_plot.csv", row.names = F)

#qc.valid <- c(1039, 1042, 1046, 1053, 1058, 1062, 1064, 1069, 1070, 1121, 1124, 1130, 1140, 1159, 1160, 1164, 1167, 1180, 1181, 1184, 1201, 1228, 1229, 1231, 1236, 1250, 1257, 1283, 1286, 1308, 1319, 1320, 1368, 1391, 1411, 1428)
              

#carbon.new.sub.qc.sum <- carbon.new.sub.qc.sum[carbon.new.sub.qc.sum$plot %in% qc.valid,]

# Create data frame of the "normal" field data for the significance test
carbon.new.sub <- cbind.data.frame(
  biomass = c(rep("AGB", length(tree_carbon2.subProv$plot)),
              rep("BGB", length(tree_carbon2.subProv$plot)),
              rep("Dead_Tree", length(dead_tree_carbon.subProv$plot)),
              rep("Stump", length(stump_carbon.subProv$plot))
  ), 
  rbind.data.frame(tree_carbon2.subProv[,c(1:3,6,7)],
                   cbind.data.frame(
                     plot = tree_carbon2.subProv[,1],
                     subplot = tree_carbon2.subProv[,2],
                     type = tree_carbon2.subProv[,3],
                     carbon = tree_carbon2.subProv[,8],
                     Province_Name = tree_carbon2.subProv[,7]
                   ),
                   dead_tree_carbon.subProv[,c(1:3,6,8)],
                   stump_carbon.subProv[,c(1:3,6,8)])
)
carbon.new.sub.norm.sum <- summarySUM(carbon.new.sub, measurevar = "carbon", groupvars = c("plot", "subplot","Province_Name", "type"))
# Subset the "normal" data frame to include only the plots of the QC sampling
carbon.new.sub.norm.sum <- carbon.new.sub.norm.sum[carbon.new.sub.norm.sum$plot %in% c(carbon.new.sub.qc.sum$plot),]
#carbon.new.sub.qc.sum <- carbon.new.sub.qc.sum[carbon.new.sub.qc.sum$plot %in% c(carbon.new.sub.norm.sum$plot),]
#carbon.new.sub.qc.sum$type <- c(as.character(carbon.new.sub.norm.sum$type[c(3:24,26:104, 106:118)]), "MDF",as.character(carbon.new.sub.norm.sum$type[c(119:141)]))
# Normality tests using Shapiro Wilks
# QC data
# Carry out Shapiro-Wilks test to check for normal distribution
dlply(carbon.new.sub.qc.sum, .(plot), function(x)
  shapiro.test(carbon.new.sub.qc.sum$sum)
)
# try log transforming
dlply(carbon.new.sub.qc.sum, .(plot), function(x)
  shapiro.test(log(carbon.new.sub.qc.sum$sum[!carbon.new.sub.qc.sum$sum == 0]))
)
# try box-cox transformation
library(car)
dlply(carbon.new.sub.qc.sum, .(plot), function(x)
  shapiro.test(carbon.new.sub.qc.sum$sum[!carbon.new.sub.qc.sum$sum == 0]*coef(powerTransform(carbon.new.sub.qc.sum$sum[!carbon.new.sub.qc.sum$sum == 0])))
)

# Norm data
# Carry out Shapiro-Wilks test to check for normal distribution
dlply(carbon.new.sub.norm.sum, .(plot), function(x)
  shapiro.test(carbon.new.sub.norm.sum$sum)
)
# try log transforming
dlply(carbon.new.sub.norm.sum, .(plot), function(x)
  shapiro.test(log(carbon.new.sub.norm.sum$sum[!carbon.new.sub.norm.sum$sum == 0]))
)
# try box-cox transformation
dlply(carbon.new.sub.norm.sum, .(plot), function(x)
  shapiro.test(carbon.new.sub.norm.sum$sum[!carbon.new.sub.norm.sum$sum == 0]*coef(powerTransform(carbon.new.sub.norm.sum$sum[!carbon.new.sub.norm.sum$sum == 0])))
)

norm.plot <- summarySE(carbon.new.sub.norm.sum, measurevar = "sum", groupvars = c("plot", "Province_Name", "type"))
norm.plot <- norm.plot[order(as.numeric(as.character(norm.plot$plot))),] 

qc.plot <- summarySE(carbon.new.sub.qc.sum, measurevar = "sum", groupvars = c("plot", "Province_Name", "type"))
qc.plot <-   qc.plot[order(qc.plot$plot),] 
qc.plot$type <- norm.plot$type

# Combine datasets by also creating a vector for "treatment", i.e. qc fro QC-data, and norm for "normal" field data
sig.carb <- cbind.data.frame(
  treat = c(rep("norm", length(norm.plot$plot)),
            rep("qc", length(qc.plot$plot))),
  rbind.data.frame(
    norm.plot,
    qc.plot
  ) 
)

# Create variable to check significant difference between treatments and forest types
sig.carb$treat.type <- paste0(sig.carb$treat, sig.carb$type)

write.csv(sig.carb, "QC-Norm_data_comp.csv", row.names = F)

# Use Nonparam. multiple contrast tests with "Tukey"-contrasts from the package "nparcom"
library(nparcomp) #loads the package

# The below analyses takes a bit of time to compute, so don't worry! ;)
# I ran it on a 2.6 GHz I& with 16 GB 1600 MHz DDR3 (OSX EL Capitain on a MacBook Pro, Mid 2012) and 
# treatprov took 131.262 sec (about 2 min) and treat.type 7.352 sec

MCTP_QC <- mctp(mean ~ treat.type, sig.carb[!sig.carb$type %in% c("EF"),], type = "Tukey")$Analysis

MCTP_QC[c(4,11,17,22),]

carbon.new.sub.qc.sum.type <- summarySE(summarySE(carbon.new.sub.qc.sum, measurevar = "sum", groupvars = c("plot", "type", "Province_Name")),
                                          measurevar = "mean", groupvars = c("type"))
carbon.new.sub.qc.sum.type90 <- summarySE90(summarySE(carbon.new.sub.qc.sum, measurevar = "sum", groupvars = c("plot", "type", "Province_Name")),
                                        measurevar = "mean", groupvars = c("type"))
carbon.new.sub.norm.sum.type <- summarySE(summarySE(carbon.new.sub.norm.sum, measurevar = "sum", groupvars = c("plot", "type", "Province_Name")),
                                        measurevar = "mean", groupvars = c("type"))
carbon.new.sub.norm.sum.type90 <- summarySE90(summarySE(carbon.new.sub.norm.sum, measurevar = "sum", groupvars = c("plot", "type", "Province_Name")),
                                            measurevar = "mean", groupvars = c("type"))
sig.carb <- cbind.data.frame(
  carbon.new.sub.norm.sum.type,
  "ci90" = carbon.new.sub.norm.sum.type90[,6],
  carbon.new.sub.qc.sum.type[,c(3:6)],
  "ci90" = carbon.new.sub.qc.sum.type90[,6],
  "p.value" = c(MCTP_QC$p.Value[c(4,11)], "0", MCTP_QC$p.Value[c(17,22)])
)

sig.carb$N <- c(5,9,0,8,18)

#########
sig.carb <- cbind.data.frame(
  treat = c(rep("norm", length(carbon.new.sub.norm.sum$plot)),
            rep("qc", length(carbon.new.sub.qc.sum$plot))),
  rbind.data.frame(
    carbon.new.sub.norm.sum,
    carbon.new.sub.qc.sum
  ) 
)


sig.npar <- cbind.data.frame(
  norm.plot[,c(1:3, 5:8)],
  qc.plot[,c(5:8)],
  p.value = c(
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[1],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[2],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[3],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[4],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[5],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[6],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[7],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[8],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[9],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[10],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[11],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[12],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[13],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[14],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[15],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[16],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[17],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[18],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[19],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[20],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[21],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[22],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[23],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[24],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[25],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[26],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[27],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[28],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[29],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[30],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[31],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[32],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[33],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[34],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[35],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[36],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[37],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[38],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[39],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[40],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[41],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[42],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[43],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[44],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[45],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[46],])$Analysis$p.Value,
    npar.t.test(sum ~ treat, sig.carb[sig.carb$plot == norm.plot$plot[47],])$Analysis$p.Value
  )
)

write.csv(sig.npar, "QC-Norm_data_sig.npar.csv", row.names = F)
### Comparison NFI II and NFI III

# NFI II
carbon.new.II <- carbon.new
carbon.new.II <- carbon.new.II[carbon.new.II$biomass %in% c("AGB", "BGB", "Dead_Tree", "Stump"),]
NFI_II_sum <- summarySUM(carbon.new.II, measurevar = "carbon", groupvars = c("plot", "type"))
# NFI III
carbon.new.III <- carbon.new
NFI_III_sum <- summarySUM(carbon.new.III, measurevar = "carbon", groupvars = c("plot", "type"))

sig.carb.NFI <- cbind.data.frame(
  treat = c(rep("II", length(NFI_II_sum$plot)),
            rep("III", length(NFI_III_sum$plot))),
  rbind.data.frame(
    NFI_II_sum,
    NFI_III_sum
  ) 
)

sig.carb.NFI$treat.type <- paste0(sig.carb.NFI$treat, sig.carb.NFI$type)
MCTP_NFI <- mctp(sum ~ treat.type, sig.carb.NFI, type = "Tukey")$Analysis
MCTP_NFI[c(3,12,20,41,44),]


NFI_II_sum.type <- summarySE(NFI_II_sum, measurevar = "sum", groupvars = c("type"))
NFI_II_sum.type90 <- summarySE90(NFI_II_sum, measurevar = "sum", groupvars = c("type"))
NFI_III_sum.type <- summarySE(NFI_III_sum, measurevar = "sum", groupvars = c("type"))
NFI_III_sum.type90 <- summarySE90(NFI_III_sum, measurevar = "sum", groupvars = c("type"))

sig.carb.NFI.table <- cbind.data.frame(
  NFI_II_sum.type,
  "ci90" = NFI_II_sum.type90[,6],
  NFI_III_sum.type[c(1,3,4,5,2),c(2:6)],
  "ci90" = NFI_III_sum.type90[c(1,3,4,5,2),][,6],
  "p.value" = c(MCTP_NFI$p.Value[c(3,20,41,44,12)])
)

# FREL
#FREL which are :
#Strata 1: EF
#Strata 2: CF-MDF-MCB
#Stratat 3: DD

carbon.new.II$new.type <- ifelse(carbon.new.II$type %in% c("CF", "MDF", "MCB"),2,
                                 ifelse(carbon.new.II$type %in% c("EF"),1,
                                 3))

carbon.new.II <- carbon.new.II[carbon.new.II$biomass %in% c("AGB", "BGB"),]

FREL_II_sum <- summarySUM(carbon.new.II, measurevar = "carbon", groupvars = c("plot", "new.type"))
FREL_II_sum$type <- FREL_II_sum$new.type
# NFI III

carbon.new.III$new.type <- ifelse(carbon.new.III$type %in% c("CF", "MDF", "MCB"),2,
                                 ifelse(carbon.new.III$type %in% c("EF"),1,
                                        3))
carbon.new.III <- carbon.new.II[carbon.new.III$biomass %in% c("AGB", "BGB"),]
FREL_III_sum <- summarySUM(carbon.new.III, measurevar = "carbon", groupvars = c("plot", "new.type"))
FREL_III_sum$type <- FREL_III_sum$new.type

sig.carb.FREL <- cbind.data.frame(
  treat = c(rep("II", length(FREL_II_sum$plot)),
            rep("III", length(FREL_III_sum$plot))),
  rbind.data.frame(
    FREL_II_sum,
    FREL_III_sum
  ) 
)

sig.carb.FREL$treat.type <- paste0(sig.carb.FREL$treat, sig.carb.FREL$type)
MCTP_FREL <- mctp(sum ~ treat.type, sig.carb.FREL, type = "Tukey")$Analysis
MCTP_FREL[c(3,8,12),]


FREL_II_sum.type <- summarySE(FREL_II_sum, measurevar = "sum", groupvars = c("type"))
FREL_II_sum.type90 <- summarySE90(FREL_II_sum, measurevar = "sum", groupvars = c("type"))
FREL_III_sum.type <- summarySE(FREL_III_sum, measurevar = "sum", groupvars = c("type"))
FREL_III_sum.type90 <- summarySE90(FREL_III_sum, measurevar = "sum", groupvars = c("type"))

sig.carb.FREL.table <- cbind.data.frame(
  FREL_II_sum.type,
  "ci90" = FREL_II_sum.type90[,6],
  FREL_III_sum.type[,c(2:6)],
  "ci90" = FREL_III_sum.type90[,6],
  "p.value" = c(MCTP_FREL$p.Value[c(3,8,12)])
)

# end of script