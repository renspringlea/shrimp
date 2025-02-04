#Code written by Ren Ryba
#Research Scientist at Animal Ask
#Contact us at www.animalask.org
#First written November 2023, updated October 2024

#Load libraries for graphing
library(ggplot2)
library(gridExtra)
library(gt)
theme_set(theme_bw())

#Import data
codes <- read.csv("FAO/CL_FI_SPECIES_GROUPS.csv") #FAO species codes
countries <- read.csv("FAO/CL_FI_COUNTRY_GROUPS.csv") #FAO country codes
fao <- read.csv("FAO/Capture_Quantity.csv") #FAO capture data
trade <- read.csv("trade_partners_quantity.csv") #FAO export data

#Rethink Priorities data on individuals
#Note that I've already added a column to the Rethink spreadsheet to manually assign clades
#to each species/statistical group of shrimp
rp <- read.csv("2020 - Shrimp captured from the wild - 2020 Shrimps captured from the wild.csv") 

#Remove the columns from the Rethink data we won't use
rp <- rp[,c(1,2,7,8,14)]
rp <- rp[c(1:92),]
names(rp) <- c("Environment","Species","GEMW_g_Lower","GEMW_g_Upper","Clade")

#Manually change a few names in the data to make them match FAO data
#These are mostly where FAO and Rethink are using different synonyms for species names
rp[which(rp$Species=="Exopalaemon modestus (Palaemon modestus)"),"Species"] <- 
  "Exopalaemon modestus"
rp[which(rp$Species=="Trachypenaeus curvirostris"),"Species"] <- 
  "Trachysalambria curvirostris"
rp[which(rp$Species=="Xiphopenaeus, Trachypenaeus sp"),"Species"] <- 
  "Xiphopenaeus, Trachypenaeus spp"
rp[which(rp$Species=="Parapenaeopsis cornuta (Kishinouyepenaeopsis cornuta)"),"Species"] <- 
  "Parapenaeopsis cornuta"
rp[which(rp$Species=="Plesiopenaeus edwardsianus"),"Species"] <- 
  "Aristaeopsis edwardsiana"

#Manually change the Palaemonidate names in both datasets to clarify
#the distinction between saltwater and freshwater Palaemonidate
rp[which(rp$Species=="Palaemonidae"),"Species"] <- 
  c("Palaemonidae_Freshwater","Palaemonidae_Saltwater")
codes[which(codes$Scientific_Name=="Palaemonidae"),"Scientific_Name"] <- 
  c("Palaemonidae_Freshwater","Palaemonidae_Saltwater")

#Calculate midpoint weights from Rethink's upper and lower
#Note that we don't actually use this midpoint in our analysis - it's purely
#illustrative. The midpoints we do report in the publication are midpoints of the estimated numbers caught,
#not midpoints of the weight ranges.
rp$GEMW_g_Midpoint <- rowMeans(rp[,c("GEMW_g_Lower","GEMW_g_Upper")])

#Specify which groups of animals we care about (shrimp)
groups <- c("Shrimps, prawns", "Freshwater crustaceans")
codes.sp <- subset(codes, ISSCAAP_Group_En %in% groups)

#Excluding non-shrimp crustaceans (e.g. crayfish)
codes.sp <- subset(codes.sp, CPC_Class_En!="Other crustaceans, live, fresh or chilled")

#Restrict FAO data to the animals we care about for this analysis
fao.sp <- subset(fao, SPECIES.ALPHA_3_CODE %in% codes.sp$X3A_Code)

#Remove rows from FAO data with no catch
fao.sp <- subset(fao.sp, VALUE!=0)

#Restrict our data to 2022 catch
fao.sp.2022 <- fao.sp[which(fao.sp$PERIOD==2022),]

#Match our data with the info from the codes.sp data frame
#and countries data frame
fao.sp.2022 <- merge(fao.sp.2022,codes.sp,
                     by.x="SPECIES.ALPHA_3_CODE",by.y="X3A_Code",
                     all.x=T,all.y=F)
fao.sp.2022 <- merge(fao.sp.2022,countries,
                     by.x="COUNTRY.UN_CODE",by.y="UN_Code",
                     all.x=T,all.y=F)

# Rename the country names, often aggregating and shortening,
# to commensurate with the map data below
fao.sp.2022$country <- fao.sp.2022$Name_En.y
fao.sp.2022[which(fao.sp.2022$country=="Brunei Darussalam"),]$country <- "Brunei"
fao.sp.2022[which(fao.sp.2022$country=="Iran (Islamic Republic of)"),]$country <- "Iran"
fao.sp.2022[which(fao.sp.2022$country=="Republic of Korea"),]$country <- "South Korea"
fao.sp.2022[which(fao.sp.2022$country=="Netherlands (Kingdom of the)"),]$country <- "Netherlands"
fao.sp.2022[which(fao.sp.2022$country=="Russian Federation"),]$country <- "Russia"
fao.sp.2022[which(fao.sp.2022$country=="Taiwan Province of China"),]$country <- "Taiwan"
fao.sp.2022[which(fao.sp.2022$country=="United Republic of Tanzania"),]$country <- "Tanzania"
fao.sp.2022[which(fao.sp.2022$country=="Türkiye"),]$country <- "Turkey"
fao.sp.2022[which(fao.sp.2022$country=="United Kingdom of Great Britain and Northern Ireland"),]$country <- "UK"
fao.sp.2022[which(fao.sp.2022$country=="United States of America"),]$country <- "USA"
fao.sp.2022[which(fao.sp.2022$country=="Venezuela (Bolivarian Republic of)"),]$country <- "Venezuela"
fao.sp.2022[which(fao.sp.2022$country=="Viet Nam"),]$country <- "Vietnam"
fao.sp.2022[which(fao.sp.2022$country=="Côte d'Ivoire"),]$country <- "Ivory Coast"
fao.sp.2022[which(fao.sp.2022$country=="Trinidad and Tobago"),]$country <- "Trinidad"
fao.sp.2022[which(fao.sp.2022$country=="Congo"),]$country <- "Republic of Congo"
fao.sp.2022[which(fao.sp.2022$country=="China, Macao SAR"),]$country <- "China"
fao.sp.2022[which(fao.sp.2022$country=="China, Hong Kong SAR"),]$country <- "China"

#Make a new row specifying where we need to use a larger group name
#rather than a species name
#these are cases where Rethink didn't estimate a mean weight for
#a particular species
fao.sp.2022$Scientific_Name_Match <- fao.sp.2022$Scientific_Name
fao.sp.2022[which(fao.sp.2022$Scientific_Name=="Atypopenaeus formosus"),"Scientific_Name_Match"] <- "Penaeidae"
fao.sp.2022[which(fao.sp.2022$Scientific_Name=="Penaeus marginatus"),"Scientific_Name_Match"] <- "Penaeus spp"
fao.sp.2022[which(fao.sp.2022$Scientific_Name=="Metapenaeus affinis"),"Scientific_Name_Match"] <- "Metapenaeus spp"
fao.sp.2022[which(fao.sp.2022$Scientific_Name=="Pandalus spp, Pandalopsis spp"),"Scientific_Name_Match"] <- "Pandalus spp"
fao.sp.2022[which(fao.sp.2022$Scientific_Name=="Parapenaeopsis sculptilis"),"Scientific_Name_Match"] <- "Parapenaeopsis spp"
fao.sp.2022[which(fao.sp.2022$Scientific_Name=="Parapenaeopsis tenella"),"Scientific_Name_Match"] <- "Parapenaeopsis spp"

# Merge with Rethink's weight data
fao.sp.2022 <- merge(fao.sp.2022,rp,
                     by.x="Scientific_Name_Match",by.y="Species",
                     all.x=T,all.y=F)

#Calculate numbers of individuals
#Catch in grams
fao.sp.2022$VALUE_g <- fao.sp.2022$VALUE*10^6

fao.sp.2022$Individuals_Lower <- fao.sp.2022$VALUE_g/fao.sp.2022$GEMW_g_Upper
fao.sp.2022$Individuals_Upper <- fao.sp.2022$VALUE_g/fao.sp.2022$GEMW_g_Lower
fao.sp.2022$Individuals_Midpoint <- rowMeans(fao.sp.2022[,c("Individuals_Lower","Individuals_Upper")])

#Save as CSV
#write.csv(fao.sp.2022,"fao_2022_prawn_catch_data_with_estimated_individuals.csv")

#Now, the fun stuff

#Aggregate and graph by various sets of variables

#Catch ~ clade
agg1 <- aggregate(Individuals_Midpoint~Clade,FUN=sum,data=fao.sp.2022)
agg1$Individuals_Lower <- aggregate(Individuals_Lower~Clade,FUN=sum,data=fao.sp.2022)$Individuals_Lower
agg1$Individuals_Upper <- aggregate(Individuals_Upper~Clade,FUN=sum,data=fao.sp.2022)$Individuals_Upper
g_agg1 <- ggplot(aes(x=Clade,y=Individuals_Midpoint,colour=Clade),data=agg1) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=Individuals_Lower,ymax=Individuals_Upper,colour=Clade),width=0.5) +
  scale_y_continuous(trans='log10', limits=c(1e10,1e15)) +
  xlab(NULL) + ylab("Individual shrimp caught (estimated)") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  coord_flip()
g_agg1
ggsave("Graphs/g_agg1.png",g_agg1,width=5,height=4)

#catch ~ continent
agg2 <- aggregate(Individuals_Midpoint~Continent_Group_En,FUN=sum,data=fao.sp.2022)
agg2$Individuals_Lower <- aggregate(Individuals_Lower~Continent_Group_En,FUN=sum,data=fao.sp.2022)$Individuals_Lower
agg2$Individuals_Upper <- aggregate(Individuals_Upper~Continent_Group_En,FUN=sum,data=fao.sp.2022)$Individuals_Upper
g_agg2 <- ggplot(aes(x=reorder(Continent_Group_En,Individuals_Midpoint),y=Individuals_Midpoint),data=agg2) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=Individuals_Lower,ymax=Individuals_Upper),width=0.5) +
  scale_y_continuous(trans='log10', limits=c(1e8,1e15),breaks=c(1e8,1e9,1e10,1e11,1e12,1e13,1e14)) +
  #facet_grid(rows=vars(Clade),scales="free") +
  xlab(NULL) + ylab("Individual shrimp caught (estimated)") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  coord_flip()
g_agg2
ggsave("Graphs/g_agg2.png",g_agg2,width=5,height=4)

#catch ~ region
agg3 <- aggregate(Individuals_Midpoint~GeoRegion_Group_En,FUN=sum,data=fao.sp.2022)
agg3$Individuals_Lower <- aggregate(Individuals_Lower~GeoRegion_Group_En,FUN=sum,data=fao.sp.2022)$Individuals_Lower
agg3$Individuals_Upper <- aggregate(Individuals_Upper~GeoRegion_Group_En,FUN=sum,data=fao.sp.2022)$Individuals_Upper
g_agg3 <- ggplot(aes(x=reorder(GeoRegion_Group_En,Individuals_Midpoint),y=Individuals_Midpoint),data=agg3) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=Individuals_Lower,ymax=Individuals_Upper),width=0.5) +
  scale_y_continuous(trans='log10', limits=c(1e6,1e14), breaks=c(1e6,1e7,1e8,1e9,1e10,1e11,1e12,1e13,1e14)) +
  #facet_grid(rows=vars(Clade),scales="free") +
  xlab(NULL) + ylab("Individual shrimp caught (estimated)") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  coord_flip()
g_agg3
ggsave("Graphs/g_agg3.png",g_agg3,width=5,height=8)

#catch ~ country (bar graph)
agg4 <- aggregate(Individuals_Midpoint~country,FUN=sum,data=fao.sp.2022)
agg4$Individuals_Lower <- aggregate(Individuals_Lower~country,FUN=sum,data=fao.sp.2022)$Individuals_Lower
agg4$Individuals_Upper <- aggregate(Individuals_Upper~country,FUN=sum,data=fao.sp.2022)$Individuals_Upper
g_agg4 <- ggplot(aes(x=reorder(country,Individuals_Midpoint),y=Individuals_Midpoint),data=agg4) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=Individuals_Lower,ymax=Individuals_Upper),width=0.5) +
  scale_y_continuous(trans='log10', limits=c(1e4,1e14), breaks=c(1e4,1e5,1e6,1e7,1e8,1e9,1e10,1e11,1e12,1e13,1e14)) +
  #facet_grid(rows=vars(Clade),scales="free") +
  xlab(NULL) + ylab("Individual shrimp caught (estimated)") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size=8)) +
  coord_flip()
g_agg4
ggsave("Graphs/g_agg4.png",g_agg4,width=6,height=10)

#catch ~ continent + clade
agg5 <- aggregate(Individuals_Midpoint~Continent_Group_En+Clade,FUN=sum,data=fao.sp.2022)
agg5$Individuals_Lower <- aggregate(Individuals_Lower~Continent_Group_En+Clade,FUN=sum,data=fao.sp.2022)$Individuals_Lower
agg5$Individuals_Upper <- aggregate(Individuals_Upper~Continent_Group_En+Clade,FUN=sum,data=fao.sp.2022)$Individuals_Upper
g_agg5 <- ggplot(aes(x=reorder(Continent_Group_En,Individuals_Midpoint),y=Individuals_Midpoint,colour=Clade),data=agg5) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=Individuals_Lower,ymax=Individuals_Upper,colour=Clade),width=0.5) +
  scale_y_continuous(trans='log10', limits=c(1e5,1e15), breaks=c(1e5,1e6,1e7,1e8,1e9,1e10,1e11,1e12,1e13,1e14)) +
  facet_grid(rows=vars(Clade)) +
  xlab(NULL) + ylab("Individual shrimp caught (estimated)") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  coord_flip()
g_agg5
ggsave("Graphs/g_agg5.png",g_agg5,width=6,height=6)

#catch ~ country + clade
agg6 <- aggregate(Individuals_Midpoint~country+Clade,FUN=sum,data=fao.sp.2022)
agg6$Individuals_Lower <- aggregate(Individuals_Lower~country+Clade,FUN=sum,data=fao.sp.2022)$Individuals_Lower
agg6$Individuals_Upper <- aggregate(Individuals_Upper~country+Clade,FUN=sum,data=fao.sp.2022)$Individuals_Upper
g_agg6 <- ggplot(aes(x=reorder(country,Individuals_Midpoint),y=Individuals_Midpoint,colour=Clade),data=agg6) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=Individuals_Lower,ymax=Individuals_Upper,colour=Clade),width=0.5) +
  scale_y_continuous(trans='log10') +
  facet_grid(rows=vars(Clade),scales="free") + xlab(NULL) + ylab("Individual shrimp caught (estimated)") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size=8)) +
  coord_flip()
g_agg6
ggsave("Graphs/g_agg6.png",g_agg6,width=6,height=14)

#catch ~ country (map)
library(maps)
world <- map_data("world")
world$Individuals_Midpoint_All <- NA
world$Individuals_Midpoint_Caridean <- NA
world$Individuals_Midpoint_Penaeid <- NA
world$Individuals_Midpoint_Sergestid <- NA
head(world)

#So we're only missing these countries:
agg4[-which(agg4$country %in% unique(world$region)),]

for (i in c(1:nrow(agg4))){
  country.tmp <- agg4[i,"country"]
  try(world[which(world$region==country.tmp),]$Individuals_Midpoint_All <- agg4[i,"Individuals_Midpoint"])
}

agg6.caridean <- agg6[which(agg6$Clade=="Caridean"),]
for (i in c(1:nrow(agg6.caridean))){
  country.tmp <- agg6.caridean[i,"country"]
  try(world[which(world$region==country.tmp),]$Individuals_Midpoint_Caridean <- agg6.caridean[i,"Individuals_Midpoint"])
}

agg6.penaeid <- agg6[which(agg6$Clade=="Penaeid"),]
for (i in c(1:nrow(agg6.penaeid))){
  country.tmp <- agg6.penaeid[i,"country"]
  try(world[which(world$region==country.tmp),]$Individuals_Midpoint_Penaeid <- agg6.penaeid[i,"Individuals_Midpoint"])
}

agg6.sergestid <- agg6[which(agg6$Clade=="Sergestid"),]
for (i in c(1:nrow(agg6.sergestid))){
  country.tmp <- agg6.sergestid[i,"country"]
  try(world[which(world$region==country.tmp),]$Individuals_Midpoint_Sergestid <- agg6.sergestid[i,"Individuals_Midpoint"])
}

#credit to: https://sarahpenir.github.io/r/making-maps/
g_map_all <- ggplot(data = world, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Individuals_Midpoint_All)) +
  scale_fill_distiller(palette ="YlOrBr", direction = 1, trans = "log10") +
  labs(title="Wild shrimp caught - all species",subtitle="Note: colour scale uses a logarithmic scale") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
g_map_all
ggsave("Graphs/g_map_all.png",g_map_all,width=10,height=8)

g_map_caridean <- ggplot(data = world, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Individuals_Midpoint_Caridean)) +
  scale_fill_distiller(palette ="OrRd", direction = 1, trans = "log10") +
  labs(title="Wild shrimp caught - caridean species only",subtitle="Note: colour scale uses a logarithmic scale") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
g_map_caridean
ggsave("Graphs/g_map_caridean.png",g_map_caridean,width=10,height=8)

g_map_penaeid <- ggplot(data = world, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Individuals_Midpoint_Penaeid)) +
  scale_fill_distiller(palette ="Greens", direction = 1, trans = "log10") +
  labs(title="Wild shrimp caught - penaeid species only",subtitle="Note: colour scale uses a logarithmic scale") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
g_map_penaeid
ggsave("Graphs/g_map_penaeid.png",g_map_penaeid,width=10,height=8)

g_map_sergestid <- ggplot(data = world, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Individuals_Midpoint_Sergestid)) +
  scale_fill_distiller(palette ="GnBu", direction = 1, trans = "log10") +
  labs(title="Wild shrimp caught - sergestid species only",subtitle="Note: colour scale uses a logarithmic scale") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
g_map_sergestid
ggsave("Graphs/g_map_sergestid.png",g_map_sergestid,width=10,height=8)

#Country-by-country tables
#For each country:
#Individuals total; percent of global catch; by clade
#Weight total; percent of global catch; by clade
#Species caught by individuals and weight (and percent of catch for that country)

agg7 <- aggregate(VALUE~country,FUN=sum,data=fao.sp.2022)

global.individuals <- sum(agg4$Individuals_Midpoint)
global.individuals.penaeid <- sum(agg6[which(agg6$Clade=="Penaeid"),]$Individuals_Midpoint)
global.individuals.sergestid <- sum(agg6[which(agg6$Clade=="Sergestid"),]$Individuals_Midpoint)
global.individuals.caridean <- sum(agg6[which(agg6$Clade=="Caridean"),]$Individuals_Midpoint)
global.weight <- sum(agg7$VALUE)

priority.countries <- agg4[order(-agg4$Individuals_Midpoint),]

for (i in c(1:nrow(priority.countries))){
country.tmp <- priority.countries[i,"country"]

cell1 <- format(round(agg7[which(agg7$country==country.tmp),]$VALUE,0),big.mark = ",",scientific=FALSE) #tonnes
cell2 <- round(100*agg7[which(agg7$country==country.tmp),]$VALUE/global.weight,3) #tonnes percent of world
cell3 <- agg4[which(agg4$country==country.tmp),]$Individuals_Midpoint #Individuals
cell4 <- agg4[which(agg4$country==country.tmp),]$Individuals_Lower #Individuals lower
cell5 <- agg4[which(agg4$country==country.tmp),]$Individuals_Upper #Individuals lower
cell6 <- round(100*agg4[which(agg4$country==country.tmp),]$Individuals_Midpoint/global.individuals,3) #Individuals percent of world
cell7 <- agg6[which(agg6$country==country.tmp & agg6$Clade=="Penaeid"),] #Penaeid individuals
cell8 <- agg6[which(agg6$country==country.tmp & agg6$Clade=="Sergestid"),] #Sergestid individuals
cell9 <- agg6[which(agg6$country==country.tmp & agg6$Clade=="Caridean"),] #Caridean individuals
cell10 <- round(100*agg6[which(agg6$country==country.tmp & agg6$Clade=="Penaeid"),"Individuals_Midpoint"]/
  global.individuals.penaeid,3) #Penaeid individuals % of world
cell11 <- round(100*agg6[which(agg6$country==country.tmp & agg6$Clade=="Sergestid"),"Individuals_Midpoint"]/
  global.individuals.sergestid,3) #Sergestid individuals % of world
cell12 <- round(100*agg6[which(agg6$country==country.tmp & agg6$Clade=="Caridean"),"Individuals_Midpoint"]/
  global.individuals.caridean,3) #Caridean individuals % of world

cell3.errorbars <- paste(formatC(cell3, format = "e", digits = 2),
                         " (",
                         formatC(cell4, format = "e", digits = 2),
                         ", ",
                         formatC(cell5, format = "e", digits = 2),
                         ")",sep="")

if(length(cell7)==0){cell7 <- NA}
if(length(cell8)==0){cell8 <- NA}
if(length(cell9)==0){cell9 <- NA}
if(length(cell10)==0){cell10 <- NA}
if(length(cell11)==0){cell11 <- NA}
if(length(cell12)==0){cell12 <- NA}

cell7.errorbars <- paste(formatC(cell7$Individuals_Midpoint, format = "e", digits = 2),
                         " (",
                         formatC(cell7$Individuals_Lower, format = "e", digits = 2),
                         ", ",
                         formatC(cell7$Individuals_Upper, format = "e", digits = 2),
                         ")",sep="")
cell8.errorbars <- paste(formatC(cell8$Individuals_Midpoint, format = "e", digits = 2),
                         " (",
                         formatC(cell8$Individuals_Lower, format = "e", digits = 2),
                         ", ",
                         formatC(cell8$Individuals_Upper, format = "e", digits = 2),
                         ")",sep="")
cell9.errorbars <- paste(formatC(cell9$Individuals_Midpoint, format = "e", digits = 2),
                         " (",
                         formatC(cell9$Individuals_Lower, format = "e", digits = 2),
                         ", ",
                         formatC(cell9$Individuals_Upper, format = "e", digits = 2),
                         ")",sep="")

if(nchar(cell7.errorbars)==5){cell7.errorbars <- NA}
if(nchar(cell8.errorbars)==5){cell8.errorbars <- NA}
if(nchar(cell9.errorbars)==5){cell9.errorbars <- NA}


country.df1 <- data.frame("Labels"=c("Weight (t)","Individuals","Individuals (Penaeid)","Individuals (Sergestid)","Individuals (Caridean)"),
           "Values" = c(cell1,cell3.errorbars,cell7.errorbars,cell8.errorbars,cell9.errorbars),
           "Percent_of_World"=c(cell2,cell6,cell10,cell11,cell12))
country.df1$Percent_of_World <- paste(country.df1$Percent_of_World,"%",sep=" ")
names(country.df1)[1] <- " "

country.df2 <- aggregate(Individuals_Midpoint~Scientific_Name,FUN=sum,data=fao.sp.2022[fao.sp.2022$country==country.tmp,])
country.df2$Individuals_Percent_of_Country <- round(100*country.df2$Individuals_Midpoint/sum(country.df2$Individuals_Midpoint),2)
country.df2 <- country.df2[order(-country.df2$Individuals_Percent_of_Country),]
country.df2$Individuals_Percent_of_Country <- paste(country.df2$Individuals_Percent_of_Country,"%",sep=" ")
country.df2$Individuals_Midpoint <- format(country.df2$Individuals_Midpoint,format="e",digits=2)
country.df2

table.names.top <- c(" ","Catch in 2022","Percent of World")
table.names.middle <- c("Species","Individuals Caught in 2022 (Midpoint)","Percent of Country")
names(country.df1) <- table.names.top
names(country.df2) <- table.names.top
table.combo <- rbind(country.df1,rep(" ",3),table.names.middle,country.df2)

swanky_table <- gt(table.combo)
swanky_table <- tab_style(swanky_table, style=cell_text(weight = "bold"),
                          locations = list(
                            cells_body(rows=7),
                            cells_column_labels()))
swanky_table <- cols_align(swanky_table, align = "center", columns = c(2,3))
swanky_table <- tab_source_note(swanky_table, html('<p style="font-size:10px">Brackets indicate the upper and lower ends of the confidence range, 
                                given the confidence range of the species-specific estimated mean weights as published
                                by Rethink Priorities (2023). For the second half of the table, species-specific
                                estimates are only given as the range midpoint (without the range upper and lower bounds) for visual simplicity.
                                NA indicates that no shrimp of that clade were recorded in the catch data for 2022 in that country.</p>'))

filename1 <- paste("Country Results/",country.tmp,".html",sep="")
gtsave(swanky_table, filename = filename1)
}



#########################
### Overarching table ###
#########################

overarching.table.countries <- agg4[order(-agg4$Individuals_Midpoint),]

overarching.table.countries[,c(2,3,4)] <- signif(overarching.table.countries[,c(2,3,4)],3)
overarching.table.countries$Catch_by_Number <- paste(overarching.table.countries$Individuals_Midpoint,
                                           " (",
                                           overarching.table.countries$Individuals_Lower,
                                           ",",
                                           overarching.table.countries$Individuals_Upper,
                                           ")",sep="")

overarching.table.countries$Catch_by_Weight <- NA

for (i in c(1:nrow(overarching.table.countries))){
  country.tmp <- overarching.table.countries[i,"country"]
  overarching.table.countries[i,"Catch_by_Weight"] <- agg7[which(agg7$country==country.tmp),"VALUE"]
}

overarching.table.countries$Individuals_Percent <- paste(round(100*overarching.table.countries$Individuals_Midpoint/
                                                   sum(overarching.table.countries$Individuals_Midpoint), 2),
                                                   "%",sep=" ")
overarching.table.countries$Weight_Percent <- paste(round(100*overarching.table.countries$Catch_by_Weight/
                                                           sum(overarching.table.countries$Catch_by_Weight), 2),
                                                    "%",sep=" ")
overarching.table.countries$Catch_by_Weight <- format(round(overarching.table.countries$Catch_by_Weight, digits=0),
                                                      big.mark = ",", scientific=FALSE)


overarching.table <- overarching.table.countries[c(1:30),c("country",
                                                           "Catch_by_Weight","Weight_Percent",
                                                           "Catch_by_Number","Individuals_Percent"
                                                    )]
names(overarching.table) <- c("Country","Catch by Weight (t)","Weight (Percent of World)",
                              "Catch by Number","Number (Percent of World)")


overarching.table.gt <- gt(overarching.table)
overarching.table.gt <- tab_style(overarching.table.gt, style=cell_text(weight = "bold"),
                          locations = list(
                            cells_column_labels()))
overarching.table.gt <- cols_align(overarching.table.gt, align = "center", columns = c(2,3,4,5))

gtsave(overarching.table.gt, filename = "overarching_table.html")


##################
### Trade data ###
##################

trade.agg <- aggregate(X2021 ~ Reporting.country.Name.En, FUN = sum, data = trade)
trade.agg.bycountry <- aggregate(X2021 ~ Partner.country.Name.En + Reporting.country.Name.En, FUN = sum, data = trade)
trade.agg.bycountry$X2021_percent <- NA

trade.countries <- unique(trade$Reporting.country.Name.En)

for (i in c(1:length(trade.countries))){
  country.tmp <- trade.countries[i]
  trade.agg.bycountry.tmp <- trade.agg.bycountry[which(trade.agg.bycountry$Reporting.country.Name.En==country.tmp),]
  
  sum.tmp <- sum(trade.agg.bycountry.tmp$X2021)
  trade.agg.bycountry.tmp$X2021_percent <- round(100*trade.agg.bycountry.tmp$X2021/sum.tmp,1)
  
  trade.agg.bycountry[which(trade.agg.bycountry$Reporting.country.Name.En==country.tmp),"X2021_percent"] <- trade.agg.bycountry.tmp$X2021_percent
  
}

trade.agg.bycountry.order <- trade.agg.bycountry[
  order(trade.agg.bycountry$Reporting.country.Name.En,-trade.agg.bycountry$X2021_percent),]

trade.agg.bycountry.order

write.csv(trade.agg.bycountry.order,"trade_aggregated_bycountry.csv")
write.csv(trade.agg,"trade_aggregated.csv")


