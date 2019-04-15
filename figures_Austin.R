#####################################################################################################################
## Figures for Austin
## 
## DATE CREATED: 4/11/2019
## AUTHOR: Paul Carvalho  
##
#####################################################################################################################

rm(list=ls()) # Clear workspace before running code

# DIRECTORIES -------------------------------------------------------------------------------------------------------
setwd("C:/Users/pgcar/Google Drive/Paul Carvalho")

# LIBRARIES ---------------------------------------------------------------------------------------------------------
library(readxl)
library(plyr)
library(dplyr)
library(xlsx)
library(ggplot2)
library(RColorBrewer)
library(plotrix)

# IMPORT DATA --------------------------------------------------------------------------------------------------------------------
# Fish
all_fish_df <- read_excel("INDO DATA/fish/MASTER_fish_data.xlsx", sheet=1, col_names=TRUE)
all_benthic_df <- read_excel("INDO DATA/benthic/MASTER_benthic_data.xlsx", sheet=1, col_names=TRUE)


# SITE_NAMES ---------------------------------------------------------------------------------------------------------------------
site_names_df <- data_frame(site_ID = all_benthic_df$site_ID,
                            site_name = all_benthic_df$site_name)
site_names_df <- unique(site_names_df)
site_names_df$site_ID <- gsub("subs","fish",site_names_df$site_ID)
all_fish_df <- join(all_fish_df, site_names_df, by="site_ID")

# FUNCTIONAL GROUPS --------------------------------------------------------------------------------------------------------------
all_fish_df$functional_group[which(all_fish_df$functional_group == "Obligate and facultative coral feeder")] <- "Corallivore"
all_fish_df$functional_group[which(all_fish_df$functional_group == "benthic invertivore" |
                                   all_fish_df$functional_group == "Sessile invertebrate feeder")] <- "Invertivore"
all_fish_df$functional_group[which(all_fish_df$functional_group == "omnivore")] <- "Omnivore"
all_fish_df$functional_group[which(all_fish_df$functional_group == "carnivore")] <- "Piscivore"
all_fish_df$functional_group[which(all_fish_df$functional_group == "detritivore")] <- "Detritivore"
all_fish_df$functional_group[which(all_fish_df$functional_group == "planktivore")] <- "Planktivore"
all_fish_df$functional_group[which(all_fish_df$functional_group == "browser" |
                                   all_fish_df$functional_group == "grazer" |
                                   all_fish_df$functional_group == "scraper" |
                                   all_fish_df$functional_group == "large excavator" |
                                   all_fish_df$functional_group == "small excavator" |
                                   all_fish_df$functional_group == "grazer/detritivore" |
                                   all_fish_df$functional_group == "herbivore")] <- "Herbivore"
all_fish_df$functional_group[which((all_fish_df$family == "Lutjanidae" |
                                    all_fish_df$family == "LUTJANIDAE") &
                                    all_fish_df$species != "Aprion virescens")] <- "Omnivore"
all_fish_df$functional_group[which(all_fish_df$species == "Aprion virescens")] <- "Piscivore"
all_fish_df$functional_group[which(all_fish_df$family == "Lethrinidae" |
                                   all_fish_df$family == "LETHRINIDAE")] <- "Omnivore"
all_fish_df$functional_group[which(all_fish_df$family == "Haemulidae" |
                                   all_fish_df$family == "HAEMULIDAE")] <- "Omnivore"
# remove some families due to inflating the total biomass
all_fish_df <- all_fish_df[-(which(all_fish_df$family == "Carangidae" | all_fish_df$family == "CARANGIDAE")),]
all_fish_df <- all_fish_df[-(which(all_fish_df$family == "Pomacentridae" | all_fish_df$family == "POMACENTRIDAE")),]
all_fish_df <- all_fish_df[-(which(all_fish_df$family == "Caesionidae" | all_fish_df$family == "CAESIONIDAE")),]
all_fish_df <- all_fish_df[-(which(all_fish_df$family == "Carcharhinidae")),]
all_fish_df <- all_fish_df[-(which(all_fish_df$family == "Scombridae")),]
all_fish_df <- all_fish_df[-(grep("Pseudanthias", all_fish_df$species)),]

# ERROR BARS ---------------------------------------------------------------------------------------------------------------------
# create unique transect_ID considering site, transect, and observer.
all_fish_df$transect_ID <- paste(all_fish_df$site_ID, all_fish_df$transect, all_fish_df$observer, sep="")
# calculate biomass for all fishes
all_fish_df$a <- as.numeric(all_fish_df$a)
all_fish_df$biomass_kg <- ((all_fish_df$a * (all_fish_df$size_cm ^ all_fish_df$b)) * all_fish_df$abundance) * 0.001
# sum biomass for all transects
all_biomass_per_transect <- ddply(all_fish_df, .(transect_ID, site_ID, site_name, region), function(df)c(sum(df$biomass_kg, na.rm=TRUE)))
names(all_biomass_per_transect)[5] <- "biomass_kg"
# mean biomass for each functional group at each site
all_biomass_per_site <- ddply(all_biomass_per_transect, .(site_ID, site_name, region), function(df)c(mean(df$biomass_kg),
                                                                                                     std.error(df$biomass_kg)))
names(all_biomass_per_site)[4] <- "biomass_kg"
all_biomass_per_site$biomass_kg_ha <- all_biomass_per_site$biomass_kg * 40
all_biomass_per_site$std_error <- all_biomass_per_site$V2 * 40
# remove sombano and na at the end
all_biomass_per_site <- all_biomass_per_site[-(which(all_biomass_per_site$site_name == "Sombano")),]
all_biomass_per_site <- all_biomass_per_site[-(which(is.na(all_biomass_per_site$site_ID))),]
# separate data by region
ra_all_biomass_per_site <- filter(all_biomass_per_site, region=="raja_ampat")
ra_all_biomass_per_site <- ra_all_biomass_per_site[,c(1,6,7)]
names(ra_all_biomass_per_site)[2] <- "all_kg_ha"
wa_all_biomass_per_site <- filter(all_biomass_per_site, region=="wakatobi")
wa_all_biomass_per_site <- wa_all_biomass_per_site[,c(1,6,7)]
names(wa_all_biomass_per_site)[2] <- "all_kg_ha"
lo_all_biomass_per_site <- filter(all_biomass_per_site, region=="lombok")
lo_all_biomass_per_site <- lo_all_biomass_per_site[,c(1,6,7)]
names(lo_all_biomass_per_site)[2] <- "all_kg_ha"
# Set pristine biomass as mean of top three sites (Mambetron [015_fish], Swandarek [001_fish], and Yenbuba 1 [005_fish])
#raja ampat
B0_ra <- filter(all_biomass_per_transect, site_name=="Mambetron")
B0_ra$biomass_kg_ha <- B0_ra$biomass_kg*40
B0_mean_ra <- mean(B0_ra$biomass_kg_ha)
B0_se_ra <- std.error(B0_ra$biomass_kg_ha)
#wakatobi
B0_wa <- filter(all_biomass_per_transect, site_name=="Tomia Atoll 1")
B0_wa$biomass_kg_ha <- B0_wa$biomass_kg*40
B0_mean_wa <- mean(B0_wa$biomass_kg_ha)
B0_se_wa <- std.error(B0_wa$biomass_kg_ha)
#lombok
B0_lo <- filter(all_biomass_per_transect, site_name=="Trawangan 2")
B0_lo$biomass_kg_ha <- B0_lo$biomass_kg*40
B0_mean_lo <- mean(B0_lo$biomass_kg_ha)
B0_se_lo <- std.error(B0_lo$biomass_kg_ha)
# Bmmsy limits
Bmmsy <- data.frame(region = c("Raja Ampat","Wakatobi","Lombok"),
                    xmin = c(0.5,0.5,0.5),
                    xmax = c(20.5,19.5,18.5),
                    ymin = c((0.25*B0_mean_ra),(0.25*B0_mean_wa),(0.25*B0_mean_lo)),
                    ymax = c((0.5*B0_mean_ra),(0.5*B0_mean_wa),(0.5*B0_mean_lo)))

# STACKED BAR --------------------------------------------------------------------------------------------------------------------
# sum biomass for each functional group on all transects
biomass_per_transect <- ddply(all_fish_df, .(transect_ID, functional_group, site_ID, site_name, region), function(df)c(sum(df$biomass_kg, na.rm=TRUE)))
names(biomass_per_transect)[6] <- "biomass_kg"
biomass_per_transect <- biomass_per_transect[-(which(is.na(biomass_per_transect$functional_group))),]
# mean biomass for each functional group at each site
biomass_per_site <- ddply(biomass_per_transect, .(site_ID, functional_group, site_name, region), function(df)c(mean(df$biomass_kg)))
names(biomass_per_site)[5] <- "biomass_kg"
biomass_per_site$biomass_kg_ha <- biomass_per_site$biomass_kg * 40
# remove Sombano
biomass_per_site <- biomass_per_site[-(which(biomass_per_site$site_name == "Sombano")),]
# reorder functional groups
biomass_per_site$functional_group <- factor(biomass_per_site$functional_group, levels=c("Piscivore",
                                                                                        "Omnivore",
                                                                                        "Corallivore",
                                                                                        "Invertivore",
                                                                                        "Planktivore",
                                                                                        "Detritivore",
                                                                                        "Herbivore"))
# separate data by region
ra_biomass_per_site <- filter(biomass_per_site, region=="raja_ampat")
wa_biomass_per_site <- filter(biomass_per_site, region=="wakatobi")
lo_biomass_per_site <- filter(biomass_per_site, region=="lombok")
# merge dataframes
ra_df <- join(ra_biomass_per_site, ra_all_biomass_per_site, by="site_ID")
wa_df <- join(wa_biomass_per_site, wa_all_biomass_per_site, by="site_ID")
lo_df <- join(lo_biomass_per_site, lo_all_biomass_per_site, by="site_ID")
all_df <- rbind(ra_df,wa_df,lo_df)
all_df$region <- gsub("raja_ampat","Raja Ampat",all_df$region)
all_df$region <- gsub("wakatobi","Wakatobi",all_df$region)
all_df$region <- gsub("lombok","Lombok",all_df$region)
all_df$region <- factor(all_df$region, levels = c("Raja Ampat","Wakatobi","Lombok"))


# MACROALGAE ~ BIOMASS -----------------------------------------------------------------------------------------------------------
# calculate mean herbivore biomass at each site
mean_herb_biomass <- filter(biomass_per_site, functional_group=="Herbivore")

# calculate mean macroalgal cover at each site
all_benthic_df$hard_soft <- gsub("makro","macro",all_benthic_df$hard_soft)
all_benthic_df$genus <- gsub("Makro","Macro",all_benthic_df$genus)

# create unique transect_ID considering site, transect, and observer
all_benthic_df$transect_ID <- paste(all_benthic_df$site_name, all_benthic_df$transect, all_benthic_df$observer)

# create genus for macroalgae in lombok data
all_benthic_df$hard_soft[which(all_benthic_df$region=="lombok" & all_benthic_df$lifeform=="HA")] <- "macro algae"
all_benthic_df$genus[which(all_benthic_df$region=="lombok" & all_benthic_df$lifeform=="HA")] <- "Macro algae"
all_benthic_df$hard_soft[which(all_benthic_df$region=="lombok" & all_benthic_df$lifeform=="MA")] <- "macro algae"
all_benthic_df$genus[which(all_benthic_df$region=="lombok" & all_benthic_df$lifeform=="MA")] <- "Macro algae"
all_benthic_df$hard_soft[which(all_benthic_df$region=="lombok" & all_benthic_df$lifeform=="TA")] <- "macro algae"
all_benthic_df$genus[which(all_benthic_df$region=="lombok" & all_benthic_df$lifeform=="TA")] <- "Macro algae"
all_benthic_df$hard_soft[which(all_benthic_df$region=="raja_ampat" & all_benthic_df$lifeform=="MA")] <- "macro algae"
all_benthic_df$genus[which(all_benthic_df$region=="raja_ampat" & all_benthic_df$lifeform=="MA")] <- "Macro algae"
all_benthic_df$hard_soft[which(all_benthic_df$region=="raja_ampat" & all_benthic_df$lifeform=="DCA")] <- "macro algae"
all_benthic_df$genus[which(all_benthic_df$region=="raja_ampat" & all_benthic_df$lifeform=="DCA")] <- "Macro algae"
all_benthic_df$hard_soft[which(all_benthic_df$region=="raja_ampat" & all_benthic_df$lifeform=="PA")] <- "macro algae"
all_benthic_df$lifeform[which(all_benthic_df$genus=="Macro algae" & all_benthic_df$lifeform=="SC")] <- "MA"
all_benthic_df$lifeform[which(all_benthic_df$genus=="Halimeda" & all_benthic_df$lifeform=="CE")] <- "HA"
all_benthic_df$value <- 1

# count macroalgae along a transect and take the mean for each site
sum_MA <- function(df){
  total_MA <- NA
  if(df$region=="raja_ampat"){
    total_MA <- length(which(df$lifeform=="MA" | df$lifeform=="HA" | df$lifeform=="PA"))
  }
  else if(df$region=="wakatobi"){
    total_MA <- length(which(df$lifeform=="AA" | df$lifeform=="HA" | df$lifeform=="MA" | df$lifeform=="TA"))
  } else {
    total_MA <- length(which(df$lifeform=="HA" | df$lifeform=="MA" | df$lifeform=="TA" | df$lifeform=="FA"))
  }
  return(total_MA)
} # function to count the number of macroalgae values on a transect
mean_ma_transect <- ddply(all_benthic_df, .(transect_ID, site_name, region), function(df)c(sum_MA(df)))
mean_ma_site <- ddply(mean_ma_transect, .(site_name, region), function(df)c(mean(df$V1)))

# remove Sombano
mean_ma_site <- mean_ma_site[-(which(mean_ma_site$site_name=="Sombano")),]
names(mean_ma_site)[3] <- "macroalgae"

# join dataframes
ma_herb_df <- join(mean_herb_biomass,mean_ma_site,by="site_name")
ma_herb_df <- ma_herb_df[,-7] 

# PLOTS --------------------------------------------------------------------------------------------------------------------------
# Biomass by site and functional groups - faceted by region ====
# reference rectangle for 0.25-0.5 B0
rect_df <- data.frame(x1=c(1),x2=c(2),y1=c(1),y2=c(2))
p1 <- ggplot(all_df, aes(x=reorder(site_name,-biomass_kg_ha), y=biomass_kg_ha, fill=functional_group)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand=c(0,100)) +
  coord_flip() +
  facet_wrap(~region,ncol=2,scales="free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.line.x = element_line(colour = 'black', size=0.7, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.7, linetype='solid')) +
  ylab("Biomass (kg/ha)") +
  xlab("") +
  geom_errorbar(aes(ymin=all_kg_ha-std_error, ymax=all_kg_ha+std_error)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.6, 0.1), legend.justification = c(1, 0)) +
  scale_fill_manual(values = c("red3","orangered","orange2","yellow2","purple","blue","darkgreen")) +
  theme(strip.text=element_text(face="bold"),
        strip.background=element_rect(fill="lightgrey", colour="black",size=1)) +
  geom_hline(yintercept=1150, linetype="dashed", color="black", size=0.75)+
  geom_rect(data=Bmmsy, aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey50", alpha=0.6)
p1


# Biomass by site and functional groups - one graph ====
tmp <- reorder(all_df$site_name,-all_df$biomass_kg_ha)
tmp <- levels(tmp)
tmp1 <- data_frame(site_name = tmp)
tmp2 <- data_frame(site_name = all_df$site_name,
                   region = all_df$region)
tmp <- join(tmp1,tmp2,by="site_name",match="first")
tmp$a <- NA
for(i in 1:length(tmp$site_name)){
  if(tmp$region[i] == "Raja Ampat"){
    tmp$a[i] <- "steelblue"
  } else if(tmp$region[i] == "Wakatobi"){
    tmp$a[i] <- "goldenrod"
  } else {
    tmp$a[i] <- "red"
  }
}

p2 <- ggplot(all_df, aes(x=reorder(site_name,-biomass_kg_ha), y=biomass_kg_ha, fill=functional_group)) +
  geom_bar(stat="identity") +
  theme_classic() +
  coord_flip() +
  theme(axis.line.x = element_line(colour = 'black', size=0.7, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.7, linetype='solid')) +
  ylab("Biomass (kg/ha)") +
  xlab("") +
  theme(axis.line.x = element_line(colour = 'black', size=0.7, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.7, linetype='solid')) +
  theme(axis.text.y = element_text(colour = tmp$a)) +
  geom_errorbar(aes(ymin=all_kg_ha-std_error, ymax=all_kg_ha+std_error)) +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values = c("red3","orangered","orange2","yellow2","purple","blue","darkgreen")) +
  geom_rect(aes(xmin=0.5,xmax=58,ymin=(B0_mean_ra*0.25),ymax=(B0_mean_ra*0.5)),fill="grey50",alpha=0.01) +
  scale_y_continuous(expand=c(0,0), limits=c(0,4000)) +
  geom_hline(yintercept=1150, linetype="dashed", color="black", size=0.75)
p2

# Macroalgae as a function of herbivore biomass ====
p3 <- ggplot(data=ma_herb_df, aes(x=biomass_kg_ha, y=macroalgae)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Herbivore Biomass (kg/ha)", y="Macroalgae (% cover)") +
  theme_classic()
p3







