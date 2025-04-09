## plotting the acad summaries
## 
## 

library(tidyverse)
library(readxl)
library(colorscience)
library(RColorBrewer)
library(ggstats)
library(scico)
library(patchwork)


re_run_data_prep <- FALSE

if(re_run_data_prep){
# install.packages("devtools")
# devtools::install_github("thomasp85/scico")
# ACAD globals ------------------------------------------------------------
# x <- "aa%.( /#) -bb"
name_rep <- function(x){
    x <- str_replace_all(x,pattern = "\\%","percent")
    x <- str_replace_all(x,pattern = "[[:punct:]]+","_")
     x <- str_replace_all(x,pattern = "[[:space:]]+","_")
     x <- str_replace_all(x,pattern = "_+","_")
     x <- str_to_lower(x)
}
acad1 <- read_xlsx("data/ACAD Global 2024.05.23.xlsx",
                  sheet = "Globals",
                  .name_repair = name_rep)


acad <- acad1 %>% 
  mutate(primary_breeding_habitat = str_replace_all(primary_breeding_habitat," Aerial",""),
         primary_nonbreeding_habitat = str_replace_all(primary_nonbreeding_habitat," Aerial",""),
         primary_breeding_habitat_major = str_extract(primary_breeding_habitat,
                                                      pattern = ".+(?=\\:)"),
         primary_breeding_habitat_sub = str_trim(str_extract(primary_breeding_habitat,
                                                      pattern = "(?<=\\:).+")),
         primary_breeding_habitat_major = ifelse(primary_breeding_habitat_major == "Forest","Forests",primary_breeding_habitat_major),
         
         primary_nonbreeding_habitat_major = str_extract(primary_nonbreeding_habitat,
                                                      pattern = ".+(?=\\:)"),
         primary_nonbreeding_habitat_major = ifelse(primary_nonbreeding_habitat_major == "Forest","Forests",primary_nonbreeding_habitat_major),
         
         primary_nonbreeding_habitat_sub = str_trim(str_extract(primary_nonbreeding_habitat,
                                                             pattern = "(?<=\\:).+")),
         primary_nonbreeding_habitat_sub = ifelse(grepl("eneralist",primary_nonbreeding_habitat_sub),
                                                  "Generalist",primary_nonbreeding_habitat_sub),
         primary_breeding_habitat_sub = ifelse(grepl("eneralist",primary_breeding_habitat_sub),
                                                  "Generalist",primary_breeding_habitat_sub),
         status = continental_importance,
         status = ifelse(grepl(continental_importance,pattern = "\\;"),
                         str_extract(status, pattern = ".+(?=\\;)"),
                         status),
         status = ifelse(grepl(pattern = "Prev",status),
                         NA,status),
         #status = ifelse(!grepl(status,pattern = "Watch") | is.na(status),"",status),
         status_alt = str_trim(str_extract(continental_importance,
                                                             pattern = "(?<=\\;).+")),
         tipping_point = ifelse(status_alt == "Tipping Point","/Tipping-Point",""),
         status = paste0(status,tipping_point),
         status = str_replace(status,"NA",replacement = ""),
         status = ifelse(status == "NA" | is.na(status),"No specific concern status",status),
         status = factor(status,
                         levels = c("No specific concern status",
                                    "Common Bird in Steep Decline",
                                    "Yellow Watch List",
                                    "Yellow Watch List/Tipping-Point",
                                    "Orange Watch List/Tipping-Point",
                                    "Red Watch List/Tipping-Point"),
                         ordered = TRUE),
         continental_importance = status,
         group = factor(group,levels = c("landbird","shorebird",
                                         "waterbird","waterfowl"),
                        labels = c("Landbirds","Shorebirds",
                                   "Waterbirds","Waterfowl"),
                        ordered = TRUE)
         
  )

tst <- acad %>% 
  filter(is.na(ccs_max))

acad <- acad %>% 
  filter(!is.na(ccs_max))

saveRDS(acad,"data/cleaned_acad_global_2024_05_23.rds")

}else{
  acad <- readRDS("data/cleaned_acad_global_2024_05_23.rds")
}


# palettes ----------------------------------------------------------------

colset2 <- rev(scico(10, palette = 'romaO'))[c(3,5,6,7,8,9)]
names(colset2) <- levels(acad$status)



# high level habitat summaries --------------------------------------------
source("functions/histogram_function.R")
# above is custom function histo_function()

all <- histo_function()

png("primary_breeding_habitat_ccsmax_summary.png",
    width = 15,
    height = 30,
    units = "cm",
    res = 300)
print(all)
dev.off()

all2 <- histo_function(grp = "primary_nonbreeding_habitat_major",
                       ylabl = "")



habitats <- all + all2 + plot_layout(guides = "collect")
pdf("figures/Habitat_summaries.pdf",
    height = 7,
    width = 11)
print(habitats)
dev.off()

sub <- histo_function(sub_grp = "primary_breeding_habitat_sub",
                      ylimu = 0.4)
sub2 <- histo_function(grp = "primary_nonbreeding_habitat_major",
                       sub_grp = "primary_nonbreeding_habitat_sub",
                       ylabl = "",
                       ylimu = 0.4)


forests <- sub[["Forests"]] + sub2[["Forests"]] + plot_layout(guides = "collect")
pdf("figures/Forest_summaries.pdf",
    height = 7,
    width = 11)
print(forests)
dev.off()



all3 <- histo_function(grp = "group",ylimu = 0.25)
pdf("figures/Group_summaries.pdf",
    height = 5,
    width = 7)
print(all3)
dev.off()


pdf("figures/Summary_of_large_groups.pdf",
    width = 8.5,
    height = 9)

print(all)
print(sub[["Forests"]])
print(all2)
print(sub2[["Forests"]])
print(all3)
dev.off()

# 

# Figure 4 ACAD vs IUCN ----------------------------------------------------------


# plotting the % of species listed on watchlist vs IUCN
#


wl_levls <- levels(acad$continental_importance)

iucn_levels <- data.frame(iucn_red_list_2023 = c("EW",
                                                 "CR (PE)",
                                                 "CR",
                                                 "EN",
                                                 "VU",
                                                 "NT"),
                          iucn = str_wrap( width = 25, exdent = 2,c("Extinct in the Wild",
                                   "Critically Endangered/Possibly Extinct",
                                   "Critically Endangered/Possibly Extinct",
                                   "Endangered",
                                   "Vulnerable",
                                   "Near Threatened")))
rl_levls <- unique(iucn_levels$iucn)


all_levels <- data.frame(listing = factor(str_wrap( width = 25, exdent = 2,c("Common Bird in Steep Decline",
                                    "Near Threatened",
                                    "Yellow Watch List",
                                    "Yellow Watch List/Tipping-Point",
                                    "Vulnerable",
                                    "Orange Watch List/Tipping-Point",
                                    "Endangered",
                                    "Red Watch List/Tipping-Point",
                                    "Critically Endangered/Possibly Extinct",
                                    "Extinct in the Wild")),
                                    levels = str_wrap( width = 25, exdent = 2,c("Common Bird in Steep Decline",
                                               "Near Threatened",
                                               "Yellow Watch List",
                                               "Yellow Watch List/Tipping-Point",
                                               "Vulnerable",
                                               "Orange Watch List/Tipping-Point",
                                               "Endangered",
                                               "Red Watch List/Tipping-Point",
                                               "Critically Endangered/Possibly Extinct",
                                               "Extinct in the Wild")),
                                    ordered = TRUE))

colset3 <- rev(scico(10, palette = 'romaO'))[c(5,5,6,7,8,8,9,9,10)]
colset3 <- c(colset3,"#000000")

names(colset3) <- levels(all_levels$listing)

## combine levels into a single column with manual colour palette
## 
## 
## 
## 


watch_red <- acad %>% 
  select(common_name,canada,usa,mexico,c_america,
         continental_importance,iucn_red_list_2023) %>% 
  left_join(iucn_levels,by = "iucn_red_list_2023") %>% 
  rowwise() %>% 
  mutate(wl = ifelse(continental_importance %in% 
                       all_levels$listing,
                     "listed",NA),
         rl = ifelse(iucn_red_list_2023 %in% c("VU","EN","CR","EW","CR (PE)","NT"),
                     "listed",NA),
         usa_canada = ifelse(canada == 1 | usa == 1,
                             1,NA),
         mexico = ifelse(mexico == 1,1,NA),
         c_america = ifelse(c_america == 1,
                            1,NA),
         continental_importance = factor(str_wrap(width = 25,exdent = 2,continental_importance),
                                         levels = levels(all_levels$listing),
                                         ordered = TRUE),
         iucn = factor(iucn,
                       levels = levels(all_levels$listing),
                       ordered = TRUE)) %>% 
  select(-c(canada,usa,iucn_red_list_2023)) %>% 
  pivot_longer(cols = c(usa_canada,mexico,c_america),
               names_to = "region",
               values_drop_na = TRUE) %>% 
  #filter(!is.na(rl) | !is.na(wl)) %>% 
  select(-c(value,wl,rl)) %>% 
  pivot_longer(cols = c(iucn,continental_importance),
               values_to = "listed",
               names_to = "list") 

#total number of species included in the region
n_by_reg <- watch_red %>%
  select(region,common_name) %>% 
  distinct() %>% 
  group_by(region) %>% 
  summarise(n_region = n())



wlrl_data <- watch_red %>%
  group_by(region,list,listed) %>% 
  summarise(n_listed = n()) %>% 
  left_join(n_by_reg) %>% 
  mutate(p_listed = 100*(n_listed/n_region),
         Region = factor(region,levels = c("usa_canada","mexico","c_america"),
                         labels = c("USA & Canada (717)","Mexico (1060)","Central America (1160)"),
                         ordered = TRUE),
         list = ifelse(list == "iucn","IUCN","ACAD")) %>% 
  filter(!is.na(listed))

#valuse of n_sp from this table used to add 
#total number of species listed to facet labels
n_by_listing <- wlrl_data %>% 
  group_by(list,region) %>%
  summarise(n_sp = sum(n_listed))
  

wlrl_data_iucn <- wlrl_data %>% 
  filter(list == "IUCN") %>% 
  mutate(Region = factor(region,levels = c("usa_canada","mexico","c_america"),
                         labels = c("USA & Canada (92)","Mexico (118)","Central America (83)"),
                         ordered = TRUE))

wlrl_data_acad <- wlrl_data %>% 
  filter(list == "ACAD")%>% 
  mutate(Region = factor(region,levels = c("usa_canada","mexico","c_america"),
                         labels = c("USA & Canada (233)","Mexico (458)","Central America (548)"),
                         ordered = TRUE))


wlrl_iucn <- ggplot(data = wlrl_data_iucn,
               aes(x = list,fill = listed,y = p_listed),
               na.rm = TRUE)+
  geom_bar(stat = "identity", width = 0.5)+
  scale_y_continuous(name = "Percent of breeding avifauna",
                     breaks = seq(0,50,by = 20),
                     labels = ~paste0(.x,"%"),
                     limits = c(0,50))+
  scale_x_discrete(name = "")+
  scale_discrete_manual(aesthetics = c("colour","fill"),
                        values = colset3,
                        name = "IUCN Listing")+
  facet_wrap(vars(Region), ncol = 1,
             strip.position = "top",
             labeller =  label_wrap_gen(16))+
  theme_bw()+
  theme(text = element_text(size = 12),
        legend.position = "top",
        legend.location = "plot",
        legend.justification = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        legend.spacing.x = unit(1,units = "mm"), 
        legend.key.size = unit(3,units = "mm"),
        legend.key.spacing.x = unit(0.1, units = "mm"),
        legend.box.just = "left",
        strip.text = element_text(size = 9),
        axis.ticks.length.x = unit(0.1,units = "mm"),
        axis.text.y = element_text(size = 9),
        panel.spacing.x = unit(0.1,units = "mm"),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

wlrl_iucn


wlrl_acad <- ggplot(data = wlrl_data_acad,
                    aes(x = list,fill = listed,y = p_listed),
                    na.rm = TRUE)+
  geom_bar(stat = "identity",width = 0.5)+
  scale_y_continuous(name = "",
                     breaks = seq(0,50,by = 20),
                     #minor_breaks = seq(0,50,by = 10),
                     labels = ~paste0(.x,"%"),
                     limits = c(0,50))+
  scale_x_discrete(name = "")+
  scale_discrete_manual(aesthetics = c("colour","fill"),
                        values = colset3,
                        name = "ACAD Listing")+
  facet_wrap(vars(Region), ncol = 1,
             strip.position = "top",
             labeller =  label_wrap_gen(16))+
  theme_bw()+
  theme(text = element_text(size = 12),
        legend.position = "top",
        legend.location = "plot",
        legend.justification = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        legend.spacing.x = unit(1,units = "mm"), 
        legend.key.size = unit(3,units = "mm"),
        legend.key.spacing.x = unit(0.1, units = "mm"),
        strip.text = element_text(size = 9),
        axis.ticks.length.x = unit(0.1,units = "mm"),
        axis.text.y = element_text(size = 9),
        panel.spacing.x = unit(0.1,units = "mm"),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

wlrl_acad

wlrl <- wlrl_iucn + wlrl_acad
pdf("figures/listing_comparison.pdf",
    width = 3.5,
    height = 7)

print(wlrl)
dev.off()





# same but on count scale instead of % ------------------------------------





wlrl_iucn <- ggplot(data = wlrl_data_iucn,
                    aes(x = list,fill = listed,y = n_listed),
                    na.rm = TRUE)+
  geom_bar(stat = "identity", width = 0.5)+
  scale_y_continuous(name = "Number of species",
                     #breaks = seq(0,50,by = 20),
                     #labels = ~paste0(.x,"%"),
                     limits = c(0,600))+
  scale_x_discrete(name = "")+
  scale_discrete_manual(aesthetics = c("colour","fill"),
                        values = colset3,
                        name = "IUCN Listing")+
  facet_wrap(vars(Region), ncol = 1,
             strip.position = "top",
             labeller =  label_wrap_gen(16))+
  theme_bw()+
  theme(text = element_text(size = 12),
        legend.position = "top",
        legend.location = "plot",
        legend.justification = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        legend.spacing.x = unit(1,units = "mm"), 
        legend.key.size = unit(3,units = "mm"),
        legend.key.spacing.x = unit(0.1, units = "mm"),
        legend.box.just = "left",
        strip.text = element_text(size = 9),
        axis.ticks.length.x = unit(0.1,units = "mm"),
        axis.text.y = element_text(size = 9),
        panel.spacing.x = unit(0.1,units = "mm"),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

wlrl_iucn


wlrl_acad <- ggplot(data = wlrl_data_acad,
                    aes(x = list,fill = listed,y = n_listed),
                    na.rm = TRUE)+
  geom_bar(stat = "identity",width = 0.5)+
  scale_y_continuous(name = "",
                     #breaks = seq(0,50,by = 20),
                     #minor_breaks = seq(0,50,by = 10),
                     #labels = ~paste0(.x,"%"),
                     limits = c(0,600))+
  scale_x_discrete(name = "")+
  scale_discrete_manual(aesthetics = c("colour","fill"),
                        values = colset3,
                        name = "ACAD Listing")+
  facet_wrap(vars(Region), ncol = 1,
             strip.position = "top",
             labeller =  label_wrap_gen(16))+
  theme_bw()+
  theme(text = element_text(size = 12),
        legend.position = "top",
        legend.location = "plot",
        legend.justification = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        legend.spacing.x = unit(1,units = "mm"), 
        legend.key.size = unit(3,units = "mm"),
        legend.key.spacing.x = unit(0.1, units = "mm"),
        strip.text = element_text(size = 9),
        axis.ticks.length.x = unit(0.1,units = "mm"),
        axis.text.y = element_text(size = 9),
        panel.spacing.x = unit(0.1,units = "mm"),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))

wlrl_acad

wlrl <- wlrl_iucn + wlrl_acad
pdf("figures/listing_comparison_count.pdf",
    width = 3.5,
    height = 7)

wlrl
dev.off()

