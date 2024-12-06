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

# palettes ----------------------------------------------------------------

### original palette used in 2016
# cmykmat <- read.csv("data/ColourScale.csv")
# cmykmat <- cmykmat/100
# 
# cmymat <- colorscience::CMYK2CMY(cmykmat)
# rgbmat <- colorscience::CMY2RGB(cmymat)
# 
# colset <- colorscience::rgb(red = pmin(255,rgbmat[,"R"]),
#               green = pmin(255,rgbmat[,"G"]),
#               blue = pmin(255,rgbmat[,"B"]),
#               maxColorValue = 255)

# comparable palette from scico
 
# colset2 <- scico(11, palette = 'romaO')[c(1:5,6,7,7,8,8,9,10,10,11,11,11,11)]
# names(colset2) <- as.character(4:20)
# col_names <- levels(acad$continental_importance)
# colset2 <- rev(scico(16, palette = 'romaO'))[c(4,8,10,11,13,15)]
# names(colset2) <- col_names

colset2 <- rev(scico(10, palette = 'romaO'))[c(3,5,6,7,8,9)]
names(colset2) <- levels(acad$status)

# # comparable palette from color brewer
# colset2 <- rev(brewer.pal(11,"RdYlBu"))[c(1:5,6,7,7,8,8,9,10,10,11,11,11,11)]
# names(colset2) <- as.character(4:20)
# 
# 
# col_names <- levels(acad$continental_importance)
# colset2 <- rev(brewer.pal(11,"RdYlBu"))[c(4,6,7,8,10,11)]
# names(colset2) <- col_names

ft_mean <- function(x,thresh = 13){
  #return(length(which(as.integer(x)> thresh))/length(x))
  return(mean(as.integer(x)))
}


# high level habitat summaries --------------------------------------------

histo_function <- function(df = acad,
               grp = "primary_breeding_habitat_major",
               sub_grp = NULL,
               scaling = "fixed",
               ylabl = "Percent of Species",
               ylimu = 0.4){
  

  if(is.null(sub_grp)){
    df[,"group_maj"] <- df[,grp]
  dfs <- df %>% 
    # group_by(group_maj,ccs_max) %>% 
    # summarise(n = n(),
    #           .groups = "drop") %>% 
    # ungroup() %>% 
    mutate(base = 0,
           ccs_max = factor(ccs_max,
                      levels = as.character(4:20),
                      ordered = TRUE)) 
  
  titl <- str_to_title(str_replace_all(grp,pattern = "_",replacement = " "))
  titl <- str_replace(titl, pattern = " Major", replacement = "")
  }else{
    df[,"group_maj"] <- df[,grp]
    df[,"group_sub"] <- df[,sub_grp]
    dfs <- df %>% 
      # group_by(group_maj,group_sub,ccs_max) %>% 
      # summarise(n = n(),
      #           .groups = "drop") %>% 
      ungroup() %>% 
      mutate(base = 0,
             ccs_max = factor(ccs_max,
                              levels = as.character(4:20),
                              ordered = TRUE))
    
    titl <- str_to_title(str_replace_all(grp,pattern = "_",replacement = " "))
    titl <- str_replace(titl, pattern = " Major", replacement = "")
    
  }
  
  dfs <- dfs %>% 
    filter(!is.na(group_maj))
  
  if(is.null(sub_grp)){
    
    nsp <- dfs %>% 
      group_by(group_maj) %>% 
      summarise(nsps = n())
    
    dfs <- dfs %>% 
      left_join(.,nsp,by = "group_maj") %>% 
      mutate(group_maj = paste0(group_maj," (",nsps,")"),
             group_maj = fct_reorder(.f = group_maj,
                                     .x = ccs_max,
                                     .fun = ft_mean))
    
  high1 <- ggplot(data = dfs,
                  aes(y = after_stat(prop),
                      x = ccs_max,
                      by = 1,
                      fill = continental_importance))+
    geom_vline(xintercept = c(8.5-3,13.5-3),
               colour = grey(0.8))+
    # geom_errorbar(aes(ymin = base,ymax = n),
    #               width = 0,
    #               linewidth = 10)+
    scale_y_continuous(breaks = c(0,0.2,0.4),labels = scales::percent,
                       limits = c(0,ylimu)) +
    geom_bar(stat = "prop",
             width = 1)+
    scale_discrete_manual(aesthetics = c("colour","fill"),
                          values = colset2,
                          name = "Continental Importance")+
    facet_wrap(facets = vars(group_maj),
               ncol = 1,
               scales = scaling,
               strip.position = "top")+
    theme_minimal()+
    xlim(levels(dfs$ccs_max))+
    labs(title = titl)+
    ylab(ylabl)+
    xlab("Continental Combined Score")+
    theme(legend.position = "right",
          text = element_text(size = 12),
          strip.text = element_text(size = 12),
          panel.spacing.x=unit(0, "lines"))
  
  #high1
  
  }else{
    
    high1 <- vector("list",length(unique(df$group_maj)))
    names(high1) <- unique(df$group_maj)
    
    if(any(is.na(names(high1)))){
      high1 <- high1[-which( is.na(names(high1)))]
    }
    for(jj in names(high1)){
      dfs1 <- dfs %>% 
        filter(group_maj == jj)
      
      nsp <- dfs1 %>% 
        group_by(group_sub) %>% 
        summarise(nsps = n())
      
      dfs1 <- dfs1 %>% 
        left_join(.,nsp,by = "group_sub") %>% 
        mutate(group_sub = paste0(group_sub," (",nsps,")"),
               group_sub = fct_reorder(.f = group_sub,
                                       .x = ccs_max,
                                       .fun = ft_mean))
      
      
      
      titl1 <- paste(jj,titl)
      high1[[jj]] <- ggplot(data = dfs1,
                            aes(y = after_stat(prop),
                                x = ccs_max,
                                by = 1,
                                fill = continental_importance))+
        geom_vline(xintercept = c(8.5-3,13.5-3),
                   colour = grey(0.8))+
        # geom_errorbar(aes(ymin = base,ymax = n),
        #               width = 0,
        #               linewidth = 10)+
        scale_y_continuous(breaks = c(0,0.2,0.4),labels = scales::percent,
                           limits = c(0,ylimu)) +
        geom_bar(stat = "prop",
                 width = 1)+
        scale_discrete_manual(aesthetics = c("colour","fill"),
                              values = colset2,
                              name = "Continental Importance")+
        facet_wrap(facets = vars(group_sub),
                   ncol = 1,
                   scales = scaling,
                   strip.position = "top")+
        theme_minimal()+
        xlim(levels(dfs1$ccs_max))+
        labs(title = titl1)+
        ylab(ylabl)+
        xlab("Continental Combined Score")+
        theme(legend.position = "right",
              text = element_text(size = 12),
              strip.text = element_text(size = 12),
              panel.spacing.x=unit(0, "lines"))
      
      
    }
    
  }
  
 return(high1)
  
}



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



all3 <- histo_function(grp = "group")


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
# 
# pdf("figures/primary_nonbreeding_habitat_summary.pdf",
#     width = 8.5,
#     height = 11)
# 
# print(all2)
# print(sub)
# dev.off()

# ACAD vs IUCN ----------------------------------------------------------


# plotting the % of species listed on watchlist vs IUCN
#
wl_levls <- levels(acad$continental_importance)

iucn_levels <- data.frame(iucn_red_list_2023 = c("EW",
                                                 "CR (PE)",
                                                 "CR",
                                                 "EN",
                                                 "VU",
                                                 "NT"),
                          iucn = c("Extinct in the Wild",
                                   "Critically Endangered or Possibly Extinct",
                                   "Critically Endangered or Possibly Extinct",
                                   "Endangered",
                                   "Vulnerable",
                                   "Near Threatened"))
rl_levls <- unique(iucn_levels$iucn)


all_levels <- data.frame(listing = factor(c("Common Bird in Steep Decline",
                                    "Near Threatened",
                                    "Yellow Watch List",
                                    "Yellow Watch List/Tipping-Point",
                                    "Vulnerable",
                                    "Orange Watch List/Tipping-Point",
                                    "Endangered",
                                    "Red Watch List/Tipping-Point",
                                    "Critically Endangered or Possibly Extinct",
                                    "Extinct in the Wild"),
                                    levels = c("Common Bird in Steep Decline",
                                               "Near Threatened",
                                               "Yellow Watch List",
                                               "Yellow Watch List/Tipping-Point",
                                               "Vulnerable",
                                               "Orange Watch List/Tipping-Point",
                                               "Endangered",
                                               "Red Watch List/Tipping-Point",
                                               "Critically Endangered or Possibly Extinct",
                                               "Extinct in the Wild"),
                                    ordered = TRUE))

colset3 <- rev(scico(10, palette = 'romaO'))[c(5,6,6,7,7,8,9,9,10)]
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
         continental_importance = factor(continental_importance,
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
                         labels = c("USA & Canada","Mexico","Central America"),
                         ordered = TRUE),
         list = ifelse(list == "iucn","IUCN","ACAD")) %>% 
  filter(!is.na(listed))

wlrl_data_iucn <- wlrl_data %>% 
  filter(list == "IUCN")

wlrl_data_acad <- wlrl_data %>% 
  filter(list == "ACAD")


wlrl_iucn <- ggplot(data = wlrl_data_iucn,
               aes(x = list,fill = listed,y = p_listed),
               na.rm = TRUE)+
  geom_bar(stat = "identity")+
  scale_y_continuous(name = "Percent of species listed",
                     breaks = seq(0,50,by = 20),
                     labels = ~paste0(.x,"%"),
                     limits = c(0,50))+
  scale_x_discrete(name = "")+
  scale_discrete_manual(aesthetics = c("colour","fill"),
                        values = colset3,
                        name = "IUCN Listing")+
  facet_wrap(vars(Region), ncol = 1,
             strip.position = "top")+
  theme_bw()+
  theme(text = element_text(size = 12),
        legend.position = "top",
        legend.direction = "vertical",
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key.size = unit(3,units = "mm"),
        strip.text = element_text(size = 7),
        axis.ticks.length.x = unit(0.1,units = "mm"),
        axis.text.y = element_text(size = 6))

wlrl_iucn


wlrl_acad <- ggplot(data = wlrl_data_acad,
                    aes(x = list,fill = listed,y = p_listed),
                    na.rm = TRUE)+
  geom_bar(stat = "identity")+
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
             strip.position = "top")+
  theme_bw()+
  theme(text = element_text(size = 12),
        legend.position = "top",
        legend.direction = "vertical",
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key.size = unit(3,units = "mm"),
        strip.text = element_text(size = 7),
        axis.ticks.length.x = unit(0.1,units = "mm"),
        axis.text.y = element_text(size = 6))

wlrl_acad

wlrl <- wlrl_iucn + wlrl_acad
pdf("figures/listing_comparison.pdf",
    width = 4,
    height = 7)

wlrl
dev.off()


