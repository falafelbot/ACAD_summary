## plotting the acad summaries
## 
## 

library(tidyverse)
library(readxl)
library(colorscience)
library(RColorBrewer)

# ACAD globals ------------------------------------------------------------
# x <- "aa%.( /#) -bb"
name_rep <- function(x){
    x <- str_replace_all(x,pattern = "\\%","percent")
    x <- str_replace_all(x,pattern = "[[:punct:]]+","_")
     x <- str_replace_all(x,pattern = "[[:space:]]+","_")
     x <- str_replace_all(x,pattern = "_+","_")
     x <- str_to_lower(x)
}
acad <- read_xlsx("data/ACAD Global 2024.05.23.xlsx",
                  sheet = "Globals",
                  .name_repair = name_rep)


acad <- acad %>% 
  mutate(primary_breeding_habitat_major = str_extract(primary_breeding_habitat,
                                                      pattern = ".+(?=\\:)"),
         primary_breeding_habitat_sub = str_trim(str_extract(primary_breeding_habitat,
                                                      pattern = "(?<=\\:).+")),
         primary_nonbreeding_habitat_major = str_extract(primary_nonbreeding_habitat,
                                                      pattern = ".+(?=\\:)"),
         primary_nonbreeding_habitat_sub = str_trim(str_extract(primary_nonbreeding_habitat,
                                                             pattern = "(?<=\\:).+")))
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

# comparable palette from color brewer
colset2 <- rev(brewer.pal(11,"RdYlBu"))[c(1:5,6,7,7,8,8,9,10,10,11,11,11,11)]
names(colset2) <- as.character(4:20)

ft_mean <- function(x){
  return(mean(as.integer(x)))
}
# high level habitat summaries --------------------------------------------

histo_function <- function(df = acad,
               grp = "primary_breeding_habitat_major",
               sub_grp = NULL){
  
  if(is.null(sub_grp)){
    df[,"group_maj"] <- df[,grp]
  dfs <- df %>% 
    group_by(group_maj,ccs_max) %>% 
    summarise(n = n(),
              .groups = "drop") %>% 
    ungroup() %>% 
    mutate(base = 0,
           ccs_max = factor(ccs_max,
                      levels = as.character(4:20),
                      ordered = TRUE)) %>% 
    mutate(group_maj = fct_reorder(.f = group_maj,
                                   .x = ccs_max,
                                   .fun = ft_mean))
  
  titl <- str_to_title(str_replace_all(grp,pattern = "_",replacement = " "))
  
  }else{
    df[,"group_maj"] <- df[,grp]
    df[,"group_sub"] <- df[,sub_grp]
    dfs <- df %>% 
      group_by(group_maj,group_sub,ccs_max) %>% 
      summarise(n = n(),
                .groups = "drop") %>% 
      ungroup() %>% 
      mutate(base = 0,
             ccs_max = factor(ccs_max,
                              levels = as.character(4:20),
                              ordered = TRUE))
  }
  
  if(is.null(sub_grp)){
  high1 <- ggplot(data = dfs,
                  aes(y = n,
                      x = ccs_max,
                      colour = ccs_max))+
    geom_vline(xintercept = c(8.5-3,13.5-3),
               colour = grey(0.8))+
    geom_errorbar(aes(ymin = base,ymax = n),
                  width = 0,
                  linewidth = 10)+
    scale_discrete_manual(aesthetics = "colour",
                          values = colset2)+
    facet_wrap(facets = vars(group_maj),
               ncol = 1,
               scales = "free_y",
               strip.position = "top")+
    theme_minimal()+
    xlim(levels(dfs$ccs_max))+
    labs(title = titl)+
    ylab("Number of Species")+
    theme(legend.position = "none",
          strip.text = element_text(size = 12))
  
  }else{
    
    high1 <- vector("list",length(unique(df$group_maj)))
    names(high1) <- unique(df$group_maj)
    
    if(any(is.na(names(high1)))){
      high1 <- high1[-which( is.na(names(high1)))]
    }
    for(jj in names(high1)){
      dfs1 <- dfs %>% 
        filter(group_maj == jj) %>% 
        mutate(group_sub = fct_reorder(.f = group_sub,
                                       .x = ccs_max,
                                       .fun = ft_mean))
      
      
      
      titl <- paste(jj)
      high1[[jj]] <- ggplot(data = dfs1,
                      aes(y = n,
                          x = ccs_max,
                          colour = ccs_max))+
        geom_vline(xintercept = c(8.5-3,13.5-3),
                   colour = grey(0.8))+
        geom_errorbar(aes(ymin = base,ymax = n),
                      width = 0,
                      linewidth = 10)+
        scale_discrete_manual(aesthetics = "colour",
                              values = colset2)+
        facet_wrap(facets = vars(group_sub),
                   ncol = 1,
                   scales = "free_y",
                   strip.position = "top")+
        xlim(levels(dfs1$ccs_max))+
        theme_minimal()+
        labs(title = titl)+
        ylab("Number of Species")+
        theme(legend.position = "none",
              strip.text = element_text(size = 12))
      
      
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

sub <- histo_function(sub_grp = "primary_breeding_habitat_sub")

pdf("figures/primary_breeding_habitat_summary.pdf",
    width = 8.5,
    height = 11)

print(all)
print(sub)
dev.off()



all <- histo_function(grp = "primary_nonbreeding_habitat_major")
sub <- histo_function(grp = "primary_nonbreeding_habitat_major",
                      sub_grp = "primary_nonbreeding_habitat_sub")

pdf("figures/primary_nonbreeding_habitat_summary.pdf",
    width = 8.5,
    height = 11)

print(all)
print(sub)
dev.off()

# data selection ----------------------------------------------------------




