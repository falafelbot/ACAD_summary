

ft_mean <- function(x,thresh = 13){
  #return(length(which(as.integer(x)> thresh))/length(x))
  return(mean(as.integer(x)))
}



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

