
vio.int <- function(
    tmp = acad,
    gplot = interhabs.b,#[c(10,3,4,12,11,6,1,9,14,7,8,13,2,5)],
    yc = "CCSmax",
    yu = NA,
    yd = 0,
    maxw = 0.4,
    ry = NA,
    thresh = 14,
    cols = NA,
    sortx = F,
    reverseplot = F,
    grp.name = "",
    ht = 7,
    wdth = 12,
    sepline = NA,
    addallsp = FALSE,
    colyaxis = F,
    png.out = T,
    eps.out = F,
    pdf.out = F,
    labs = ""
){
  
  transp.func <- function(cl = "", trans = 0.5){
    transp <- 255*trans
    cl1 <- rgb(red = col2rgb(cl)[1,1],green = col2rgb(cl)[2,1], 
               blue = col2rgb(cl)[3,1],alpha = transp,maxColorValue = 255)
    return(cl1)
  }
  
  if(eps.out){png.out <- F} 
  if(pdf.out){png.out <- F} 
  x = 1:length(gplot)
  
  yt = as.numeric(tmp[,yc])
  if(is.na(yu)) yu <- max(yt,na.rm = T)
  if (is.na(ry)) ry <- diff(range(yt,na.rm = T))+1
  if(is.na(cols[1])){
    n.col <- min(11,ry)
    cols = rev(brewer.pal(n.col,"RdYlBu"))
    if(n.col < ry){  #### this function will only work if ry < 21
      dfc <- ry-n.col
      remc <- n.col-dfc
      lowc <- floor(remc/2)+1
      highc <- n.col-ceiling(remc/2)
      cols <- c(cols[c(1:(lowc-1))],
                rep(cols[lowc:highc],each = 2),
                cols[c((highc+1):n.col)])
    }
    # plot(1:length(cols),col = cols,pch = 20)
  }#end colour definition 
  
  
  
  
  if(png.out){  
    png(res = 400,file = paste(yc,grp.name,"violin plot.png"),
        height = ht*300,
        width = wdth*300)
    
    par(mar = c(11,5,3,1),
        las = 1,
        xpd = F)
  }
  if(eps.out){
    setEPS()
    postscript(file = paste(yc,grp.name,"violin plot.eps"),
               title = paste(yc,grp.name,"violin plot.eps"),
               height = ht,
               width = wdth)
    
    par(mar = c(11,5,3,1),
        las = 1,
        xpd = F)
    
  }
  if(pdf.out){
    pdf(file = paste(yc,grp.name,"violin plot.pdf"),
        height = ht,
        width = wdth)
    
    par(mar = c(11,5,3,1),
        las = 1,
        xpd = F,
        cex.lab = 1)
    
  }
  
  
  if(reverseplot){
    plot(x = x,y = rep(max(yt,na.rm = T),length(x)),ylim = rev(c(min(yt,na.rm = T)-1,max(yt,na.rm = T)+1)),
         xlim = c(0,max(x)+0.5),
         col = "white",xaxt = "n",xaxs = "i",
         yaxt = "n",yaxs = "i",xlab = "",ylab = "Conservation Concern",
         main = "",
         col.axis = grey(0.5),
         bty = "n"#paste("distribution of",yc)
    )
  }else{
    plot(x = x,y = rep(max(yt,na.rm = T),length(x)),ylim = c(min(yt,na.rm = T)-1,max(yt,na.rm = T)+1),
         xlim = c(0.5,max(x)+0.5),
         col = "white",xaxt = "n",xaxs = "i",
         yaxt = "n",yaxs = "i",xlab = "",ylab = "Conservation Concern",
         main = "",
         col.axis = grey(0.5),
         bty = "n"#paste("distribution of",yc)
    )
  }
  if(!is.na(sepline)){
    lines(y = rev(c(min(yt,na.rm = T)-1,max(yt,na.rm = T)+1)),x = rep(which(gplot == sepline)+0.5,2),col = "grey",lwd = 2)
  }  
  tya <- data.frame(scr = c(min(yt,na.rm = T):max(yt,na.rm = T)))   
  ymaxx <- 0 
  nsp <- rep(0,length(gplot))
  names(nsp) <- gplot
  means.y <- rep(0,length(gplot))
  names(means.y) <- gplot
  pwt <- means.y
  for(p in x){
    gx = gplot[p]
    tmp1 = tmp[which(tmp[,gx] == 1),]
    y = tmp1[,yc] 
    ytab <- table(y)
    #for(ct in unique(tmp1[,yc])){
    
    #}
    means.y[gx] <- mean(y,na.rm = T)
    pwt[gx] <- sum(ytab[as.character(c(14:20))],na.rm = T)/sum(ytab,na.rm = T)
    for(j in tya$scr){
      if(is.na(ytab[as.character(j)])){
        tya[which(tya$scr == j),paste0(gx,"n.species")] <- 0
        tya[which(tya$scr == j),paste0(gx,"p.species")] <- 0
        tya[which(tya$scr == j),paste0(gx," names.species")] <- NA  
      }else{
        tya[which(tya$scr == j),paste0(gx,"n.species")] <- ytab[as.character(j)]
        tya[which(tya$scr == j),paste0(gx,"p.species")] <- ytab[as.character(j)]/sum(ytab)
        tya[which(tya$scr == j),paste0(gx," names.species")] <- paste(tmp1[which(tmp1[,yc] == j),"Common.Name"],collapse = "; ")
        
        
      }
      
    }#j
    ymaxx <- max(ymaxx,max(tya[,paste0(gx,"p.species")]))
    nsp[p] <- sum(tya[,paste0(gx,"n.species")])
  }#p - end of dataprep
  
  xexp <- (maxw*2)/ymaxx
  
  
  if(addallsp){  #### all species dataprep
    #### #### #### #### #### #### 
    #### #### #### #### #### #### 
    gx = "All Species"
    y = tmp[,yc] 
    ytab <- table(y)
    tyall <- data.frame(scr = c(4:20))   
    
    #for(ct in unique(tmp1[,yc])){
    
    #}
    means.yall <- mean(y,na.rm = T)
    pwt.all <- sum(ytab[as.character(c(14:20))])/sum(ytab)
    
    for(j in tyall$scr){
      if(is.na(ytab[as.character(j)])){
        tyall[which(tyall$scr == j),paste0(gx,"n.species")] <- 0
        tyall[which(tyall$scr == j),paste0(gx,"p.species")] <- 0
        tyall[which(tyall$scr == j),paste0(gx," names.species")] <- NA  
      }else{
        tyall[which(tyall$scr == j),paste0(gx,"n.species")] <- ytab[as.character(j)]
        tyall[which(tyall$scr == j),paste0(gx,"p.species")] <- ytab[as.character(j)]/sum(ytab)
        tyall[which(tyall$scr == j),paste0(gx," names.species")] <- paste(tmp1[which(tmp1[,yc] == j),"Common.Name"],collapse = "; ")
        
        
      }
      
      #### #### #### #### #### #### 
      #### #### #### #### #### #### 
      
    } 
    
  }## end if addallsp
  
  abline(v = rep(x,each = 2)+(xexp*(c(-0.05,0.05))),
         col = grey(0.9),lty = 2,lwd = c(1))
  
  #   text(x = rep(x[1],2)+xexp*(c(-0.05,0.05)),
  #        y = 3.5,
  #        c("10%"),
  #        col = grey(0.7),
  #        srt = 90,
  #        cex = 0.6)  
  
  #    abline(v = rep(x,each = 4)+(xexp*(c(-0.05,-0.125,0.05,0.125))),
  #           col = grey(0.9),lty = 2,lwd = c(1,2,1,2))
  # 
  #    text(x = rep(x[1],2)+xexp*(c(-0.05,-0.125,0.05,0.125)),
  #         y = 3.5,
  #         c("10%","25%"),
  #         col = grey(0.7),
  #         srt = 90,
  #         cex = 0.6)
  if(sortx){
    #   sortorder <- rev(order(pwt))
    #   pwt <- pwt[sortorder]
    #   nsp <- nsp[sortorder]
    
    sortorder <- rev(order(means.y))
    means.y <- means.y[sortorder]
    nsp <- nsp[sortorder]
    if(labs[1] == ""){labs <- names(means.y)}else{
      labs <- labs[sortorder]}
    
  }  
  for(p in x){
    gx = names(means.y)[p]
    #    gx = names(pwt)[p]
    
    abline(v = p,
           col = grey(0.7),lty = 1,lwd = c(1))
    
    
    #  lines(x = c(p,p),
    #        y = c(3.5,20.5),
    #        col = grey(0.7),
    #        lty = 3,
    #        lwd = 1) #grey centre line for each plot
    
    for(y in tya[,"scr"]){
      yw = (tya[which(tya$scr == y),paste0(gx,"p.species")]/2)*xexp
      if(yw == 0){next}
      polygon(x = c(p-yw,p+yw,p+yw,p-yw),
              y = c(rep(y-0.5,2),rep(y+0.5,2)),
              col = cols[y-3],border = NA)#,
      #lwd = 0.5)
      
    }    
    if(addallsp){  
      gx2 <-  "All Species"
      xl <- 1
      xr <- 1
      yl<- 1
      yr<- 1
      j <- 1
      for(y in tyall[,"scr"]){
        yw = (tyall[which(tyall$scr == y),paste0(gx2,"p.species")]/2)*xexp
        xl[j:(j+1)] = c(p-yw,p-yw)
        xr[j:(j+1)] = c(p+yw,p+yw)
        yl[j:(j+1)] = c(y-0.5,y+0.5)
        yr[j:(j+1)] = c(y-0.5,y+0.5)
        j <- j+2
      }    
      polygon(x = c(xl,rev(xr)),
              y = c(yl,rev(yr)),
              #col = transp.func("white",1),
              border = grey(0.8),
              lwd = 0.5)   
    }#end if addallsp    
    
    
  }#p end of polygon plotting  
  lines(y = rep(thresh-0.5,2),x = c(0,max(x)+0.5),col = "black")
  lines(y = rep(9-0.5,2),x = c(0,max(x)+0.5),col = "black")
  mtext(side = 4, at = c(4:20),as.character(4:20), las = 3,cex = 0.5,col = grey(0.5))
  
  if(png.out){
    mtext(side = 1,at = x,paste0(labs," (",nsp,",",round(means.y,1),")"),cex = 0.8, 
          line = 0.5,las = 2)
    #     mtext(side = 1,at = x,paste0(names(pwt)," (",nsp,")"),cex = 0.8, 
    #           line = 0.5,las = 2)
    #axis(side = 2,at = tya$scr)
    mtext(side = 2,text = c("Low","Moderate","High"),at = c(6,11,17),las = 3,
          line = 1.3,cex = 0.9)
    
  }else{  
    mtext(side = 1,at = x,paste0(labs," (",nsp,",",round(means.y,1),")"),cex = 0.7, 
          line = 0.5,las = 2)
    #     mtext(side = 1,at = x,paste0(names(pwt)," (",nsp,")"),cex = 0.7, 
    #           line = 0.5,las = 2)
    #axis(side = 2,at = tya$scr)
    mtext(side = 2,text = c("Low","Moderate","High"),at = c(6,11,17),las = 3,
          line = 1.5,cex = 1.3)
  }
  
  if(colyaxis){
    for(j in 1:length(cols)){
      y = j+3
      polygon(x = c(0,0.25,0.25,0),
              y = c(y-0.5,y-0.5,y+0.5,y+0.5),
              col = cols[j],
              border = NA)
      lines(x = c(-0.02,0.27),
            y = c(y-0.5,y-0.5),
            col = grey(0.3))
      lines(x = c(0.27,-0.02),
            y = c(y+0.5,y+0.5),
            col = grey(0.3))
    }
  }
  
  
  dev.off()
  
  write.csv(tya,paste0(yc,grp.name," violin plot data.csv"))
  
}#end vio.int

