
#FUNCTIONS TO PLOT THE COURT

# Construct_court creates the list of dataframes 
# with the coordinates of each element in the court

construct_court <- function() {
  
  
  ###outside box:
  outside_box <- data.frame(
    x=c(-25,-25,25,25,-25),
    y=c(0,47,47,0,0),
    type = 'outside box'
    
  )
  
  ###solid FT semicircle above FT line:
  solid_FT <- data.frame(
    x=c(-6000:(-1)/1000,1:6000/1000),
    y=c(19+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2)),
    type = 'solid ft'
  )
  
  ###dashed FT semicircle below FT line:
  dashed_FT <- data.frame(
    x=c(-6000:(-1)/1000,1:6000/1000),
    y=c(19-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2)),
    type = 'dashed ft'
  )
  
  ###key:
  key <- data.frame(
    x=c(-8,-8,8,8,-8),
    y=c(0,19,19,0,0),
    type = 'key'
  )
  
  ###box inside the key:
  box_inKey <- data.frame(
    x=c(-6,-6,6,6,-6),
    y=c(0,19,19,0,0),
    type = 'box in key'
  )
  ###restricted area semicircle:
  restrictedArea <- data.frame(
    x=c(-4000:(-1)/1000,1:4000/1000),
    y=c(5.25+sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2)),
    type = 'restricted area'
  )
  
  ###halfcourt semicircle:
  halfcourt_semic <- data.frame(
    x=c(-6000:(-1)/1000,1:6000/1000),
    y=c(47-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2)),
    type = 'halfcourt semic'
  )
  
  ###rim:
  rim <- data.frame(
    x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),
    y=c(c(5.25+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(5.25-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2))),
    type = 'rim'
  )
  
  ###backboard:
  backboard <- data.frame(
    x=c(-3,3),
    y=c(4,4),
    type = 'backboard'
  )
  
  ###three-point line:
  threeP <- data.frame(
    x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),
    y=c(0,169/12,5.25+sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),169/12,0),
    type = 'threeP'
  )
  
  
  
  court <- list(outside_box     = outside_box,
                key             = key,
                box_inKey       = box_inKey,
                
                solid_FT        = solid_FT,
                restrictedArea  = restrictedArea,
                halfcourt_semic = halfcourt_semic,
                rim             = rim,
                threeP          = threeP,
                
                dashed_FT       = dashed_FT,
                backboard       = backboard
                
                )
  
  
  
  
  court
  
}

# -----------------------------------------------------------------------------

# Plot_court

plot_court <- function(title){
  
  court    <- construct_court()
  
  plot <- ggplot(data=data.frame(x=1,y=1),aes(x,y))+ ggtitle(title)+ theme(plot.title = element_text(size = 14, hjust = 0.5, face= 'bold'))+
    
    #Draw court
    ## Half-court adapted from (Edward Kupfer) https://gist.github.com/edkupfer/6354404
    
    #boxes
    
    geom_path(data= court$outside_box                   , colour = 'black')+
    geom_path(data= court$key                           , colour = 'black')+
    geom_path(data= court$box_inKey                     , colour = 'black')+
    
    #circles
    geom_path(data = court$solid_FT        ,aes(x=x,y=y), colour = 'black')+
    geom_path(data = court$restrictedArea  ,aes(x=x,y=y), colour = 'black')+
    geom_path(data = court$halfcourt_semic ,aes(x=x,y=y), colour = 'black')+
    geom_path(data = court$rim             ,aes(x=x,y=y), colour = 'black')+
    geom_path(data = court$threeP          ,aes(x=x,y=y), colour = 'black')+
    
    #special lines
    geom_path(data= court$dashed_FT ,aes(x=x,y=y),linetype='dashed', colour = 'black')+
    geom_path(data= court$backboard ,lineend='butt', colour = 'black')+
    
    ###fix aspect ratio to 1:1
    coord_fixed()
  
  plot
  
}






