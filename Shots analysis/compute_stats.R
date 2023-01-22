#FUNCTIONS TO COMPUTE STATS

# Assign a zone to each shot
divide_shots_by_zone <- function(df){
  
  #Create variables is_C3, is_NC3 and is_2PT
  
  df <- df %>%
    mutate(is_C3 = if_else( (y <= 7.8 & (x > 22 | x < -22)),TRUE, FALSE )
    )
  
  df <- df %>%
    mutate(is_NC3 = if_else( (y > 7.8 & sqrt(x^2 + y^2) > 23.75)
                             ,TRUE, FALSE)
    )
  
  df <- df %>%
    mutate(is_2PT = if_else( !(is_C3 | is_NC3) ,TRUE, FALSE)
    )
  
  df
  
}

#Computes team number of shots attempted, made or missed per zone 
compute_num_team_shots <- function(inputdf, t_team, shot_t){
  
  
  inputdf <- filter(inputdf, team == t_team)
  
  if(shot_t == 'made') inputdf <- filter(inputdf, fgmadeLogic)
  if(shot_t == 'missed') inputdf <- filter(inputdf, !fgmadeLogic)
  
  #count num of shots per zone
  C3_shots  <- inputdf %>% count(is_C3)
  NC3_shots <- inputdf %>% count(is_NC3)
  PT2_shots <- inputdf %>% count(is_2PT)
  
  C3_shots  <- C3_shots$n[2]
  NC3_shots <- NC3_shots$n[2]
  PT2_shots <- PT2_shots$n[2]
  
  
  
  team_shots <- list(C3_shots ,NC3_shots, PT2_shots )
  team_shots
  
}
#Computes number of shots attempted, made or missed per zone by both teams
compute_num_shots <- function(inputdf, shot_t){
  
  if(shot_t == 'made') inputdf <- filter(inputdf, fgmadeLogic)
  if(shot_t == 'missed') inputdf <- filter(inputdf, !fgmadeLogic)
  
  number_shots <- inputdf %>% count(team)
  
  
  shotsA <- compute_num_team_shots(inputdf, 'Team A',shot_t)
  shotsB <- compute_num_team_shots(inputdf, 'Team B',shot_t)
  
  
  number_shots$C3 <- c(shotsA[1], shotsB[1])
  number_shots$NC3 <- c(shotsA[2], shotsB[2])
  number_shots$PT2 <- c(shotsA[3], shotsB[3])
  number_shots <- number_shots[, c(1,3,4,5,2)]
  
  number_shots
  
}



#Computes shoting percentage and efg per zone returns a list of three df
compute_stats <- function(inputdf, stats){
  
  # Number of shots attempted per zone
  number_shots <- compute_num_shots(inputdf, 'attempted')
  
  # Number of shots made per zone
  number_shots_made <- compute_num_shots(inputdf, 'made')
  
  
  zones = c('C3', 'NC3', 'PT2')
  
  fgm = -10000
  threesM = -10000
  fga = -10000
  for(zone in zones){
    
    # Compute shot percentage of each zone
    shot_percentage = as.numeric(unlist(number_shots[zone])) / as.numeric(unlist(number_shots['n']))
    stats[paste('shot%_', zone, sep='')] = shot_percentage * 100
    
    # Compute effective field goal percentage
    # efg% = (FGM + 0.5*3PM)/ FGA
    
    if(zone =='C3'|zone =='NC3'){
      fgm = as.numeric(unlist(number_shots_made[zone]))
      threesM = as.numeric(unlist(number_shots_made[zone]))
      fga = as.numeric(unlist(number_shots[zone]))
      
    }
    else{
      fgm = as.numeric(unlist(number_shots_made[zone]))
      threesM = 0
      fga = as.numeric(unlist(number_shots[zone]))
      
    }
    
    efg_percentage = ((fgm + 0.5*threesM)/fga)*100
    stats[paste('eFG%_', zone, sep='')] = efg_percentage
  }
  
  fgm = as.numeric(unlist(number_shots_made['n']))
  threesM = as.numeric(unlist(number_shots_made['C3'])) + as.numeric(unlist(number_shots_made['NC3']))
  fga = as.numeric(unlist(number_shots['n']))
  
  stats[paste('eFG%_', 'total', sep='')] = ((fgm + 0.5*threesM)/fga)*100
  
  
  
  return(list(attempted = number_shots, made = number_shots_made, stats = stats))
  
}

# creates shooting stats
create_shot_stats <- function(inputdf){
  
  #Initialize output df
  columns = c("team", "shot%_C3" ,"shot%_NC3","shot%_PT2",
              "eFG%_C3","eFG%_NC3","eFG%_PT2")
  
  stats = data.frame(matrix(nrow = 2, ncol = 0))
  stats[columns] <- NA
  stats$team <- c('Team A', 'Team B')
  
  #Compute Stats
  tablesdf <- compute_stats(inputdf, stats)
  
  tablesdf
  
}

# Assign to each shot their zone stats
shots_add_stats <- function(df, stats){
  
  df <- df %>%
    mutate( efg = case_when(
      is_C3   & team == 'Team A'  ~ stats[1,'eFG%_C3' ] ,
      is_NC3  & team == 'Team A'  ~ stats[1,'eFG%_NC3'],
      is_2PT  & team == 'Team A'  ~ stats[1,'eFG%_PT2'],
      is_C3   & team == 'Team B'  ~ stats[2,'eFG%_C3' ],
      is_NC3  & team == 'Team B'  ~ stats[2,'eFG%_NC3'],
      is_2PT  & team == 'Team B'  ~ stats[2,'eFG%_PT2']
      
    )
    
    )
  
  df <- df %>%
    mutate( usage = case_when(
      is_C3   & team == 'Team A'  ~ stats[1,'shot%_C3' ] ,
      is_NC3  & team == 'Team A'  ~ stats[1,'shot%_NC3'],
      is_2PT  & team == 'Team A'  ~ stats[1,'shot%_PT2'],
      is_C3   & team == 'Team B'  ~ stats[2,'shot%_C3' ],
      is_NC3  & team == 'Team B'  ~ stats[2,'shot%_NC3'],
      is_2PT  & team == 'Team B'  ~ stats[2,'shot%_PT2']
      
    )
    
    )
  
  df
  
}