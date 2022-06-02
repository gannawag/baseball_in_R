library(baseballr) #https://billpetti.github.io/baseballr/
library(data.table)

####GET PBP and MANAGER INFO####

#will need this to add team IDs using team names
(teams_lu_table_dt <- data.table(teams_lu_table))

#make an empty list that will be populated with data tables
#where each element is a data table of play by play data for a given GAME
game_dt_list = list() 

#loop over all dates in a given date range
(DATERANGE <- seq.Date(as.Date("2019-06-01"),as.Date("2019-06-30"), by="days")) 
for (GAMEDATE in DATERANGE){
  GAMEDATE <- as.Date(GAMEDATE, origin='1970-01-01') #need to coerce to date format
  print(paste0("gamedate: ", GAMEDATE))
  
  #empty list that will be populated with play by play data tables, 
  #where each element is a data table of play by play data for a given GAME
  dt_list <- list()
  for (X in mlb_game_pks(GAMEDATE)$game_pk){ #mlb_game_pks(DATE)$game_pk gives game IDs 
    print(paste0("game pk: ", X))
    dt <- as.data.table(get_pbp_mlb(X)) #get pbp data for given game ID
    dt_list[[X]] <- dt #this list contains all pbp data for games on a given date
  }
  #this data table has all pbp data for all games on the given date
  (dt <- rbindlist(dt_list, fill=T)) 
  
  #add team id, using team name from lookup table loaded above
  setkey(teams_lu_table_dt, name)
  setkey(dt, home_team)
  dt[teams_lu_table_dt, home_team_id := id]
  setkey(dt, away_team)
  dt[teams_lu_table_dt, away_team_id := id]
  
  #get manager info - home team
  for (X in unique(dt$home_team_id)){
    print(paste0("home_team_id: ", X))
    
    dt[home_team_id==eval(X), #limit to one home team at a time
       c("home_manager_full_name","home_manager_id") := #get manager name and id
         data.table( #convert to data.table
           mlb_rosters( #this is the function that gets roster info
             team_id = X, #the team id
             season = year(GAMEDATE), #the season
             roster_type = 'coach', #limit to coaches
             date = GAMEDATE) #just this one date
           )[job=="Manager" #limit to only the manager (not the pitching coach etc)
             ][,.(person_full_name,person_id)] #the two variables of interest 
       ]
  }
  #get manager info - away team
  for (X in unique(dt$away_team_id)){
    print(paste0("away_team_id: ",X))
    dt[away_team_id==eval(X), #limit to one home team at a time
       c("away_manager_full_name","away_manager_id") := #get manager name and id
         data.table( #convert to data.table
           mlb_rosters( #this is the function that gets roster info
             team_id = X, #the team id
             season = year(GAMEDATE), #the season
             roster_type = 'coach', #limit to coaches
             date = GAMEDATE) #just this one date
         )[job=="Manager" #limit to only the manager (not the pitching coach etc)
         ][,.(person_full_name,person_id)] #the two variables of interest 
    ]
  }

  game_dt_list[[GAMEDATE]] <- dt #now dt has all the info for the given date
}

(dt <- rbindlist(game_dt_list, fill=T)) #bind all dates together into one master data table
dim(dt)
dt[,.N, by = game_date]
dt[,.N, by = home_team_id]
(setorder(dt[,.N, by = matchup.pitcher.fullName], -N))
(setorder(dt[,.N, by = .(matchup.pitcher.fullName, matchup.batter.fullName)], -N))
