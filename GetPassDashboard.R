#' Shot Dashboard stats on players or teams
#' 
#' @param stat statistic to pull (e.g. 'Defender Distance')
#' @param type either 'player' or 'team'
#' @param id either player's ID or team's ID
#' @param opponent.id opponent team's ID
#' @return data frame of stats
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' GetPassDashboard('Pass Recieved', 'player', '201147')
#' 
GetPassDashboard <- function(stat, type = 'team', id, opponent.id = '0') {
  
  options(stringsAsFactors = FALSE)
  
  if (type == 'team') {
    url <- paste0('http://stats.nba.com/stats/teamdashptpass?',
                  'DateFrom=&',
                  'DateTo=&', 
                  'GameSegment=&', 
                  'LastNGames=0&', 
                  'LeagueID=00&', 
                  'Location=&', 
                  'MeasureType=Base&', 
                  'Month=0&', 
                  'OpponentTeamID=', opponent.id, '&', 
                  'Outcome=&',
                  'PaceAdjust=N&',
                  'PerMode=Totals&',
                  'Period=0&',
                  'PlusMinus=N&',
                  'Rank=N&',
                  'Season=2015-16&',
                  'SeasonSegment=&',
                  'SeasonType=Regular+Season&',
                  'TeamID=', id, '&',
                  'VsConference=&',
                  'VsDivision=')
    
    json <- fromJSON(file = url)[[3]]
    
    if (stat == 'Pass Made') {
      json <- json[[1]]
    }
    if(stat == 'Pass Recieved'){
      json <- json[[2]]
    }
    
    stats <- json$rowSet
    
    # Create raw data frame
    stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
    stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
    
    # Get column headers
    colnames(stats) <- json$headers
    
    stats[, -c(2, 3, 5)] <- lapply(stats[, -c(2, 3, 5)], as.numeric)
    
  } else {
    url <- paste0('http://stats.nba.com/stats/playerdashptpass?',
                  'DateFrom=&',
                  'DateTo=&', 
                  'GameSegment=&', 
                  'LastNGames=0&', 
                  'LeagueID=00&', 
                  'Location=&', 
                  'Month=0&', 
                  'OpponentTeamID=', opponent.id, '&', 
                  'Outcome=&',
                  'PerMode=Totals&',
                  'Period=0&',
                  'PlayerID=', id, '&',
                  'Season=2015-16&',
                  'SeasonSegment=&',
                  'SeasonType=Regular+Season&',
                  'TeamID=0&',
                  'VsConference=&',
                  'VsDivision=')
    
    json <- fromJSON(file = url)[[3]]
    
    
    if (stat == 'Pass Made') {
      json <- json[[1]]
    }
    if(stat == 'Pass Recieved'){
      json <- json[[2]]
    }
    
    stats <- json$rowSet
    
    # Create raw data frame
    stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
    stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
    
    
    
    # Get column headers
    colnames(stats) <- json$headers
    
    stats[, -c(2, 3, 5,7)] <- lapply(stats[, -c(2, 3, 5,7)], as.numeric)
  }
  
  return(stats)
}
