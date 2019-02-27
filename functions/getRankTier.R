# Ref.) https://developer.riotgames.com/api-methods/#league-v4/GET_getAllLeaguePositionsForSummoner

# Get summoner's rank game position(tier)
# @param summonerName
# Return type : list
# note) This function requires 'getSummoner' function first.
getRankTier <- function(summonerName) {
  GET(url = URLencode(iconv(paste0("https://kr.api.riotgames.com/lol/league/v4/positions/by-summoner/",
                                   getSummoner(summonerName)$id), to = "UTF-8")),
      add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% 
    content()
}