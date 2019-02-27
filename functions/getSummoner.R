# Ref.) https://developer.riotgames.com/api-methods/#summoner-v4/GET_getBySummonerName

# Get represents a summoner
# @param summonerName
# Return type : list
getSummoner <- function(summonerName) {
  GET(url = URLencode(iconv(paste0("https://kr.api.riotgames.com/lol/summoner/v4/summoners/by-name/",
                                   summonerName), to = "UTF-8")),
      add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% 
    content()
}