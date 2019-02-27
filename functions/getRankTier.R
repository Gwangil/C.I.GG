getRankTier <- function(SummonerName) {
  GET(url = URLencode(iconv(paste0("https://kr.api.riotgames.com/lol/league/v4/positions/by-summoner/",
                                   getSummoner(SummonerName)$id), to = "UTF-8")),
      add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% 
    content()
}