getSummoner <- function(SummonerName) {
  GET(url = URLencode(iconv(paste0("https://kr.api.riotgames.com/lol/summoner/v4/summoners/by-name/",
                                   SummonerName), to = "UTF-8")),
      add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% 
    content()
}