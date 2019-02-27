# ing...

getMatchHistory <- function(SummonerName,
                       champion = NULL,
                       queue = NULL, # 420 : Solo Rank, 900 : URF
                       season = NULL, # 13 : Season 2019, 12 : PreSeason 2019
                       endIndex = NULL,
                       beginIndex = NULL) {
  api_url <- "https://kr.api.riotgames.com/lol/match/v4/matchlists/by-account/"
  summonerId <- getSummoner(SummonerName)$accountId
  url_final <- URLencode(iconv(paste0(api_url, summonerId), to = "UTF-8"))
  
  res <- GET(url = url_final,
             add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% 
    content()
  
  return(res)
}