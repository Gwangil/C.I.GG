# Ref.) https://developer.riotgames.com/api-methods/#match-v4/GET_getMatchlist

## Get summoner's palyed game list
# @param summonerName
# @param champion   - default NULL, need champion code
# @param queue      - default NULL, specify queue type ex) 420 : Solo Rank / 450 : HowlingAbyss / 900 : URF
# @param season     - default NULL, specify season ex) 13 : Season 2019 / 12 : Pre-Season 2019
# @param endIndex   - default NULL, detail in Ref.
# @param beginIndex - default NULL, detail in Ref.
# Return type : tibble, data.frame
# note) This function requires 'getSummoner' function first.
getMatchHistory <- function(summonerName,
                            champion = NA,
                            queue = NA, # 420 : Solo Rank, 450 : HowlingAbyss, 900 : URF
                            season = NA, # 13 : Season 2019, 12 : Pre-Season 2019
                            endIndex = NA,
                            beginIndex = NA) {
  api_url <- "https://kr.api.riotgames.com/lol/match/v4/matchlists/by-account/"
  summonerId <- getSummoner(summonerName)$accountId
  
  if (all(is.na(c(champion, queue, season, endIndex, beginIndex)))) {
    url_final <- URLencode(iconv(paste0(api_url, summonerId), to = "UTF-8"))
  } else {
    url_final <- paste0(api_url, summonerId, "?")
    if (!is.na(champion)) url_final <- paste0(url_final, "champion=", champion, "&")
    if (!is.na(queue)) url_final <- paste0(url_final, "queue=", queue, "&")
    if (!is.na(season)) url_final <- paste0(url_final, "season=", season, "&")
    if (!is.na(endIndex)) url_final <- paste0(url_final, "endIndex=", endIndex, "&")
    if (!is.na(beginIndex)) url_final <- paste0(url_final, "beginIndex=", beginIndex, "&")
    url_final <- URLencode(iconv(url_final, to = "UTF-8"))
  }
  
  res <- GET(url = url_final,
             add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% 
    content()
  
  res1 <- res[["matches"]] %>% dplyr::bind_rows()
  attr(res1, "startIndex") <- res$startIndex
  attr(res1, "endIndex") <- res$endIndex
  attr(res1, "totalGames") <- res$totalGames
  
  return(res1)
}