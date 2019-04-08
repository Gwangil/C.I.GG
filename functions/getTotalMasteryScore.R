# Ref.) https://developer.riotgames.com/api-methods/#champion-mastery-v4/GET_getChampionMasteryScore

## Get sum of summoner's champion mastery scores
# @param gotSummoner : result of getSummoner()
# Return type : tibble, data.frame
# note) This function requires 'getSummoner' function first.
getTotalMasteryScore <- function(gotSummoner) {
  GET(url = URLencode(iconv(paste0("https://kr.api.riotgames.com/lol/champion-mastery/v4/scores/by-summoner/",
                                   gotSummoner$id), to = "UTF-8")),
      add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% 
    content()
}