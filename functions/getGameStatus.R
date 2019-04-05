# Ref.) https://developer.riotgames.com/api-methods/#match-v4/GET_getMatch

## Get summoner's game status
# @param gameId
# @param summonerName
# Return type : tibble, data.frame
# note) This function requires 'getSummoner' function first.
getGameStatus <- function(gameId, summonerName) {
  suppressWarnings({
    lapply(gameId, function(x) {
      temp <- GET(url = paste0("https://kr.api.riotgames.com/lol/match/v4/matches/", x),
                  add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% content
      me <- which(temp$participantIdentities %>% lapply(FUN =  `[[`, "player") %>% sapply(`[[`, "summonerName") == summonerName)
      res <- data.frame(
        "gameId" = temp$gameId,
        "Victory" = temp$participants[[me]]$stats$win,
        "K.D.A" = paste0(temp$participants[[me]]$stats$kills, "/",
                         temp$participants[[me]]$stats$deaths, "/",
                         temp$participants[[me]]$stats$assists),
        "KDA" = sprintf("%.2f", (temp$participants[[me]]$stats$kills + temp$participants[[me]]$stats$assists) / temp$participants[[me]]$stats$deaths),
        "DPM" = round(temp$participants[[me]]$stats$totalDamageDealtToChampions / (temp$gameDuration / 60), 2),
        "DTPM" = round(temp$participants[[me]]$stats$totalDamageTaken / (temp$gameDuration / 60), 2),
        "VisionScore" = temp$participants[[me]]$stats$visionScore,
        "CS" = temp$participants[[me]]$stats$totalMinionsKilled + temp$participants[[me]]$stats$neutralMinionsKilled,
        "Gold" = temp$participants[[me]]$stats$goldEarned,
        "VisionWard" = temp$participants[[me]]$stats$visionWardsBoughtInGame,
        "Core" = runeCore %>% filter(id == temp$participants[[me]]$stats$perk0) %>% select(name) %>% unlist,
        "SpellD" = spellId %>% filter(key == temp$participants[[me]]$spell1Id) %>% select(name) %>% unlist,
        "SpellF" = spellId %>% filter(key == temp$participants[[me]]$spell2Id) %>% select(name) %>% unlist,
        "participantNo" = me)
    }) %>% bind_rows() -> res2
    
    return(res2)
  })
}