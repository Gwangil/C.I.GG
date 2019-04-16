sapply(paste0("./functions/", list.files("./functions")), source)

gotSummoner <- getSummoner("신뢰구간")

getTier(gotSummoner)
getMatchHistory(gotSummoner)
getChampionMastery(gotSummoner)
getTotalMasteryScore(gotSummoner)
getGameStatus(getMatchHistory(gotSummoner)$gameId[1:5], gotSummoner)