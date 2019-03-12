sapply(paste0("./functions/", list.files("./functions")), source)

getSummoner("신뢰구간")
getTier("신뢰구간")
getMatchHistory("신뢰구간")
getChampionMastery("신뢰구간")
getTotalMasteryScore("신뢰구간")
getGameStatus(getMatchHistory("신뢰구간")$gameId[1:5], "신뢰구간")