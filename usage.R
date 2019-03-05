sapply(paste0("./functions/", list.files("./functions")), source)

getSummoner("신뢰구간")
getRankTier("신뢰구간")
getMatchHistory("신뢰구간")
getChampionMastery("신뢰구간")
getTotalMasteryScore("신뢰구간")