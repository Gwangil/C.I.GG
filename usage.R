sapply(paste0("./functions/", list.files("./functions")), source)

getSummoner("신뢰구간")
getRankTier("신뢰구간")
getMatchHistory("신뢰구간")[["matches"]][1:2]