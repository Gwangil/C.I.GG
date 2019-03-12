# LOL patch version : 9.5.1
if (!"httr" %in% installed.packages()) {
  install.packages("httr")
  library(httr)
} else {library(httr)}
if (!"tidyverse" %in% installed.packages()) {
  install.packages("tidyverse")
  library(tidyverse)
} else {library(tidyverse)}
options(RiotApiKey = "your_Riot_api_key") ### https://developer.riotgames.com

championId <- sapply(GET(url = "http://ddragon.leagueoflegends.com/cdn/9.5.1/data/ko_KR/champion.json") %>%
                       content %>% `[[`("data"),
                     `[`,
                     c("key", "name")) %>% t() %>% data.frame() %>%
  rownames_to_column(var = "championName") %>%
  mutate(championId = as.numeric(key),
         championNameKo = as.character(name)) %>% select(-key, -name)
items <- GET(url = "http://ddragon.leagueoflegends.com/cdn/9.5.1/data/ko_KR/item.json") %>% content
spells <- GET(url = "http://ddragon.leagueoflegends.com/cdn/9.5.1/data/ko_KR/summoner.json") %>% content %>% `[[`("data")
spellId <- sapply(spells, `[`, c("key", "name")) %>% t() %>% data.frame()
runes <- GET(url = "http://ddragon.leagueoflegends.com/cdn/9.5.1/data/ko_KR/runesReforged.json") %>% content
runeCore <- do.call(bind_rows,
                    lapply(1:5, function(x) runes[[x]][["slots"]][[1]][["runes"]] %>% bind_rows() %>% select(id, name)))
queueType <- suppressWarnings(read_tsv("queueType.txt")[, 1:3])