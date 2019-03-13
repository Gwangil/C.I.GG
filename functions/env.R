options(RiotApiKey = "your_Riot_api_key") # https://developer.riotgames.com
options(LOLPatch = "9.5.1") # LOL patch version : 9.5.1
options(DDragon = "http://ddragon.leagueoflegends.com/cdn/") # base url for static data

if (!"httr" %in% installed.packages()) {
  install.packages("httr")
  library(httr)
} else {library(httr)}
if (!"tidyverse" %in% installed.packages()) {
  install.packages("tidyverse")
  library(tidyverse)
} else {library(tidyverse)}
if (!"countcolors" %in% installed.packages()) {
  install.packages("countcolors")
  library(countcolors)
} else {library(countcolors)}

champions <- GET(url = paste0(getOption("DDragon"), getOption("LOLPatch"), "/data/ko_KR/champion.json")) %>% content %>% `[[`("data")
championId <- sapply(champions, `[`, c("key", "name")) %>% t() %>% data.frame() %>%
  rownames_to_column(var = "championName") %>%
  mutate(championId = as.numeric(key),
         championNameKo = as.character(name)) %>% select(-key, -name)
championImageFile <- lapply(lapply(champions, `[[`, "image"), `[[`, "full")

items <- GET(url = paste0(getOption("DDragon"), getOption("LOLPatch"), "/data/ko_KR/item.json")) %>% content

spells <- GET(url = paste0(getOption("DDragon"), getOption("LOLPatch"), "/data/ko_KR/summoner.json")) %>% content %>% `[[`("data")
spellId <- sapply(spells, `[`, c("key", "name")) %>% t() %>% data.frame()

runes <- GET(url = paste0(getOption("DDragon"), getOption("LOLPatch"), "/data/ko_KR/runesReforged.json")) %>% content
runeCore <- do.call(bind_rows,
                    lapply(1:5, function(x) runes[[x]][["slots"]][[1]][["runes"]] %>% bind_rows() %>% select(id, name)))

queueType <- suppressWarnings(read_tsv("queueType.txt")[, 1:3])