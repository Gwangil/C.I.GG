if (!"httr" %in% installed.packages()) {
  install.packages("httr")
  library(httr)
} else {library(httr)}
if (!"jsonlite" %in% installed.packages()) {
  install.packages("jsonlite")
  library(jsonlite)
} else {library(jsonlite)}
if (!"tidyverse" %in% installed.packages()) {
  install.packages("tidyverse")
  library(tidyverse)
} else {library(tidyverse)}
if (!"countcolors" %in% installed.packages()) {
  install.packages("countcolors")
  library(countcolors)
} else {library(countcolors)}
if (!"shiny" %in% installed.packages()) {
  install.packages("shiny")
  library(shiny)
} else {library(shiny)}
if (!"shinyjs" %in% installed.packages()) {
  install.packages("shinyjs")
  library(shinyjs)
} else {library(shinyjs)}
if (!"DT" %in% installed.packages()) {
  install.packages("DT")
  library(DT)
} else {library(DT)}
if (!"plotly" %in% installed.packages()) {
  install.packages("plotly")
  library(plotly)
} else {library(plotly)}

options(RiotApiKey = "YOUR_RIOT_API_KEY") # https://developer.riotgames.com
options(LOLPatch = read_json("https://ddragon.leagueoflegends.com/api/versions.json")[[1]]) # LOL patch version
options(DDragon = "http://ddragon.leagueoflegends.com/cdn/") # base url for static data

champions <- read_json(paste0(getOption("DDragon"), getOption("LOLPatch"), "/data/ko_KR/champion.json")) %>% `[[`("data")
championId <- sapply(champions, `[`, c("key", "name")) %>% t() %>% data.frame() %>%
  rownames_to_column(var = "championName") %>%
  mutate(championId = as.numeric(key),
         championNameKo = as.character(name)) %>% select(-key, -name)
championImageFile <- lapply(lapply(champions, `[[`, "image"), `[[`, "full")

items <- read_json(paste0(getOption("DDragon"), getOption("LOLPatch"), "/data/ko_KR/item.json"))

spells <- read_json(paste0(getOption("DDragon"), getOption("LOLPatch"), "/data/ko_KR/summoner.json")) %>% `[[`("data")
spellId <- sapply(spells, `[`, c("key", "name")) %>% t() %>% data.frame()

runes <- read_json(paste0(getOption("DDragon"), getOption("LOLPatch"), "/data/ko_KR/runesReforged.json"))
runeCore <- do.call(bind_rows,
                    lapply(1:5, function(x) {
                      runes[[x]][["slots"]][[1]][["runes"]] %>%
                        bind_rows() %>% select(id, name)}
                    )
)

queueType <- suppressWarnings(read_tsv("queueType.txt")[, 1:3])

summonersLift <- grid::rasterGrob(GET(url = "https://s3-us-west-1.amazonaws.com/riot-developer-portal/docs/map11.png") %>% content, interpolate = T)
howlingAbyss <- grid::rasterGrob(GET(url = "https://s3-us-west-1.amazonaws.com/riot-developer-portal/docs/map12.png") %>% content, interpolate = T)

conversionPoint <- function(data) {data %>%
    group_by(section.id) %>%
    filter(mins %in% c(min(mins), max(mins))) %>%
    ungroup() %>%
    mutate(mid.x = ifelse(section.id == 1 | 
                            section.id == lag(section.id), 
                          NA,
                          mins - (mins - lag(mins)) / 
                            (diffGold - lag(diffGold)) * (diffGold - 0))) %>% 
    select(mid.x, diffGold, section.id) %>%
    rename(mins = mid.x) %>%
    mutate(diffGold = 0) %>%
    na.omit()}