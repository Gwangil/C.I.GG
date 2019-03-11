sapply(paste0("./functions/", list.files("./functions")), source)
if (!"shiny" %in% installed.packages()) {
  install.packages("shiny")
  library(shiny)
} else {library(shiny)}
if (!"DT" %in% installed.packages()) {
  install.packages("DT")
  library(DT)
} else {library(DT)}

ui <- fluidPage(
  titlePanel("C.I.GG"),
  fluidRow(
    column(width = 3,
           textInput(inputId = "summonerName",
                     label = NULL,
                     value = "Hide On Bush")),
    column(1, actionButton("go", "Go")),
    column(8, textOutput("teir"))
  ),
  fluidRow(column(width = 12,
                  h4("챔피언 숙련도"),
                  DTOutput("championMastery"))),
  fluidRow(column(width = 12,
                  h4("당신의 행적"),
                  DTOutput("matchHistory")))
)

championId <- sapply(GET(url = "http://ddragon.leagueoflegends.com/cdn/9.4.1/data/ko_KR/champion.json") %>%
                       content %>% `[[`("data"),
                     `[`,
                     c("key", "name")) %>% t() %>% data.frame() %>%
  rownames_to_column(var = "championName") %>%
  mutate(championId = as.numeric(key),
         championNameKo = as.character(name)) %>% select(-key, -name)
queueType <- suppressWarnings(read_tsv("queueType.txt")[, 1:3])

server <- function(input, output) {
  summonerRepresent <- eventReactive(eventExpr = input$go,
                                     valueExpr = input$summonerName)
  
  output$championMastery <- renderDT({getChampionMastery(summonerRepresent()) %>% 
      left_join(championId, by = "championId") %>% 
      mutate(lastPlayTime = (lastPlayTime / 1000) %>% lubridate::as_datetime() %>% `+`(lubridate::hours(9)) %>% str_sub(end = -1)) %>%
      select("챔피언" = championNameKo,
             "숙련도 레벨" = championLevel,
             "숙련도 점수" = championPoints,
             "최근 사용" = lastPlayTime,
             "상자 획득" = chestGranted,
             "영문명" = championName)},
      options = list(pageLength = 5))
  
  output$matchHistory <- renderDT(getMatchHistory(summonerRepresent()) %>% left_join(championId, by = c("champion" = "championId")) %>%
                                    left_join(queueType, by = c("queue" = "ID")) %>%
                                    mutate(timestamp = (timestamp / 1000) %>% lubridate::as_datetime() %>% `+`(lubridate::hours(9)) %>% str_sub(end = -1)) %>% 
                                    select("일시" = timestamp,
                                           "게임종류" = DESCRIPTION,
                                           "챔피언" = championNameKo))
  
  output$teir <- renderText(paste0(getSummoner(summonerRepresent())$name,
                                   ", 그는 ", ifelse(is.null(getTier(summonerRepresent())$tier),"Unranked",
                                                   paste0(getTier(summonerRepresent())$tier,
                                                          "-", getTier(summonerRepresent())$rank,
                                                          "-", getTier(summonerRepresent())$leaguePoints)),
                                   " 인가"))
}

shinyApp(ui, server)
