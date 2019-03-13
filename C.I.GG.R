sapply(paste0("./functions/", list.files("./functions")), source)
if (!"shiny" %in% installed.packages()) {
  install.packages("shiny")
  library(shiny)
} else {library(shiny)}
if (!"DT" %in% installed.packages()) {
  install.packages("DT")
  library(DT)
} else {library(DT)}
if (!"shinyjs" %in% installed.packages()) {
  install.packages("shinyjs")
  library(shinyjs)
} else {library(shinyjs)}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("C.I.GG"),
  fluidRow(column(width = 3,
                  textInput(inputId = "summonerName",
                            label = "소환사명",
                            value = "Hide On Bush")),
           column(width = 3,
                  selectInput(inputId = "queueType",
                              label = "큐타입",
                              choices = c("전체" = "all", "솔로랭크" = "420", "무작위 총력전" = "450", "우르프" = "900"),
                              selected = "전체")),
           column(1, h3(actionButton("go", "Go"),
                        shinyjs::hidden(p(id = "btnText", "Processing...")))),
           column(2, "Veteran",
                  plotOutput(outputId = "Veteran",
                             width = "80px",
                             height = "80px")),
           column(2, "Latest",
                  plotOutput(outputId = "Latest",
                             width = "80px",
                             height = "80px"))),
  fluidRow(column(width = 12,
                  h2(textOutput("teir")))),
  fluidRow(column(width = 12,
                  h4("챔피언 숙련도"),
                  DTOutput("championMastery"))),
  fluidRow(column(width = 12,
                  h4("당신의 행적-최근 10게임"),
                  DTOutput("matchHistory"))),
  fluidRow(column(2, h1("C.I.GG")),
           column(3, h1("Made by Gwangil")),
           column(3, h1("https://github.com/Gwangil/C.I.GG")))
)

server <- function(input, output) {
  
  observeEvent(input$go, {
    shinyjs::disable("go")
    shinyjs::show("btnText")
  })
  
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
  
  output$matchHistory <- renderDT({temp <- getMatchHistory(summonerRepresent(), queue = ifelse(input$queueType == "all", NA, input$queueType), endIndex = 10) %>%
    left_join(championId, by = c("champion" = "championId")) %>%
    left_join(queueType, by = c("queue" = "ID")) %>%
    mutate(timestamp = (timestamp / 1000) %>% lubridate::as_datetime() %>% `+`(lubridate::hours(9)) %>% str_sub(end = -1)) %>%
    select("일시" = timestamp,
           "게임종류" = DESCRIPTION,
           "챔피언" = championNameKo,
           "gameId" = gameId)  %>%
    left_join(getGameStatus(.$gameId, summonerRepresent()), by = "gameId") %>% select(-gameId)
  shinyjs::enable("go")
  shinyjs::hide("btnText")
  temp},
  options = list(pageLenght = 5))
  
  output$teir <- renderText(paste0(getSummoner(summonerRepresent())$name,
                                   ", 그는 ", ifelse(is.null(getTier(summonerRepresent())$tier),"Unranked",
                                                   paste0(getTier(summonerRepresent())$tier,
                                                          "-", getTier(summonerRepresent())$rank,
                                                          "-", getTier(summonerRepresent())$leaguePoints)),
                                   " 인가"))
  
  output$Veteran <- renderPlot({
    url_final <- paste0(getOption("DDragon"), getOption("LOLPatch"),"/img/champion/",
                        championImageFile[[getChampionMastery(summonerRepresent()) %>% slice(1:1) %>%
                                             left_join(championId, by = "championId") %>% select(championName) %>% unlist]])
    par(mar = c(0, 0, 0, 0))
    countcolors::plotArrayAsImage(GET(url = url_final) %>% content)
  })
  
  output$Latest <- renderPlot({
    url_final <- paste0(getOption("DDragon"), getOption("LOLPatch"),"/img/champion/",
                        championImageFile[[getMatchHistory(summonerRepresent(), queue = ifelse(input$queueType == "all", NA, input$queueType), endIndex = 1) %>%
                                             left_join(championId, by = c("champion" = "championId")) %>% select(championName) %>% unlist]])
    par(mar = c(0, 0, 0, 0))
    countcolors::plotArrayAsImage(GET(url = url_final) %>% content)
  })
}

shinyApp(ui, server)
