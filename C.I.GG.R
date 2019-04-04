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
                              choices = c("전체" = "all", "솔로랭크" = "420", "자유랭크" = "440", "무작위 총력전" = "450", "우르프" = "900"),
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
                  h2(htmlOutput("teir")))),
  fluidRow(column(width = 12,
                  h4("챔피언 숙련도"),
                  DTOutput("championMastery"))),
  fluidRow(downloadButton(outputId = 'downMastery', label = 'DownloadMastery')),
  fluidRow(column(width = 12,
                  h4("당신의 행적-최근 10게임"),
                  DTOutput("matchHistory"))),
  fluidRow(downloadButton(outputId = 'downHistory', label = 'DownloadHistory')),
  fluidRow(column(2, h1("C.I.GG")),
           column(3, h1("Made by Gwangil")),
           column(3, h1("https://github.com/Gwangil/C.I.GG")))
)

server <- function(input, output) {
  Name <- reactiveValues(summonerName = NA)
  
  observeEvent(input$go, {
    shinyjs::disable("go")
    shinyjs::show("btnText")
    Name$summonerName <- input$summonerName
    
    gotMastery <- getChampionMastery(Name$summonerName) %>% 
      left_join(championId, by = "championId") %>% 
      mutate(lastPlayTime = (lastPlayTime / 1000) %>% lubridate::as_datetime() %>% `+`(lubridate::hours(9)) %>% str_sub(end = -1)) %>%
      select("챔피언" = championNameKo,
             "숙련도 레벨" = championLevel,
             "숙련도 점수" = championPoints,
             #"최근 사용" = lastPlayTime,
             "상자 획득" = chestGranted,
             "영문명" = championName)
    
    output$championMastery <- renderDT(gotMastery, options = list(pageLength = 5))
    
    output$downMastery <- downloadHandler(filename = function() {paste0(Name$summonerName,"_MatchMastery_utf8.csv")},
                                          content = function(file) {
                                            write.csv(gotMastery[input$championMastery_rows_all, , drop = T], file, row.names = F, fileEncoding = "UTF-8")
                                          })
    
    gotHistory <- getMatchHistory(Name$summonerName, queue = ifelse(input$queueType == "all", NA, input$queueType), endIndex = 20) %>%
      left_join(championId, by = c("champion" = "championId")) %>%
      left_join(queueType, by = c("queue" = "ID")) %>%
      mutate(timestamp = (timestamp / 1000) %>% lubridate::as_datetime() %>% `+`(lubridate::hours(9)) ,
             DESCRIPTION = as.factor(DESCRIPTION),
             championNameKo = as.factor(championNameKo)) %>%
      select("일시" = timestamp,
             "게임종류" = DESCRIPTION,
             "챔피언" = championNameKo,
             "gameId" = gameId) %>%
      left_join(getGameStatus(.$gameId, Name$summonerName), by = "gameId") %>% select(-gameId) %>%
      mutate(Core = as.factor(Core),
             SpellD = as.factor(SpellD),
             SpellF = as.factor(SpellF))
    
    output$matchHistory <- renderDT(gotHistory,
                                    options = list(pageLength = 5, scrollX = T), filter = "top")
    
    output$downHistory <- downloadHandler(filename = function() {paste0(Name$summonerName,"_MatchHistory_utf8.csv")},
                                          content = function(file) {
                                            write.csv(gotHistory[input$matchHistory_rows_all, , drop = T], file, row.names = F, fileEncoding = "UTF-8")
                                          })
    
    gotTier <- getTier(Name$summonerName)
    
    output$teir <- renderUI({HTML(paste0(gotTier$summonerName[1], ",<br/>", paste(paste0("그의 ", gotTier$queueType, "은 ",
                                                                                         if(is.null(gotTier$tier)) {"Unranked"} else {
                                                                                           paste0(gotTier$tier,
                                                                                                  "-", gotTier$rank,
                                                                                                  "-", gotTier$leaguePoints)},
                                                                                         " 인가?<br/>", 
                                                                                         gotTier$wins + gotTier$losses, "전 ",
                                                                                         gotTier$wins, "승 ", gotTier$losses, "패, 승률: ",
                                                                                         round(gotTier$wins / (gotTier$wins + gotTier$losses) * 100, 2), "%"), collapse = "<br/>")))})
    output$Veteran <- renderPlot({
      url_final <- paste0(getOption("DDragon"), getOption("LOLPatch"),"/img/champion/",
                          championImageFile[[gotMastery %>% slice(1:1) %>% select("영문명") %>% unlist]])
      par(mar = c(0, 0, 0, 0))
      countcolors::plotArrayAsImage(GET(url = url_final) %>% content)
    })
    
    output$Latest <- renderPlot({
      url_final <- paste0(getOption("DDragon"), getOption("LOLPatch"),"/img/champion/",
                          championImageFile[[championId %>% filter(championNameKo == gotHistory %>% slice(1:1) %>% select("챔피언") %>% unlist()) %>% select(championName) %>% unlist]])
      par(mar = c(0, 0, 0, 0))
      countcolors::plotArrayAsImage(GET(url = url_final) %>% content)
    })
    
    shinyjs::enable("go")
    shinyjs::hide("btnText")
  })
}

shinyApp(ui, server)