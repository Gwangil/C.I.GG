sapply(paste0("./functions/", list.files("./functions")), source)

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
                        shinyjs::hidden(p(id = "btnText", "Processing..."))))),
  
  fluidRow(column(width = 12,
                  h2(htmlOutput("teir")))),
  
  tabsetPanel(
    tabPanel("챔피언 숙련도",  fluidRow(column(width = 12,
                                         shinyjs::hidden(imageOutput(outputId = "veteran", width = "100%", height = "100%")),
                                         DTOutput("championMastery"),
                                         shinyjs::hidden(downloadButton(outputId = 'downMastery', label = 'DownloadMastery'))))),
    tabPanel("최근 게임", fluidRow(column(width = 12,
                                      DTOutput("matchHistory"),
                                      shinyjs::hidden(downloadButton(outputId = 'downHistory', label = 'DownloadHistory'))))),
    tabPanel("최근 게임 동선", fluidRow(column(width = 12,
                                         column(width = 4,
                                                shinyjs::hidden(plotOutput(outputId = "playingPath"))),
                                         column(width = 4,
                                                shinyjs::hidden(plotOutput(outputId = "playingPath2"))),
                                         column(width = 4,
                                                shinyjs::hidden(plotOutput(outputId = "playingPath3")))))),
    tabPanel("최근 게임 골드차이", fluidRow(column(width = 12,
                                           column(width = 4,
                                                  plotlyOutput(outputId = "globalGold")),
                                           column(width = 4,
                                                  plotlyOutput(outputId = "globalGold2")),
                                           column(width = 4,
                                                  plotlyOutput(outputId = "globalGold3")))))),
  
  fluidRow(column(2, h1("C.I.GG")),
           column(3, h1("Made by Gwangil")),
           column(3, h1("https://github.com/Gwangil/C.I.GG")))
)

server <- function(input, output) {
  ReactValue <- reactiveValues()
  
  observeEvent(input$go, {
    shinyjs::disable("go")
    shinyjs::show("btnText")
    
    ##### Summoner Represent
    ReactValue$gotSummoner <- getSummoner(input$summonerName)
    
    ##### Summoner's Tier
    gotTier <- getTier(ReactValue$gotSummoner)
    
    output$teir <- renderUI({HTML(paste0(ReactValue$gotSummoner$name, ",<br/>",
                                         if(is.null(gotTier$tier)) {
                                           paste0("5252, 너는 언랭이라구.")
                                           } else {
                                             paste(paste0("그의 ", gotTier$queueType, "은 ",
                                                          paste0(gotTier$tier,
                                                                 "-", gotTier$rank,
                                                                 "-", gotTier$leaguePoints),
                                                          " 인가?<br/>",
                                                          gotTier$wins + gotTier$losses, "전 ",
                                                          gotTier$wins, "승 ", gotTier$losses, "패, 승률: ",
                                                          round(gotTier$wins / (gotTier$wins + gotTier$losses) * 100, 2), "%"),
                                                   collapse = "<br/>")
                                             }
                                         )
                                  )
      })
    
    ##### Champion mastery
    gotMastery <- getChampionMastery(ReactValue$gotSummoner) %>% 
      left_join(championId, by = "championId") %>% 
      mutate(lastPlayTime = (lastPlayTime / 1000) %>% lubridate::as_datetime() %>% `+`(lubridate::hours(9)) %>% str_sub(end = -1)) %>%
      select("챔피언" = championNameKo,
             "숙련도 레벨" = championLevel,
             "숙련도 점수" = championPoints,
             "상자 획득" = chestGranted,
             "영문명" = championName)
    
    output$championMastery <- renderDT(gotMastery, options = list(pageLength = 5))
    
    output$downMastery <- downloadHandler(filename = function() {paste0(ReactValue$gotSummoner$name,"_ChampionMastery_utf8.csv")},
                                          content = function(file) {
                                            write.csv(gotMastery[input$championMastery_rows_all, , drop = T], file, row.names = F, fileEncoding = "UTF-8")
                                          })
    
    output$veteran <- renderImage({
      outfile <- tempfile(fileext='.png')
      url_image <- paste0(getOption("DDragon"), getOption("LOLPatch"), "/img/champion/",
                          gotMastery %>% slice(1:10) %>% pull("영문명"), ".png")
      
      png(outfile, width = 1000, height = 200)
      par(mfrow = c(1, length(url_image)))
      sapply(url_image, function(x) {countcolors::plotArrayAsImage(GET(url = x) %>% content)})
      mtext("Top 10 of veteran champions", at = -5, line = 0, cex = 2)
      dev.off()
      
      list(src = outfile)
    }, deleteFile = TRUE)
    
    ##### Match History
    gotHistory <- getMatchHistory(ReactValue$gotSummoner, queue = ifelse(input$queueType == "all", NA, input$queueType), endIndex = 20) %>%
      left_join(championId, by = c("champion" = "championId")) %>%
      left_join(queueType, by = c("queue" = "ID")) %>%
      mutate(timestamp = (timestamp / 1000) %>% lubridate::as_datetime() %>% `+`(lubridate::hours(9)) ,
             DESCRIPTION = as.factor(DESCRIPTION),
             championNameKo = as.factor(championNameKo)) %>%
      select("일시" = timestamp,
             "게임종류" = DESCRIPTION,
             "챔피언" = championNameKo,
             "gameId" = gameId) %>%
      left_join(getGameStatus(.$gameId, ReactValue$gotSummoner), by = "gameId") %>%
      mutate(Core = as.factor(Core),
             SpellD = as.factor(SpellD),
             SpellF = as.factor(SpellF))
    
    output$matchHistory <- renderDT(gotHistory %>% select(-gameId, -participantNo),
                                    options = list(pageLength = 5, scrollX = T), filter = "top")
    
    output$downHistory <- downloadHandler(filename = function() {paste0(ReactValue$gotSummoner$name,"_MatchHistory_utf8.csv")},
                                          content = function(file) {
                                            write.csv(gotHistory[input$matchHistory_rows_all, , drop = T], file, row.names = F, fileEncoding = "UTF-8")
                                          })
    
    timeline1 <- GET(url = paste0("https://kr.api.riotgames.com/lol/match/v4/timelines/by-match/", gotHistory$gameId[1]),
                     add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% content %>% unlist() %>% enframe()
    timeline2 <- GET(url = paste0("https://kr.api.riotgames.com/lol/match/v4/timelines/by-match/", gotHistory$gameId[2]),
                     add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% content %>% unlist() %>% enframe()
    timeline3 <- GET(url = paste0("https://kr.api.riotgames.com/lol/match/v4/timelines/by-match/", gotHistory$gameId[3]),
                     add_headers("X-Riot-Token" = getOption("RiotApiKey"))) %>% content %>% unlist() %>% enframe()
    
    output$playingPath <- renderPlot({
      g <- timeline1 %>% filter(str_detect(name, paste0(gotHistory$participantNo[1], ".position"))) %>%
        pull(value) %>% as.integer() %>% matrix(ncol = 2, byrow = T, dimnames = list(NULL, c("x", "y"))) %>% as.tibble() %>%
        ggplot(mapping = aes(x = x, y = y))
      
      if(gotHistory$게임종류[1] %>% str_detect("ARAM")) {
        g <- g +
          annotation_custom(howlingAbyss) +
          coord_fixed(xlim = c(-28, 12849), ylim = c(-19, 12858), ratio = 1, expand = F) +
          annotation_custom(grid::rasterGrob(GET(url = paste0(getOption("DDragon"),
                                                              getOption("LOLPatch"),"/img/champion/",
                                                              championImageFile[[championId %>%
                                                                                   filter(championNameKo == gotHistory$챔피언[1]) %>%
                                                                                   pull(championName)]])) %>% content, interpolate = T),
                            xmin = 0, xmax = 1800, ymin = 11200)
      } else {
        g <- g +
          annotation_custom(summonersLift) +
          coord_fixed(xlim = c(-120, 14870), ylim = c(-120, 14980), ratio = 1, expand = F) +
          annotation_custom(grid::rasterGrob(GET(url = paste0(getOption("DDragon"),
                                                              getOption("LOLPatch"),"/img/champion/",
                                                              championImageFile[[championId %>%
                                                                                   filter(championNameKo == gotHistory$챔피언[1]) %>%
                                                                                   pull(championName)]])) %>% content, interpolate = T),
                            xmin = 0, xmax = 1800, ymin = 13200)
      }
      g +
        geom_path(aes(size = 1.5, colour = "red")) +
        ggtitle("Movement in the most recent game") +
        theme(axis.title = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "none",
              panel.grid = element_blank())
    })
    shinyjs::show("playingPath")
    
    output$playingPath2 <- renderPlot({
      g <- timeline2 %>% filter(str_detect(name, paste0(gotHistory$participantNo[2], ".position"))) %>%
        pull(value) %>% as.integer() %>% matrix(ncol = 2, byrow = T, dimnames = list(NULL, c("x", "y"))) %>% as.tibble() %>%
        ggplot(mapping = aes(x = x, y = y))
      
      if(gotHistory$게임종류[2] %>% str_detect("ARAM")) {
        g <- g +
          annotation_custom(howlingAbyss) +
          coord_fixed(xlim = c(-28, 12849), ylim = c(-19, 12858), ratio = 1, expand = F) +
          annotation_custom(grid::rasterGrob(GET(url = paste0(getOption("DDragon"),
                                                              getOption("LOLPatch"),"/img/champion/",
                                                              championImageFile[[championId %>%
                                                                                   filter(championNameKo == gotHistory$챔피언[2]) %>%
                                                                                   pull(championName)]])) %>% content, interpolate = T),
                            xmin = 0, xmax = 1800, ymin = 11200)
      } else {
        g <- g +
          annotation_custom(summonersLift) +
          coord_fixed(xlim = c(-120, 14870), ylim = c(-120, 14980), ratio = 1, expand = F) +
          annotation_custom(grid::rasterGrob(GET(url = paste0(getOption("DDragon"),
                                                              getOption("LOLPatch"),"/img/champion/",
                                                              championImageFile[[championId %>%
                                                                                   filter(championNameKo == gotHistory$챔피언[2]) %>%
                                                                                   pull(championName)]])) %>% content, interpolate = T),
                            xmin = 0, xmax = 1800, ymin = 13200)
      }
      g +
        geom_path(aes(size = 1.5, colour = "red")) +
        ggtitle("Movement in the 2nd most recent game") +
        theme(axis.title = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "none",
              panel.grid = element_blank())
    })
    shinyjs::show("playingPath2")
    
    output$playingPath3 <- renderPlot({
      g <- timeline3 %>% filter(str_detect(name, paste0(gotHistory$participantNo[3], ".position"))) %>%
        pull(value) %>% as.integer() %>% matrix(ncol = 2, byrow = T, dimnames = list(NULL, c("x", "y"))) %>% as.tibble() %>%
        ggplot(mapping = aes(x = x, y = y))
      
      if(gotHistory$게임종류[3] %>% str_detect("ARAM")) {
        g <- g +
          annotation_custom(howlingAbyss) +
          coord_fixed(xlim = c(-28, 12849), ylim = c(-19, 12858), ratio = 1, expand = F) +
          annotation_custom(grid::rasterGrob(GET(url = paste0(getOption("DDragon"),
                                                              getOption("LOLPatch"),"/img/champion/",
                                                              championImageFile[[championId %>%
                                                                                   filter(championNameKo == gotHistory$챔피언[3]) %>%
                                                                                   pull(championName)]])) %>% content, interpolate = T),
                            xmin = 0, xmax = 1800, ymin = 11200)
      } else {
        g <- g +
          annotation_custom(summonersLift) +
          coord_fixed(xlim = c(-120, 14870), ylim = c(-120, 14980), ratio = 1, expand = F) +
          annotation_custom(grid::rasterGrob(GET(url = paste0(getOption("DDragon"),
                                                              getOption("LOLPatch"),"/img/champion/",
                                                              championImageFile[[championId %>%
                                                                                   filter(championNameKo == gotHistory$챔피언[3]) %>%
                                                                                   pull(championName)]])) %>% content, interpolate = T),
                            xmin = 0, xmax = 1800, ymin = 13200)
      }
      g +
        geom_path(aes(size = 1.5, colour = "red")) +
        ggtitle("Movement in the 3rd most recent game") +
        theme(axis.title = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "none",
              panel.grid = element_blank())
    })
    shinyjs::show("playingPath3")
    
    output$globalGold <- renderPlotly({
      g <- timeline1 %>% filter(str_detect(name, "totalGold")) %>%
        pull(value) %>% as.integer() %>% matrix(ncol = 10, byrow = T) %>% as_tibble() %>%
        mutate(mins = row_number(),
               blueGold = rowSums(.[1:5]),
               redGold = rowSums(.[6:10]),
               diffGold = blueGold - redGold,
               above.h = diffGold >= 0,
               changed = is.na(lag(above.h)) | lag(above.h) != above.h,
               section.id = cumsum(changed)) %>%
        select(mins, diffGold, section.id) %>%
        rbind(conversionPoint(.)) %>%
        ggplot(data = ., aes(x = mins)) +
        geom_ribbon(data = . %>% filter(diffGold >= 0),
                    aes(ymin = 0, ymax = diffGold), fill = "skyblue") +
        geom_ribbon(data = . %>% filter(diffGold <= 0),
                    aes(ymin = diffGold, ymax = 0), fill = "hotpink") +
        geom_point(data = . %>% filter(diffGold != 0),
                   aes(y = diffGold)) +
        ggtitle(gotHistory$챔피언[1] %>% as.character()) +
        theme_void() +
        expand_limits(y = 500)
      ggplotly(g)})
    
    output$globalGold2 <- renderPlotly({
      g <- timeline2 %>% filter(str_detect(name, "totalGold")) %>%
        pull(value) %>% as.integer() %>% matrix(ncol = 10, byrow = T) %>% as_tibble() %>%
        mutate(mins = row_number(),
               blueGold = rowSums(.[1:5]),
               redGold = rowSums(.[6:10]),
               diffGold = blueGold - redGold,
               above.h = diffGold >= 0,
               changed = is.na(lag(above.h)) | lag(above.h) != above.h,
               section.id = cumsum(changed)) %>%
        select(mins, diffGold, section.id) %>%
        rbind(conversionPoint(.)) %>%
        ggplot(data = ., aes(x = mins)) +
        geom_ribbon(data = . %>% filter(diffGold >= 0),
                    aes(ymin = 0, ymax = diffGold), fill = "skyblue") +
        geom_ribbon(data = . %>% filter(diffGold <= 0),
                    aes(ymin = diffGold, ymax = 0), fill = "hotpink") +
        geom_point(data = . %>% filter(diffGold != 0),
                   aes(y = diffGold)) +
        ggtitle(gotHistory$챔피언[2] %>% as.character()) +
        theme_void() +
        expand_limits(y = 500)
      ggplotly(g)})
    
    output$globalGold3 <- renderPlotly({
      g <- timeline3 %>% filter(str_detect(name, "totalGold")) %>%
        pull(value) %>% as.integer() %>% matrix(ncol = 10, byrow = T) %>% as_tibble() %>%
        mutate(mins = row_number(),
               blueGold = rowSums(.[1:5]),
               redGold = rowSums(.[6:10]),
               diffGold = blueGold - redGold,
               above.h = diffGold >= 0,
               changed = is.na(lag(above.h)) | lag(above.h) != above.h,
               section.id = cumsum(changed)) %>%
        select(mins, diffGold, section.id) %>%
        rbind(conversionPoint(.)) %>%
        ggplot(data = ., aes(x = mins)) +
        geom_ribbon(data = . %>% filter(diffGold >= 0),
                    aes(ymin = 0, ymax = diffGold), fill = "skyblue") +
        geom_ribbon(data = . %>% filter(diffGold <= 0),
                    aes(ymin = diffGold, ymax = 0), fill = "hotpink") +
        geom_point(data = . %>% filter(diffGold != 0),
                   aes(y = diffGold)) +
        ggtitle(gotHistory$챔피언[3] %>% as.character()) +
        theme_void() +
        expand_limits(y = 500)
      ggplotly(g)})
    
    shinyjs::show("veteran")
    shinyjs::show("downMastery")
    shinyjs::show("downHistory")
    shinyjs::enable("go")
    shinyjs::hide("btnText")
  })
}

shinyApp(ui, server)