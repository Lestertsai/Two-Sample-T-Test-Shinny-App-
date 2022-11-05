library(shiny)
library(reshape)
ui <- fluidPage(titlePanel("Two Samples t-Test"),
                
                sidebarLayout(
                  sidebarPanel(
                    fileInput(
                      "file1",
                      "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                    ),
                    
                    checkboxInput("header", "Header", TRUE),
                    
                    radioButtons(
                      "sep",
                      "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"
                      ),
                      selected = ","
                    ),
                    
                    radioButtons(
                      "quote",
                      "Quote",
                      choices = c(
                        None = "",
                        "Double Quote" = '"',
                        "Single Quote" = "'"
                      ),
                      selected = '"'
                    ),
                    radioButtons(
                      "disp",
                      "Display",
                      choices = c(Head = "head",
                                  All = "all"),
                      selected = "head"
                    ),
                    
                    actionButton("action", "Calculate")
                    
                  ),
                  mainPanel(tabsetPanel(
                    tabPanel(
                      "Main",
                      htmlOutput("header"),
                      tableOutput("contents"),
                      htmlOutput("Text")
                    )
                  ),
                  
                  fillRow(
                    flex = c(1, 1),
                    fillCol(uiOutput('choose_columns')),
                    fillCol(fluidRow(column(8, uiOutput(
                      'kdv'
                    ))))
                  ))
                  
                ))

server <- function(input, output, session) {
  global <- reactiveValues(picks = c())
  observeEvent(input$q1, {
    global$picks <- input$q1
  })
  output$contents <- renderTable({
    req(input$file1)
    
    df <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
    if (input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$choose_columns <- renderUI({
    if (input$action) {
      df <- read.csv(
        input$file1$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
      selectizeInput(
        "q1",
        label = "Please select two variables",
        choices = names(df),
        multiple = TRUE,
        options = list(maxItems = 2)
      )
    }
    
  })
  t_test <- function(df, first, second) {
    df_two = df[c(first, second)]
    df_long <- melt(df_two)
    result <-
      var.test(value ~ variable , data = df_long)$p.value > 0.05
    aa <- t.test(df_two[first], df_two[second], var.equal = result)
    result <- aa$p.value
    if (result > 0.05) {
      return({
        paste0(
          'Because the p-value =',
          round(result, 3),
          ', there is no significant
               difference between the means of the two variables'
        )
      })
    }
    else{
      return({
        paste(
          'Because the p-value =',
          round(result, 3),
          ', there is a significant
               difference between the means of the two variables'
        )
      })
    }
    
  }
  output$Text <- renderText({
    if (length(global$picks) == 2 & input$action) {
      df <- read.csv(
        input$file1$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
      first <- global$picks[1]
      second <- global$picks[2]
      if (sapply(df[first], class) == "character" &
          sapply(df[second], class) != "character") {
        return({
          paste0('Variable ', first, ' is not numeric!')
        })
      }
      else if (sapply(df[second], class) == "character" &
               sapply(df[first], class) != "character") {
        return({
          paste0('Variable ', second, ' is not numeric!')
        })
      }
      else if (sapply(df[second], class) == "character" &
               sapply(df[first], class) == "character") {
        return({
          paste0('Both Variables are not numeric!')
        })
      }
      else{
        if (nrow(df) >= 300) {
          t_test(df, first, second)
        }
        else{
          ans1 <- shapiro.test(df[[first]])$p.value < 0.05
          ans2 <- shapiro.test(df[[second]])$p.value < 0.05
          if (ans1 | ans2) {
            if (ans1 & !ans2) {
              return({
                paste0('Variable ', first, ' is not normal distributed!')
              })
            }
            else if (ans2 & !ans1) {
              return({
                paste0('Variable ',
                       second,
                       ' is not normal distributed!')
              })
            }
            else{
              return({
                'Both variables are not normal distributed!'
              })
            }
            
          }
          else{
            t_test(df, first, second)
          }
        }
      }
      
    }
  })
}

shinyApp(ui, server)