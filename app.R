library(shiny)
library(reshape)
library(MKinfer)
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
  var_test <- function(df, first, second) {
    df_two <- df[c(first, second)]
    df_long <- melt(df_two)
    result <-
      var.test(value ~ variable , data = df_long)$p.value > 0.05
    return(list(df_long, result))
  }
  word <- function(result, types) {
    if (types == 'Bootstrap') {
      text_first = ' but according to the bootstrap two-sample test, '
    }
    else{
      text_first = 'Based on the two-sample t test,'
    }
    if (result > 0.05) {
      return({
        paste0(
          text_first,
          'because the p-value =',
          result,
          ', there is no significant
               difference between the means of the two variables.'
        )
      })
    }
    else{
      return({
        paste(
          text_first,
          'because the p-value =',
          result,
          ', there is a significant
               difference between the means of the two variables.'
        )
      })
    }
  }
  boot_test <- function(df, result) {
    tt <-
      boot.t.test(
        value ~ variable,
        data = df,
        alternative = "two.sided",
        paired = FALSE,
        var.equal = result,
        conf.level = 0.95,
        R = 1000
      )
    return(list(round(tt$p.value, 3), 'Bootstrap'))
  }
  t_test <- function(df, result) {
    aa <- t.test(value ~ variable, data = df, var.equal = result)
    return(list(round(aa$p.value, 3), 'T Test'))
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
          equal <- var_test(df, first, second)
          means <- t_test(equal[[1]], equal[[2]])
          word(means[[1]], means[[2]])
        }
        else{
          ans1 <- shapiro.test(df[[first]])$p.value < 0.05
          ans2 <- shapiro.test(df[[second]])$p.value < 0.05
          if (ans1 | ans2) {
            equal <- var_test(df, first, second)
            boot_p <- boot_test(equal[[1]], equal[[2]])
            if (ans1 & !ans2) {
              return({
                paste0(
                  'Variable ',
                  first,
                  ' is not normally distributed,',
                  word(boot_p[[1]], boot_p[[2]])
                )
              })
            }
            else if (ans2 & !ans1) {
              return({
                paste0(
                  'Variable ',
                  second,
                  ' is not normally distributed,',
                  word(boot_p[[1]], boot_p[[2]])
                )
              })
            }
            else{
              return({
                paste0(
                  'Both variables are not normally distributed,',
                  word(boot_p[[1]], boot_p[[2]])
                )
              })
            }
            
          }
          else{
            equal <- var_test(df, first, second)
            means <- t_test(equal[[1]], equal[[2]])
            word(means[[1]], means[[2]])
          }
        }
      }
      
    }
  })
}

shinyApp(ui, server)
