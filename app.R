

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
  pageWithSidebar ( # standard shiny layout, controls on the
    # left, output on the right
    
    headerPanel (textOutput ("title")), 
    
    sidebarPanel(
      
      
      h3 (textOutput ("Input")),
      
      numericInput(inputId = "he4",
                   label = textOutput ("he4c"),
                   value = "50",
                   min = "28.75",
                   max = "3847",
                   width='100%'),
      
      numericInput(inputId = "ca125",
                   label = textOutput ("ca125c"),
                   value = "10",
                   min="6.42",
                   max="5000",
                   width='100%'),
      
      
      #radioButtons(inputId = "status",
      #             label = "Status",
      #             choices = list ('Premenopause'='PRE', 'Postmenopause'='POST')),
      
      conditionalPanel(condition="input.language == 'en'",
      radioButtons(inputId = "status",
                   label = "Mensturation Cycle Status",
                   choices = list ('Premenopause'='PRE', 'Postmenopause'='POST'))
      ),
      
      conditionalPanel(condition="input.language == 'ee'",
                       radioButtons(inputId = "statusee",
                                    label = "Menstruatsiooni tsükli staatus",
                                    choices = list ('Premenopaus'='PRE', 'Postmenopaus'='POST'))
      ),
      
      conditionalPanel(condition="input.language == 'ru'",
                       radioButtons(inputId = "statusru",
                                    label = "Статус менструального цикла",
                                    choices = list ('Пременопауза'='PRE', 'Постменопауза'='POST'))
      ),
      
      checkboxInput('calc_visible', textOutput ("calcv"), FALSE),#to show equation
      
      br (),
      
      radioButtons(inputId = "language",
                   label = "",
                   choices = list("English"="en", "Eesti" = "ee"),
                   selected = "en",
                   inline=TRUE),
      
      tableOutput("footer"),
      width = 3
    ),
    
    mainPanel( 
 #     tableOutput("test"),
      h3(textOutput ("Result")),
      tableOutput("given"),
      tableOutput("result"),
      h3(textOutput ("Interpretation")),
      tableOutput("interpretation"),
      uiOutput('calc'),
      br(),
 #     tableOutput("statt"),
      tableOutput("source")
    )
  )
  ))

server <- shinyServer(function(input, output) {
  
 output$test <- renderText(paste0(input$language))
  
  load ("translation.bin")
  
  tr <- function(text){ # translates text into current language
    sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
  }
  
  statstat <- function (text){ # tests if status == text
    if (!is.na (input$status) & (input$language=='en')){
      statusall <- input$status
    } else if (!is.na (input$statusee) & (input$language=='ee')){
      statusall <- input$statusee
    } else if (!is.na (input$statusru) & (input$language=='ru')){
      statusall <- input$statusru
    }
    statusall==text
  }
  
  output$statt <- renderText({
    paste0 (statstat ("PRE"))
  }) 
  
  output$stat <- renderText({
    if (!is.na (input$status) & (input$language=='en')){
      statusall <- input$status
    } else if (!is.na (input$statusee) & (input$language=='ee')){
      statusall <- input$statusee
    } else if (!is.na (input$statusru) & (input$language=='ru')){
      statusall <- input$statusru
    }
      paste0 (statusall)
  }) 
  
  
  output$calc <- renderUI({
    if (!input$calc_visible) {
      return ()
    } else {
      if (statstat ("PRE")){
        
        withMathJax(
          helpText( tr ("calcpr"), '$$ROMA = \\frac {e^{-12 + 2.38 \\ln {[HE4]} + 0.0626 \\ln {[CA125]}}}{1+e^{-12 + 2.38 \\ln {[HE4]} + 0.0626 \\ln {[CA125]}}} \\times  100\\%$$')
        )
      } else {
        withMathJax(
          helpText(tr ("calcpo"), '$$ROMA = \\frac {e^{-8.09 + 1.04 \\ln {[HE4]} + 0.732 \\ln {[CA125]}}}{1+e^{-8.09 + 1.04 \\ln {[HE4]} + 0.732 \\ln {[CA125]}}} \\times 100\\%$$')
        )
        
      }
    }
    
  })
  
  
  
  output$given <- renderText ({ 
    
    if (statstat ("PRE")){
      pind <- -12 + 2.38 * log (input$he4) + 0.0626 * log (input$ca125)
    } else {
      pind <- -8.09 + 1.04 * log (input$he4) + 0.732 * log (input$ca125)
    }
    
    roma <- exp (pind) / (1+exp (pind))
    
    paste0 (tr ("given1"), strong (em (input$he4)), # from the text
            tr ("given2"), strong (em (input$ca125)),  # input control as
            tr ("given3")#in ui.R
      )
    
  })
  
  output$result <- renderText ({ 
    #calculation in here
    if (statstat ("PRE")){
      pind <- -12 + 2.38 * log (input$he4) + 0.0626 * log (input$ca125)
    } else {
      pind <- -8.09 + 1.04 * log (input$he4) + 0.732 * log (input$ca125)
    }
    roma <- exp (pind) / (1+exp (pind))#roma calculation
    romap <- round (roma * 100, digits=2)#fit for reporting
    
    #some beauty for reporting - different size digits
    romapp <- as.character (romap)#making a string 
    
    library(stringi)
    n <- as.numeric (unlist (stri_locate_all (pattern = '.', romapp, fixed = TRUE)[[1]][1]))#finding location of "."
    if (is.na (n)){ #in case result is for instance exactly "5"
      floor <- as.character (floor (romap))
      extra <- "0"
    } else {
      floor <- substr (romapp, 1, n-1) #part before "."
      extra <- substr (romapp, n+1, nchar (romapp))#part after "."
    }
    
    #background color based on result
    if (statstat ("PRE")){
      if (roma >= 0.114){
        color <- "#fde7f1"#green for low risk
      } else {
        color <- "#c8f6a1"#pink for high risk
      }
    } else {
      if (roma >= 0.299){
        color <- "#fde7f1"#green for low risk
      } else {
        color <- "#c8f6a1"#pink for high risk
      }
    }
    
    paste0 ("<div style = 'background-color: ", color, "; width: 100%; height: 110px; border-radius: 10px'; vertical-align='middle'; align='center'><br><em><b><font size=10>", floor, "</font><font size=5 color=#778899>.", extra, "</font><font size=6>%","</font></b></em></div>"
    )
    
  })
  
  
  output$interpretation <- renderText ({ 
    if (statstat ("PRE")){
      pind <- -12 + 2.38 * log (input$he4) + 0.0626 * log (input$ca125)
    } else {
      pind <- -8.09 + 1.04 * log (input$he4) + 0.732 * log (input$ca125)
    }
    
    roma <- exp (pind) / (1+exp (pind))
    
    if (statstat ("PRE")){
      if (roma >= 0.114){
        paste0 (tr ("interph"))
      } else {
        paste0 (tr("interpl"))
      }
    } else {
      if (roma >= 0.299){
        paste0 (tr ("interph"))
      } else {
        paste0 (tr ("interpl"))
      }
    }
  })
  
  output$title <- renderText({
    tr ("title")
    
  }) 
  
  output$Input <- renderText({
    tr ("Input")
    
  }) 
  
  output$ca125c <- renderText({
    tr ("ca125c")
    
  }) 
  
  output$he4c <- renderText({
    tr ("he4c")
    
  })
  
  output$Status <- renderText({
    tr ("Status")
    
  }) 
  
  output$Premenopause <- renderText({
    tr ("Premenopause")
            
  })
    
  output$Postmenopause <- renderText({
    tr ("Postmenopause")
            
  })
  
  output$Status <- renderText({
    tr ("status")
    
  })
  
  output$calcv <- renderText({
    tr ("calcv")
    
  })
  
  output$source <- renderText({
    paste0 (tr ("source"))
    
  }) 
  
  output$Result <- renderText({
    paste0 (tr ("Result"))
    
  }) 
  
  output$Interpretation<- renderText({
    paste0 (tr ("Interpretation"))
    
  }) 
  
  #vanity fair
  output$footer <- renderText ({ 
    paste0 (tr ("footer"))
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

