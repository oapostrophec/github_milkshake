###Milkshaker UI###
###Last updated 02/10/2014###

require('shiny')
require('datasets')
require('data.table')
require('plyr')
require('rCharts')
require('ggplot2')
require('devtools')

shinyUI(pageWithSidebar(
  headerPanel("Milkshaker"),
  sidebarPanel(
    fileInput("files", h4("Select a full report:"), multiple=FALSE, accept = 
                c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    uiOutput("trustSelector"),
    uiOutput("goldsSeen"),
    uiOutput("lastTimes"),
    conditionalPanel(condition = '0==1',
    sliderInput("dummyslider", "", min=0, max=1, value=0)),
    tags$style(type="text/css", ".tab-content { overflow: visible; }", ".svg { height: 150%; }", ".y.axis{ ticks: 20; } ", 
               ".yAxis { scale: 20;}")
  ),
  mainPanel(
    tabsetPanel(
     tabPanel("Total Answer",
      uiOutput("questionSelector"),
      selectInput(inputId = "state_chosen", label= " ",
                   c("All" = "all",
                     "Golds" = "golden",
                     "Units" = "normal")),
      tabsetPanel(
        tabPanel("Answer Distros Graph",        
          uiOutput("titleTotalGraph"),
          showOutput("total_distros", "nvd3")),        
        tabPanel("Graph Summary"))
      ),
     tabPanel("Contributor Answers",
      textInput(inputId="id_chosen", 
                label="Choose a worker id to graph:", value=""),
      uiOutput("questionSelectorContrib"),
      selectInput(inputId = "state_chosen_contrib", label= " ",
                  c("All" = "all",
                    "Golds" = "golden",
                    "Units" = "normal")),
      showOutput("contrib_distros", "nvd3") 
      #htmlOutput("html_table_workers")
              ),
     tabPanel("Who the f@*k put that?",
              uiOutput("answer_columns")),
     tabPanel("Set Milkshake")
    )
  ) #Close mainPanel
)) #Close ui.R