###Milkshaker UI###
###Last updated 02/26/2014###
##recently add comparitive graph and description###

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
      selectInput(inputId = "crowd_chosen", 
                  label="Pick a crowd",
                  c("All" = "all",
                     "Trusted" = "false",
                      "Untrusted" = "true")),
      uiOutput("questionSelector"),
      selectInput(inputId = "state_chosen", label= "Pick a unit type",
                   c("All" = "all",
                     "Golds" = "golden",
                     "Units" = "normal")),
      tabsetPanel(
        tabPanel("Answer Distros Graph",        
          uiOutput("titleTotalGraph"),
          showOutput("total_distros", "nvd3")),        
        tabPanel("Graph Summary",
          htmlOutput("create_summary_table")))
      ),
     tabPanel("Contributor Answers",
      textInput(inputId="id_chosen", 
                label="Choose a worker id to graph:", value=""),
      selectInput(inputId = "contrib_crowd_chosen", 
                  label="Pick a crowd",
                  c("All" = "all",
                    "Trusted" = "false",
                    "Untrusted" = "true")),
      uiOutput("questionSelectorContrib"),
      selectInput(inputId = "state_chosen_contrib", label= "Pick a unit type",
                  c("All" = "all",
                    "Golds" = "golden",
                    "Units" = "normal")),
      showOutput("contrib_distros", "nvd3"),
      uiOutput("graphDesc")
              ),
     tabPanel("Who the f@*k put that?",
              textInput(inputId="answer_chosen",
                        label="Search for:", value=""),
              uiOutput("percentageSelector"),
              uiOutput("questionSelectorSearch"),
              htmlOutput("create_answer_index_table")),
     tabPanel("Set Milkshake")
    )
  ) #Close mainPanel
)) #Close ui.R