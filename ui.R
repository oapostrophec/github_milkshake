###Milkshaker UI###
###Last updated 04/03/2014###
##Total Answer Distros
##Who the f%@k keeps putting that?
##Set Milkshake Rules

require('shiny')
require('datasets')
require('data.table')
require('plyr')
require('rCharts')
require('ggplot2')
require('devtools')
require('gridExtra')
require('reshape2')

options(RCHART_LIB = 'polycharts')

shinyUI(pageWithSidebar(
  headerPanel("Milkshaker"),
  sidebarPanel(
    numericInput("job_id_chosen", h4("Enter a job id:"), value = 0),
    h4("OR"),
    fileInput("files", h4("Upload a full report:"), multiple=FALSE, accept = 
                c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    htmlOutput("summary_message"),
   # conditionalPanel(condition = '0==1',
  #  sliderInput("dummyslider", "", min=0, max=1, value=0)),
    htmlOutput("ddLewis"),
    tags$style(type="text/css", ".tab-content { overflow: visible; }", ".svg { height: 150%; }", ".y.axis{ ticks: 20; } ", 
               ".yAxis { scale: 20;}")
  ),
  mainPanel(
    HTML("<script>mixpanel.track_links('a', 'milkshaker tab_or_link_click', function(ele) { return { type: $(ele).text()}})</script>"),
    tabsetPanel(
     tabPanel("Total Answer",
      tabsetPanel(
        tabPanel("Answer Distros Graph",
          uiOutput("questionSelector"), 
          uiOutput("titleTotalGraph"),
          showOutput("total_distros", "nvd3"),
          hr(),
          br(),
          div(uiOutput("trustSelector"), class="span10 alert alert-info"),
          br(),
          div(selectInput(inputId = "crowd_chosen", 
                       label="Pick a crowd",
                         c("All" = "all",
                           "Trusted" = "false",
                           "Untrusted" = "true")), class="span5 well"),
          div(selectInput(inputId = "state_chosen", label= "Pick a unit type",
                       c("All" = "all",
                         "Golds" = "golden",
                         "Units" = "normal")), class="span5 well")
          ),        
        tabPanel("Graph Summary",
          div(tableOutput("summary_stats_country"), class="span5"),
          div(tableOutput("summary_stats_channel"), class="span5")
        ))),
     tabPanel("Who the f@*k put that?",
              uiOutput("questionSelectorSearch"),
              #uiOutput("percentageSelector"),
              htmlOutput("createSearchTableButton"),
              br(),
              dataTableOutput("createSearchTable")),
     tabPanel("Milkshake Analyzer",
              uiOutput("questionSelectorMilkshaker"),
      tabsetPanel(
        tabPanel("Histogram",
              plotOutput("milkshakeDensity")),
        tabPanel("Set Milkshake",
                 uiOutput("answerSelectorMilkshaker"),
                 p("Use the adjusters below to isolate the answers distros you think are acceptable."),
                 div(uiOutput("xSelector"), class="span5 alert alert-info"),
                 div(uiOutput("ySelector"), class="span6 alert alert-success"),
                 br(),
                 plotOutput("milkshakeQuartile"),
                 hr(),
                 br(),
                 h5("Note this answer \"\" is defined as blank answers in your job output."),
                 div(htmlOutput("suggestedRules"), class="well"),
                 h4("Current work you will lose if you implement the rule given above:"),
                 dataTableOutput("createAnswerRejects"))
              )),
     tabPanel("Contributor Answers",
              textInput(inputId="id_chosen", 
                        label="Choose a worker id to graph:", value=""),
              checkboxInput(inputId="order_chosen", label="Show most common answers by all"),
              showOutput("contrib_distros", "nvd3"),
              uiOutput("graphDesc"),
              selectInput(inputId = "contrib_crowd_chosen", 
                          label="Pick a crowd",
                          c("All" = "all",
                            "Trusted" = "false",
                            "Untrusted" = "true")),
              uiOutput("questionSelectorContrib"),
              selectInput(inputId = "state_chosen_contrib", label= "Pick a unit type",
                          c("All" = "all",
                            "Golds" = "golden",
                            "Units" = "normal")))
    )
  ) #Close mainPanel
)) #Close ui.R