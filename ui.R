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
require('gridExtra')
require('reshape2')

options(RCHART_LIB = 'polycharts')

shinyUI(pageWithSidebar(
  headerPanel("Milkshaker"),
  sidebarPanel(
    #includeHTML("../shared/mixpanel.js"),
    #htmlOutput("mixpanelEvent_job_id"),
    #HTML("<script>mixpanel.track('milkshaker')</script>"),
    fileInput("files", h4("Select a full report:"), multiple=FALSE, accept = 
                c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    uiOutput("trustSelector"),
    #uiOutput("goldsSeen"),
    #uiOutput("lastTimes"),
    h4("***"),
    p("Adjuster For Wtfpt?"),
    uiOutput("percentageSelector"),
    h4("***"),
    p("Adjusters for Set Milkshake:"),
    uiOutput("xSelector"),
    uiOutput("ySelector"),
    conditionalPanel(condition = '0==1',
    sliderInput("dummyslider", "", min=0, max=1, value=0)),
    p("DDLew and his Milkshake:"),
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
          selectInput(inputId = "crowd_chosen", 
                       label="Pick a crowd",
                         c("All" = "all",
                           "Trusted" = "false",
                           "Untrusted" = "true")),
          selectInput(inputId = "state_chosen", label= "Pick a unit type",
                       c("All" = "all",
                         "Golds" = "golden",
                         "Units" = "normal"))),        
        tabPanel("Graph Summary",
          tableOutput("summary_stats_country"),
          tableOutput("summary_stats_channel")))
      ),
     tabPanel("Who the f@*k put that?",
              textInput(inputId="answer_chosen",
                        label="Search for:", value=""),
              uiOutput("questionSelectorSearch"),
              #htmlOutput("create_answer_index_table")),
              dataTableOutput("createSearchTable")),
     tabPanel("Milkshake Analyzer",
              uiOutput("questionSelectorMilkshaker"),
              uiOutput("answerSelectorMilkshaker"),
      tabsetPanel(
        tabPanel("Histogram",
              plotOutput("milkshakeDensity")),
        tabPanel("Set Milkshake",
                 plotOutput("milkshakeQuartile"),
                 htmlOutput("suggestedRules"),
                 h4("Current work you will lose if you implement the rule given above:"),
                 htmlOutput("create_rejected_html_table"))
              )),
     tabPanel("Contributor Answers",
              textInput(inputId="id_chosen", 
                        label="Choose a worker id to graph:", value=""),
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