###Milkshaker Server###
###Last updated 02/26/2014###
##Total Answer Distros && group by - times, trust, golds seen
##Compare contrib id with total, with untrusted, with trusted
##Who the f%@k keeps putting that?
##Milkshaker

require('shiny')
require('datasets')
require('data.table')
require('plyr')
require('rCharts')
require('ggplot2')
require('devtools')
require('stringr')
require('gridExtra')
require('reshape2')

options(stringsAsFactors = F)
options(shiny.maxRequestSize=150*1024^2)

shinyServer(function(input, output){
  
  output$ddLewis <- renderText ({
    image_path = "http://cf-public-view.s3.amazonaws.com/coolstuff/milkshake_pic.jpg"
    html_image = paste("<img src=", image_path, " width=\"65%\"/>", sep="")
    paste(html_image)
    
  })
  
  ### read in file    
  full <- reactive(function() {
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      inFile <- input$files
      full = read.csv(inFile$datapath, na.strings="NaN", stringsAsFactors=FALSE)
      full$X_created_at = as.POSIXct(full$X_created_at,
                                     format='%m/%d/%Y %H:%M:%S')
      return(full)
    }
  })

  job_id <- reactive({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      inFile <- input$files$name
      job_id = gsub(inFile, pattern="^f", replacement="")
      job_id = str_extract(job_id, "\\d{6}")
      return(job_id)
    }
  })
  
  full_file_contrib_id <- reactive({
    if (is.na(input$files[1]) || input$id_chosen == "") {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file_contrib_id = full()
      id = input$id_chosen
      full_file_contrib_id = full_file_contrib_id[full_file_contrib_id$X_worker_id == id,]
      
      full_file_contrib_id
    }
    
  })
  
  ###Grab Answer and Gold Columns
  full_file_gold_answers <- reactive({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = full()
      gold_cols = grepl(".\\gold$", names(full_file)) & !grepl(".\\golden",names(full_file))
      gold_cols_names = names(full_file)[gold_cols]    
      gold_cols_names
    }
    
  })
  
  create_summarized_df <- reactive ({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      file = full()  
      gold_cols = grepl(".\\gold$", names(file)) & !grepl(".\\golden",names(file))
      gold_cols_names = names(file)[gold_cols]    
      ans_cols_names = gsub(gold_cols_names, pattern=".\\gold", replacement="")
    
      file[is.na(file)] = ""
    
      full_melt = file[,c("X_worker_id", ans_cols_names)]
      melted_df_full = melt(full_melt, id="X_worker_id")
    
      melted_df_full$value = as.character(melted_df_full$value)
      melted_df_full$variable = as.character(melted_df_full$variable)
      melted_df_full$X_worker_id = as.numeric(as.character(melted_df_full$X_worker_id))
      melted_df_full$value[melted_df_full$value =="" ] = "\"\""
    
      summarized_df = ddply(melted_df_full, .(X_worker_id,variable), summarize, 
                            percent = prop.table(table(value)), 
                            answer = names(table(value)),
                            num_j = rep(sum(table(value)),times=length(table(value)))
      )
      summarized_df
    }
  })
  
  create_plot_df <- reactive({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      
      answers_df = create_summarized_df()
      answers_df$X_worker_id = as.character(answers_df$X_worker_id)
    
      question_input = input$question_chosen_milkshaker
      if(is.null(question_input)){
        question_input = answers_df$variable[1]
      }
     
      answer_plot = input$answer_chosen_milkshaker
      if(is.null(answer_plot)){
        answer_plot = answers_df$answer[1]
      }
    
      answers_df = answers_df[answers_df$variable == question_input & answers_df$answer == answer_plot,]
      answers_df
      
    }
  })
  
  output$xSelector <- renderUI({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      index_distros = create_summarized_df()
      threshold_min = min(index_distros$num_j, na.rm=T)
      threshold_max = max(index_distros$num_j, na.rm=T)
      default = mean(index_distros$num_j)
      sliderInput(inputId="x_axis_chosen",
                   label="Milkshake Num Judgments to Threshold",
                   min = threshold_min, max = threshold_max,
                   step = 1, value = default)
    
    }
  })
  
  output$ySelector <- renderUI({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
    threshold_min = 0.00
    threshold_max = 1.05
    
    sliderInput(inputId="y_axis_chosen",
                 label="Milkshake Answer Percent Range to Threshold",
                 min = threshold_min, max = threshold_max,
                 step = 0.01, value=c(threshold_min + .01, threshold_max - .01))
    
    }
  })
  
  output$suggestedRules <- renderText({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
    df = create_plot_df()
    percent_min = min(input$y_axis_chosen)
    percent_max = max(input$y_axis_chosen)
    num_judgments = input$x_axis_chosen
    rule = df$answer[1]
    question = df$variable[1]
    judgment = paste("Set Activation Threshold to", num_judgments, "judgments.", sep=" ")
    rule = paste("Enter (", rule, ") into the Acceptable answer distribution for ", question,
                 ".", sep="")
    min_max = paste("Set the Min percentage to ", percent_min, ". Set the Max percentage to ",
                    percent_max, ".", sep="")
    
    suggested_rules = paste(judgment, rule, min_max, sep="<br>")
    }
  })
  
  output$milkshakeQuartile <- renderPlot({
    
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      answers_df = create_plot_df()
      x_threshold = input$x_axis_chosen
      max_y_threshold = max(input$y_axis_chosen)
      min_y_threshold = min(input$y_axis_chosen)
      set_scatter_plot <- ggplot(answers_df, aes(num_j, percent)) + geom_point(aes(color=answer)) +
        geom_vline(xintercept = x_threshold, color="darkorange") +
        geom_hline(yintercept = max_y_threshold, color="darkblue") +
        geom_hline(yintercept = min_y_threshold, color="darkblue")
      #p8 <- rPlot(percent ~ num_j, data = answers_df, type='point', color = 'num_j')
      #p8$addParams(height = 400, dom ='milkshakeQuartile')
      #p8
      print(set_scatter_plot)
    }
  })
  
  create_work_reject <- reactive({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      workers = workers()
      table = create_plot_df()
      
      judgments_threshold = input$x_axis_chosen
      max_percent_accepted = max(input$y_axis_chosen)
      min_percent_accepted = min(input$y_axis_chosen)
      
      
      table = table[table$num_j > judgments_threshold,] 
      print("Table Judgs")
      print(head(table))
      
      table = table[table$percent < min_percent_accepted  || table$percent > max_percent_accepted,]
      print("Table after Percent")
      print(head(table))
      
      print("WTF is yer probles")
      worker_ids = table$X_worker_id
      print(worker_ids)
      
      workers_lost = workers[(workers$X_worker_id %in% worker_ids),]
      print(workers_lost)
      
      workers_lost
    }
  })
  
  output$create_rejected_html_table <- renderText ({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      job_id =  job_id()
      worker_table = create_work_reject()
      html_table = "<table border=1>"
      worker_table$last_submission = as.character(worker_table$last_submission)
      worker_table = rbind(names(worker_table),
                           worker_table)
      for (i in 1:nrow(worker_table)) {
        this_row = worker_table[i,]
        html_table = paste(html_table, '<tr>', sep="\n")
        if (i == 1) {
          for (value in this_row) {
            html_table = paste(html_table, '<td>', sep="\n")
            html_table = paste(html_table,
                               paste("<b>",value, "</b>"),
                               sep="\n") # pastes value!
            html_table = paste(html_table, '</td>', sep="\n")
          }
        } else {
          for (value_id in 1:length(this_row)) {
            value = this_row[value_id]
            html_table = paste(html_table, '<td>', sep="\n")
            if (value_id == 1) {
              value_link = paste("https://crowdflower.com/jobs/",
                                 job_id,
                                 "/contributors/",
                                 value,
                                 sep=""
              )
              value_to_paste= paste("<a href=\"",
                                    value_link,
                                    "\" target=\"_blank\">",
                                    value,
                                    "</a>")
              html_table = paste(html_table, value_to_paste, sep="\n") # pastes value!
            } else {
              html_table = paste(html_table, value, "&nbsp;&nbsp;", sep="\n") # pastes value!
            }
            html_table = paste(html_table, '</td>', sep="\n")
          }
        }
        html_table = paste(html_table, '</tr>', sep="\n")
      }
      html_table = paste(html_table,"</table>", sep="\n")
      return(html_table)
    }
})
  
  output$milkshakeDensity <- renderPlot({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      question_input = input$question_chosen_milkshaker
      if(is.null(question_input)){
        question_input = answers_df$variable[1]
       }
      empty <- ggplot()+geom_point(aes(1,1), colour="white") +
        theme(                              
          plot.background = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
        )
       plot_this = create_summarized_df()
       plot_this = plot_this[plot_this$variable == question_input,]
        
       box_plot <- ggplot(plot_this, aes(x=answer, y=percent)) + geom_boxplot(aes(fill=answer, color=answer))
       box_plot
       density_plot <- ggplot(plot_this, aes(percent, fill=answer)) + geom_density(alpha = 1) + coord_flip()
       density_plot
       scatter_plot <- ggplot(plot_this, aes(num_j, percent)) + geom_point(aes(color=answer)) +
        theme(legend.position = "none")
       scatter_plot
       grid.arrange(box_plot, empty, scatter_plot, density_plot, ncol=2, nrow=2, widths=c(4,2), heights=c(2,3))
      
    }
  })
  
  
  workers <- reactive(function(){
    
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
      
    }else{
      
      full_file = full()
      workers_answers = ddply(full_file, .(X_worker_id), summarize,
                              channel = X_channel[1],
                              country = X_country[1],
                              untrusted = X_tainted[1],
                              trust = X_trust[1],
                              num_judgments = length(X_unit_id),
                              num_golds_seen = length(X_unit_id[X_golden == 'true']),
                              golds_missed = paste(unique(X_unit_id[X_missed == 'true']), collapse="<br>"),
                              last_submission = X_created_at[length(X_created_at)]                           
      )
      
      workers_answers
      
    }
  })
  
  create_answer_index <- reactive({
    
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      
      summarized_df = create_summarized_df()
      
      field_name = input$question_chosen_search
      search_for = input$answer_chosen
      percentage = input$percentage_chosen
      
      
      if (field_name != "all"){
        summarized_df = summarized_df[summarized_df$variable == field_name,]
      }
      
      if(search_for != ""){
        summarized_df = summarized_df[summarized_df$answer == search_for,]
      }
      
      
      print("Percentages Input")
      print(percentage)
      if(min(percentage) != 0.01 || max(percentage) != 1.0){
        print("Did you make it here?")
        summarized_df = summarized_df[summarized_df$percent >= min(percentage) & summarized_df$percent <= max(percentage),]
        print(tail(summarized_df))
      }
      
      summarized_df = summarized_df[order(summarized_df$num_j, decreasing=T),]
      summarized_df
    }
  })
  
  output$create_answer_index_table <- renderText({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      job_id = job_id()
      table= create_answer_index()
      if (nrow(table) > 50){
        max_count = min(50, nrow(table))
        table = table[1:max_count,]
      }
      html_table = "<table border=1>"
      #worker_table_ip$last_submit_ip = as.character(worker_table_ip$last_submit_ip)
      table = rbind(names(table), table)
      for (i in 1:nrow(table)) {
        this_row = table[i,]
        html_table = paste(html_table, '<tr>', sep="\n")
        if (i == 1) {
          for (value in this_row) {
            html_table = paste(html_table, '<td>', sep="\n")
            html_table = paste(html_table,
                               paste("<b>",value, "</b>"),
                               sep="\n") # pastes value!
            html_table = paste(html_table, '</td>', sep="\n")
          }
        } else {
          for (value_id in 1:length(this_row)) {
            value = this_row[value_id]
            html_table = paste(html_table, '<td>', sep="\n")
            if (value_id == 1) {
              value_link = paste("https://crowdflower.com/jobs/",
                                 job_id,
                                 "/contributors/",
                                 value,
                                 sep=""
              )
              value_to_paste= paste("<a href=\"",
                                    value_link,
                                    "\" target=\"_blank\">",
                                    value,
                                    "</a>")
              html_table = paste(html_table, value_to_paste, sep="\n") # pastes value!
            } else {
              html_table = paste(html_table, value, "&nbsp;&nbsp;", sep="\n") # pastes value!
            }
            html_table = paste(html_table, '</td>', sep="\n")
          }
        }
        html_table = paste(html_table, '</tr>', sep="\n")
      }
      html_table = paste(html_table,"</table>", sep="\n")
      paste(html_table)
    }
  })
  
  output$percentageSelector <- renderUI({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
    sliderInput("percentage_chosen", "Percentage Range:",
                     min = .01, max = 1, 
                     value = c(0.01,1.00), 
                     step = .01)
    
    }
  })
  
  
  output$trustSelector <- renderUI({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      workers = workers()
      trust_levels = range(workers$trust)
      sliderInput(inputId = "trust_chosen",
                  label = "Overall Trust",
                  min = trust_levels[1] - .001, max = trust_levels[2] + .001,
                  value = c(trust_levels[1] - .001, trust_levels[2] + .001), step=.001)
    }
  })
  
  output$goldsSeen <- renderUI({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = workers()
      golds_range = range(full_file$num_golds_seen)
      sliderInput(inputId = "golds_chosen",
                  label = "Golds Seen Range",
                  min = golds_range[1] - 1, max = golds_range[2] + 1,
                  value = c(golds_range[1] - 1, golds_range[2] + 1), step= 1)
    }
  })
  
  output$lastTimes <- renderUI({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = workers()
      times_range = range(full_file$last_submission)
      num_time2 = as.numeric(times_range[2])
      num_time1 = as.numeric(times_range[1])
      max_time = ((num_time2 - num_time1)/3600)
      sliderInput(inputId = "times_chosen",
                  label = "Last Submit Times by Hour",
                  min = 0, max = max_time,
                  value = c(0, max_time),
                  step = .5)
      
    }
  })
  
  output$questionSelector <- renderUI({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_gold_answers()
      questions = gsub(questions, pattern=".\\gold", replacement="")
      selectInput(inputId="question_chosen", label="Select question to display:", 
                  questions)
    }
  })
  
  output$questionSelectorContrib <- renderUI({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_gold_answers()
      questions = gsub(questions, pattern=".\\gold", replacement="")
      selectInput(inputId="question_chosen_contrib", label="Select question to display:", 
                  questions)
    }
  })
  
  output$questionSelectorMilkshaker <- renderUI({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_gold_answers()
      questions = gsub(questions, pattern=".\\gold", replacement="")
      selectInput(inputId="question_chosen_milkshaker", label="Select question to display:", 
                  questions)
    }
  })
  
  output$answerSelectorMilkshaker <- renderUI({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      question_subset = input$question_chosen_milkshaker
      answers_list = create_summarized_df()
      answers_list = answers_list[answers_list$variable == question_subset,]
      answers = unique(answers_list$answer)
      selectInput(inputId="answer_chosen_milkshaker", label="Select answer to display in Polychart:", 
                  answers)
    }
  })
  
  output$titleTotalGraph <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      total_workers = workers()
      subset_workers = table_for_answer_distros()
      num_total_workers = length(unique(total_workers$X_worker_id))
      num_subset_workers = length(unique(subset_workers$X_worker_id))
      num_judgments = length(subset_workers$X_worker_id)
      if (num_subset_workers != num_total_workers){
        puts = paste("Answer distributions for", num_subset_workers, "of", num_total_workers, "total workers |", 
                     num_judgments, "judgments used", sep=" ")
      } else {
        puts = paste("Pulling answer distributions from", num_total_workers, "workers |", 
                     num_judgments, "judgments used", sep=" ")          
      }
      h4(puts)
    }
  })    
  
  output$questionSelectorSearch <- renderUI({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_gold_answers()
      questions = gsub(questions, pattern=".\\gold", replacement="")
      questions = c("all", questions)
      selectInput(inputId="question_chosen_search", label="In which columns should we look?", 
                  questions)
    }
  })
  
  table_for_answer_distros <- reactive({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = full()
      workers = workers()
      
      if(input$crowd_chosen != 'all'){
        full_file = full_file[full_file$X_tainted == input$crowd_chosen,]
      }
      
      chosen_state = input$state_chosen
      
      if ("X_golden" %in% names(full_file)) {
        if (chosen_state == "golden") {
          full_file = full_file[full_file$X_golden == 'true',]
        } else if (chosen_state == "normal") {
          full_file = full_file[full_file$X_golden != 'true',]
        }
      }
      
      if (!is.null(input$trust_chosen)) {
        full_file = full_file[full_file$X_trust <= max(input$trust_chosen) &
                                full_file$X_trust >= min(input$trust_chosen),]
      }
      
      if(max(input$golds_chosen) != max(workers$num_golds_seen) || 
           min(input$golds_chosen) != min(workers$num_golds_seen)){
        ids = workers$X_worker_id[workers$num_golds_seen <= max(input$golds_chosen) &
                                    workers$num_golds_seen >= min(input$golds_chosen)]
        
        full_file = full_file[(full_file$X_worker_id %in% ids),]
      }
      
      ###Time Conversion Holders
     max_input_time = max(input$times_chosen)*3600
     min_input_time = min(input$times_chosen)*3600
     convert_max_worker = as.numeric(max(workers$last_submission))
     convert_min_worker = as.numeric(min(workers$last_submission))
     min_worker = min(workers$last_submission)
     max_worker = max(workers$last_submission)
      
     if(max_input_time != convert_max_worker || min_input_time != convert_min_worker){
       ids = workers$X_worker_id[workers$last_submission <= (min_worker + max_input_time) &
                                   workers$last_submission >= (min_worker + min_input_time)]
        
        
       full_file = full_file[(full_file$X_worker_id %in% ids),]
     }
      full_file
    }
  })
  
  table_for_distros_summary <- reactive({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      workers = workers()
      subsetted_workers = table_for_answer_distros()
      subsetted_workers = subsetted_workers$X_worker_id
    
      workers = workers[(workers$X_worker_id %in% subsetted_workers),]
      workers
    }
  })
  
  output$summary_stats_country <- renderTable({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
    workers = table_for_distros_summary()
    total = nrow(workers)
    stats_country_table = ddply(workers, .(country), summarise,
                      num_contribs_country = table(country))
    
    stats_country_table = stats_country_table[order(stats_country_table$num_contribs_country, decreasing=T),]
    for(i in 1:nrow(stats_country_table)){
    stats_country_table$percent[i] = 
      stats_country_table$num_contribs_country[i]/sum(stats_country_table$num_contribs_country)
    stats_country_table
    }
    
    if(nrow(stats_country_table) > 10){
      max_count = min(10, nrow(stats_country_table))
      stats_country_table = stats_country_table[1:max_count,]
      }
    
    stats_country_table
    }
  })
  
  output$summary_stats_channel <- renderTable({
    if (is.na(input$files[1]) || is.null(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      workers = table_for_distros_summary()
      total = nrow(workers)
      
      stats_channel_table = ddply(workers, .(channel), summarise,
                                  num_contribs_channel = table(channel))
      
      stats_channel_table = stats_channel_table[order(stats_channel_table$num_contribs_channel, decreasing=T),]
      
      for(i in 1:nrow(stats_channel_table)){
        stats_channel_table$percent[i] =
          stats_channel_table$num_contribs_channel[i]/sum(stats_channel_table$num_contribs_channel)
        stats_channel_table
      }
      
      if(nrow(stats_channel_table) > 10){
        max_count = min(10, nrow(stats_channel_table))
        stats_channel_table = stats_channel_table[1:max_count,]
      }
      
      stats_channel_table
    }
  })

  
  output$total_distros <- renderChart({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = table_for_answer_distros()
      answer_cols = grepl(pattern=".\\gold$", names(full_file)) &
        !grepl(pattern=".\\golden",names(full_file))
      answer_cols_names = names(full_file)[answer_cols]
      answer_cols_names = gsub(answer_cols_names, pattern=".\\gold", replacement="")
      
      chosen_q = input$question_chosen
      question_index = which(answer_cols_names == chosen_q)
      
      responses = lapply(answer_cols_names, function(x) {
        responses = table(full_file[,names(full_file)==x])
        responses/sum(responses)
      }
      )
      
      responses_table = responses[[question_index]]
      
      responses_table_transformed = data.frame(questions = names(responses_table),
                                               numbers = as.numeric(responses_table),
                                               group = chosen_q)
      
      responses_table_transformed = 
        responses_table_transformed[order(-responses_table_transformed$numbers),]
      
      if (nrow(responses_table_transformed) > 9 ) {
        responses_table_transformed1 = responses_table_transformed[1:9,]
        responses_table_transformed1[10,] =
          c("Other Values", sum(responses_table_transformed$numbers[10:length(responses_table_transformed)]),
            chosen_q)
        responses_table_transformed = responses_table_transformed1
      }
      
      responses_table_transformed$questions[responses_table_transformed$questions==""] = "\"\""
      
      
      p3 <- nPlot(numbers ~ questions, data=responses_table_transformed,
                  group = 'group', type='multiBarChart', 
                  dom='total_distros', width=800, margin=60, overflow="visible") 
      
      p3$xAxis(rotateLabels=45)
      p3$xAxis(axisLabel='Total Responses')
      p3$chart(reduceXTicks = FALSE)
      p3
    }
    
  })
  
  output$contrib_distros <- renderChart({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = full_file_contrib_id()
      full_file_all = full()
      
      if(input$contrib_crowd_chosen != 'all'){
        full_file_all = full_file_all[full_file_all$X_tainted == input$contrib_crowd_chosen,]
        
      }
      
      answer_cols = grepl(pattern=".\\gold$", names(full_file)) &
        !grepl(pattern=".\\golden",names(full_file))
      answer_cols_names = names(full_file)[answer_cols]
      answer_cols_names = gsub(answer_cols_names, pattern=".\\gold", replacement="")
      
      
      chosen_q = input$question_chosen_contrib
      question_index = which(answer_cols_names == chosen_q)
      
      chosen_state = input$state_chosen_contrib
      
      if ("X_golden" %in% names(full_file)) {
        if (chosen_state == "golden") {
          full_file = full_file[full_file$X_golden == 'true',]
          full_file_all = full_file_all[full_file_all$X_golden == 'true',]
        } else if (chosen_state == "normal") {
          full_file = full_file[full_file$X_golden != 'true',]
          full_file_all = full_file_all[full_file_all$X_golden != 'true',]
        }
      }
      
      responses = lapply(answer_cols_names, function(x) {
        responses = table(full_file[,names(full_file)==x])
        responses/sum(responses)
      })
      
      responses_all = lapply(answer_cols_names, function(y){
        responses_all = table(full_file_all[,names(full_file_all)==y])
        responses_all/sum(responses_all)
      })
      
      individual = as.data.frame(responses[[question_index]])
      all = as.data.frame(responses_all[[question_index]])
      missing_var1 = all$Var1[!(all$Var1 %in% individual$Var1)]
      
      if (length(missing_var1) != 0){
        missing_rows = data.frame(Var1 = missing_var1, Freq = 0 )
        individual = rbind(individual, missing_rows)
      }
      
      responses_table_bind = rbind(individual, all)
      group_var = c(rep("individual", times=nrow(individual)),
                    rep("all", times=nrow(all)))
      responses_table_bind$group_var = group_var
      
      responses_table_transformed = data.frame(questions = as.character(responses_table_bind$Var1), 
                                               numbers = as.numeric(responses_table_bind$Freq),
                                               group = responses_table_bind$group_var)
      
      
      
      responses_table_transformed_a = 
        responses_table_transformed[responses_table_transformed$group == 'all',]
      responses_table_transformed_a = 
        responses_table_transformed[order(responses_table_transformed$numbers, decreasing=T),]
       responses_table_transformed_b =
         responses_table_transformed[responses_table_transformed$group == 'individual',]
       responses_table_transformed_b =
         responses_table_transformed_b[order(responses_table_transformed_b$numbers, decreasing=T),]
      
      if (nrow(responses_table_transformed_a) > 9 ) {
          responses_table_transformed1 = responses_table_transformed_a[1:9,]
          responses_table_transformed1[10,] =
            c("Other Values All", sum(responses_table_transformed_a$numbers[10:length(responses_table_transformed_a)]),
              chosen_q)
          responses_table_transformed_a = responses_table_transformed1
        }
      
       if(nrow(responses_table_transformed_b) > 9){
         responses_table_transformed2 = responses_table_transformed_b[1:9,]
         responses_table_transformed2[10,] =
           c("Other Values Individual", sum(responses_table_transformed_b$numbers[10:length(responses_table_transformed_b)]),
             chosen_q)
         responses_table_transformed_b = responses_table_transformed2         
       }
      
    
      responses_table_transformed <- rbind(responses_table_transformed_a, responses_table_transformed_b)
      
      responses_table_transformed$questions[responses_table_transformed$questions==""] = "\"\""
     
      p4 <- nPlot(numbers ~ questions, data=responses_table_transformed,
                  group = 'group', type='multiBarChart', 
                  dom='contrib_distros', width=800, margin=60, overflow="visible") 
      
      p4$xAxis(rotateLabels=45)
      p4$chart(reduceXTicks = FALSE)
      
      p4
    }
  })
  
  output$graphDesc <- renderUI({ 
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      profile_id = input$id_chosen
      puts <- paste('This graph compares answers from', profile_id, 'with the total distributions of the job.', sep=" ")
      puts
    }   
  })
  
})
