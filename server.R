###Milkshaker Server###
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
require('stringr')
require('gridExtra')
require('reshape2')

options(stringsAsFactors = F)
options(shiny.maxRequestSize=150*1024^2)

shinyServer(function(input, output){

#File Dissection:
  
  ### read in file    
  full <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else if (input$job_id_chosen > 0) {
      inFile = input$job_id
      #tryCatch(
      system(paste('s3cmd --force get s3://crowdflower_prod/f',
                   inFile,'.csv.zip /tmp/f', inFile, '.zip',sep=''),
             intern=T)
      
      system(paste('unzip -o /tmp/f', inFile, '.zip -d /tmp', sep=''))
      val <- try(read.csv(paste('/tmp/f',inFile,'.csv',sep='')), silent=TRUE)
      if (inherits(val, "try-error")) {
        stop(paste("Sorry, we could not find your file. Either job", input$job_id,
                   "does not exist or its Full Report has not been generated yet.",
                   "Check", paste("http://crowdflower.com/jobs/",input$job_id,"/reports.",sep=""),
                   "\nRefresh this page before uploading a csv."))
      } else {
        full_file = read.csv(paste('/tmp/f',inFile,'.csv',sep=''))
        log_id = inFile
      }
    } else {
      inFile <- input$files
      full = read.csv(inFile$datapath, na.strings="NaN", stringsAsFactors=FALSE)
      log_id = input$files$name
      
      return(full)
    }
  })
  
  ###Extract Job ID
  job_id <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else if(input$job_id_chosen){
      return(input$job_id_chosen)
    } else {
      inFile <- input$files$name
      job_id = gsub(inFile, pattern="^f", replacement="")
      job_id = str_extract(job_id, "\\d{6}")
      return(job_id)
    }
  })
  
  ###Output Summary Message: Sidebar
  output$summary_message <- renderText({
    if ((is.null(input$files[1]) || is.na(input$files[1])) && input$job_id_chosen==0) {
      # User has not uploaded a file yet
      return("<p>You have not uploaded any files yet</p>")
    } else {
      full_file = full()
      num_contributors = length(unique(full_file$X_worker_id))
      num_gold_units = length(unique(full_file$X_unit_id[full_file$X_golden=='true']))
      num_nongold_units = length(unique(full_file$X_unit_id)) - num_gold_units
      num_trusted_judgments = sum(full_file$X_tainted != 'true')
      num_untrusted_judgments = sum(full_file$X_tainted == 'true')
      if (num_untrusted_judgments == 0) {
        tainted_message = "<p style='color:red;'>It looks like you don't have any <b>tainted judgments</b> in your file. Change reporting settings and regenerate the report if this is not intentional.</p>"
      } else {
        tainted_message = ""
      }
      if (num_gold_units == 0) {
        gold_message = "<p style='color:red;'>It looks like you don't have any <b>Gold units</b> in yout file. Change reporting settings and regenerate the report if this is not intentional.</p>"
      } else {
        gold_message = ""
      }
      overall_message = paste("<p>The report you uploaded has:<br>",
                              num_contributors, " contributors,<br>",
                              num_gold_units, " gold units,<br>",
                              num_nongold_units, " ordinary units,<br>",
                              num_trusted_judgments, " trusted judgments,<br>",
                              num_untrusted_judgments, " untrusted judgments.<br>",
                              "</p>", sep="")
      paste(overall_message, tainted_message, gold_message)
    } 
  })
  
  ###Grab Answer Columns
  full_file_answers_columns <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = full()
      gold_cols = grepl(".\\gold$", names(full_file)) & !grepl(".\\golden",names(full_file))
      gold_cols_names = names(full_file)[gold_cols]    
      answer_columns = gsub(gold_cols_names, pattern=".(gold)$", replacement="")
      answer_columns
    }
    
  })
  
  ###Subset based on contributor id
  
  full_file_contrib_id <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$id_chosen == "") {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file_contrib_id = full()
      id = input$id_chosen
      full_file_contrib_id = full_file_contrib_id[full_file_contrib_id$X_worker_id == id,]
      
      full_file_contrib_id
    }
    
  })
  
  ###Aggregate Basic Worker Infos
  workers <- reactive({
    
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      return(NULL)
    }else{
      
      full_file = full()
      
      if("X_tainted" %in% names(full_file)){
      workers_answers = ddply(full_file, .(X_worker_id), summarize,
                              channel = X_channel[1],
                              country = X_country[1],
                              untrusted = X_tainted[1],
                              trust = X_trust[1],
                              num_judgments = length(X_unit_id),
                              last_submission = X_created_at[length(X_created_at)])
      } else {
      workers_answers = ddply(full_file, .(X_worker_id), summarize,
                              channel = X_channel[1],
                              country = X_country[1],
                              #untrusted = X_tainted[1],
                              trust = X_trust[1],
                              num_judgments = length(X_unit_id),
                              last_submission = X_created_at[length(X_created_at)])  
      }
      
      
      workers_answers  
    }
  })
  
  
#SideBar Panel Outputs:
  
  output$trustSelector <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
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
  
  output$percentageSelector <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      sliderInput("percentage_chosen", "Percentage Range:",
                  min = .01, max = 1, 
                  value = c(0.01,1.00), 
                  step = .01)   
    }
  })
  
  output$xSelector <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
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
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      threshold_min = 0.00
      threshold_max = 1.00
      
      sliderInput(inputId="y_axis_chosen",
                  label="Milkshake Answer Percent Range to Threshold",
                  min = threshold_min -.01, max = threshold_max + .01,
                  step = 0.01, value=c(threshold_min - .01, threshold_max + .01))
      
    }
  })
  
  output$ddLewis <- renderText ({
    image_path = "http://cf-public-view.s3.amazonaws.com/coolstuff/milkshake_pic.jpg"
    html_image = paste("<img src=", image_path, " width=\"65%\"/>", sep="")
    paste(html_image)
    
  })
  
#   output$mixpanelEvent_job_id <- renderText({
#     paste0("<script>mixpanel.track('milkshaker job_id',{job_id: ",job_id(),"})</script>")
#   })
  
  
  create_summarized_df <- reactive ({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
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
    if (is.null(input$files[1]) || is.na(input$files[1])) {
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
  
  
  output$suggestedRules <- renderText({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
    df = create_plot_df()
    percent_min = min(input$y_axis_chosen) * 100
    percent_max = max(input$y_axis_chosen) * 100
    num_judgments = input$x_axis_chosen
    rule = df$answer[1]
      
    question = df$variable[1]
    judgment = paste("Set Activation Threshold to", num_judgments, "judgments.", sep=" ")
    rule_reg_ex = paste("Enter (", rule, ") into the Acceptable answer distribution for ", question,
                 ".", sep="")
    
    if(rule == "\"\""){
      rule = "^\\s+"
      rule_reg_ex = paste("Enter ", rule, " into the Acceptable answer distribution for ", question,
                          ".", sep="")
    } 
    
    min_max = paste("Set the Min percentage to ", percent_min, ". Set the Max percentage to ",
                    percent_max, ".", sep="")
    
    if(percent_min > -1 || percent_max < 101){
      if(percent_min == -1){
        min_max = paste("Set the Min percentage to 0. Set the Max percentage to ",
                        percent_max, ".", sep="")
      }
      if(percent_max == 101){
        min_max = paste("Set the Min percentage to ", percent_min, ". Set the Max percentage to 100.", sep="")  
      }
      suggested_rules = paste(judgment, rule_reg_ex, min_max, sep="<br>")
    } else{
    suggested_rules = paste("No data detected.")
    }
    
    suggested_rules
    }
  })
  
  output$milkshakeQuartile <- renderPlot({
    
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      answers_df = create_plot_df()
      
      x_threshold = input$x_axis_chosen
      max_y_threshold = max(input$y_axis_chosen)
      min_y_threshold = min(input$y_axis_chosen)
      set_scatter_plot <- ggplot(answers_df, aes(num_j, percent)) + geom_point(aes(color=answer), color = "#0033cc") +
        geom_vline(xintercept = x_threshold, color="#0000cc") +
        geom_hline(yintercept = max_y_threshold, color="#009933") +
        geom_hline(yintercept = min_y_threshold, color="#009933")
      
      ##DO NOT DELETE
        print(set_scatter_plot)
      
    }
  })
  
  create_work_reject <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      workers = workers()
      table = create_plot_df()
      
      judgments_threshold = input$x_axis_chosen
      max_percent_accepted = max(input$y_axis_chosen)
      min_percent_accepted = min(input$y_axis_chosen)
      

      table_1 = table[(table$num_j > judgments_threshold & table$percent < min_percent_accepted),]
      #table = table[(table$percent < min_percent_accepted),]
      table_2 = table[(table$num_j > judgments_threshold & table$percent > max_percent_accepted),]
      
     # worker_ids = table_1$X_worker_id
      worker_ids = c(table_2$X_worker_id, table_1$X_worker_id)
      
      workers_lost = workers[(workers$X_worker_id %in% worker_ids),]
      
      workers_lost
    }
  })
  
  output$createAnswerRejects <- renderDataTable({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      table = create_work_reject()
      job_id = job_id()
      
      if(nrow(table) != 0){
        workers = table$X_worker_id
      
        for(i in 1:nrow(table)){
          table$X_worker_id[i] = 
            paste('<a target=\"_blank\" href=\"https://crowdflower.com/jobs/', job_id,
                 '/contributors/', table$X_worker_id[i], 
                 '\">', table$X_worker_id[i], '</a>', sep="")
        
          table$reject[i] = 
            paste('<button class=\"btn btn-danger action-button shiny-bound-input\" 
                 data-toggle=\"button\" id=\"get', workers[i],
                 '\" type=\"button\">Reject</button>', sep="")
        
        }
       table
      } else {
        #if the table is empty return nothing
        return(NULL)
      }
     
    }
  })
  
  output$milkshakeDensity <- renderPlot({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
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
       
       num_workers = length(unique(plot_this$X_worker_id))
       
       num_ans = length(unique(plot_this$answer))
       unique_answers = unique(plot_this$answer)
       plot_this = plot_this[plot_this$variable == question_input,]
       plot_this = plot_this[order(plot_this$percent, decreasing=T),]
       
       if (num_ans > 10){
         plot_this_1 = 
           plot_this[plot_this$answer == unique_answers[1:5],] 
         
         plot_this = plot_this[order(plot_this$percent, decreasing=F),]
         plot_this_2 =
           plot_this[plot_this$answer == unique_answers[1:5],]
         
         
         plot_this = rbind(plot_this_1, plot_this_2)
       }
        
       box_plot <- ggplot(plot_this, aes(x=answer, y=percent)) + geom_boxplot(aes(fill=answer, color=answer)) + 
        opts(axis.text.x=theme_blank()) 
       box_plot 
       density_plot <- ggplot(plot_this, aes(percent, fill=answer)) + geom_density(alpha = 1) + coord_flip() +
        theme(legend.position = "none") 
       density_plot
       scatter_plot <- ggplot(plot_this, aes(num_j, percent)) + geom_point(aes(color=answer)) +
        theme(legend.position = "none") 
       scatter_plot
       grid.arrange(box_plot, empty, scatter_plot, density_plot, ncol=2, nrow=2, widths=c(4,2), heights=c(2,3))
      
    }
  })
    
  create_answer_index <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      
      summarized_df = create_summarized_df()
      
      field_name = input$question_chosen_search
      #percentage = input$percentage_chosen
      
      
      if (field_name != "all"){
        summarized_df = summarized_df[summarized_df$variable == field_name,]
      }
      
      
    # if(min(percentage) != 0.01 || max(percentage) != 1.0){
    #   summarized_df = 
    #   summarized_df[summarized_df$percent >= min(percentage) & summarized_df$percent <= max(percentage),]
    # }
      
      summarized_df = summarized_df[order(summarized_df$num_j, decreasing=T),]
      summarized_df
    }
  })
  
  output$createSearchTableButton <- renderText({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
        button = paste('<button class=\"btn btn-danger action-button shiny-bound-input\" 
                        data-toggle=\"button\" id=\"get_workers\" type=\"button\">Reject All</button>', sep="")    
        button
    }
  })

  output$createSearchTable <- renderDataTable({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      job_id = job_id()
      table = create_answer_index()
       
      print("Is this updating?")
      print(head(table))
      table
    }
  }, options = list(bSortClasses = TRUE))
  
  output$questionSelector <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_answers_columns()
      selectInput(inputId="question_chosen", label="Select question to display:", 
                  questions)
    }
  })
  
  output$questionSelectorContrib <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_answers_columns()
      selectInput(inputId="question_chosen_contrib", label="Select question to display:", 
                  questions)
    }
  })
  
  output$questionSelectorMilkshaker <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_answers_columns()
      selectInput(inputId="question_chosen_milkshaker", label="Select question to display:", 
                  questions)
    }
  })
  
  output$answerSelectorMilkshaker <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      question_subset = input$question_chosen_milkshaker
      answers_list = create_summarized_df()
      answers_list = answers_list[answers_list$variable == question_subset,]
      answers = unique(answers_list$answer)
      selectizeInput("answer_chosen_milkshaker", 
                      "Select answer to display below:", 
                      choices = answers,
                      multiple = FALSE,
                      selected = answers[1],
                      #selectize = TRUE
                      options = list(
                        plugins = I("['optgroup_columns']"))
                )
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
        puts = paste("Answer distributions for", 
                     num_subset_workers, "of", num_total_workers, "total workers |", 
                     num_judgments, "judgments used", sep=" ")
      } else {
        puts = paste("Pulling answer distributions from", num_total_workers, "workers |", 
                     num_judgments, "judgments used", sep=" ")          
      }
      h4(puts)
    }
  })    
  
  output$questionSelectorSearch <- renderUI({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      questions = full_file_answers_columns()
      questions = c("all", questions)
      selectInput(inputId="question_chosen_search", label="In which columns should we look?", 
                  questions)
    }
  })
  
  table_for_answer_distros <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      return(NULL)
    } else {
      full_file = full()
      #workers = workers()
     
      if("X_tainted" %in% names(full_file)){
       if(input$crowd_chosen != 'all'){
         full_file = full_file[full_file$X_tainted == input$crowd_chosen,]
       }
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
      
      full_file
    }
  })
  
  table_for_distros_summary <- reactive({
    if (is.null(input$files[1]) || is.na(input$files[1])) {
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
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
    workers = table_for_distros_summary()
    total = nrow(workers)
    stats_country_table = ddply(workers, .(country), summarise,
                      num_contribs_country = table(country))
    
    stats_country_table = 
      stats_country_table[order(stats_country_table$num_contribs_country, decreasing=T),]
    
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
    if (is.null(input$files[1]) || is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      workers = table_for_distros_summary()
      total = nrow(workers)
      
      stats_channel_table = ddply(workers, .(channel), summarise,
                                  num_contribs_channel = table(channel))
      
      stats_channel_table = 
        stats_channel_table[order(stats_channel_table$num_contribs_channel, decreasing=T),]
      
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
    if (is.null(input$files[1]) || is.na(input$files[1]) || is.null(input$question_chosen)) {
      return(NULL)
    } else {
      full_file = table_for_answer_distros()
     
      answer_cols_names = full_file_answers_columns()

      chosen_q = input$question_chosen
      question_index = which(answer_cols_names == chosen_q)
      if(is.null(question_index)){
         question_index = answer_cols_names[1]
       }
      
      responses = lapply(answer_cols_names, function(x) {
        responses = table(full_file[,names(full_file)==x])
        responses/sum(responses)
      })
      
      responses_table = responses[[question_index]]
      
      responses_table_transformed = data.frame(questions = names(responses_table),
                                               numbers = as.numeric(responses_table),
                                               group = chosen_q)
      
      responses_table_transformed = 
        responses_table_transformed[order(-responses_table_transformed$numbers),]
      
      if (nrow(responses_table_transformed) > 9 ) {
        responses_table_transformed1 = responses_table_transformed[1:9,]
        responses_table_transformed1[10,] =
          c("Other Values", 
            sum(responses_table_transformed$numbers[10:length(responses_table_transformed)]),
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
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$id_chosen == "") {
      return(NULL)
    } else {
      full_file = full_file_contrib_id()
      full_file_all = full()
      
      answer_cols_names = full_file_answers_columns()
      chosen_q = input$question_chosen_contrib
      question_index = which(answer_cols_names == chosen_q)      
      chosen_state = input$state_chosen_contrib
      
      if("X_tainted" %in% names(full_file_all)){
      if(input$contrib_crowd_chosen != 'all'){
        full_file_all = full_file_all[full_file_all$X_tainted == input$contrib_crowd_chosen,]
        
        }
      }
      
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
            
    
      responses_table_transformed <- rbind(responses_table_transformed_a, responses_table_transformed_b)
      
      responses_table_transformed$questions[responses_table_transformed$questions==""] = "\"\""
      
      
      if(nrow(responses_table_transformed) > 10){
        responses_table_transformed_1 = responses_table_transformed[responses_table_transformed$group == "all",]
          responses_table_transformed_1 = responses_table_transformed_1[1:10,]
        responses_table_transformed_2 = responses_table_transformed[responses_table_transformed$group == "individual",]
          responses_table_transformed_2 = responses_table_transformed_2[1:10,]
        responses_table_transformed_3 = rbind(responses_table_transformed_1, responses_table_transformed_2)
       
        responses_table_transformed = responses_table_transformed_3
       }
      
      if(input$order_chosen == FALSE){
        responses_table_transformed = responses_table_transformed[with(responses_table_transformed,
                                                                       order(-(as.integer(factor(group))))),]
      } else {
        responses_table_transformed = responses_table_transformed[with(responses_table_transformed,
                                                                       order(as.integer(factor(group)))),]
      }
      
      p4 <- nPlot(numbers ~ questions, data=responses_table_transformed,
                  group = 'group', type='multiBarChart', 
                  dom='contrib_distros', width=800, margin=60, overflow="visible") 
      
      p4$xAxis(rotateLabels=45)
      p4$chart(reduceXTicks = FALSE)
      
      p4
    }
  })
  
  output$graphDesc <- renderUI({ 
    if (is.null(input$files[1]) || is.na(input$files[1]) || input$id_chosen == "") {
      return(NULL)
    } else {
      profile_id = input$id_chosen
      puts <- paste('This graph compares answers from', 
                    profile_id, 'with the total distributions of the job.', sep=" ")
      puts
    }   
  })
  
})
