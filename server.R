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


options(stringsAsFactors = F)
options(shiny.maxRequestSize=150*1024^2)

shinyServer(function(input, output){
  
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
    
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    }else{
      
      file = full()  
      gold_cols = grepl(".\\gold$", names(file)) & !grepl(".\\golden",names(file))
      gold_cols_names = names(file)[gold_cols]    
      ans_cols_names = gsub(gold_cols_names, pattern=".\\gold", replacement="")
      #answer_data = c(gold_cols_names, ans_cols_names)
      #print(answer_data)
      file[is.na(file)] = ""
      print('bout to melt')
      print(head(file))
      full_melt = file[,c("X_worker_id", ans_cols_names)]
      print(names(full_melt))
      print('bout to melt 2')
      print(head(full_melt))
      melted_df_full = melt(full_melt, id="X_worker_id")
      melted_df_full$value = as.character(melted_df_full$value)
      melted_df_full$variable = as.character(melted_df_full$variable)
      melted_df_full$X_worker_id = as.numeric(as.character(melted_df_full$X_worker_id))
     # melted_df_full$value[melted_df_full$value =="" ] = "\"\""
      print(head(melted_df_full))
      summarized_df = ddply(melted_df_full, .(X_worker_id, variable, value), summarize, 
                    percent = 
                      length(value)/sum(melted_df_full$X_worker_id == X_worker_id[1] & melted_df_full$variable == variable[1]), 
                    num_j = sum(melted_df_full$X_worker_id == X_worker_id[1]))
      print(head(summarized_df))
      summarized_df
    }
  })
  
  output$create_answer_index_table <- renderText({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      table= create_answer_index()
      #if (nrow(table) > 50){
      #  max_count = min(50, nrow(table))
      #  table = table[1:max_count,]
      #}
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
            html_table = paste(html_table, value, "&nbsp;&nbsp;", sep="\n") # pastes value!
            html_table = paste(html_table, '</td>', sep="\n")
          }
        }
        html_table = paste(html_table, '</tr>', sep="\n")
      }
      html_table = paste(html_table,"</table>", sep="\n")
      paste(html_table)
    }
  })
  
  
  output$trustSelector <- renderUI({
    if (is.na(input$files[1])) {
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
  
  table_for_answer_distros <- reactive({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = full()
      workers = workers()
      
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
  
  output$total_distros <- renderChart({
    if (is.na(input$files[1])) {
      # User has not uploaded a file yet
      return(NULL)
    } else {
      full_file = table_for_answer_distros()
      
      if(input$crowd_chosen != 'all'){
        full_file = full_file[full_file$X_tainted == input$crowd_chosen,]
        
      }
      
      answer_cols = grepl(pattern=".\\gold$", names(full_file)) &
        !grepl(pattern=".\\golden",names(full_file))
      answer_cols_names = names(full_file)[answer_cols]
      answer_cols_names = gsub(answer_cols_names, pattern=".\\gold", replacement="")
      
      chosen_q = input$question_chosen
      question_index = which(answer_cols_names == chosen_q)
      
      chosen_state = input$state_chosen
      
      if ("X_golden" %in% names(full_file)) {
        if (chosen_state == "golden") {
          full_file = full_file[full_file$X_golden == 'true',]
        } else if (chosen_state == "normal") {
          full_file = full_file[full_file$X_golden != 'true',]
        }
      }
      
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
      
      
      #responses_table_transformed = 
      #  responses_table_transformed[order(-responses_table_transformed$numbers),]
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
      
      #print(responses_table_transformed_b)
      responses_table_transformed <- rbind(responses_table_transformed_a, responses_table_transformed_b)
      #print(responses_table_transformed)
      responses_table_transformed$questions[responses_table_transformed$questions==""] = "\"\""
      #View(responses_table_transformed)    
      
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
