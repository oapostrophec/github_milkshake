    ###Milkshaker Server###
    ###Last updated 02/10/2014###
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
    setwd('~/Documents/milkshaker')
    #full = read.csv('f355916.csv', stringsAsFactors = F)
    
    shinyServer(function(input, output){
      
      ### read in file    
      full <- reactive(function() {
        if (is.na(input$files[1])) {
          # User has not uploaded a file yet
          return(NULL)
        } else {
          inFile <- input$files
          full = read.csv(inFile$datapath, na.strings="NaN", stringsAsFactors=FALSE)
          #print(head(full$X_created_at))
          #print(full$X_created_at)
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
      
        return(workers_answers)
            
      }
    })
      
    output$answer_columns <- renderText(function(){
      
      if (is.na(input$files[1])) {
        # User has not uploaded a file yet
        return(NULL)
        
      }else{
        
      file = full()  
      gold_cols = grepl(".\\gold$", names(file)) & !grepl(".\\golden",names(file))
      gold_cols_names = names(file)[gold_cols]    
      
      #ans_cols = gsub(gold_cols, pattern=".\\gold", replacement="")
      ans_cols_names = gsub(gold_cols_names, pattern=".\\gold", replacement="")
      answer_data = c(gold_cols_names, ans_cols_names)
      print(answer_data)
      return(answer_data)
      
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
          #print(times_range)
          num_time2 = as.numeric(times_range[2])
          num_time1 = as.numeric(times_range[1])
          #print(num_time1)
          #print(num_time2)
          max_time = ((num_time2 - num_time1)/3600)
          #print(max_time)
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
      #if (new_workers_trust > 50){
      #  max_count = min(50, nrow(new_workers_trust))
      #  new_workers_trust = 50
      #}
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
          
          if(max(input$golds_chosen) != max(workers$num_golds_seen) || min(input$golds_chosen) != min(workers$num_golds_seen)){
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
  #      workers = workers()
        
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
          
  #     for(i in 1:length(full_file$X_worker_id)){
  #       full_file$type[i] = "individual" 
  #     }
          
  #     for(i in 1:length(full_file_all$X_worker_id)){
  #       full_file_all$type[i] = "all"
  #     }
             
      #full_file = rbind(full_file, full_file_all)
      #print("combined files")
      #print(full_file)
          
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
          
      responses_table = responses[[question_index]]
      #print(typeof(responses_table))
      #print(responses_table)
      #responses_table$type = "individual"
      responses_table_all = responses[[question_index]]
      print("Total Table")
      print(responses_table_all)
      #responses_table$type = "total"
      
      responses_table_bind = rbind(responses_table, responses_table_all)
      print("Binded Table")
      print(responses_table_bind)
      print(names(responses_table_bind))
      responses_table_transformed = data.frame(questions = names(responses_table),
                                               numbers = as.numeric(responses_table),
                                               group = chosen_q)
          
      #responses_table_bind_transformed = data.frame(questions = names(responses_table_bind),
      #                                              numbers = as.numeric(responses_table_bind),
      #                                              group = chosen_q)
      
      print(responses_table_transformed)
          
          
      if (nrow(responses_table_transformed) > 9 ) {
        responses_table_transformed1 = responses_table_transformed[1:9,]
        responses_table_transformed1[10,] =
          c("Other Values", sum(responses_table_transformed$numbers[10:length(responses_table_transformed)]),
            chosen_q)
        responses_table_transformed = responses_table_transformed1
      }
          
      responses_table_transformed$questions[responses_table_transformed$questions==""] = "\"\""
          
      
      p4 <- nPlot(numbers ~ questions, data=responses_table_transformed,
                  group = 'group', type='multiBarChart', 
                  dom='contrib_distros', width=800, margin=60, overflow="visible") 
          
      p4$xAxis(rotateLabels=45)
      p4$xAxis(axisLabel='Worker Responses')
      p4$chart(reduceXTicks = FALSE)
      #scale.tickFormat(20, "$,.2f");
      p4
    }
  })
         
  #     output$contrib_distros <- renderChart({
  #       if (is.na(input$files[1])) {
  #         # User has not uploaded a file yet
  #         return(NULL)
  #       } else {
  #         full_file = full_file_contrib_id()
  #         #workers = workers()
  #         
  #         answer_cols = grepl(pattern=".\\gold$", names(full_file)) &
  #           !grepl(pattern=".\\golden",names(full_file))
  #         answer_cols_names = names(full_file)[answer_cols]
  #         answer_cols_names = gsub(answer_cols_names, pattern=".\\gold", replacement="")
  #         
  #         chosen_q = input$question_chosen_contrib
  #         question_index = which(answer_cols_names == chosen_q)
  #         
  #         chosen_state = input$state_chosen_contrib
  #         
  #         if ("X_golden" %in% names(full_file)) {
  #           if (chosen_state == "golden") {
  #             full_file = full_file[full_file$X_golden == 'true',]
  #           } else if (chosen_state == "normal") {
  #             full_file = full_file[full_file$X_golden != 'true',]
  #           }
  #         }
  #         
  #         responses = lapply(answer_cols_names, function(x) {
  #           responses = table(full_file[,names(full_file)==x])
  #           responses/sum(responses)
  #         }
  #         )
  #         
  #         responses_table = responses[[question_index]]
  #         
  #         responses_table_transformed = data.frame(questions = names(responses_table),
  #                                                  numbers = as.numeric(responses_table),
  #                                                  group = chosen_q)
  #         
  #         responses_table_transformed = 
  #           responses_table_transformed[order(-responses_table_transformed$numbers),]
  #         
  #         if (nrow(responses_table_transformed) > 9 ) {
  #           responses_table_transformed1 = responses_table_transformed[1:9,]
  #           responses_table_transformed1[10,] =
  #             c("Other Values", sum(responses_table_transformed$numbers[10:length(responses_table_transformed)]),
  #               chosen_q)
  #           responses_table_transformed = responses_table_transformed1
  #         }
  #         
  #         responses_table_transformed$questions[responses_table_transformed$questions==""] = "\"\""
  #         
  #         
  #         p4 <- nPlot(numbers ~ questions, data=responses_table_transformed,
  #                     group = 'group', type='multiBarChart', 
  #                     dom='contrib_distros', width=800, margin=60, overflow="visible") 
  #         
  #         p4$xAxis(rotateLabels=45)
  #         p4$xAxis(axisLabel='Worker Responses')
  #         p4$chart(reduceXTicks = FALSE)
  #         p4
  #       }
  #       
  #     })    
  })
