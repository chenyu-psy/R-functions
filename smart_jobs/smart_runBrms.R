#' A smart function which can find a appropriate time to launch models.
#' 
#' @author Chenyu Li
#' @description
#' This is a function used to running brms in job environment. 
#' The function relies on job log file. It will detect how many jobs are running 
#' and how many cores are available on the computer.
#' Only when the computer has sufficient free cores and the model is in the first sequence, the model will start.
#' 
#'@param formula a formula. The formula that you want to input into `brm` function.
#'@param data a data.frame. The data that you want to use to train the model.
#'@param args a list. All the other arguments that you want to input into `brm` function.
#'@param model_path a folder path. Where do you want to save your model.
#'@param name a string. The name of the file.
#'@param log_path a folder path. Where do you want to save your job log file.
#'@param priority a number. The default is 1, higher number indicates higher priority. 
#'Models with higher priority will be launched in advance.
#'@param maxCore a number. How many cores are available on this computer.
#'@param checkInt a number. How long would the function check the job log. The default value is 30 seconds.
#'
#'@return There is no return. The model will be saved as a file automatically.
#'
#'
smart_runBrms <- function(formula=NULL, data=NULL, prior=NULL, args, model_path, name, log_path = model_path, priority = 1, maxCore = NULL, checkInt = 30){
  
  # check packages
  packages = c("brms", "dplyr", "stringr", "glue", "progress")
  for (pkg in packages) {
    
    if (!require(pkg, character.only = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  
  # check whether the job_log file exists
  log_file = str_glue("{log_path}job_log.rds")
  if (!file.exists(log_file)) {
    
    Table_job_status <- data.frame(
      index = 0,
      core = 0,
      status = "completed",
      priority = 1)
    
    write_rds(Table_job_status, log_file)
    
    message("There is no job log file, a new one has been created")
  } else {
    
    Table_job_status <- read_rds(log_file)
    
  }
  
  # if the maximum cores is not defined, read the maximum cores from the computer
  if (is.null(maxCore)) {
    maxCore = parallel::detectCores(logical = FALSE)
  }
  
  # get the usage of the cores
  nCore = args$cores
  
  # check whether the maxCore was smaller than the need of the core
  if (nCore > maxCore) stop("The current model requires more cores than the maximum number of cores available on the computer.")
  if (maxCore > parallel::detectCores()) stop("The current model requires more cores than the maximum number of cores available on the computer.")
  
  # set a index for the model
  model_index <- nrow(Table_job_status)
  
  # input model information to the log file
  Table_job_status <- Table_job_status %>% 
    rbind(data.frame(
      index = model_index,
      core = nCore,
      status = "waiting",
      priority = priority
    ))
  
  write_rds(Table_job_status, log_file)
  
  # set up the progress bar
  message("\nThe model is now in the waiting list ...")
  pb = txtProgressBar(min = 0, max = model_index, initial = 0, style = 3)
  
  
  #' check how many models are running,
  #' wait until the model meet the running condition
  while (TRUE) {
    
    # Read job log
    Table_job_status <- read_rds(log_file)
    
    # How many models are running
    Table_running <- Table_job_status %>% 
      filter(status=="running")
    # How many cores have been used
    Using_cores = sum(Table_running$core)
    
    # Adjust the waiting list
    Table_waiting <- Table_job_status %>% 
      filter(status=="waiting") %>% 
      mutate(prio_idx = max(priority) - priority) %>% 
      arrange(prio_idx,index)
    
    # Get the waiting index
    WaitIndex = which(Table_waiting$index == model_index)
    
    
    # If the model are in the first place and there are sufficient cores, run the model
    if (WaitIndex==1 & nCore <= maxCore - Using_cores) {
      
      # update progress bar
      setTxtProgressBar(pb, value = model_index)
      close(pb)
      
      break
      
      # Print the waiting information
    } else {
      
      # update progress bar
      setTxtProgressBar(pb, value = model_index - WaitIndex)
      
      # Recheck everything after n*checkInt seconds
      Sys.sleep(WaitIndex*checkInt)
    }
    
  }
  
  # print the start time
  start_time = Sys.time()
  message(str_glue("The model starts to run at {start_time}"))
  
  # model arguments
  model_args <- args
  if (!is.null(formula)) model_args <- append(list(formula = formula),model_args)
  if (!is.null(data)) model_args <- append(list(data = data),model_args)
  if (!is.null(prior)) model_args <- append(list(prior = prior),model_args)
  
  # Specify the file's save location
  file_model_name <- str_glue("{model_path}{name}")
  model_args <- model_args %>% 
    append(list(file = file_model_name, file_refit = "on_change"))
  
  
  tryCatch(
    expr = {
      # Adjust the status of the model as running.
      Table_job_status <- read_rds(log_file)
      Table_job_status[which(Table_job_status$index == model_index), "status"] = "running"
      write_rds(Table_job_status, log_file)
      
      # Run the model
      brm_model <- do.call(brms::brm, model_args)
      
      # Adjust the status of the model as completed.
      Table_job_status <- read_rds(log_file)
      Table_job_status[which(Table_job_status$index == model_index), "status"] = "completed"
      write_rds(Table_job_status, log_file)
      
    },
    error = function(e){
      
      # Adjust the status of the model as running.
      Table_job_status <- read_rds(log_file)
      Table_job_status[which(Table_job_status$index == model_index), "status"] = "failed"
      write_rds(Table_job_status, log_file)
      
      # message error
      message(e)
      
      return(NA)
    },
    finally = {
      # print the end time
      end_time = Sys.time()
      message(str_glue("The model has done at {end_time}"))
      message(str_glue("It takes {round(as.numeric(end_time - start_time, units = 'hours'),2)} hours."))
    }
  )
  
}
