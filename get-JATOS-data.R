#' Download data from JATOS
#' 
#' This function downloads result data from a JATOS server using an API token and specified UUID and batch ID.
#' It then unzips and extracts the relevant data, including metadata, file names, and file contents.
#' The function returns a data frame with the relevant data.
#'
#' @param token An API token, you can create a new one on JATOS. Details see: https://www.jatos.org/JATOS-API.html#personal-access-tokens
#' @param url Server address. The default value is the address of our lab server.
#' @param uuId A unique code of study, you can find it on JATOS
#' @param batchId A unique code of a batch session in the study.
#' @param dataPath A path used to save data. If NUll, data will be saved in working directory.
#'
#' @return data.frame
#' @export
#'
get_JATOS_data <- function(token, url = "https://coglab.xyz/jatos/api/v1/results", studyId, batchId, dataPath = NULL) {
  
  # package list
  packages = c("dplyr", "stringr", "httr", "jsonlite")
  # get the package names that are not installed
  missing_packages <- setdiff(packages, rownames(installed.packages()))
  # install the missing packages
  if (length(missing_packages) > 0) install.packages(missing_packages)
  # load the packages
  library(dplyr)
  
  if (is.null(dataPath)) dataPath = "./" # Set the default data path
  
  # Create the authorization header with the specified token
  headers = c(`Authorization` = stringr::str_glue("Bearer {token}"))
  
  # Initialize the outcome data frame
  outcome <- data.frame()
  
  # Read the data from the JATOS server
  for (batch in batchId) {
    
    # Create the file paths for the zipped and unzipped data
    file_zip <- stringr::str_glue("{dataPath}JATOS_DATA_{batch}.jrzip")
    file_unzip <- stringr::str_glue("{dataPath}JATOS_DATA_{batch}")
    
    # Send an HTTP GET request to the specified URL, passing in the UUID and batch ID
    # Also pass in the authorization header and write the response to a temporary file
    res <- httr::GET(
      url = stringr::str_glue("{url}?studyId={studyId}&batchId={batch}"),
      httr::add_headers(.headers=headers), 
      httr::write_disk(file_zip, overwrite = TRUE)
    )
    
    # Unzip the downloaded file and extract the file names into a list
    filelist = unzip(
      file_zip, 
      exdir=file_unzip, 
      overwrite = TRUE)
    
    # Extract relevant data from the file names and store in a data frame
    file_table <- data.frame(filelist) %>% 
      dplyr::rename(file = filelist) %>% 
      dplyr::mutate(
        resultID = stringr::str_extract(file,"study_result_([0-9]+)"),
        componentID = stringr::str_extract(file,"comp-result_([0-9]+)"),
        resultID = as.numeric(stringr::str_extract(resultID,"([0-9]+)")),
        componentID = as.numeric(stringr::str_extract(componentID,"([0-9]+)")))
    
    # Remove the temporary file
    file.remove(file_zip)
    
    # Read the metadata from the last file in the list (assuming it is in JSON format)
    metaData <- jsonlite::read_json(filelist[length(filelist)])$data[[1]]$studyResults
    
    # Extract relevant metadata from the metadata list and store in a data frame
    info_table <- data.frame()
    for (a in 1:length(metaData)) {
      # ID
      info_table[a, "resultID"] = metaData[[a]]$id
      info_table[a, "componentID"] = metaData[[a]]$componentResults[[1]]$id
      # duration
      if (is.null(metaData[[a]]$duration)) {
        info_table[a, "duration"] = NA
      } else {
        exp_duration = metaData[[a]]$duration
        count_colon = stringr::str_count(exp_duration, ":")
        info_table[a, "duration"] = dplyr::case_match(
          count_colon,
          3 ~ 1440, # if the duration is more than 24 hours, we will treat it as 24 hours
          2 ~ as.numeric(lubridate::hms(exp_duration), "minutes"),
          1 ~ as.numeric(lubridate::ms(exp_duration), "minutes"),
          TRUE ~ NA
        )
      }
      # Study State
      info_table[a, "studyState"] = metaData[[a]]$studyState
    }
    
    # Combine the file data and metadata into a single data frame
    results <- info_table %>% 
      dplyr::right_join(file_table) %>% 
      dplyr::filter(!is.na(resultID)) %>% 
      dplyr::mutate(size = file.info(file)$size/1024)
    
    outcome <- rbind(outcome, results)
    
  }
  
  # Return the combined data frame
  return(outcome)
}
