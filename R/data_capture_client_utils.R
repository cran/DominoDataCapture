get_current_timestamp <- function() {
  tm <- as.POSIXlt(Sys.time(), tz = "UTC")
  return(strftime(tm, "%Y-%m-%dT%H:%M:%S%z", usetz = TRUE))
}

get_instance_id <- function(instance_id = NULL) {
  model_version_id <- get_model_version_id()
  if (identical(instance_id, character(0)) && !is.null(model_version_id)) {
    return(model_version_id)
  } else {
    return(instance_id)
  }
}

get_model_version_id <- function() {
  host_name <- Sys.getenv("HOSTNAME")
  # note: the host_name will be like
  # eg: model-6100391064067162909bf2f2-7d75566cb-49l9j
  if (startsWith(host_name, "model-")) {
    return(strsplit(host_name, "-")[[1]][2])
  }
  return(NULL)
}

is_model_running_in_dev_mode <- function() {
  return(is.null(get_model_version_id()))
}

get_log_file <- function() {
  prediction_data_directory <- Sys.getenv("PREDICTION_DATA_DIRECTORY")
  if (prediction_data_directory == "") {
    prediction_data_directory <- "/tmp/"
  }
  if (!endsWith(prediction_data_directory, "/")) {
    prediction_data_directory <- paste0(prediction_data_directory, "/")
  }
  log_filename <- "dev"
  if (!is_model_running_in_dev_mode()) {
    log_filename <- Sys.getenv("HOSTNAME")
  }
  log_file <- paste0(prediction_data_directory, log_filename, ".log")
  return(log_file)
}

init_logger <- function() {
  domino_data_client_logger <- lgr::get_logger("dominodataingest")
  domino_data_client_logger$set_propagate(FALSE)
  if (is_model_running_in_dev_mode()) {
    domino_data_client_logger$set_appenders(
      list(cons = lgr::AppenderConsole$new())
    )
    domino_data_client_logger$appenders$cons$layout$set_fmt(paste(
      "The information",
      "shown below represents the structure and format of the prediction data",
      "recorded by Domino when this model is deployed as a Domino Model API",
      "%m"
    ))
  } else {
    domino_data_client_logger$set_appenders(
      list(cons = lgr::AppenderFile$new(get_log_file()))
    )
    domino_data_client_logger$appenders$cons$layout$set_fmt("%m")
  }

  return(domino_data_client_logger)
}

release_questions <- function() {
  c("Have you run all the tests (with `devtools::test()`)?",
    "Have you updated the docs (with `roxygen2::roxygenise()`)?")
}
