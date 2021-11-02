library(testthat)
context("data capture client")

test_that(
  "test get_instance_id when instance_id is not passed and
  HOSTNAME env variable is not set", {
  expected_instance_id <- character(0)
  Sys.unsetenv("HOSTNAME")
  actual_instance_id <- get_instance_id(character(0))
  expect_equal(expected_instance_id, actual_instance_id)
})

test_that(
  "test get_instance_id when instance_id is not passed and
  HOSTNAME env variable is set", {
  expected_instance_id <- "6100391064067162909bf2f2"
  Sys.unsetenv("HOSTNAME")
  actual_instance_id <-
    get_instance_id(expected_instance_id)
  expect_equal(expected_instance_id, actual_instance_id)
  hostname <-
    paste("model", expected_instance_id, "7d75566cb",
          "49l9j", sep = "-")
  Sys.setenv("HOSTNAME" = hostname)
  actual_instance_id <-
    get_instance_id(expected_instance_id)
  expect_equal(expected_instance_id, actual_instance_id)
})

test_that("test get_instance_id when instance_id is passed", {
  expected_instance_id <- "6100391064067162909bf2f2"
  actual_instance_id <- get_instance_id(expected_instance_id)
  expect_equal(expected_instance_id, actual_instance_id)
})

test_that("test get_instance_id when instance_id is not passed", {
  expected_instance_id <- "6100391064067162909bf2f2"
  Sys.setenv("HOSTNAME" = "model-6100391064067162909bf2f2")
  actual_instance_id <- get_instance_id(character(0))
  expect_equal(expected_instance_id, actual_instance_id)
})

test_that(
  "test get_instance_id when instance_id is not passed and
  model version id not present", {
  expected_instance_id <- ""
  Sys.setenv("HOSTNAME" = "model--afasfad-adadsf")
  actual_instance_id <- get_instance_id(character(0))
  expect_equal(expected_instance_id, actual_instance_id)
})

test_that("test get_instance_id when instance_id is not passed in dev mode", {
  expected_instance_id <- character(0)
  Sys.unsetenv("HOSTNAME")
  actual_instance_id <- get_instance_id(character(0))
  expect_equal(expected_instance_id, actual_instance_id)
  Sys.setenv("HOSTNAME" = "run-6100391064067162909bf2f2")
  actual_instance_id <- get_instance_id(character(0))
  expect_equal(expected_instance_id, actual_instance_id)
})

test_that("get_model_version_id when HOSTNAME env variable is not set", {
  Sys.unsetenv("HOSTNAME")
  expect_equal(NULL, get_model_version_id())
})

test_that("get_model_version_id when the library is running in dev mode", {
  Sys.setenv("HOSTNAME" = "run-6100391064067162909bf2f2-7d75566cb-49l9j")
  expect_equal(NULL, get_model_version_id())
})

test_that("get_model_version_id when a valid HOSTNAME is present", {
  Sys.setenv("HOSTNAME" = "model-6100391064067162909bf2f2-7d75566cb-49l9j")
  expect_equal("6100391064067162909bf2f2", get_model_version_id())
})

test_that("get_log_file when HOSTNAME starts with model-", {
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp/")
  Sys.setenv("HOSTNAME" = "model-6100391064067162909bf2f2-7d75566cb-49l9j")
  expect_equal(
    "/tmp/model-6100391064067162909bf2f2-7d75566cb-49l9j.log",
    get_log_file()
  )
})

test_that("get_log_file when HOSTNAME starts with model-
 and PREDICTION_DATA_DIRECTORY doesn't end with /", {
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp/log")
  Sys.setenv("HOSTNAME" = "model-6100391064067162909bf2f2-7d75566cb-49l9j")
  expect_equal(
    "/tmp/log/model-6100391064067162909bf2f2-7d75566cb-49l9j.log",
    get_log_file()
  )
})

test_that("get_log_file when HOSTNAME starts with model-
 and PREDICTION_DATA_DIRECTORY is not present", {
  Sys.unsetenv("PREDICTION_DATA_DIRECTORY")
  Sys.setenv("HOSTNAME" = "model-6100391064067162909bf2f2-7d75566cb-49l9j")
  expect_equal(
    "/tmp/model-6100391064067162909bf2f2-7d75566cb-49l9j.log",
    get_log_file()
  )
})

test_that("get_log_file in dev mode", {
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp/log/")
  Sys.unsetenv("HOSTNAME")
  expect_equal("/tmp/log/dev.log", get_log_file())
})

test_that("is_model_running_in_dev_mode when HOSTNAME is not present", {
  Sys.unsetenv("HOSTNAME")
  expect_equal(TRUE, is_model_running_in_dev_mode())
})

test_that("is_model_running_in_dev_mode
 when HOSTNAME doesn't start with model-", {
  Sys.setenv("HOSTNAME" = "run-6100391064067162909bf2f2-7d75566cb-49l9j")
  expect_equal(TRUE, is_model_running_in_dev_mode())
})

test_that("is_model_running_in_dev_mode
 when HOSTNAME start with model-", {
  Sys.setenv("HOSTNAME" = "model-6100391064067162909bf2f2-7d75566cb-49l9j")
  expect_equal(FALSE, is_model_running_in_dev_mode())
})
