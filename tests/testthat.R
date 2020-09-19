library(testthat)

# the next line downloads the most recent copy of the test file
if(!file.exists("tests/testthat/")) dir.create("tests/testthat/")

repo="https://raw.githubusercontent.com/AdamWilsonLabEDU/geo511_tasks_tests"

download.file(paste0(repo,"/master/tests.R"),
              "tests/testthat/test-case_studies.R")

# run all tests in the testhat folder
test_dir("tests/testthat/",stop_on_failure = TRUE)
