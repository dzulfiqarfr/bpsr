test_that("`bps_dataset_spreadsheet()` works", {
  expect_s3_class(
    bps_dataset_spreadsheet(keyword = "inflation", page = 2, lang = "eng"),
    "tbl_df"
  )
})


test_that("`bps_download_spreadsheet()` works", {
  file <- withr::local_tempfile(fileext = ".xls")
  expect_identical(bps_download_spreadsheet("915", file), file)
})


test_that("`bps_dataset_spreadsheet()` fails due to invalid `month`", {
  expect_error(bps_dataset_spreadsheet(month = "February"))
  expect_error(bps_dataset_spreadsheet(month = c(1, 2)))
})


test_that("`bps_dataset_spreadsheet()` fails due to invalid `year`", {
  expect_error(bps_dataset_spreadsheet(year = c(2021, 2022)))
})


test_that("`bps_download_spreadsheet()` fails due to invalid `dataset_id`", {
  expect_error(bps_download_spreadsheet("abcd"))
  expect_error(bps_download_spreadsheet(c("915", "916")))
})


test_that("`bps_download_spreadsheet()` fails due to invalid `file_path`", {
  expect_error(
    bps_download_spreadsheet("915", withr::local_tempfile(fileext = ".xlsx"))
  )

  expect_error(
    bps_download_spreadsheet("915", withr::local_tempfile(fileext = ".xlsx"))
  )
})
