test_that("`bps_add_date()` works", {
  inflation <- bps_get_dataset("1927", keep = "period_id")
  with_date <- bps_add_date(inflation)
  expect_s3_class(with_date, "tbl_df")
  expect_true(any("date" == names(with_date)))
})


test_that("`bps_add_date()` fails due to invalid `data`", {
  expect_error(bps_add_date(mtcars))

  data <- bps_get_dataset("1927")
  expect_error(bps_add_date(data))
})
