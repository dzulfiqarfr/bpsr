test_that("`bps_vertical_var()` works", {
  expect_s3_class(bps_vertical_var(dataset_id = "1927"), "tbl_df")
})


test_that("`bps_derived_var()` works", {
  expect_s3_class(bps_derived_var(dataset_id = "1905"), "tbl_df")
  expect_s3_class(bps_derived_var(derived_var_group_id = "5"), "tbl_df")
})


test_that("`bps_year()` works", {
  expect_s3_class(bps_year(dataset_id = "849"), "tbl_df")
})


test_that("`bps_period()` works", {
  expect_s3_class(bps_period(dataset_id = "850"), "tbl_df")
})


test_that("`bps_vertical_var()` fails due to invalid `dataset_id`", {
  expect_error(bps_vertical_var(dataset_id = "abcd"))
  expect_error(bps_vertical_var(dataset_id = c("416", "417")))
})


test_that("`bps_derived_var()` fails due to invalid `dataset_id`", {
  expect_error(bps_derived_var(dataset_id = "abcd"))
  expect_error(bps_derived_var(dataset_id = c("1905", "1906")))
})


test_that("`bps_derived_var()` fails due to invalid `derived_var_group_id`", {
  expect_error(bps_derived_var(derived_var_group_id = "abcd"))
  expect_error(bps_derived_var(derived_var_group_id = c("1", "5")))
})


test_that("`bps_year()` fails due to invalid `dataset_id`", {
  expect_error(bps_year(dataset_id = "abcd"))
  expect_error(bps_year(dataset_id = c("416", "417")))
})


test_that("`bps_period()` fails due to invalid `dataset_id`", {
  expect_error(bps_period(dataset_id = "abcd"))
  expect_error(bps_year(dataset_id = c("1905", "1906")))
})
