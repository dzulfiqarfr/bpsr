test_that("`bps_domain()` works", {
  expect_s3_class(bps_domain(level = "city"), "tbl_df")
})


test_that("`bps_domain()` fails due to invalid `level`", {
  expect_error(bps_domain(level = "prov"))

  expect_error(bps_domain(level = "kab"))
})
