test_that("`bps_sdg()` works", {
  expect_s3_class(bps_sdg(goal = 2, page = Inf), "tbl_df")
})


test_that("`bps_sdg()` fails due to invalid `goal`", {
  expect_error(bps_sdg(goal = seq(1, 17)))
})
