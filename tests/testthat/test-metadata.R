test_that("`bps_metadata()` works", {
  dataset <- bps_get_dataset("416")

  expect_s3_class(bps_metadata(dataset), "bpsr_metadata")

  trade <- bps_get_trade_hs_chapter("03", "export", year = 2017)

  expect_s3_class(bps_metadata(trade), "bpsr_metadata")
})


test_that("`bps_metadata()` fails due to invalid `data`", {
  expect_error(bps_metadata(tibble(x = 1:3, y = c("a", "b", "c"))))
})
