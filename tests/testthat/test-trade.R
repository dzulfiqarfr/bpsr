test_that("`bps_get_trade_hs_chapter()` works", {
  expect_s3_class(
    bps_get_trade_hs_chapter(
      c("03", "25", "98"),
      "export",
      year = c(2015, 2016, 2017)
    ),
    "bpsr_tbl"
  )
})


test_that("`bps_get_trade_hs_full()` works", {
  expect_s3_class(
    bps_get_trade_hs_full(
      "02102000",
      "import",
      year = Inf,
      freq = "annual"
    ),
    "bpsr_tbl"
  )
})


test_that("`bps_get_trade_hs_full()` works but returns no data", {
  expect_equal(bps_get_trade_hs_full("10019100", "import"), NULL)
})


test_that("`bps_guess_latest_trade_year()` works", {
  expect_type(bps_guess_latest_trade_year(), "integer")
})


test_that("`bps_hs_code()` works", {
  expect_s3_class(bps_hs_code(), "tbl_df")
})


test_that("`bps_get_trade_hs()` fails due to invalid `hs_code`", {
  expect_error(bps_get_trade_hs_chapter("15111000", "import"))

  expect_error(bps_get_trade_hs_full("03", "export"))
})


test_that("`bps_get_trade_hs()` fails due to invalid `year`", {
  expect_error(bps_get_trade_hs_full("17011400", "import", year = 2011))

  expect_error(
    bps_get_trade_hs_chapter("03", "export", year = c(2011, 2017))
  )
})
