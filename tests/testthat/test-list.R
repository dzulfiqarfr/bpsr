test_that("`bps_list()` works", {
  expect_s3_class(bps_list("subcat"), "bpsr_table")

  expect_s3_class(
    bps_list("subject", domain_id = "1100", page = Inf, lang = "eng"),
    "bpsr_table"
  )
})


test_that("`bps_list()` parses an empty response correctly", {
  resp <- bps_list("subject", page = c(1, 2, 6))
  expect_equal(resp$table[[3]], NULL)
  expect_equal(resp$result_info[[3]], NULL)
})


test_that("`bps_list()` fails due to invalid `model`", {
  expect_error(bps_list("invalid"))
  expect_error(bps_list("data"))
})


test_that("`bps_list()` fails due to invalid `domain_id", {
  expect_error(bps_list("th", domain_id = "abcd"))
  expect_error(bps_list("turth", domain_id = c("1100", "1200")))
})


test_that("`bps_list()` fails due to invalid `lang`", {
  expect_error(bps_list("turth", lang = "tur"))
})
