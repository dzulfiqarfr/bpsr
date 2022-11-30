test_that("`bps_search()` works", {
  expect_s3_class(
    bps_search("pressrelease", keyword = "inflasi", page = c(2, 3)),
    "tbl_df"
  )
})


test_that("`bps_search()` fails due to invalid `.keyword`", {
  expect_error(bps_search("pressrelease", keyword = c("IHK", "PDB")))
})


test_that("`bps_search()` fails due to invalid `model`", {
  expect_error(bps_search("data"))
})
