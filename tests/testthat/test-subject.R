test_that("`bps_subject_category()` works", {
  expect_s3_class(bps_subject_category(), "tbl_df")
})


test_that("`bps_subject()` works", {
  expect_s3_class(
    bps_subject(subject_category_id = "2", page = c(2, 3)),
    "tbl_df"
  )
})


test_that("`bps_subject()` fails due to invalid `subject_category_id`", {
  expect_error(bps_subject(subject_category_id = "abc"))
  expect_error(bps_subject(subject_category_id = c("2", "3")))
})
