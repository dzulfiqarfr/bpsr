test_that("`bps_view()` works", {
  expect_s3_class(bps_view("statictable", "915"), "bpsr_file")
})


test_that("`bps_view()` fails due to invalid `model`", {
  expect_error(bps_view("subject"))

  expect_error(bps_view("glosarium"))
})


test_that("`bps_view()` fails due to invalid `id`", {
  expect_error(bps_view("statictable", c("915", "916")))
})


test_that("`bps_view()` fails due to invalid `lang`", {
  expect_error(bps_view("pressrelease", lang = "tur"))
})
