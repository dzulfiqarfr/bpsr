test_that("`bps_dataset()` works", {
  expect_s3_class(bps_dataset(subject_id = "26"), "tbl_df")
  expect_s3_class(bps_dataset(vertical_var_group_id = "1"), "tbl_df")
})


test_that("`bps_get_dataset()` works", {
  expect_s3_class(
    bps_get_dataset(
      "1905",
      vertical_var_id = c("3100", "3273"),
      derived_var_id = c("1553", "1666"),
      year_id = c("120", "121"),
      period_id = c("1", "12")
    ),
    "bpsr_tbl"
  )

  expect_s3_class(
    bps_get_dataset("1906", year_id = "120:122", period_id = "1:6"),
    "bpsr_tbl"
  )
})


test_that("`keep` in `bps_get_dataset()` works", {
  data_all <- bps_get_dataset("417", keep = "all")
  id_column <- c("vertical_var_id", "derived_var_id", "year_id", "period_id")
  expect_true(all(id_column %in% names(data_all)))

  some_id_column <- c("vertical_var_id", "period_id")
  data_vervar_period <- bps_get_dataset("1927", keep = some_id_column)
  expect_true(all(some_id_column %in% names(data_vervar_period)))
})


test_that("`bps_get_datasets()` works", {
  data <- bps_get_datasets(c("1910", "1956"))
  expect_s3_class(data[[1]], "bpsr_tbl")
  expect_s3_class(data[[2]], "bpsr_tbl")
})


test_that("`composite_id` column gets parsed correctly", {
  phone_ownership <- bps_get_dataset("111")

  aceh <- dplyr::filter(phone_ownership, vertical_var == "ACEH", year == 2011)

  overall <- aceh |>
    dplyr::filter(derived_var == "Perkotaan+Perdesaan") |>
    dplyr::pull(var)

  urban <- aceh |>
    dplyr::filter(derived_var == "Perkotaan") |>
    dplyr::pull(var)

  rural <- aceh |>
    dplyr::filter(derived_var == "Perdesaan") |>
    dplyr::pull(var)

  expect_equal(overall, 3.29)
  expect_equal(urban, 6.25)
  expect_equal(rural, 2.13)
})


test_that("`bps_get_dataset()` parses Indonesian, English data correctly", {
  ind <- bps_get_dataset("1955", keep = "all")
  eng <- bps_get_dataset("1955", lang = "eng", keep = "all")

  ind_bare <- dplyr::select(
    ind,
    !c(vertical_var, derived_var, derived_var_id, period, year)
  )

  eng_bare <- dplyr::select(
    eng,
    !c(vertical_var, derived_var, derived_var_id, period, year)
  )

  expect_true(
    dplyr::all_equal(
      ind_bare,
      eng_bare,
      ignore_col_order = FALSE,
      ignore_row_order = FALSE
    )
  )
})


test_that("`bps_get_dataset()` fails due to invalid `dataset_id", {
  expect_error(bps_get_dataset("abcd"))
  expect_error(bps_get_dataset(c("1905", "1906")))
})


test_that("`bps_get_dataset()` fails due to invalid `vertical_var_id`", {
  expect_error(bps_get_dataset("1905", vertical_var_id = "abcd"))

  expect_error(
    bps_get_dataset("1905", vertical_var_id = c("3100", "3273", "abcd"))
  )

  expect_error(bps_get_dataset("1905", vertical_var_id = "3100;3273"))
})


test_that("`bps_get_dataset()` fails due to invalid `derived_var_id`", {
  expect_error(bps_get_dataset("1905", derived_var_id = "abcd"))

  expect_error(
    bps_get_dataset("1905", derived_var_id = c("1553", "1666", "0000"))
  )

  expect_error(bps_get_dataset("1905", derived_var_id = "1553;1666"))
})


test_that("`bps_get_dataset()` fails due to invalid `year_id`", {
  expect_error(bps_get_dataset("416", year_id = "2009"))
  expect_error(bps_get_dataset("416", year_id = c("114", "2009")))
  expect_error(bps_get_dataset("416", year_id = "114;115;116"))
})


test_that("`bps_get_dataset()` fails due to invalid `period_id`", {
  expect_error(bps_get_dataset("1905", period_id = "01"))
  expect_error(bps_get_dataset("1905", period_id = c("1", "01")))
  expect_error(bps_get_dataset("1905", period_id = "1;2;3"))
})


test_that("`bps_get_datasets()` fails due to invalid `dataset_id`", {
  expect_error(bps_get_datasets("1905"))
  expect_error(bps_get_dataset(c("416", "abcd")))
})


test_that("`bps_dataset()` fails due to invalid `vertical_var_group_id`", {
  expect_error(bps_dataset(vertical_var_group_id = "abcd"))
  expect_error(bps_dataset(vertical_var_group_id = c("1", "5")))
})
