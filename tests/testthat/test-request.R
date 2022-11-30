test_that("`bps_request()` works", {
  expect_s3_class(bps_request("domain", type = "all"), "bpsr_response")
})


test_that("`bps_request_multiple()` works", {
  expect_s3_class(
    bps_request_multiple(
      "domain",
      list(
        type = "kabbyprov",
        prov = c("1100", "1200")
      )
    ),
    "bpsr_multiple_responses"
  )
})


test_that("`bps_request_paginated()` works", {
  expect_s3_class(
    bps_request_paginated(
      "list",
      model = "subject",
      domain = "0000",
      page = c(1, 2)
    ),
    "bpsr_multiple_responses"
  )

  expect_s3_class(
    bps_request_paginated(
      "list",
      model = "subject",
      domain = "1200",
      page = Inf
    ),
    "bpsr_multiple_responses"
  )
})


test_that("core request functions work but return no data", {
  resp <- bps_request(
    "dataexim",
    jenishs = "2",
    kodehs = "10019100",
    sumber = "1",
    tahun = 2022,
    periode = "1"
  )

  expect_true(has_no_data(resp))

  resp2 <-  bps_request_multiple(
    "list",
    list(
      model = "subject",
      page = c(100, 500)
    )
  )

  expect_true(purrr::every(resp2, has_no_data))

  resp3 <-  bps_request_paginated("list", model = "subject", page = seq(6, 10))

  expect_true(purrr::every(resp3, has_no_data))
})


test_that("core request functions fail due to missing params", {
  expect_error(bps_request("domain"))
  expect_error(bps_request_multiple("list", list(lang = "eng")))
  expect_error(bps_request_paginated("list", page = c(1, 2)))
})


test_that("core request functions fail due to typos", {
  expect_error(bps_request("domain", type = "ka"))

  expect_error(
    bps_request_multiple(
      "list",
      list(model = c("sub", "dat"), domain = "0000")
    )
  )

  expect_error(
    bps_request_paginated(
      "list",
      model = "sub",
      domain = "1200",
      page = c(1, 2)
    )
  )
})



test_that("core request functions fail due to invalid `url_path`", {
  expect_error(bps_request("invalid"))

  expect_error(
    bps_request_multiple(url_path = c("domain", "list"), list(type = "all"))
  )

  expect_error(bps_request_paginated("domain", type = "all"))
})


test_that("`bps_request_multiple()` fails due to invalid `query_params`", {
  expect_error(bps_request_multiple("list", c(model = "subject", page = 1)))
  expect_error(bps_request_multiple("list", list("subject", 1)))
})
