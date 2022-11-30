test_that("`bps_download()` works", {
  file_spreadsheet <- withr::local_tempfile(fileext = ".xls")
  spreadsheet <- bps_dataset_spreadsheet()

  expect_identical(
    bps_download(spreadsheet$file_url[[1]], file_spreadsheet),
    file_spreadsheet
  )

  file_publication <- withr::local_tempfile(fileext = ".pdf")
  publication <- bps_search("publication")

  expect_identical(
    bps_download(publication$file_url[[1]], file_publication),
    file_publication
  )

  file_infographics <- c(
    withr::local_tempfile(fileext = ".png"),
    withr::local_tempfile(fileext = ".png")
  )

  infographics <- bps_search("infographic")

  expect_identical(
    bps_download(infographics$file_url[c(1, 2)], file_infographics),
    file_infographics
  )
})


test_that("`bps_download()` fails due to invalid `url` and `file_path` size", {
  infographics <- bps_search("infographic")

  expect_error(
    bps_download(infographics$file_url[c(1, 2)],
    withr::local_tempfile(fileext = ".png")
    )
  )
})


test_that("`bps_download()` fials due to invalid `url`", {
  expect_error(
    bps_download(
      paste0(
        "https://www.bps.go.id/statictable/",
        "2012/02/02/908/",
        "inflasi-umum-inti-harga-yang-diatur-pemerintah-dan-",
        "barang-bergejolak-inflasi-indonesia-2009-2022.html"
      ),
      withr::local_tempfile(fileext = ".xls")
    )
  )
})


test_that("`bps_download()` fials due to invalid `file_path`", {
  spreadsheet <- bps_dataset_spreadsheet()

  expect_error(
    bps_download(
      spreadsheet$file_url[[1]],
      withr::local_tempfile(fileext = ".xlsx")
    )
  )
})
