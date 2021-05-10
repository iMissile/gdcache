test_that("multiplication works", {
  expect_equal(2 * 2, 4)

  # syncGdCache(here::here("cloud_data"), gdrive_folder = "1pMs9-auXmGsw9OGZD-9KiCCEaPryZ52s")
  # pkg_test https://drive.google.com/drive/u/3/folders/1pMs9-auXmGsw9OGZD-9KiCCEaPryZ52s
  # data.qs https://drive.google.com/drive/folders/1HZvneoJBz9OWKowh3gyKptZycYe3JIbi
  # syncGdCache(here::here("data/"), gdrive_folder = "10DzkyU5ddjCLn2FM2toiF5eodgu5V2JC")
})
