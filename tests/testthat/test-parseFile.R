library(FlyBaseInR)

test_that("parseFile checks file name ends with .txt", {
  expect_error(parseFile("geneList.csv"))
  expect_error(parseFile("geneList"))
  expect_error(parseFile(""))
})

# [END]
