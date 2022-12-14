library(FlyBaseInR)

test_that("getSequence checks user input FlyBase ID is valid", {
  expect_warning(getSequence("FBpp", "FBwewisrres"))
  expect_warning(getSequence("FBpp", "FBgn9083927"))
  expect_warning(getSequence("FBgn", "FBgn9054jih"))
})

test_that("getSequence return a character", {
  expect_visible(getSequence("FBpp", "FBgn0036925"))
  expect_type(getSequence("FBpp", "FBgn0036925"), "character")
  expect_visible(getSequence("FBtr", "FBgn0036925"))
  expect_type(getSequence("FBtr", "FBgn0036925"), "character")
  expect_visible(getSequence("exon", "FBgn0036925"))
  expect_type(getSequence("exon", "FBgn0036925"), "character")
})

test_that("getAllSequences checks user input Sequence Type is valid", {
  expect_error(getAllSequences(geneListExample, "itron"))
  expect_error(getAllSequences(geneListExample, "five"))
  expect_error(getAllSequences(geneListExample, ""))
})

test_that("getAllSequences returns invisible NULL, and output a text file", {
  message <- paste0("Please check output file FBppSequences.txt ",
                    "in your current working directory.")
  expect_message(getAllSequences(geneListExample, "FBpp"), message)
  compare_file_text("FBppSequences.txt","FBppSequencesCorrect.txt")
  unlink("FBppSequences.txt")

})

# [END]
