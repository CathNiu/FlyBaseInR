test_that("getExpression checks user input FlyBase ID is valid and it has
          expression in LFQ database", {
  expect_message(getExpression("FBwewisrres"))
  expect_message(getExpression("FBgn9083927"))
  expect_message(getExpression("FBgn0036925"))
  expect_message(getExpression("FBgn0036924"))
})

test_that("getExpression return expression data of type double", {
  # For gene that have embryogenesis expression data
  expect_visible(getExpression("FBgn0267977"))
  expect_type(getExpression("FBgn0267977"), "double")
  # For gene that do not have embryogenesis expression data
  expect_visible(getExpression("FBgn0013275"))
  expect_type(getExpression("FBgn0013275"), "double")
})

test_that("getAllExpression returns a list, and output a text file", {
  message <- paste0("Please check output file Expression.txt ",
                    "in your current working directory.")
  expect_message(getAllExpression(geneListExample), message)
  compare_file_text("Expression.txt","ExpressionCorrect.txt")
  unlink("Expression.txt")
})

# [END]
