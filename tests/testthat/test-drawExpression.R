test_that("drawExpression checks of user input value of typeData", {
  expect_error(drawExpression(expressionDataExample, "Embryo", "title"))
  expect_error(drawExpression(expressionDataExample, "Life", "title"))
  expect_error(drawExpression(expressionDataExample, "", "title"))
  expect_error(drawExpression(expressionDataExample, "", ""))
  expect_error(drawExpression(expressionDataExample, ""))
})

test_that("drawExpression returns a figure", {
  expect_type(drawExpression(expressionDataExample, "Embryogenesis", "title"),
              "list")
  expect_type(drawExpression(expressionDataExample, "LifeCycle", "title"),
              "list")
})

test_that("sortHeatMapData returns a list ", {
  expect_type(sortHeatMapData(expressionDataExample, "LifeCycle"), "list")
  expect_type(sortHeatMapData(expressionDataExample, "Embryogenesis"), "list")
})
