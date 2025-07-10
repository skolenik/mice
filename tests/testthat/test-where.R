test_that("check.where() breaks proprely on bad inputs", {
  
  nhanes_imp <- mice(nhanes, seed = 10305, printFlag = FALSE)
  
  expect_equal(
    check.where(where=nhanes_imp$where, data=nhanes, blocks=nhanes_imp$blocks),
    nhanes_imp$where
  )
  expect_error(
    check.where(where=list(a="b"), data=nhanes, blocks=nhanes_imp$blocks),
    "Argument `where` not a matrix or data frame"
  )
  expect_error(
    check.where(where=nhanes_imp$where, data=mtcars, blocks=nhanes_imp$blocks),
    "Arguments `data` and `where` not of same size"
  )
  expect_error(
    check.where(where=nhanes_imp$where, blocks=nhanes_imp$blocks,
      data=mtcars[1:nrow(nhanes), 1:ncol(nhanes)]  ),
    "Columns of data and where are not the same or are not in the same order"
  )
  expect_error(
    check.where(where=nhanes>1, 
      data=nhanes, blocks=nhanes_imp$blocks),
    "Argument `where` contains missing values"
  )
})
