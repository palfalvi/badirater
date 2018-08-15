context("First test")
library(badirater)

test_that("test tree decoding", {
  expect_equal(nrow(bd_decode_tree_ids(tree_id = "branch_ids.tree")), 23, info = "Tree decoding")
})
