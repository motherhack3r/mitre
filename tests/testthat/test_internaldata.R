test_that("Check data sets", {
  # ATTCK
  expect_true(is.data.frame(attck.groups))
  expect_true(is.data.frame(attck.mitigations))
  expect_true(is.data.frame(attck.relations))
  expect_true(is.data.frame(attck.software))
  expect_true(is.data.frame(attck.tactics))
  expect_true(is.data.frame(attck.techniques))
  # SHIELD
  expect_true(is.data.frame(shield.opportunities))
  expect_true(is.data.frame(shield.procedures))
  expect_true(is.data.frame(shield.relations))
  expect_true(is.data.frame(shield.tactics))
  expect_true(is.data.frame(shield.techniques))
  expect_true(is.data.frame(shield.use_cases))
})
#> Test passed 😸
