test_that("Check data sets", {
  expect_true(is.list(standards))
  # ATTCK
  expect_true(is.data.frame(standards$attck$attck.groups))
  expect_true(is.data.frame(standards$attck$attck.mitigations))
  expect_true(is.data.frame(standards$attck$attck.relations))
  expect_true(is.data.frame(standards$attck$attck.software))
  expect_true(is.data.frame(standards$attck$attck.tactics))
  expect_true(is.data.frame(standards$attck$attck.techniques))
  # SHIELD
  expect_true(is.data.frame(standards$shield$shield.opportunities))
  expect_true(is.data.frame(standards$shield$shield.procedures))
  expect_true(is.data.frame(standards$shield$shield.relations))
  expect_true(is.data.frame(standards$shield$shield.tactics))
  expect_true(is.data.frame(standards$shield$shield.techniques))
  expect_true(is.data.frame(standards$shield$shield.use_cases))
})
#> Test passed 😸
