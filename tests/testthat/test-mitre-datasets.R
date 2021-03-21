test_that("mitre data is a list of standards and network", {
  # List of standards and network
  expect_true(is.list(mitredata))
  expect_true(length(mitredata) == 2)
  expect_true(identical(names(mitredata), c("standards", "mitrenet")))
  # Complete list of standards
  expect_true(all(names(mitredata$standards) %in% c("shield","attck","cpe","cve","cwe","capec","car")))
  # Network as nodes and edges
  expect_true(all(names(mitredata$mitrenet) %in% c("nodes","edges")))
})
#> Test passed 😸

