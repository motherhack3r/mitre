test_that("mitre att&ck standard", {
  attck <- mitredata$standards$attck
  # List of shield objects
  expect_true(is.list(attck))
  expect_true(length(attck) == 6)
  expect_true(identical(names(attck), c("tactics","techniques","mitigations",
                                        "groups","software","attcknet")))
})
#> Test passed 😸

test_that("mitre att&ck tactics data frame", {
  df <- mitredata$standards$attck$tactics
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 13)
  expect_true(identical(names(df), c("domain","type","mitreid","name","description",
                                     "x_mitre_shortname","created","created_by_ref",
                                     "modified","id","url","x_mitre_deprecated","revoked")))
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "TA\\d+", df$mitreid)))
})
#> Test passed 😸

test_that("mitre att&ck techniques data frame", {
  df <- mitredata$standards$attck$techniques
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 30)
  # expect_true(identical(names(df), c("id","name","description","long_description")))
  # expect_true(all(apply(df, 2, class) == "character"))
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "T\\d+[\\.\\d+]?", df$mitreid)))
})

test_that("mitre shield network", {
  net <- mitredata$standards$shield$shieldnet
  expect_true(is.list(net))
  expect_true(length(net) == 2)
  expect_true(all(names(net) %in% c("nodes","edges")))
  # Check nodes
  expect_true(ncol(net$nodes) == 9)
  expect_true(all(names(net$nodes) %in% nodenames))
  # Check edges
  expect_true(ncol(net$edges) == 7)
  expect_true(all(names(net$edges) %in% nodeedges))
})
#> Test passed 😸
