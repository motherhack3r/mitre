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
  # Check if any column is all NA
  # expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "T\\d+[\\.\\d+]?", df$mitreid)))
})

test_that("mitre att&ck mitigations data frame", {
  df <- mitredata$standards$attck$mitigations
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 13)
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "(M|T)\\d+", df$mitreid)))
  # Check old id's starting with T as deprecated
  expect_true(all(df[grepl(pattern = "T\\d+", df$mitreid), "x_mitre_deprecated"]))

})

test_that("mitre att&ck groups data frame", {
  df <- mitredata$standards$attck$groups
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 15)
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "G\\d+", df$mitreid)))
})

test_that("mitre att&ck software data frame", {
  df <- mitredata$standards$attck$software
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 16)
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "S\\d+", df$mitreid)))
})

test_that("mitre att&ck network", {
  net <- mitredata$standards$shield$shieldnet
  expect_true(is.list(net))
  expect_true(length(net) == 2)
  expect_true(all(names(net) %in% c("nodes","edges")))
  # Check nodes
  expect_true(ncol(net$nodes) == 9)
  expect_true(all(names(net$nodes) %in% nodenames))
  expect_false(any(apply(net$nodes, 2, function(x) all(is.na(x)))))
  # Check edges
  expect_true(ncol(net$edges) == 7)
  expect_true(all(names(net$edges) %in% nodeedges))
  expect_false(any(apply(net$edges, 2, function(x) all(is.na(x)))))
})
#> Test passed 😸
