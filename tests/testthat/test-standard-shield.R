test_that("mitre shield standard", {
  shield <- mitredata$standards$shield
  # List of shield objects
  expect_true(is.list(shield))
  expect_true(length(shield) == 6)
  expect_true(identical(names(shield), c("tactics","techniques","opportunities","procedures","usecases","shieldnet")))
})
#> Test passed 😸

test_that("mitre shield tactics data frame", {
  df <- mitredata$standards$shield$tactics
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 4)
  expect_true(identical(names(df), c("id","name","description","long_description")))
  expect_true(all(apply(df, 2, class) == "character"))
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "DTA\\d+", df$id)))
})
#> Test passed 😸

test_that("mitre shield techniques data frame", {
  df <- mitredata$standards$shield$techniques
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 4)
  expect_true(identical(names(df), c("id","name","description","long_description")))
  expect_true(all(apply(df, 2, class) == "character"))
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "DTE\\d+", df$id)))
})
#> Test passed 😸

test_that("mitre shield opportunities data frame", {
  df <- mitredata$standards$shield$opportunities
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 2)
  expect_true(identical(names(df), c("id", "description")))
  expect_true(all(apply(df, 2, class) == "character"))
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "DOS\\d+", df$id)))
})
#> Test passed 😸

test_that("mitre shield procedures data frame", {
  df <- mitredata$standards$shield$procedures
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 2)
  expect_true(identical(names(df), c("id", "description")))
  expect_true(all(apply(df, 2, class) == "character"))
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "DPR\\d+", df$id)))
})
#> Test passed 😸

test_that("mitre shield usecases data frame", {
  df <- mitredata$standards$shield$usecases
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 2)
  expect_true(identical(names(df), c("id", "description")))
  expect_true(all(apply(df, 2, class) == "character"))
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "DUC\\d+", df$id)))
})
#> Test passed 😸

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
