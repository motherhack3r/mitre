test_that("mitre cwe standard", {
  cwe <- mitredata$standards$cwe
  # List of shield objects
  expect_true(is.list(cwe))
  expect_true(length(cwe) == 2)
  expect_true(identical(names(cwe), c("cwe","cwenet")))
})
#> Test passed 😸

test_that("mitre cwe data frame", {
  df <- mitredata$standards$cwe$cwe
  expect_true(is.data.frame(df))
  expect_true(ncol(df) == 24)
  expect_true(identical(names(df), c("ID","Name","Abstraction","Structure","Status",
                                     "CWE_Type","Code_Standard","Description",
                                     "Extended_Description","Related_Weakness",
                                     "Weakness_Ordinality","Applicable_Platforms",
                                     "Background_Details","Alternate_Terms",
                                     "Modes_Of_Introduction","Likelihood_Of_Exploit",
                                     "Common_Consequences","Detection_Methods",
                                     "Potential_Mitigations","Observed_Examples",
                                     "Functional_Areas","Affected_Resources",
                                     "Taxonomy_Mappings","Related_Attack_Patterns")))
  expect_true(all(apply(df, 2, class) == "character"))
  # Check if any column is all NA
  expect_false(any(apply(df, 2, function(x) all(is.na(x)))))
  # Check column id format
  expect_true(all(grepl(pattern = "CWE-\\d+", df$Code_Standard)))
})
#> Test passed 😸

test_that("mitre cwe network", {
  net <- mitredata$standards$cwe$cwenet
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
