library(dplyr)

df_inventory <- mitre::cpe_make_title(df = df_ubi_sample, verbose = verbose)
df_inventory <- mitre::cpe_generate(df_inventory = df_inventory, verbose = verbose)
df <- mitre::cpe_find_vulnerabilities(df_inventory = df_inventory, verbose = verbose)

vulns <- bind_rows(lapply(df$cves[df$cves != "[]"], jsonlite::fromJSON)) %>%
  group_by(cve, conditional) %>%
  summarise(num_vulns = n()) %>%
  ungroup()

mitrenet <- readRDS("inst/extdata/mitrenet.rds")

vulns <- left_join(vulns %>% filter(!conditional),
                   mitrenet$nodes %>% select(id, standard),
                   by = c("cve" = "standard"))

v1 <- left_join(x = vulns,
                y = mitrenet$edges %>% select(from, label, to),
                by = c("id" = "from")) %>%
  rename("label1" = "label", "to1" = "to")
v1 <- v1[!(v1$to1 %in% unique(vulns$id)), ]

v2 <- left_join(x = v1 %>% select(to1) %>% unique(),
                y = mitrenet$edges %>% select(from, label, to),
                by = c("to1" = "from")) %>%
  rename("label2" = "label", "to2" = "to")
v2 <- v2[!(v2$to2 %in% unique(vulns$id)), ]
v2 <- v2[!(v2$to2 %in% unique(v1$to1)), ]

v3 <- left_join(x = v2 %>% select(to2) %>% unique(),
                y = mitrenet$edges %>% select(from, label, to),
                by = c("to2" = "from")) %>%
  rename("label3" = "label", "to3" = "to")
v3 <- v3[!(v3$to3 %in% unique(vulns$id)), ]
v3 <- v3[!(v3$to3 %in% unique(v1$to1)), ]
v3 <- v3[!(v3$to3 %in% unique(v2$to2)), ]

v4 <- left_join(x = v3 %>% select(to3) %>% unique(),
                y = mitrenet$edges %>% select(from, label, to),
                by = c("to3" = "from")) %>%
  rename("label4" = "label", "to4" = "to")
v4 <- v4[!(v4$to4 %in% unique(vulns$id)), ]
v4 <- v4[!(v4$to4 %in% unique(v1$to1)), ]
v4 <- v4[!(v4$to4 %in% unique(v2$to2)), ]
v4 <- v4[!(v4$to4 %in% unique(v3$to3)), ]

v5 <- left_join(x = v4 %>% select(to4) %>% unique(),
                y = mitrenet$edges %>% select(from, label, to),
                by = c("to4" = "from")) %>%
  rename("label5" = "label", "to5" = "to")
v5 <- v5[!(v5$to5 %in% unique(vulns$id)), ]
v5 <- v5[!(v5$to5 %in% unique(v1$to1)), ]
v5 <- v5[!(v5$to5 %in% unique(v2$to2)), ]
v5 <- v5[!(v5$to5 %in% unique(v3$to3)), ]
v5 <- v5[!(v5$to5 %in% unique(v4$to4)), ]

v6 <- left_join(x = v5 %>% select(to5) %>% unique(),
                y = mitrenet$edges %>% select(from, label, to),
                by = c("to5" = "from")) %>%
  rename("label6" = "label", "to6" = "to")
v6 <- v6[!(v6$to6 %in% unique(vulns$id)), ]
v6 <- v6[!(v6$to6 %in% unique(v1$to1)), ]
v6 <- v6[!(v6$to6 %in% unique(v2$to2)), ]
v6 <- v6[!(v6$to6 %in% unique(v3$to3)), ]
v6 <- v6[!(v6$to6 %in% unique(v4$to4)), ]
v6 <- v6[!(v6$to6 %in% unique(v5$to5)), ]

v7 <- left_join(x = v6 %>% select(to6) %>% unique(),
                y = mitrenet$edges %>% select(from, label, to),
                by = c("to6" = "from")) %>%
  rename("label7" = "label", "to7" = "to")
v7 <- v7[!(v7$to7 %in% unique(vulns$id)), ]
v7 <- v7[!(v7$to7 %in% unique(v1$to1)), ]
v7 <- v7[!(v7$to7 %in% unique(v2$to2)), ]
v7 <- v7[!(v7$to7 %in% unique(v3$to3)), ]
v7 <- v7[!(v7$to7 %in% unique(v4$to4)), ]
v7 <- v7[!(v7$to7 %in% unique(v5$to5)), ]
v7 <- v7[!(v7$to7 %in% unique(v6$to6)), ]


vulns_net <- left_join(v1, v2, "to1") %>%
  left_join(v3, by = "to2") %>%
  left_join(v4, by = "to3") %>%
  left_join(v5, by = "to4") %>%
  left_join(v6, by = "to5") %>%
  left_join(v7, by = "to6")

vulns_net <- vulns_net[complete.cases(vulns_net), ]
