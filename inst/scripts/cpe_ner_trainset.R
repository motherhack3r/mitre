# library(mitre)

# CREATE TRAIN SET FOR NER

p_type <- "vpv"
p_num_samples <- 10000
p_seed <- 42
verbose <- TRUE

if (verbose) print(paste0("[*] ", "Starting process for custom CPE train set..."))
set.seed(p_seed)

# Get latest CPE data frame
if (verbose) print(paste0("[|] ", "Get latest CPE data frame"))
cpes <- mitre::cpe_latest_data(remote = T, keep_deprecated = T)

# Annotate title for NER as new column and remove not annotated
if (verbose) print(paste0("[|] ", "Annotate title for NER"))
df <- mitre::cpe_add_notation(df = cpes, type = p_type)
df <- df[!is.na(df$annotated), ]

# Add column with vendor popularity
if (verbose) print(paste0("[|] ", "Add column with vendor popularity"))
cpes_sts <- mitre::cpe_stats(df = cpes)
df <- dplyr::left_join(df, cpes_sts[, c("vendor", "popular")], by = "vendor")

# Sample weight by `popular` column
if (verbose) print(paste0("[|] ", "Select ", as.character(p_num_samples),
                          " samples weighted by popularity"))
df <- dplyr::slice_sample(df, weight_by = popular, n = p_num_samples)

if (verbose) print(paste0("[.] ", "Training set completed. Summary:"))
if (verbose) summary(df)


# Get local software inventory
if (verbose) print(paste0("[*] ", "Starting process for local inventory data set..."))
df_inv <- mitre::getInventory()
