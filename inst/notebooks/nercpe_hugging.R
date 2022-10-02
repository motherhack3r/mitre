# # Load reticulate into current R session
# library(reticulate)
# library(here)
#
reticulate::use_condaenv("textrpp_condaenv")
#
# # Install Python package into virtual environment
# # > conda install pytorch torchvision torchaudio cudatoolkit=11.3 -c pytorch
# # reticulate::py_install(packages = c("pytorch", "torchvision", "torchaudio", "cudatoolkit"), pip = TRUE)
# "torch==1.11.0" "transformers==4.19.2" numpy nltk
#
# # Retrieve/force initialization of Python
reticulate::py_config()
#
# # Check if python is available
reticulate::py_available()

library(text)

# Initialize the installed conda environment.
# save_profile = TRUE saves the settings so that you don't have to run textrpp_initialize() after restarting R.
textrpp_initialize(condaenv = "textrpp_condaenv", prompt = FALSE)

# text::textModelsRemove("Neurona/cpener-test")

library(mitre)
library(dplyr)

predict_cpe <- function(df_inventory = mitre::getInventory(),
                        model_name = "Neurona/cpener-test",
                        hastitle = FALSE) {
  # Function for processing NER output
  embed2cpener <- function(df_ner = data.frame()) {
    df_cpe <- as.data.frame(df_ner$x_NER)
    df_cpe$entity <- stringr::str_replace_all(string = df_cpe$entity,
                                              pattern = "^[BIOLU]\\-(cpe.+)$",
                                              replacement = "\\1")
    df_cpe <- inner_join(x = df_cpe %>%
                           group_by(entity) %>%
                           summarise(score = mean(score)),
                         y = df_cpe %>%
                           select(entity, word) %>%
                           group_by(entity) %>%
                           mutate(word = paste(word, collapse = " ")) %>%
                           unique() %>%
                           as.data.frame(),
                         by = "entity")
    df_cpe$word <- stringr::str_replace_all(string = df_cpe$word, pattern = "\\s##", replacement = "")
    df_cpe$word <- stringr::str_replace_all(string = df_cpe$word, pattern = "^\\s*##", replacement = "")

    return(df_cpe)
  }

  # Function for CPE creation
  cpener2cpe23 <- function(df_ner = data.frame()) {
    part <- "a"
    vendor <- ifelse(test = "cpe_vendor" %in% df_ner$entity,
                     yes = df_ner$word[df_ner$entity == "cpe_vendor"],
                     no = "*")
    product <- ifelse(test = "cpe_product" %in% df_ner$entity,
                      yes = df_ner$word[df_ner$entity == "cpe_product"],
                      no = "*")
    version <- ifelse(test = "cpe_version" %in% df_ner$entity,
                      yes = df_ner$word[df_ner$entity == "cpe_version"],
                      no = "*")
    vendor <- stringr::str_replace_all(vendor, " ", "_")
    product <- stringr::str_replace_all(product, " ", "_")
    version <- stringr::str_replace_all(version, "(\\d) (\\d)", "\\1\\.\\2")
    version <- stringr::str_replace_all(version, "(\\d) (\\d)", "\\1\\.\\2")
    cpe <- paste("cpe", "2.3", part, vendor, product, version, "*:*:*:*:*:*:*", sep = ":")
    return(cpe)
  }

  if (!hastitle) {
    sw_title <- paste(mitre::cpe_wfn_vendor(df_inventory$vendor),
                      mitre::cpe_wfn_product(df_inventory$name), sep = " ")
    df_pred <- data.frame(title = sapply(sw_title,
                                         function(x)
                                           paste(unique(unlist(strsplit(x, " "))), collapse = " ")),
                          cpe = rep(NA, length(sw_title)))
    df_pred$title <- paste(df_pred$title, df_inventory$version)
  } else {
    df_pred <- df_inventory
  }

  df_inventory$cpe <- sapply(df_pred$title,
                             function(x) {
                               cpener2cpe23(embed2cpener(textNER(x = x,
                                                                 model = model_name,
                                                                 device = "gpu")))
                             }) %>% as.character()

  return(df_inventory)
}


df_inventory <- mitre::getInventory()
df_inv <- df_inventory %>% sample_n(50)

df_inv <- predict_cpe(df_inventory = df_inv)

