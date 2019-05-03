# create model df

# load libraries

library(tidyverse)
library(stringr)
library(forcats)
library(readxl)
library(broom)
library(janitor)
library(openxlsx)



# process data
df_raw <- read_excel("data/inventory_list_09-19-18.xlsx", 
                     skip = 2)

prepare_df <- function(df) {
  df <- clean_names(df)
  df$present_or_not <- as.factor(df$present_or_not)
  df <- df %>%
    mutate(is_missing = fct_recode(present_or_not,
                                   "0" = "Present",
                                   "0" = "CheckedOut",
                                   "1" = "NotOnShelf-Verified",
                                   "1" = "LMBO")) %>%
    mutate(is_accountedfor = fct_recode(present_or_not,
                                   "1" = "Present",
                                   "1" = "CheckedOut",
                                   "0" = "NotOnShelf-Verified",
                                   "0" = "LMBO")) %>%
    mutate(location_group = as.factor(location_code),
           location_group = fct_infreq(location_code),
           location_group = fct_recode(location_code,
                                       "hlm" = "ilr",
                                       "hlm" = "hote",
                                       "hlm" = "jgsm",
                                       "asia" = "asia",
                                       "asia" = "was",
                                       "asia" = "ech",
                                       "asia" = "sasa"))
  
  df$has_circulated <- ifelse(df$historical_voyager_circs > 0, 1, 0)
  df$is_oversize <- ifelse(str_detect(df$call_nbr_display, "\\+"), 1, 0)
  df$begin_pub_date <- as.numeric(df$begin_pub_date)
  # 
  df <- df %>%
    mutate(age = 2018 - begin_pub_date)
  # df$firstletter <- as.factor(df$firstletter)
  df$condition <- as.factor(df$condition)
  df$item_type_name <- as.factor(df$item_type_name)
  
  return(df)
}

# generate df

df <- prepare_df(df_raw)
df <- df %>%
  filter( !(location_code == "law" & item_type_name == "nocirc") )

# save df to disk

wb <- createWorkbook()
addWorksheet(wb, "model_data")
writeData(wb, 1, df)
saveWorkbook(wb, file= "data/working_df_for_model/cul_validation_201810.xlsx", overwrite = TRUE)

write_csv(df, "data/working_df_for_model/cul_validation_201810.csv")
write_rds(df, "data/working_df_for_model/cul_validation_201810.rds", compress = "none")
