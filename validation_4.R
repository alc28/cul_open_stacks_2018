# primary script for project. Selects sample for validation study.

library(tidyverse)
library(readr)
library(stringr)
library(RODBC)

# library(DBI)
# https://github.com/r-dbi/odbc#windows

# set up ODBC session and run large query

ch <- odbcConnect("Voyager", uid = "dbread", pwd = "dbread")

validation_head_sql <- function(num_records) {
  mysql <- paste("SELECT BIB_MFHD.BIB_ID, ITEM_VW.MFHD_ID, ITEM_VW.ITEM_ID, ITEM_VW.PERM_LOCATION_CODE, ITEM_VW.CALL_NO, ITEM_STATUS_TYPE.ITEM_STATUS_DESC FROM ITEM_STATUS_TYPE INNER JOIN (ITEM_STATUS INNER JOIN (MFHD_MASTER INNER JOIN (BIB_MASTER INNER JOIN ((ITEM_VW INNER JOIN BIB_MFHD ON ITEM_VW.MFHD_ID = BIB_MFHD.MFHD_ID) INNER JOIN BIB_TEXT ON BIB_MFHD.BIB_ID = BIB_TEXT.BIB_ID) ON BIB_MASTER.BIB_ID = BIB_MFHD.BIB_ID) ON MFHD_MASTER.MFHD_ID = BIB_MFHD.MFHD_ID) ON ITEM_STATUS.ITEM_ID = ITEM_VW.ITEM_ID) ON ITEM_STATUS_TYPE.ITEM_STATUS_TYPE = ITEM_STATUS.ITEM_STATUS WHERE (((BIB_MFHD.BIB_ID)<",  num_records  ,") AND ((BIB_TEXT.BIB_FORMAT)='am') AND ((BIB_MASTER.SUPPRESS_IN_OPAC)='N') AND ((MFHD_MASTER.SUPPRESS_IN_OPAC)='N'))")
return(mysql)
}


validation_all_sql <- function() {
  mysql <- paste("SELECT BIB_MFHD.BIB_ID, ITEM_VW.MFHD_ID, ITEM_VW.ITEM_ID, ITEM_VW.PERM_LOCATION_CODE, ITEM_VW.CALL_NO, ITEM_STATUS_TYPE.ITEM_STATUS_DESC FROM ITEM_STATUS_TYPE INNER JOIN (ITEM_STATUS INNER JOIN (MFHD_MASTER INNER JOIN (BIB_MASTER INNER JOIN ((ITEM_VW INNER JOIN BIB_MFHD ON ITEM_VW.MFHD_ID = BIB_MFHD.MFHD_ID) INNER JOIN BIB_TEXT ON BIB_MFHD.BIB_ID = BIB_TEXT.BIB_ID) ON BIB_MASTER.BIB_ID = BIB_MFHD.BIB_ID) ON MFHD_MASTER.MFHD_ID = BIB_MFHD.MFHD_ID) ON ITEM_STATUS.ITEM_ID = ITEM_VW.ITEM_ID) ON ITEM_STATUS_TYPE.ITEM_STATUS_TYPE = ITEM_STATUS.ITEM_STATUS WHERE BIB_TEXT.BIB_FORMAT='am' AND BIB_MASTER.SUPPRESS_IN_OPAC='N' AND MFHD_MASTER.SUPPRESS_IN_OPAC='N'")
  
  return(mysql)
}

#system.time(result <- sqlQuery(ch,  validation_head_sql(90000) )   )
system.time(result <- sqlQuery(ch,  validation_all_sql() )   )

result$PERM_LOCATION_CODE <- as.character(result$PERM_LOCATION_CODE)
result$CALL_NO <- as.character(result$CALL_NO)
result$ITEM_STATUS_DESC <- as.character(result$ITEM_STATUS_DESC)


# filter out microfilm and in process

df <- result %>%
  filter(!str_detect(CALL_NO, regex("FILM |in process|fiche", ignore_case = TRUE)))

write_tsv(df, gzfile("data/df_all_after_filter.tsv.zip"))

remove(result)
############# STEP 2 : get sample #############


get_df_sample <- function(df, samplesize) {
  df$rownumber <- seq.int(nrow(df))
  sample_increment <- as.integer(nrow(df)/ samplesize)
  keepers <- seq(1, nrow(df), sample_increment)
  keepers <- as.data.frame(keepers)
  names(keepers)[1] <- "rownumber"
  df_increment_select <- right_join(df, keepers, by = "rownumber")
  return(df_increment_select)
}

location_list <- c("afr","hote","ilr","jgsm","law","mann","math","mus","asia","ech","olin","sasa","was","uris","vet")

population_to_draw_from <- df %>%
  filter(PERM_LOCATION_CODE %in% location_list)

sample_6000 <- get_df_sample(population_to_draw_from, 5997)

sum_population_to_draw_from <- population_to_draw_from %>%
  count(PERM_LOCATION_CODE, sort = TRUE) %>%
  mutate(population_total = nrow(population_to_draw_from), pct_of_total_population = round(n / population_total,4) * 100   )

sum_sample6000 <- sample_6000 %>%
  count(PERM_LOCATION_CODE, sort = TRUE) %>%
  mutate(total_sample = nrow(sample_6000), pct_of_total_sample = round(n / total_sample,4) * 100   )

df_joined <- left_join(sum_population_to_draw_from, sum_sample6000, by = "PERM_LOCATION_CODE")


# add OCLC numbers

bibs_w_oclc <- read_delim("data/validation_Bibs_leaderisam.BIB.list", 
                                             "\t", escape_double = FALSE, col_names = FALSE,  trim_ws = TRUE)

names(bibs_w_oclc)[1] <- "BIB_ID"
names(bibs_w_oclc)[3] <- "f035"
bibs_w_oclc$X2 <- NULL

bibs_oclc_clean <- bibs_w_oclc %>%
  filter( grepl("OCoLC", f035) ) %>%
  mutate(oclc_num = str_extract(f035, "(\\d+)") )

bibs_oclc_clean$f035 <- NULL

sample_6000 <- left_join(sample_6000, bibs_oclc_clean, by = "BIB_ID")

write_csv(sample_6000, "output/sample_6000.csv")
write_csv(df_joined, "output/sum_comparison_all_vs_sample.csv")




