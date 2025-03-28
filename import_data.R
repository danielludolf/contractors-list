
library(tidyverse)

# read in entity registration data for those registered in SAM to do business with the federal govt
  # you will need to go to the url below, download the zip file, and then within the zip file there's
  # a dat file that you can add into this repo to run the script
    # https://sam.gov/data-services/Entity%20Registration/Public%20V2?privacy=Public
    # NOTE I had to manually download data, you can't import from the url link
dat <- read.table("~/contractors-list/SAM_PUBLIC_MONTHLY_V2_20250302.dat", header = F, sep = "|",
                  fill = T, quote = "", stringsAsFactors = F, skip = 1)

dat %>% 
  # select company name, address information, and the company's website
  select(V12, V16:V21, V27) %>% 
  # trim whitespace
  mutate(across(everything(), ~ .x %>% str_squish())) %>%
  # condense columns related to the company's address into one column
  mutate(`Company Address` = if_else(
    is.na(V21) | V21 == "", 
    str_glue("{V16} {V17} {V18}, {V19} {V20}"),
    str_glue("{V16} {V17} {V18}, {V19} {V20}-{V21}")),
    # place website names in all lowercase
    V27 = str_to_lower(V27)) %>% 
  # renaming columns and adjusting how they're presented
  rename(`Company Name` = V12, `Company Website` = V27) %>% 
  select(-V16:-V21, `Company Name`, `Company Website`, `Company Address`) %>% 
  # trim whitespace again
  mutate(across(everything(), ~ .x %>% str_squish())) %>%
  # remove duplicate rows 
  distinct() %>%
  # write csv file
  write_csv("~/contractors-list/contractors_list.csv")
