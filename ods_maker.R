#make a better ODS lookup than the coordinating office one
library(tidyverse)
library(readxl)

nhs_ods_list = read_excel('location_data/NHS_ODS.xlsx')

paste5 <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}

#now add the others

`%ni%` = Negate(`%in%`)

ods_hospital_list = read_csv('location_data/ecare.csv') %>% 
  mutate(ods_address = paste5(address_1, address_2, address_3, sep = ', ', na.rm = T)) %>% 
  select(-address_1, -address_2, -address_3) %>% 
  mutate(trust_code = str_extract(ods_code, "^.{3}")) 

ods_trust_list = read_csv('location_data/etrust.csv') %>% 
  mutate(trust_address = paste5(address_1, address_2, address_3, sep = ', ', na.rm = T)) %>% 
  select(-address_1, -address_2, -address_3) %>% select(-X4) %>% 
  mutate(ods_code_n = str_count(ods_trust_code)) %>% 
  filter(ods_code_n <5) %>% 
  select(-ods_code_n) %>% 
  select(-region, -postcode)

ods_auth_list = read_csv('location_data/eauth.csv') %>% 
  mutate(auth_address = paste5(address_1, address_2, address_3, sep = ', ', na.rm = T)) %>% 
  select(-address_1, -address_2, -address_3) %>% select(-X3)

additional_trusts = ods_hospital_list %>% 
  filter(trust_code %ni% ods_trust_list$ods_trust_code) %>% 
  mutate(ods_code_n = str_count(ods_code)) %>% 
  filter(ods_code_n <5) %>% 
  select(-ods_code_n) %>% 
  rename(ods_trust_code = ods_code, 
         trust_name = organisation_name,
         national_grouping_code = commission_ods_code,
         city_1 = city,
         trust_address = ods_address) %>% 
  select(-region, - trust_code, -postcode)

additional_sites = read_csv('location_data/etrust.csv') %>% 
  mutate(trust_address = paste5(address_1, address_2, address_3, sep = ', ', na.rm = T)) %>% 
  select(-address_1, -address_2, -address_3) %>% select(-X4) %>% 
  mutate(ods_code_n = str_count(ods_trust_code)) %>% 
  filter(ods_code_n >3) %>% 
  filter(ods_trust_code %ni% ods_hospital_list$ods_code) %>% 
  select(-ods_code_n) %>% 
  rename(ods_code = ods_trust_code,
         organisation_name = trust_name,
         commission_ods_code = national_grouping_code,
         city = city_1) %>% 
  filter(ods_code %ni% ods_hospital_list$ods_code) %>% 
  mutate(trust_code = str_extract(ods_code, "^.{3}")) 

ods_hospital_list = ods_hospital_list %>% 
  bind_rows(additional_sites)

ods_trust_list = ods_trust_list %>% 
  bind_rows(additional_trusts)

ods_hospital_list = ods_hospital_list %>% 
  mutate(ods_code_n = str_count(ods_code)) %>% 
  filter(ods_code_n >3) %>% 
  select(-ods_code_n)


#now join them up
ods_overall_list = ods_hospital_list %>% 
  left_join(ods_trust_list, by = c('trust_code' = 'ods_trust_code')) %>% 
  left_join(ods_auth_list, by = c('commission_ods_code' = 'ods_commission_code')) %>% 
  distinct(ods_code, .keep_all = T) %>% 
  mutate_all(str_to_title) %>% 
  mutate(postcode = toupper(postcode),
         ods_code = toupper(ods_code),
         commission_ods_code = toupper(commission_ods_code),
         trust_code = toupper(trust_code),
         national_grouping_code = toupper(national_grouping_code),
         commission_postcode = toupper(commission_postcode))


#now combine with already existing list
ods_out = ods_overall_list %>% 
  rename(`ODS code` = ods_code,
         `ODS Name` = organisation_name,
         Postcode = postcode,
         `Parent Code` = trust_code,
         `Parent Trust` = trust_name,
         `National Grouping Name` = commision_name) %>% 
  select(`ODS code`, `ODS Name`, Postcode, `Parent Code`, `Parent Trust`, `National Grouping Name`) %>% 
  filter(`ODS code` %ni% nhs_ods_list$`ODS code`)

nhs_ods_list2 = bind_rows(nhs_ods_list, ods_out)

write_csv(nhs_ods_list2, 'location_data/ods_out_2.csv')
