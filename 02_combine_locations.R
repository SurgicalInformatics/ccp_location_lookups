#TD 25/04/2020 run this to the bottom 
#You need to download and unzip this file - https://geoportal.statistics.gov.uk/datasets/national-statistics-postcode-lookup-february-2020 too big github. Then just take out NSPL_FEB_2020_UK.csv
#Compile locations
library(tidyverse)
library(readxl)
library(stringr)
library(ggmap)

`%ni%` = Negate(`%in%`)

source('01_pull_centres.R')

postcode_regex_string = '(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})'

#google maps set up
api_key = 'AIzaSyADK-Xycf5uqnyvYDPP58wSQdnLJZCzrr8' #Google maps API key

register_google(api_key)

#load in locations

#First the file from ewen
nhs_ods_list = read_xslx('location_data/NHS_ODS.xlsx')
hosp_2_list_in = read_csv('location_data/Hospital2.csv')
ni_list = read_csv('location_data/niorg.csv', col_names = F)
wales_list = read_csv('location_data/wlhbsite.csv', col_names = F)
scotland_list = read_csv('location_data/scotland_hospitals.csv')

postcode_lookup = read_csv('location_data/NSPL_FEB_2020_UK.csv')

#First, fix errors in hosp2 list as there's lots of them
hosp_2_list_keep = hosp_2_list_in %>% 
  filter(grepl(postcode_regex_string, Postcode)) 

hosp_2_list_fix = hosp_2_list_in %>% 
  filter(!grepl(postcode_regex_string, Postcode)) %>% 
  mutate(search_string = paste0(OrganisationName, ', ', ifelse(!is.na(Address1),Address1, ''),
                                ifelse(!is.na(Address2),Address2, ''),
                                ifelse(!is.na(Address3),Address3, ''),', UK'))

hosp_2_fix_lat_lon = geocode(hosp_2_list_fix$search_string, output = "latlona", source = "google") 

hosp_2_fixed_lat_lon = cbind(hosp_2_list_fix, hosp_2_fix_lat_lon) %>% 
  mutate(Postcode = toupper(str_extract(address, postcode_regex_string)),
         Latitude = lat,
         Longitude = lon)

hosp_2_list_in = hosp_2_fixed_lat_lon %>% select(colnames(hosp_2_list_keep)) %>% rbind(hosp_2_list_keep)

#Select useful variables

scotland_list %>% select(Location, LocationName, Postcode) %>% 
  rename(org_code = Location,
         place_name = LocationName,
         postcode = Postcode) %>% 
  mutate(postcode = toupper(postcode),
         place_name = str_to_title(place_name),
         country = 'Scotland') -> scotland_list

wales_list %>% select(X1, X2, X10) %>% 
  rename(org_code = X1,
         place_name = X2,
         postcode = X10) %>% 
  mutate(postcode = toupper(postcode),
         place_name = str_to_title(place_name),
         country = 'Wales')  -> wales_list

ni_list %>% select(X1, X2, X10) %>% 
  rename(org_code = X1,
         place_name = X2,
         postcode = X10) %>% 
  mutate(postcode = toupper(postcode),
         place_name = str_to_title(place_name),
         country = 'Northern Ireland')  -> ni_list

hosp_2_list_in %>% select(OrganisationCode, OrganisationName, Postcode) %>% 
  rename(org_code = OrganisationCode,
         place_name = OrganisationName,
         postcode = Postcode) %>% 
  mutate(postcode = toupper(postcode),
         place_name = str_to_title(place_name),
         country = 'England') -> hosp_2_list

nhs_ods_list %>% select(`ODS code`, `ODS Name`, Postcode) %>%
  rename(org_code = `ODS code`,
         place_name = `ODS Name`,
         postcode = Postcode) %>%
  mutate(postcode = toupper(postcode),
         place_name = str_to_title(place_name),
         country = NA,
         search_name = paste0(place_name, ', ', postcode)) %>% filter(!is.na(place_name)) -> nhs_ods_list

#bind rows
look_up_all = rbind(wales_list, ni_list, hosp_2_list, scotland_list) %>% 
              mutate(search_name = paste0(place_name, ', ', postcode),
                     org_code = ifelse(org_code == 'RHM00', 'RHM00', org_code)) %>% 
              distinct(org_code, .keep_all = T)

#write_csv(look_up_all, 'look_up_all_uk_hospital_codes.csv')

ccp_looked_up = ccp_ids_labelled %>% 
  select(redcap_data_access_group, dag_id) %>% 
  distinct(dag_id, .keep_all = T) %>% 
  mutate(dag_id = str_replace_all(dag_id, 'O', '0')) %>% 
  left_join(look_up_all, by = c('dag_id' = 'org_code')) 

look_up_unmatch_wales = look_up_all %>% mutate(org_code = ifelse(country == 'Wales', paste0('WB', substring(org_code, 3)), org_code),
                                               org_code = ifelse(org_code == 'WB2AL', 'WB3AL', org_code),
                                               org_code = ifelse(org_code == 'WB2BL', 'WF1BL', org_code),
                                               org_code = ifelse(org_code == 'RWEAA', 'TWEAA', org_code))# %>% rbind(nhs_ods_list)

#find those without a match then match them for misspelt names
ccp_looked_up %>% 
  filter(is.na(place_name)) %>% 
  select(redcap_data_access_group, dag_id) %>% 
  distinct(dag_id, .keep_all = T) %>% 
  left_join(look_up_unmatch_wales, by = c('dag_id' = 'org_code')) -> ccp_missings

ccp_missings %>% 
  filter(!is.na(place_name)) -> ccp_missings_found

#now select the ones even ccp has missed
ccp_missings %>% 
  filter(is.na(place_name)) -> ccp_still_missings

#Do some relabelling
ccp_still_missings %>% 
  mutate(redcap_data_access_group = ifelse(redcap_data_access_group == 'Queen Elizabeth University Hospital, Glasgow University Hospital Monklands Airdrie',
                                           'Queen Elizabeth University Hospital, Glasgow', redcap_data_access_group),
         redcap_data_access_group = ifelse(redcap_data_access_group == 'Lanarkshire NHSFT',
                                           'University Hospital Monklands Airdrie', redcap_data_access_group),
         redcap_data_access_group = ifelse(redcap_data_access_group == 'Mid Yorkshire Hospitals NHST',
                                           'Pinderfields', redcap_data_access_group),
         redcap_data_access_group = ifelse(redcap_data_access_group == 'Chelsea & Westminster Hospital',
                                           'Chelsea and Westminster Hospital NHS, 369 Fulham Rd, London', redcap_data_access_group),
         redcap_data_access_group = gsub('NHST', 'NHS Trust', redcap_data_access_group),
         redcap_data_access_group = ifelse(redcap_data_access_group == 'University Hospitals of Leicester',
                                           'Leicester Royal Infirmary', redcap_data_access_group)) %>% 
  filter(!is.na(redcap_data_access_group)) -> ccp_still_missings

#select the completes
ccp_looked_up %>% 
  filter(!is.na(place_name)) %>% select(-search_name) %>% 
  mutate(postcode = ifelse(redcap_data_access_group == 'Queen Elizabeth University Hospital, Glasgow University Hospital Monklands Airdrie' & dag_id == 'RHM01',
                         'G51 4TF', postcode),
         place_name = ifelse(redcap_data_access_group == 'Queen Elizabeth University Hospital, Glasgow University Hospital Monklands Airdrie' & dag_id == 'RHM01',
                         'Queen Elizabeth University Hospital', place_name),
         country = ifelse(redcap_data_access_group == 'Queen Elizabeth University Hospital, Glasgow University Hospital Monklands Airdrie' & dag_id == 'RHM01',
                           'Scotland', country))-> ccp_looked_up_complete

unique(ccp_looked_up_complete$redcap_data_access_group)
#Run the still missings through google
result_lat_long = geocode(ccp_still_missings$redcap_data_access_group, output = "latlona", source = "google") 

ccp_still_missings = cbind(ccp_still_missings, result_lat_long)

#extract postcodes from these missing ones

ccp_still_missings$postcode = toupper(str_extract(ccp_still_missings$address, postcode_regex_string))

#Combine, but keep ccp_still_missings separate

ccp_missings_found %>% 
  select(-search_name) %>% 
  rbind(ccp_looked_up_complete) -> ccp_combined_1

ccp_still_missings %>% 
  select(redcap_data_access_group, dag_id, postcode, lon, lat) -> ccp_combined_2

#remove the ones with lat lons available from hosp_2_list
ccp_combined_1 %>% 
  filter(dag_id %ni% hosp_2_list_in$OrganisationCode) -> ccp_combined_1_to_search

ccp_combined_result_lat_long = geocode(ccp_combined_1_to_search$postcode, output = "latlona", source = "google") 

ccp_combined_with_ll_dags = cbind(ccp_combined_1_to_search, ccp_combined_result_lat_long) %>% select(redcap_data_access_group, dag_id, place_name, postcode, country, lon, lat)

#now combine with lat lons available from hosp_2_list
ccp_combined_1 %>% 
  filter(dag_id %in% hosp_2_list_in$OrganisationCode) -> ccp_combined_1_has_ll


hosp_2_list_in %>% select(OrganisationCode, Longitude, Latitude) %>% 
  rename(org_code = OrganisationCode,
         lon = Longitude,
         lat = Latitude) -> hosp_2_list_ll

ccp_combined_1_has_ll %>% 
  left_join(hosp_2_list_ll, by = c('dag_id' = 'org_code')) -> ccp_combined_1_has_ll

combined_all = rbind(ccp_combined_with_ll_dags, ccp_combined_1_has_ll)

#Lookup country 

postcode_lookup = postcode_lookup %>% 
  mutate(country = ifelse(startsWith(ccg, 'S0'), 'Scotland', NA),
         country = ifelse(startsWith(ccg, 'E'), 'England', country),
         country = ifelse(startsWith(ccg, 'W1'), 'Wales', country),
         country = ifelse(startsWith(ccg, 'ZC'), 'Northern Ireland', country))

postcode_country = postcode_lookup %>% mutate(ccg = ifelse(country != 'England', hlthau, ccg)) %>% select(pcds, country, ccg) 

combined_all = ccp_combined_2 %>% 
  left_join(postcode_country, by = c('postcode' = 'pcds')) %>% 
  mutate(place_name = redcap_data_access_group) %>% 
  select(colnames(combined_all)) %>% rbind(combined_all)

postcode_ccg = postcode_lookup %>% mutate(ccg = ifelse(country != 'England', hlthau, ccg)) %>% select(pcds, ccg)

combined_all = combined_all %>% 
  left_join(postcode_ccg, by = c('postcode' = 'pcds'))

#write a csv
save_date = Sys.Date() %>% format('%d-%B-%Y')

write_csv(combined_all, paste0('ccp_dag_id_lookup_', save_date, '.csv'))
