#TD 25/04/2020 run this to the bottom 
#You need to download and unzip this file - https://geoportal.statistics.gov.uk/datasets/national-statistics-postcode-lookup-february-2020 too big github. Then just take out NSPL_FEB_2020_UK.csv
#Compile locations
library(tidyverse)
library(readxl)
library(stringr)
library(ggmap)
library(janitor)

`%ni%` = Negate(`%in%`)

source('01_pull_centres.R')

postcode_regex_string = '(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})'

#google maps set up
api_key = 'AIzaSyADK-Xycf5uqnyvYDPP58wSQdnLJZCzrr8' #Google maps API key

register_google(api_key)

#load in locations

#First the file from ewen
nhs_ods_list = read_excel('location_data/NHS_ODS.xlsx')
hosp_2_list_in = read_csv('location_data/Hospital2.csv')
ni_list = read_csv('location_data/niorg.csv', col_names = F)
wales_list = read_csv('location_data/wlhbsite.csv', col_names = F)
scotland_list = read_csv('location_data/scotland_hospitals.csv')
townsend_scores_output_areas = read_csv('location_data/townsend_oa_2011.csv') %>% clean_names() %>% select(geo_code, tds)
postcode_lookup = read_csv('location_data/NSPL_FEB_2020_UK.csv') %>% 
  left_join(townsend_scores_output_areas, by = c('oa11' = 'geo_code')) %>% 
  group_by(ccg) %>% 
  mutate(tds_mean = mean(tds, na.rm = T),
         imd_average = mean(imd, na.rm = T)) %>% 
  ungroup()

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

#Add a row for RMH

#write_csv(look_up_all, 'look_up_all_uk_hospital_codes.csv')

ccp_looked_up = ccp_ids_labelled %>% 
  filter(subjid != 'RHM01-0001') %>% 
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
                           'Scotland', country)) -> ccp_looked_up_complete

#unique(ccp_looked_up_complete$redcap_data_access_group)

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

postcode_country = postcode_lookup %>% mutate(ccg = ifelse(country != 'England', hlthau, ccg)) %>% select(pcds, country, ccg, tds_mean) 

combined_all = ccp_combined_2 %>% 
  left_join(postcode_country, by = c('postcode' = 'pcds')) %>% 
  mutate(place_name = redcap_data_access_group) %>% 
  select(colnames(combined_all)) %>% rbind(combined_all)

postcode_ccg = postcode_lookup %>% mutate(ccg = ifelse(country != 'England', hlthau, ccg)) %>% select(pcds, ccg, tds_mean)

combined_all = combined_all %>% 
  left_join(postcode_ccg, by = c('postcode' = 'pcds'))

#Add manual edits

combined_all %>% filter(is.na(postcode))

combined_all = combined_all %>% 
               mutate(postcode = ifelse(dag_id == 'RMH01', 'SO16 6YD', postcode),
                      country = ifelse(dag_id == 'RMH01', 'England', country),
                      lon = ifelse(dag_id == 'RMH01', -1.4350899457931519, lon),
                      lat = ifelse(dag_id == 'RMH01', 50.933021545410156, lat),
                      ccg = ifelse(dag_id == 'RMH01', 'E38000167', ccg),
                      place_name = ifelse(dag_id == 'SL116', 'Western General Hospital', place_name),
                      postcode = ifelse(dag_id == 'SL116', 'EH4 2XU', postcode),
                      country = ifelse(dag_id == 'SL116', 'Scotland', country),
                      lon = ifelse(dag_id == 'SL116', -3.2338808, lon),
                      lat = ifelse(dag_id == 'SL116', 55.9621052, lat),
                      ccg = ifelse(dag_id == 'SL116', 'S08000024', ccg),
                      place_name = ifelse(dag_id == 'RW5JJ', 'Royal Preston Hospital', place_name),
                      postcode = ifelse(dag_id == 'RW5JJ', 'PR2 9HT', postcode),
                      country = ifelse(dag_id == 'RW5JJ', 'England', country),
                      lon = ifelse(dag_id == 'RW5JJ', -2.7040145, lon),
                      lat = ifelse(dag_id == 'RW5JJ', 53.7908668, lat),
                      ccg = ifelse(dag_id == 'RW5JJ', 'E38000227', ccg),
                      place_name = ifelse(dag_id == 'RVV00' , 'Queen Elizabeth The Queen Mother Hospital', place_name),
                      postcode = ifelse(dag_id == 'RVV00', 'CT9 4AN', postcode),
                      country = ifelse(dag_id == 'RVV00', 'England', country),
                      lon = ifelse(dag_id == 'RVV00', 1.3893986940383911, lon),
                      lat = ifelse(dag_id == 'RVV00', 51.3780517578125, lat),
                      ccg = ifelse(dag_id == 'RVV00', 'E38000184', ccg),
                      place_name = ifelse(dag_id == 'RK590', 'Derriford Hospital', place_name),
                      postcode = ifelse(dag_id == 'RK590', 'PL6 8DH', postcode),
                      country = ifelse(dag_id == 'RK590', 'England', country),
                      lon = ifelse(dag_id == 'RK590', -4.1136713027954102, lon),
                      lat = ifelse(dag_id == 'RK590', 50.416728973388672, lat),
                      ccg = ifelse(dag_id == 'RK590', 'E38000230', ccg),
                      place_name = ifelse(dag_id == 'RNG90', 'Peterborough City Hospital', place_name),
                      postcode = ifelse(dag_id == 'RNG90', 'PE3 9GZ', postcode),
                      country = ifelse(dag_id == 'RNG90', 'England', country),
                      lon = ifelse(dag_id == 'RNG90', -0.2785687, lon),
                      lat = ifelse(dag_id == 'RNG90', 52.5837926, lat),
                      ccg = ifelse(dag_id == 'RNG90', 'E38000026', ccg),
                      place_name = ifelse(dag_id == 'RAWAS', 'Royal Shrewsbury Hospital', place_name),
                      postcode = ifelse(dag_id == 'RAWAS', 'SY3 8XQ', postcode),
                      country = ifelse(dag_id == 'RAWAS', 'England', country),
                      lon = ifelse(dag_id == 'RAWAS', -2.7937374114990234, lon),
                      lat = ifelse(dag_id == 'RAWAS', 52.709362030029297, lat),
                      ccg = ifelse(dag_id == 'RAWAS', 'E38000147', ccg),
                      place_name = ifelse(dag_id == 'RV001', 'William Harvey Hospital (Ashford)', place_name),
                      postcode = ifelse(dag_id == 'RV001', 'TN24 0LZ', postcode),
                      country = ifelse(dag_id == 'RV001', 'England', country),
                      lon = ifelse(dag_id == 'RV001', 0.91622304916381836, lon),
                      lat = ifelse(dag_id == 'RV001', 51.141487121582031, lat),
                      ccg = ifelse(dag_id == 'RV001', 'E38000002', ccg))

#Now lets add a city to postcode
postcode_to_city = read_csv('location_data/postcode_city_district.csv') %>% 
                   clean_names() %>% 
                   select(postcode, region) %>% 
                   rename(postcode_start = postcode,
                          city = region) %>% distinct(postcode_start, .keep_all = T)

combined_all = combined_all %>% 
  mutate(postcode_start = gsub("[[:space:]].*", '', postcode)) %>% 
  left_join(postcode_to_city, by = 'postcode_start')

#Finally, add back in the townsend average scores to those which needed new postcodes
lookup_tds_avg_missing = postcode_lookup %>% select(pcds, tds_mean) %>% rename(tds_mean_new = tds_mean)

combined_all = combined_all %>% 
  left_join(lookup_tds_avg_missing, by = c('postcode' = 'pcds')) %>% 
  mutate(tds_mean = ifelse(is.na(tds_mean), tds_mean_new, tds_mean)) %>% 
  select(-tds_mean_new)

# #CCGs not in 
# list_english_nhs_trusts = read_csv('location_data/list_of_nhs_trusts.csv', col_names = F) %>%
#   rename(org_code_prefix = X1,
#          org_name = X2,
#          city = X8,
#          address = X5,
#          y_code = X3,
#          q_code = X4,
#          postcode = X10) %>% select(org_code_prefix, org_name, city, address, postcode, y_code, q_code)
# 
# list_english_nhs_trusts = list_english_nhs_trusts %>%
#   filter(!grepl('ambulance', org_name, ignore.case = T)) %>%
#   filter(!grepl('mental', org_name, ignore.case = T)) %>%
#   filter(!grepl('community', org_name, ignore.case = T)) %>%
#   filter(!grepl('partnership', org_name, ignore.case = T)) %>%
#   filter(!grepl('weston area', org_name, ignore.case = T)) %>%
#   filter(!grepl('solent', org_name, ignore.case = T)) %>%
#   filter(!grepl('public health', org_name, ignore.case = T)) %>%
#   filter(!grepl('TEES, ESK AND WEAR VALLEYS NHS FOUNDATION TRUST', org_name, ignore.case = T)) %>%
#   filter(!grepl('NORTH EAST LONDON NHS FOUNDATION TRUST', org_name, ignore.case = T)) %>%
#   filter(!grepl('WEST LONDON NHS TRUST', org_name, ignore.case = T)) %>%
#   filter(!grepl('NORTH STAFFORDSHIRE COMBINED HEALTHCARE NHS TRUST', org_name, ignore.case = T)) %>%
#   filter(!grepl('OXLEAS NHS FOUNDATION TRUST', org_name, ignore.case = T)) %>%
#   filter(!grepl('BERKSHIRE HEALTHCARE NHS FOUNDATION TRUST', org_name, ignore.case = T)) %>%
#   filter(!grepl('MERSEY CARE NHS FOUNDATION TRUST', org_name, ignore.case = T)) %>%
#   filter(!grepl('SOUTHERN HEALTH NHS FOUNDATION TRUST', org_name, ignore.case = T)) %>%
#   filter(!grepl('EAST LONDON NHS FOUNDATION TRUST', org_name, ignore.case = T)) %>%
#   filter(!grepl('NORTH STAFFORDSHIRE COMBINED HEALTHCARE NHS TRUST', org_name, ignore.case = T))
# 
# #ccg lookup
# hospitals_with_ccg = look_up_all %>%
#     mutate(org_code_prefix = str_sub(org_code, 1,3)) %>%
#     left_join(postcode_ccg, by = c('postcode' = 'pcds')) %>%
#     filter(country == 'England') %>%
#     filter(!startsWith(org_code, 'NT'))
# 
# acute_nhs_trusts_joined = list_english_nhs_trusts %>%
#     left_join(hospitals_with_ccg, by = 'org_code_prefix')
# 
# postcode_ccg %>%
#   filter(ccg %ni% acute_nhs_trusts_joined$ccg) %>%
#   filter(startsWith(ccg, 'E')) %>%
#   distinct(ccg)
# 
# acute_nhs_trusts_joined %>% 
#   group_by(ccg) %>% 
#   distinct(place_name, .keep_all = T) %>% 
#   summarise(n = n()) %>% 
#   arrange(desc(n)) -> ccg_list_uk

#write a csv
save_date = Sys.Date() %>% format('%d-%B-%Y')

write_csv(combined_all, paste0('ccp_dag_id_lookup_', save_date, '.csv'))
