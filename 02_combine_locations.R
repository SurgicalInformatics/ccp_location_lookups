#TD 25/04/2020 run this to the bottom 
#You need to download and unzip this file - https://geoportal.statistics.gov.uk/datasets/national-statistics-postcode-lookup-february-2020 too big github. Then just take out NSPL_FEB_2020_UK.csv
#Scotland OA populations come from here:
# https://www.scotlandscensus.gov.uk/ods-web/data-warehouse.html#bulkdatatab
# download.file('https://www.scotlandscensus.gov.uk/ods-web/download/getDownloadFile.html?downloadFileIds=Output%20Area%20blk', destfile = 'scotland_oa.zip')

weighted.harmean <- function(x, w, ...)
{
  return(sum(w)/(sum(w/x, ...)))
}

weighted.quantile <- function(x, w, probs=seq(0,1,0.25), na.rm=TRUE) {
  x <- as.numeric(as.vector(x))
  w <- as.numeric(as.vector(w))
  if(anyNA(x) || anyNA(w)) {
    ok <- !(is.na(x) | is.na(w))
    x <- x[ok]
    w <- w[ok]
  }
  stopifnot(all(w >= 0))
  if(all(w == 0)) stop("All weights are zero", call.=FALSE)
  #'
  oo <- order(x)
  x <- x[oo]
  w <- w[oo]
  Fx <- cumsum(w)/sum(w)
  #'
  result <- numeric(length(probs))
  for(i in seq_along(result)) {
    p <- probs[i]
    lefties <- which(Fx <= p)
    if(length(lefties) == 0) {
      result[i] <- x[1]
    } else {
      left <- max(lefties)
      result[i] <- x[left]
      if(Fx[left] < p && left < length(x)) {
        right <- left+1
        y <- x[left] + (x[right]-x[left]) * (p-Fx[left])/(Fx[right]-Fx[left])
        if(is.finite(y)) result[i] <- y
      }
    }
  }
  names(result) <- paste0(format(100 * probs, trim = TRUE), "%")
  return(result)
}

`%ni%` = Negate(`%in%`)

#Weighted median

weighted.median <- function(x, w, na.rm=TRUE) {
  unname(weighted.quantile(x, probs=0.5, w=w, na.rm=na.rm))
}

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
api_key = '' #Google maps API key

register_google(api_key)

#load in locations

#First the file from ewen
site = read_csv('location_data/site_list.csv')
# nhs_ods_list = read_excel('location_data/NHS_ODS.xlsx')
nhs_ods_list = read_csv('location_data/ods_out_2.csv')
hosp_2_list_in = read_csv('location_data/hospital_3.csv') %>% filter(OrganisationCode %ni% nhs_ods_list$`ODS code`)
ni_list = read_csv('location_data/niorg.csv', col_names = F)
wales_list = read_csv('location_data/wlhbsite.csv', col_names = F)
scotland_list = read_csv('location_data/scotland_hospitals.csv')
townsend_scores_output_areas = read_csv('location_data/townsend_oa_2011.csv') %>% clean_names() %>% select(geo_code, tds)
postcode_lookup = read_csv('location_data/NSPL_FEB_2020_UK.csv')
england_oa_populations = read_csv('location_data/eng_oa_to_population.csv') %>% rename(oa11 = `2011 output area`,
                                                                                       total_pop_eng = `All usual residents`) %>% 
  select(oa11, total_pop_eng)

scotland_oa_populations = read_csv('location_data/KS101SC.csv') %>% rename(oa11 = X1,
                                                                           total_pop_scot = `All people`) %>% 
  select(oa11, total_pop_scot)

# scotland_imd_20_dz_lookup = read_excel('location_data/sco_datazone_imd20.xlsx', sheet = 3) %>% rename(oa11 = DZ,
#                                                                                                       simd20_rank = SIMD2020v2_Rank)

nhs_eng_region_ccg = read_csv('location_data/nhs_regions_to_ccg.csv')

#Townsend lookup
postcode_lookup_tds = read_csv('location_data/NSPL_FEB_2020_UK.csv') %>% 
  left_join(townsend_scores_output_areas, by = c('oa11' = 'geo_code')) %>% 
  left_join(scotland_oa_populations, by = 'oa11') %>% 
  left_join(england_oa_populations, by = 'oa11') %>% 
  mutate(country = ifelse(startsWith(ccg, 'S0'), 'Scotland', NA),
         country = ifelse(startsWith(ccg, 'E'), 'England', country),
         country = ifelse(startsWith(ccg, 'W1'), 'Wales', country),
         country = ifelse(startsWith(ccg, 'ZC'), 'Northern Ireland', country)) %>% 
  mutate(total_pop = ifelse(country == 'Scotland', total_pop_scot, NA),
         total_pop = ifelse(country == 'England' | country == 'Wales' , total_pop_eng, total_pop)) %>% select(-total_pop_eng, -total_pop_scot) %>%
  filter(!is.na(total_pop))


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
         country = 'Wales') %>% 
  mutate(org_code = case_when(org_code == '7A2AJ' ~ 'WB2AJ',
                              TRUE ~ org_code)) -> wales_list

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
         search_name = paste0(place_name, ', ', postcode)) %>% filter(!is.na(place_name)) %>% 
  select(-country) -> nhs_ods_list

#add country
nhs_ods_list = nhs_ods_list %>%  left_join(
postcode_lookup %>% select(pcds, oa11) %>% 
  mutate(country = case_when(startsWith(oa11, 'S0') ~ 'Scotland',
                             startsWith(oa11, 'E') ~ 'England',
                             startsWith(oa11, 'W') ~ 'Wales')) %>% 
  select(-oa11), by = c('postcode' = 'pcds'))

#bind rows
look_up_all = rbind(wales_list, ni_list, hosp_2_list, scotland_list, nhs_ods_list %>% select(-search_name)) %>% 
  mutate(search_name = paste0(place_name, ', ', postcode),
         org_code = ifelse(org_code == 'RHM00', 'RHM00', org_code)) #%>% 


#  distinct(org_code, .keep_all = T)

#Add a row for RMH

#write_csv(look_up_all, 'look_up_all_uk_hospital_codes.csv')

ccp_looked_up = ccp_ids_labelled %>% 
  filter(subjid != 'RHM01-0001') %>% 
  select(redcap_data_access_group, dag_id) %>% 
  #distinct(dag_id, .keep_all = T) %>% 
  mutate(dag_id = toupper(str_replace_all(dag_id, 'O', '0'))) %>% 
  left_join(look_up_all, by = c('dag_id' = 'org_code')) %>% 
  mutate(dag_id = gsub("\\_.*","",dag_id)) %>% 
  mutate(dag_id = gsub("\\-.*","",dag_id)) %>% 
  mutate(dag_id = gsub("\\ .*","",dag_id)) %>% 
  mutate(dag_id = trimws(dag_id))

look_up_unmatch_wales = look_up_all %>% mutate(org_code = ifelse(country == 'Wales', paste0('WB', substring(org_code, 3)), org_code),
                                               org_code = ifelse(org_code == 'WB2AL', 'WB3AL', org_code),
                                               org_code = ifelse(org_code == 'WB2BL', 'WF1BL', org_code),
                                               org_code = ifelse(org_code == 'RWEAA', 'TWEAA', org_code))# %>% rbind(nhs_ods_list)

#find those without a match then match them for misspelt names
ccp_looked_up %>% 
  filter(is.na(place_name)) %>% 
  select(redcap_data_access_group, dag_id) %>% 
  #distinct(dag_id, .keep_all = T) %>% 
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
  filter(!is.na(redcap_data_access_group)) %>% 
  distinct(dag_id, .keep_all = T) -> ccp_still_missings

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
  filter(dag_id %ni% hosp_2_list_in$OrganisationCode) %>% 
  distinct(dag_id, .keep_all = T) %>% 
  drop_na(postcode) -> ccp_combined_1_to_search

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
#rename(imd_average_postcodes = imd_average)

#
postcode_country = postcode_lookup %>% 
  mutate(ccg = ifelse(country != 'England', hlthau, ccg)) %>% 
  select(pcds, country, ccg) 

combined_all = ccp_combined_2 %>% 
  left_join(postcode_country, by = c('postcode' = 'pcds')) %>% 
  mutate(place_name = redcap_data_access_group) %>% 
  select(colnames(combined_all)) %>% rbind(combined_all)

postcode_ccg = postcode_lookup %>% 
  mutate(ccg = ifelse(country != 'England', hlthau, ccg)) %>% 
  select(pcds, ccg)

combined_all = combined_all %>% 
  mutate(dag_id = trimws(dag_id)) %>% 
  mutate(dag_id = gsub("\\_.*","",dag_id)) %>% 
  mutate(dag_id = gsub("\\-.*","",dag_id)) %>% 
  mutate(dag_id = gsub("\\ .*","",dag_id)) %>% 
  mutate(dag_id = ifelse(str_count(dag_id) > 6, str_sub(dag_id, end = -4), dag_id)) %>% 
  left_join(postcode_ccg, by = c('postcode' = 'pcds'))

#Add manual edits

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
         place_name = ifelse(dag_id == 'RVV00' | dag_id == 'RVV09', 'Queen Elizabeth The Queen Mother Hospital', place_name),
         postcode = ifelse(dag_id == 'RVV00' | dag_id == 'RVV09', 'CT9 4AN', postcode),
         country = ifelse(dag_id == 'RVV00' | dag_id == 'RVV09', 'England', country),
         lon = ifelse(dag_id == 'RVV00' | dag_id == 'RVV09', 1.3893986940383911, lon),
         lat = ifelse(dag_id == 'RVV00' | dag_id == 'RVV09', 51.3780517578125, lat),
         ccg = ifelse(dag_id == 'RVV00' | dag_id == 'RVV09', 'E38000184', ccg),
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
         ccg = ifelse(dag_id == 'RV001', 'E38000002', ccg),
         place_name = ifelse(dag_id == 'S341H' | dag_id == 'S134H' | dag_id == 'S214H', 'Royal Infirmary Of Edinburgh At Little France', place_name),
         postcode = ifelse(dag_id == 'S341H' | dag_id == 'S134H' | dag_id == 'S214H', 'EH16 4SA', postcode),
         country = ifelse(dag_id == 'S341H' | dag_id == 'S134H' | dag_id == 'S214H', 'Scotland', country),
         lon = ifelse(dag_id == 'S341H' | dag_id == 'S134H' | dag_id == 'S214H', -3.1347719, lon),
         lat = ifelse(dag_id == 'S341H' | dag_id == 'S134H' | dag_id == 'S214H', 55.9218084, lat),
         ccg = ifelse(dag_id == 'S341H' | dag_id == 'S134H' | dag_id == 'S214H', 'S08000024', ccg),
         place_name = ifelse(dag_id == 'RYGHQ', 'Caludon Centre', place_name),
         postcode = ifelse(dag_id == 'RYGHQ', 'CV2 2TE', postcode),
         country = ifelse(dag_id == 'RYGHQ', 'England', country),
         lon = ifelse(dag_id == 'RYGHQ', -1.4452332, lon),
         lat = ifelse(dag_id == 'RYGHQ', 52.4225547, lat),
         ccg = ifelse(dag_id == 'RYGHQ', 'E38000038', ccg),
         place_name = ifelse(dag_id == 'NR501', 'Mount Gould Hospital', place_name),
         postcode = ifelse(dag_id == 'NR501', 'PL4 7QD', postcode),
         country = ifelse(dag_id == 'NR501', 'England', country),
         lon = ifelse(dag_id == 'NR501', -4.111707, lon),
         lat = ifelse(dag_id == 'NR501', 50.3789894, lat),
         ccg = ifelse(dag_id == 'NR501', 'E38000230', ccg),
         place_name = ifelse(dag_id == 'RDR05', 'Brighton General Hospital', place_name),
         postcode = ifelse(dag_id == 'RDR05', 'BN2 3EW', postcode),
         country = ifelse(dag_id == 'RDR05', 'England', country),
         lon = ifelse(dag_id == 'RDR05', 0.1145, lon),
         lat = ifelse(dag_id == 'RDR05', 50.8310, lat),
         ccg = ifelse(dag_id == 'RDR05', 'E38000021', ccg),
         place_name = ifelse(dag_id == 'RZWAS', 'Royal Shrewsbury Hospital', place_name),
         postcode = ifelse(dag_id == 'RZWAS', 'SY3 8XQ', postcode),
         country = ifelse(dag_id == 'RZWAS', 'England', country),
         lon = ifelse(dag_id == 'RZWAS', -2.79373741149902, lon),
         lat = ifelse(dag_id == 'RZWAS', 52.7093620300293, lat),
         ccg = ifelse(dag_id == 'RZWAS', 'E38000147', ccg),
         place_name = ifelse(dag_id == 'RMP01', 'Tameside General Hospital', place_name),
         postcode = ifelse(dag_id == 'RMP01', 'OL6 9RW', postcode),
         country = ifelse(dag_id == 'RMP01', 'England', country),
         lon = ifelse(dag_id == 'RMP01', -2.0715971, lon),
         lat = ifelse(dag_id == 'RMP01', 53.4930995, lat),
         ccg = ifelse(dag_id == 'RMP01', 'E38000182', ccg),
         place_name = ifelse(dag_id == 'R8QL0', 'Broomfield Hospital', place_name),
         postcode = ifelse(dag_id == 'R8QL0', 'CM1 7ET', postcode),
         country = ifelse(dag_id == 'R8QL0', 'England', country),
         lon = ifelse(dag_id == 'R8QL0', 0.4667492, lon),
         lat = ifelse(dag_id == 'R8QL0', 51.77446, lat),
         ccg = ifelse(dag_id == 'R8QL0', 'E38000106', ccg),
         place_name = ifelse(dag_id == 'RAT01', 'Southend Hospital', place_name),
         postcode = ifelse(dag_id == 'RAT01', 'SS0 0RY', postcode),
         country = ifelse(dag_id == 'RAT01', 'England', country),
         lon = ifelse(dag_id == 'RAT01', 0.6877328, lon),
         lat = ifelse(dag_id == 'RAT01', 51.55386, lat),
         ccg = ifelse(dag_id == 'RAT01', 'E38000168', ccg),
         place_name = ifelse(dag_id == 'TAJ01', 'Southend Hospital', place_name),
         postcode = ifelse(dag_id == 'TAJ01', 'SS0 0RY', postcode),
         country = ifelse(dag_id == 'TAJ01', 'England', country),
         lon = ifelse(dag_id == 'TAJ01', 0.6877328, lon),
         lat = ifelse(dag_id == 'TAJ01', 51.55386, lat),
         ccg = ifelse(dag_id == 'TAJ01', 'E38000168', ccg),
         place_name = ifelse(dag_id == 'aRVR05', 'St Helier Hospital', place_name),
         postcode = ifelse(dag_id == 'aRVR05', 'SM5 1AA', postcode),
         country = ifelse(dag_id == 'aRVR05', 'England', country),
         lon = ifelse(dag_id == 'aRVR05', -0.184798, lon),
         lat = ifelse(dag_id == 'aRVR05', 51.38041, lat),
         ccg = ifelse(dag_id == 'aRVR05', 'E38000179', ccg),
         place_name = ifelse(dag_id == 'R0A01' | dag_id == 'R0S02', 'Manchester Royal Infirmary', place_name),
         postcode = ifelse(dag_id == 'R0A01' | dag_id == 'R0S02', 'M13 9WL', postcode),
         country = ifelse(dag_id == 'R0A01' | dag_id == 'R0S02', 'England', country),
         lon = ifelse(dag_id == 'R0A01' | dag_id == 'R0S02',-2.226157 , lon),
         lat = ifelse(dag_id == 'R0A01' | dag_id == 'R0S02', 53.46287, lat),
         ccg = ifelse(dag_id == 'R0A01' | dag_id == 'R0S02', 'E38000217', ccg),
         place_name = ifelse(dag_id == 'REJ01', 'Royal Stoke University Hospital', place_name),
         postcode = ifelse(dag_id == 'REJ01', 'ST4 6QG', postcode),
         country = ifelse(dag_id == 'REJ01', 'England', country),
         lon = ifelse(dag_id == 'REJ01', -2.21294, lon),
         lat = ifelse(dag_id == 'REJ01', 53.00381, lat),
         ccg = ifelse(dag_id == 'REJ01', 'E38000175', ccg),
         place_name = ifelse(dag_id == 'RRK02' | dag_id == 'RRL02' | dag_id == 'RR002', 'Queen Elizabeth University Hospital Birmingham', place_name),
         postcode = ifelse(dag_id == 'RRK02' | dag_id == 'RRL02' | dag_id == 'RR002', 'B15 2GW', postcode),
         country = ifelse(dag_id == 'RRK02' | dag_id == 'RRL02' | dag_id == 'RR002', 'England', country),
         lon = ifelse(dag_id == 'RRK02' | dag_id == 'RRL02' | dag_id == 'RR002', -1.9429313, lon),
         lat = ifelse(dag_id == 'RRK02' | dag_id == 'RRL02' | dag_id == 'RR002', 52.4512054, lat),
         ccg = ifelse(dag_id == 'RRK02' | dag_id == 'RRL02' | dag_id == 'RR002', 'E38000220', ccg),
         place_name = ifelse(dag_id == 'VLXF4', 'Medway Community Healthcare', place_name),
         postcode = ifelse(dag_id == 'VLXF4', 'ME1 3QY', postcode),
         country = ifelse(dag_id == 'VLXF4', 'England', country),
         lon = ifelse(dag_id == 'VLXF4', 0.5163534, lon),
         lat = ifelse(dag_id == 'VLXF4', 51.3770708, lat),
         ccg = ifelse(dag_id == 'VLXF4', 'E38000237', ccg),
         place_name = ifelse(dag_id == 'RRRR1', 'Belfast', place_name),
         postcode = ifelse(dag_id == 'RRRR1', 'BT9 7AB', postcode),
         country = ifelse(dag_id == 'RRRR1', 'Northern Ireland', country),
         lon = ifelse(dag_id == 'RRRR1', -5.9441331, lon),
         lat = ifelse(dag_id == 'RRRR1', 54.5872059, lat),
         ccg = ifelse(dag_id == 'RRRR1', '', ccg),
         place_name = ifelse(dag_id == 'RHX01', 'Royal Sussex County Hospital', place_name),
         postcode = ifelse(dag_id == 'RHX01', 'BN2 5BE', postcode),
         country = ifelse(dag_id == 'RHX01', 'England', country),
         lon = ifelse(dag_id == 'RHX01', -0.1182958, lon),
         lat = ifelse(dag_id == 'RHX01', 50.81935, lat),
         ccg = ifelse(dag_id == 'RHX01', 'E38000021', ccg),
         place_name = ifelse(grepl('N101|N100H',dag_id, ignore.case = T), 'Aberdeen Royal Infirmary', place_name),
         postcode = ifelse(grepl('N101|N100H',dag_id, ignore.case = T), 'AB25 2ZN', postcode),
         country = ifelse(grepl('N101|N100H',dag_id, ignore.case = T), 'Scotland', country),
         lon = ifelse(grepl('N101|N100H',dag_id, ignore.case = T),-2.137269 , lon),
         lat = ifelse(grepl('N101|N100H',dag_id, ignore.case = T), 57.15470, lat),
         ccg = ifelse(grepl('N101|N100H',dag_id, ignore.case = T), 'S08000020', ccg),
         place_name = ifelse(grepl('RTX02',dag_id, ignore.case = T), 'Royal Lancaster Infirmary', place_name),
         postcode = ifelse(grepl('RTX02',dag_id, ignore.case = T), 'LA1 4RP', postcode),
         country = ifelse(grepl('RTX02',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RTX02',dag_id, ignore.case = T), -2.800020, lon),
         lat = ifelse(grepl('RTX02',dag_id, ignore.case = T), 54.04307, lat),
         ccg = ifelse(grepl('RTX02',dag_id, ignore.case = T), 'E38000228', ccg),
         place_name = ifelse(grepl('ARVR', dag_id, ignore.case = T), 'St Helier Hospital', place_name),
         postcode = ifelse(grepl('ARVR',dag_id, ignore.case = T), 'SM5 1AA', postcode),
         country = ifelse(grepl('ARVR',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('ARVR',dag_id, ignore.case = T), -0.1845814, lon),
         lat = ifelse(grepl('ARVR',dag_id, ignore.case = T), 51.38063, lat),
         ccg = ifelse(grepl('ARVR',dag_id, ignore.case = T), 'E38000179', ccg),
         place_name = ifelse(grepl('R0A07',dag_id, ignore.case = T), 'Wythenshawe Hospital', place_name),
         postcode = ifelse(grepl('R0A07',dag_id, ignore.case = T), 'M23 9LT', postcode),
         country = ifelse(grepl('R0A07',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('R0A07',dag_id, ignore.case = T), -2.292308, lon),
         lat = ifelse(grepl('R0A07',dag_id, ignore.case = T), 53.38822, lat),
         ccg = ifelse(grepl('R0A07',dag_id, ignore.case = T), 'E38000217', ccg),
         place_name = ifelse(grepl('RXQ02|RZQ02',dag_id, ignore.case = T), 'Stoke Mandeville Hospital', place_name),
         postcode = ifelse(grepl('RXQ02|RZQ02',dag_id, ignore.case = T), 'HP21 8AL', postcode),
         country = ifelse(grepl('RXQ02|RZQ02',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RXQ02|RZQ02',dag_id, ignore.case = T), -0.8008387, lon),
         lat = ifelse(grepl('RXQ02|RZQ02',dag_id, ignore.case = T), 51.79839, lat),
         ccg = ifelse(grepl('RXQ02|RZQ02',dag_id, ignore.case = T), 'E38000223', ccg),
         place_name = ifelse(grepl('RVV09',dag_id, ignore.case = T), 'Queen Elizabeth The Queen Mother Hospital', place_name),
         postcode = ifelse(grepl('RVV09',dag_id, ignore.case = T), 'CT9 4AN', postcode),
         country = ifelse(grepl('RVV09',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RVV09',dag_id, ignore.case = T), 1.3893987, lon),
         lat = ifelse(grepl('RVV09',dag_id, ignore.case = T), 51.37805, lat),
         ccg = ifelse(grepl('RVV09',dag_id, ignore.case = T), 'E38000184', ccg),
         place_name = ifelse(grepl('RDEE', dag_id, ignore.case = T), 'Colchester General Hospital', place_name),
         postcode = ifelse(grepl('RDEE',dag_id, ignore.case = T), 'CO4 5JL', postcode),
         country = ifelse(grepl('RDEE',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RDEE',dag_id, ignore.case = T), 0.8997715, lon),
         lat = ifelse(grepl('RDEE',dag_id, ignore.case = T), 51.91029, lat),
         ccg = ifelse(grepl('RDEE',dag_id, ignore.case = T), 'E38000117', ccg),
         place_name = ifelse(grepl('RX1CC',dag_id, ignore.case = T), 'Nottingham University Hospitals Nhs Trust - City Campus', place_name),
         postcode = ifelse(grepl('RX1CC',dag_id, ignore.case = T), 'NG5 1PB', postcode),
         country = ifelse(grepl('RX1CC',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RX1CC',dag_id, ignore.case = T), -1.16008, lon),
         lat = ifelse(grepl('RX1CC',dag_id, ignore.case = T), 52.99022, lat),
         ccg = ifelse(grepl('RX1CC',dag_id, ignore.case = T), 'E38000132', ccg),
         place_name = ifelse(grepl('RMC',dag_id, ignore.case = T), 'Royal Bolton Hospital', place_name),
         postcode = ifelse(grepl('RMC',dag_id, ignore.case = T), 'BL4 0JR', postcode),
         country = ifelse(grepl('RMC',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RMC',dag_id, ignore.case = T), -2.433355, lon),
         lat = ifelse(grepl('RMC',dag_id, ignore.case = T), 53.55443, lat),
         ccg = ifelse(grepl('RMC',dag_id, ignore.case = T), 'E38000016', ccg),
         place_name = ifelse(grepl('RJN71',dag_id, ignore.case = T), 'Macclesfield District General Hospital', place_name),
         postcode = ifelse(grepl('RJN71',dag_id, ignore.case = T), 'SK10 3BL', postcode),
         country = ifelse(grepl('RJN71',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RJN71',dag_id, ignore.case = T), -2.142082, lon),
         lat = ifelse(grepl('RJN71',dag_id, ignore.case = T), 53.26183, lat),
         ccg = ifelse(grepl('RJN71',dag_id, ignore.case = T), 'E38000056', ccg),
         place_name = ifelse(grepl('RXN',dag_id, ignore.case = T), 'Royal Preston Hospital', place_name),
         postcode = ifelse(grepl('RXN',dag_id, ignore.case = T), 'PR2 9HT', postcode),
         country = ifelse(grepl('RXN',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RXN',dag_id, ignore.case = T), -2.704496, lon),
         lat = ifelse(grepl('RXN',dag_id, ignore.case = T), 53.79086, lat),
         ccg = ifelse(grepl('RXN',dag_id, ignore.case = T), 'E38000227', ccg),
         place_name = ifelse(grepl('RXPCP',dag_id, ignore.case = T), 'University Hospital Of North Durham', place_name),
         postcode = ifelse(grepl('RXPCP',dag_id, ignore.case = T), 'DH1 5TW', postcode),
         country = ifelse(grepl('RXPCP',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RXPCP',dag_id, ignore.case = T),-1.593417 , lon),
         lat = ifelse(grepl('RXPCP',dag_id, ignore.case = T), 54.78880, lat),
         ccg = ifelse(grepl('RXPCP',dag_id, ignore.case = T), 'E38000116', ccg),
         place_name = ifelse(grepl('RXPXP',dag_id, ignore.case = T), 'University Hospital Of North Durham', place_name),
         postcode = ifelse(grepl('RXPXP',dag_id, ignore.case = T), 'DH1 5TW', postcode),
         country = ifelse(grepl('RXPXP',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RXPXP',dag_id, ignore.case = T),-1.593417 , lon),
         lat = ifelse(grepl('RXPXP',dag_id, ignore.case = T), 54.78880, lat),
         ccg = ifelse(grepl('RXPXP',dag_id, ignore.case = T), 'E38000116', ccg),
         place_name = ifelse(grepl('RXPD',dag_id, ignore.case = T), 'Darlington Memorial Hospital', place_name),
         postcode = ifelse(grepl('RXPD',dag_id, ignore.case = T), 'DL3 6HX', postcode),
         country = ifelse(grepl('RXPD',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RXPD',dag_id, ignore.case = T), -1.563310, lon),
         lat = ifelse(grepl('RXPD',dag_id, ignore.case = T), 54.53039, lat),
         ccg = ifelse(grepl('RXPD',dag_id, ignore.case = T), 'E38000042', ccg),
         place_name = ifelse(grepl('RX1R',dag_id, ignore.case = T), "Nottingham University Hospitals Nhs Trust - Queen's Medical Centre Campus", place_name),
         postcode = ifelse(grepl('RX1R',dag_id, ignore.case = T), 'NG7 2UH', postcode),
         country = ifelse(grepl('RX1R',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RX1R',dag_id, ignore.case = T), -1.185, lon),
         lat = ifelse(grepl('RX1R',dag_id, ignore.case = T), 52.94435, lat),
         ccg = ifelse(grepl('RX1R',dag_id, ignore.case = T), 'E38000132', ccg),
         place_name = ifelse(grepl('RWDLA',dag_id, ignore.case = T), 'Pilgrim Hospital', place_name),
         postcode = ifelse(grepl('RWDLA',dag_id, ignore.case = T), 'PE21 9QS', postcode),
         country = ifelse(grepl('RWDLA',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RWDLA',dag_id, ignore.case = T), -0.0097936, lon),
         lat = ifelse(grepl('RWDLA',dag_id, ignore.case = T), 52.99102, lat),
         ccg = ifelse(grepl('RWDLA',dag_id, ignore.case = T), 'E38000099', ccg),
         place_name = ifelse(grepl('RD13',dag_id, ignore.case = T), '', place_name),
         postcode = ifelse(grepl('RD13',dag_id, ignore.case = T), 'BA1 3NG', postcode),
         country = ifelse(grepl('RD13',dag_id, ignore.case = T), 'England', country),
         lon = ifelse(grepl('RD13',dag_id, ignore.case = T), -2.391191, lon),
         lat = ifelse(grepl('RD13',dag_id, ignore.case = T), 51.39284, lat),
         ccg = ifelse(grepl('RD13',dag_id, ignore.case = T), 'E38000009', ccg))#,
         # place_name = ifelse(grepl('RD13',dag_id, ignore.case = T), '', place_name))#,
         # postcode = ifelse(grepl('',dag_id, ignore.case = T), '', postcode),
         # country = ifelse(grepl('',dag_id, ignore.case = T), '', country),
         # lon = ifelse(grepl('',dag_id, ignore.case = T), , lon),
         # lat = ifelse(grepl('',dag_id, ignore.case = T), , lat),
         # ccg = ifelse(grepl('',dag_id, ignore.case = T), '', ccg),
         # place_name = ifelse(grepl('',dag_id, ignore.case = T), '', place_name),
         # postcode = ifelse(grepl('',dag_id, ignore.case = T), '', postcode),
         # country = ifelse(grepl('',dag_id, ignore.case = T), '', country),
         # lon = ifelse(grepl('',dag_id, ignore.case = T), , lon),
         # lat = ifelse(grepl('',dag_id, ignore.case = T), , lat),
         # ccg = ifelse(grepl('',dag_id, ignore.case = T), '', ccg),
         # place_name = ifelse(grepl('',dag_id, ignore.case = T), '', place_name),
         # postcode = ifelse(grepl('',dag_id, ignore.case = T), '', postcode),
         # country = ifelse(grepl('',dag_id, ignore.case = T), '', country),
         # lon = ifelse(grepl('',dag_id, ignore.case = T), , lon),
         # lat = ifelse(grepl('',dag_id, ignore.case = T), , lat),
         # ccg = ifelse(grepl('',dag_id, ignore.case = T), '', ccg),
         # place_name = ifelse(grepl('',dag_id, ignore.case = T), '', place_name),
         # postcode = ifelse(grepl('',dag_id, ignore.case = T), '', postcode),
         # country = ifelse(grepl('',dag_id, ignore.case = T), '', country),
         # lon = ifelse(grepl('',dag_id, ignore.case = T), , lon),
         # lat = ifelse(grepl('',dag_id, ignore.case = T), , lat),
         # ccg = ifelse(grepl('',dag_id, ignore.case = T), '', ccg),
         # place_name = ifelse(grepl('',dag_id, ignore.case = T), '', place_name),
         # postcode = ifelse(grepl('',dag_id, ignore.case = T), '', postcode),
         # country = ifelse(grepl('',dag_id, ignore.case = T), '', country),
         # lon = ifelse(grepl('',dag_id, ignore.case = T), , lon),
         # lat = ifelse(grepl('',dag_id, ignore.case = T), , lat),
         # ccg = ifelse(grepl('',dag_id, ignore.case = T), '', ccg),
         # place_name = ifelse(grepl('',dag_id, ignore.case = T), '', place_name),
         # postcode = ifelse(grepl('',dag_id, ignore.case = T), '', postcode),
         # country = ifelse(grepl('',dag_id, ignore.case = T), '', country),
         # lon = ifelse(grepl('',dag_id, ignore.case = T), , lon),
         # lat = ifelse(grepl('',dag_id, ignore.case = T), , lat),
         # ccg = ifelse(grepl('',dag_id, ignore.case = T), '', ccg))
# place_name = ifelse(dag_id == '', '', place_name),
# postcode = ifelse(dag_id == '', '', postcode),
# country = ifelse(dag_id == '', '', country),
# lon = ifelse(dag_id == '', -0.1182958, lon),
# lat = ifelse(dag_id == '', , lat),
# ccg = ifelse(dag_id == '', '', ccg),

# No location

no_location = combined_all %>% filter(is.na(postcode) | grepl('_', place_name)) %>% 
  mutate(incorrect_dag_id = 'Incorrect')

correct_ids = combined_all %>% 
  filter(!dag_id %in% no_location$dag_id) %>% 
  rename_all(paste0, "_corrected") 

no_location_add = no_location %>% 
  left_join(correct_ids, by = c('postcode' = 'postcode_corrected')) %>% 
         mutate(place_name = ifelse(is.na(place_name_corrected), place_name, place_name_corrected),
                lon = ifelse(is.na(lon_corrected), lon, lon_corrected),
                lat = ifelse(is.na(lat_corrected), lat, lat_corrected),
                country = ifelse(is.na(country_corrected), country, country_corrected),
                ccg = ifelse(is.na(ccg_corrected), ccg, ccg_corrected)) %>% 
           filter(!is.na(place_name_corrected)) %>% 
  filter(place_name != 'Surgery Department') %>% 
  select(-contains('_corrected'), -incorrect_dag_id)

#remove postcoded looked up from no_location
no_location = no_location %>% 
  filter(!dag_id %in% no_location_add$dag_id)

combined_all = combined_all %>% 
  filter(!dag_id %in% no_location_add$dag_id) %>% 
  rbind(., no_location_add)

#finally with the misspelt ones, work out what they might be based on DAG names

#so most common by dag

commonest_dags = ccp_data %>%
  mutate(dag_id = gsub("\\-.*","", subjid)) %>% 
  mutate(dag_id = trimws(dag_id)) %>% 
  mutate(dag_id = gsub("\\_.*","",dag_id)) %>% 
  mutate(dag_id = gsub("\\-.*","",dag_id)) %>% 
  mutate(dag_id = gsub("\\ .*","",dag_id)) %>% 
  filter(grepl('adm', redcap_event_name, ignore.case = T)) %>% 
  filter(is.na(redcap_repeat_instrument)) %>% 
  group_by(redcap_data_access_group) %>% 
  count(dag_id) %>% 
  group_by(redcap_data_access_group) %>% 
  arrange(desc(n)) %>% 
  slice(1) %>% 
  ungroup()

best_names = combined_all %>% 
  filter(!dag_id %in% no_location$dag_id) %>% 
  filter(dag_id %in% commonest_dags$dag_id) %>% 
  distinct(redcap_data_access_group, .keep_all = T) %>% 
  rename_all(paste0, "_corrected") 

no_location_2 = no_location %>% 
  left_join(best_names, by = c('redcap_data_access_group' = 'redcap_data_access_group_corrected')) %>% 
  mutate(place_name = ifelse(is.na(place_name_corrected), place_name, place_name_corrected),
         lon = ifelse(is.na(lon_corrected), lon, lon_corrected),
         lat = ifelse(is.na(lat_corrected), lat, lat_corrected),
         country = ifelse(is.na(country_corrected), country, country_corrected),
         postcode = ifelse(is.na(postcode_corrected), postcode, postcode_corrected),
         ccg = ifelse(is.na(ccg_corrected), ccg, ccg_corrected)) %>% 
  filter(!is.na(place_name_corrected)) %>% 
  filter(place_name != 'Surgery Department') %>% 
  select(-contains('_corrected'), -incorrect_dag_id)

#change this if not to guess
guess_dags = T

if(guess_dags == T){
  combined_all = combined_all %>% 
    filter(!dag_id %in% no_location_2$dag_id) %>% 
    rbind(., no_location_2) %>% 
    distinct(dag_id, redcap_data_access_group, place_name, .keep_all = T)
  
}

# no_location = no_location %>% filter(is.na(postcode)) %>% distinct(dag_id, .keep_all = T)
# write_rds(no_location, 'no_location.rds')

#Now lets add a city to postcode
postcode_to_city = read_csv('location_data/postcode_city_district.csv') %>% 
  clean_names() %>% 
  select(postcode, region) %>% 
  rename(postcode_start = postcode,
         city = region) #%>% distinct(postcode_start, .keep_all = T)

combined_all = combined_all %>% 
  mutate(postcode_start = gsub("[[:space:]].*", '', postcode)) %>% 
  left_join(postcode_to_city, by = 'postcode_start')

#Now add in type of site
combined_all = combined_all %>% 
  left_join(site, by = c('dag_id'= 'NHS_ODS'))
  
lookup_missing_sites_type = combined_all %>% 
  filter(!is.na(site.type)) %>% select(place_name, site_type2 = site.type) %>% distinct(place_name, .keep_all = T)

combined_all = combined_all %>% 
  left_join(lookup_missing_sites_type, by = 'place_name') %>% 
  mutate(site_type = ifelse(is.na(site.type), site_type2, site.type)) %>% 
  select(-site.type, -site_type2) %>% 
  mutate(site_type = ifelse(dag_id == '14142','Acute', site_type),
         site_type = ifelse(dag_id == '14152','Acute', site_type),
         site_type = ifelse(dag_id == 'REMR0','Acute', site_type),
         site_type = ifelse(dag_id == 'RM325','Acute', site_type),
         site_type = ifelse(dag_id == 'RN770','Acute', site_type),
         site_type = ifelse(dag_id == 'RRRR1','Acute', site_type),
         site_type = ifelse(dag_id == 'rw602','Acute', site_type),
         site_type = ifelse(dag_id == 'RX219','Community', site_type),
         site_type = ifelse(dag_id == '7A1A8','Acute', site_type),
         site_type = ifelse(dag_id == '7A1A9','Acute', site_type),
         site_type = ifelse(dag_id == '7A1AY','Acute', site_type),
         site_type = ifelse(dag_id == '7A1DD','Acute', site_type),
         site_type = ifelse(dag_id == 'A103H','Community', site_type),
         site_type = ifelse(dag_id == 'A215H','Community', site_type),
         site_type = ifelse(dag_id == 'ZT003','Acute', site_type),
         site_type = ifelse(dag_id == 'RQY01','Community', site_type))

combined_all %>% filter(is.na(site_type))

#Finally, add back in the townsend average scores to those which needed new postcodes
rank_by_lsoa = postcode_lookup_tds %>% 
  mutate(ccg = ifelse(country != 'England', hlthau, ccg)) %>% 
  distinct(oa11, .keep_all = T) %>%
  group_by(country) %>% 
  mutate(imd_rank = rank(desc(imd), ties.method= "max")) %>% 
  mutate(imd_decile = ntile(imd_rank, 10)) %>% 
  mutate(imd_quintile = ntile(imd_rank, 5)) %>% 
  select(oa11, imd_rank, imd_decile, imd_quintile)

rank_by_ccg = postcode_lookup_tds %>% 
  mutate(ccg = ifelse(country != 'England', hlthau, ccg)) %>% 
  distinct(oa11, .keep_all = T) %>%
  group_by(country) %>% 
  mutate(imd_rank = rank(desc(imd), ties.method= "max")) %>% 
  mutate(imd_decile = ntile(imd_rank, 10)) %>% 
  mutate(imd_quintile = ntile(imd_rank, 5)) %>% 
  group_by(ccg) %>% 
  summarise(median_ccg_imd_rank = median(imd_rank, na.rm = T),
            median_ccg_imd_quintile = median(imd_quintile, na.rm = T),
            median_ccg_imd_decile = median(imd_decile, na.rm = T),
            mean_ccg_imd_rank = mean(imd_rank, na.rm = T),
            mean_ccg_imd_quintile = mean(imd_quintile, na.rm = T),
            mean_ccg_imd_decile = mean(imd_decile, na.rm = T)) %>% 
  select(ccg, median_ccg_imd_rank, median_ccg_imd_quintile, median_ccg_imd_decile,
         mean_ccg_imd_rank, mean_ccg_imd_quintile, mean_ccg_imd_decile)


#rank_by_area %>% filter(country =='England') %>% select(pcds, imd_rank, imd_quintile) %>% data.frame()

postcode_lookup_tds = postcode_lookup_tds %>% 
  mutate(ccg = ifelse(country != 'England', hlthau, ccg)) %>% 
  group_by(oa11) %>% 
  ungroup() %>% 
  distinct(oa11, .keep_all = T) %>% 
  group_by(country) %>% 
  mutate(total_pop_sum = sum(total_pop),
         weight_as_prop_t_pop = total_pop/ total_pop_sum) %>% 
  ungroup() %>% 
  group_by(ccg) %>% 
  mutate(imd_average_postcodes_new = mean(imd, na.rm = T),
         tds_mean = mean(tds, na.rm = T),
         w_me_tds = weighted.mean(tds, weight_as_prop_t_pop),
         w_me_imd = weighted.mean(imd, weight_as_prop_t_pop),
         w_har_tds = weighted.harmean(tds, weight_as_prop_t_pop),
         w_har_imd = weighted.harmean(imd, weight_as_prop_t_pop),
         w_med_imd = weighted.median(imd, weight_as_prop_t_pop),
         w_med_tds = weighted.median(tds, weight_as_prop_t_pop)) %>% 
  ungroup() %>% 
  #work out proportions in lowest deciles.
  group_by(country) %>% 
  mutate(imd_decile = ntile(imd, 10)) %>% 
  ungroup() %>% 
  mutate(n_within_10_imd = ifelse(imd_decile == 1, total_pop, 0),
         n_within_20_imd = ifelse(imd_decile <= 2, total_pop, 0)) %>% 
  group_by(ccg) %>% 
  mutate(prop_within_10_imd_ccg = sum(n_within_10_imd)/sum(total_pop),
         prop_within_20_imd_ccg =  sum(n_within_20_imd)/sum(total_pop)) %>% 
  ungroup() %>% 
  mutate(tds_decile = ntile(tds, 10)) %>% 
  mutate(n_within_10_tds = ifelse(tds_decile == 10, total_pop, 0),
         n_within_20_tds = ifelse(tds_decile >= 9, total_pop, 0)) %>% 
  group_by(ccg) %>% 
  mutate(prop_within_10_tds_ccg = sum(n_within_10_tds)/sum(total_pop),
         prop_within_20_tds_ccg =  sum(n_within_20_tds)/sum(total_pop)) %>% 
  ungroup() %>% 
  select(ccg, imd_average_postcodes_new, tds_mean, w_me_imd, w_me_tds, w_har_tds, w_har_imd,
         w_med_imd, w_med_tds, prop_within_10_imd_ccg, prop_within_20_imd_ccg, prop_within_10_tds_ccg, prop_within_20_tds_ccg) %>%
  distinct(ccg, .keep_all = T) 

combined_all = combined_all %>% 
  left_join(postcode_lookup_tds, by = c('ccg' = 'ccg')) %>% 
  left_join(rank_by_ccg, by = c('ccg' = 'ccg'))

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

#Join ccp ids in 
rm(list=setdiff(ls(), c('ccp_ids', 'combined_all', 'nhs_eng_region_ccg')))

ccp_dag_unlabelled = ccp_ids %>% 
  distinct(dag_id, .keep_all = T) %>% 
  select(dag_id, redcap_data_access_group) %>% 
  rename(redcap_data_access_group_unlabelled = redcap_data_access_group)

#Now lets add region - England region/ Scotland/ Wales/ NI
#now join in NHS region
combined_all = combined_all %>% left_join(nhs_eng_region_ccg %>% select(CCG19CD, NHSER19NM), by = c('ccg' = 'CCG19CD'))

combined_all = combined_all %>% 
  rename(nhs_region = NHSER19NM) %>%
  mutate(nhs_region = ifelse(country == 'England', nhs_region, country)) %>% 
  mutate(nhs_region = ifelse(dag_id == 'VLXF4', 'South East', nhs_region))

#
combined_all = combined_all %>% 
  distinct(dag_id, .keep_all = T) %>% 
  left_join(ccp_dag_unlabelled, by = 'dag_id') %>% 
  select(redcap_data_access_group, redcap_data_access_group_unlabelled, everything())

combined_all_no_imd = combined_all %>% 
  select(-contains('imd'),
         -contains('tds'))



#write a csv
save_date = Sys.Date() %>% format('%d-%B-%Y')

write_csv(combined_all_no_imd, paste0('data_out_ccp_lookups/ccp_dag_id_lookup_wo_imd_', save_date, '.csv'))
write_csv(combined_all_no_imd, paste0('data_out_ccp_lookups/ccp_dag_id_lookup_wo_imd.csv'))

write_csv(combined_all, paste0('data_out_ccp_lookups/ccp_dag_id_lookup_', save_date, '.csv'))
write_csv(combined_all, paste0('data_out_ccp_lookups/ccp_dag_id_lookup.csv'))
