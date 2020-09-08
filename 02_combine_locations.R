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
api_key = 'AIzaSyADK-Xycf5uqnyvYDPP58wSQdnLJZCzrr8' #Google maps API key

register_google(api_key)

#load in locations

#First the file from ewen
nhs_ods_list = read_excel('location_data/NHS_ODS.xlsx')
hosp_2_list_in = read_csv('location_data/hospital_3.csv')
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
         total_pop = ifelse(country == 'England', total_pop_eng, total_pop)) %>% select(-total_pop_eng, -total_pop_scot) %>% 
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
         org_code = ifelse(org_code == 'RHM00', 'RHM00', org_code)) #%>% 

#  distinct(org_code, .keep_all = T)

#Add a row for RMH

#write_csv(look_up_all, 'look_up_all_uk_hospital_codes.csv')

ccp_looked_up = ccp_ids_labelled %>% 
  filter(subjid != 'RHM01-0001') %>% 
  select(redcap_data_access_group, dag_id) %>% 
  #distinct(dag_id, .keep_all = T) %>% 
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
  distinct(dag_id, .keep_all = T) -> ccp_combined_1_to_search

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
         ccg = ifelse(dag_id == 'RHX01', 'E38000021', ccg))

no_location = combined_all %>% filter(is.na(postcode))
# no_location = combined_all2 %>% filter(is.na(postcode)) %>% distinct(dag_id, .keep_all = T)

#Now lets add a city to postcode
postcode_to_city = read_csv('location_data/postcode_city_district.csv') %>% 
  clean_names() %>% 
  select(postcode, region) %>% 
  rename(postcode_start = postcode,
         city = region) #%>% distinct(postcode_start, .keep_all = T)

combined_all = combined_all %>% 
  mutate(postcode_start = gsub("[[:space:]].*", '', postcode)) %>% 
  left_join(postcode_to_city, by = 'postcode_start')

#Finally, add back in the townsend average scores to those which needed new postcodes
postcode_lookup_tds = postcode_lookup_tds %>% 
  mutate(ccg = ifelse(country != 'England', hlthau, ccg)) %>% 
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
  left_join(postcode_lookup_tds, by = c('ccg' = 'ccg')) #%>% 

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
