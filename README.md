COVID19 Clinical Information Network (CO-CIN) / ISARIC-4C CCP Lookup
==========

* [ISARIC COVID-19 Clincial Research Resources](https://isaric.tghn.org/covid-19-clinical-research-resources/)
* [ISARIC4C - Coronavirus Clinical Characterisation Consortium](https://isaric4c.net/)

This script looks up the subject ID prefix and maps it to the contributing centre. 

## Caution
### Data security

These are patient-level data that contain disclosive information. Only use in a secure environment and do not hold data on a removable device including laptops. 

### Always check the data

It is the end-users responsibility to understand the processes contained in these scripts, the assumptions that are used, and to check the data created conforms to their expectations. 

### How to use
### `02_combine_locations.R`

**Description**: Run once to generate a CSV to lookup centres.

The output is found in **data_out_ccp_lookups**. A CSV per run is generated, but also an undated version is available to link to in order to provide an up to date file that can be 'plugged' into analyses.
