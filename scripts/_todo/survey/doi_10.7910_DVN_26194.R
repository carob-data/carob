# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey (2010) of rice farmers in Cambodia, Pakistan and Sri Lanka. The 290 files are 30 survey
# tables, most split into up to 11 files (no suffix and suffixes _1 to _10), one per survey area:
# Cambodia (none, _1 to _4), Philippines (_5), Pakistan (_6), Sri Lanka (_7 to _10).
# Mapped: the rice variety table (tbl04aricevariety_x*), one record per farmer x season x parcel x plot x
# variety with area (ha) and production (kg), read in a loop over the 10 Cambodia/Pakistan/Sri Lanka files that
# share the same layout. District and village come from the matching tbl00surveyinfo* file.
# Farmer ids are reused across files, so hhid includes the file suffix.
# Sri Lanka seasons Maha (main, north-east monsoon) and Yala (minor) are mapped to "wet" and "dry"; the original
# season names are in season_local_.

## ISSUES
# The _5 (Philippines) files are the 2012-2013 Laguna survey of GSR Phase 2 (doi:10.7910/DVN/26195, mapped
#   there); the _5 rice variety file also has a different layout (production in local units). Not used here.
# Not mapped: tbl06a/b/c "largest parcel" tables (planting and harvest month, grain and straw production, price
#   for the largest parcel per season), material inputs (fertilizer, pesticides; tbl06matinputs*), labour, power,
#   costs, consumption, income, credit, decisions, training, stress-tolerant variety and household tables.
#   The largest parcel and input tables could be added to give planting/harvest month and N/P/K.
# Year of the seasons is not given (survey year 2010, interview dates mostly mid-2010, with some clear entry
#   errors, e.g. 2001, 2020, 6474); planting_date and harvest_date are NA.
# Area is documented as ha, but Pakistan (_6) areas (median 4, max 50) and production look like acres; Pakistan
#   yields (median 1280 kg/ha) may be per acre. Check before use.
# "Wet 2" in the Cambodia files is not explained (perhaps the second wet season or a late wet crop); mapped
#   as "wet", original in season_local_.
# Crop establishment "Planting" (Sri Lanka _8, 2 records) is ambiguous and set to NA. "Sowing" (Puttalam) is
#   mapped as "direct seeding". WSR (wet seeded rice) and DSR as "direct seeding".
# Production is taken as fresh paddy (moisture not given). There are 0-production records (e.g. "No any
#   production (because flood)"), kept as 0.
# Variety names are as recorded (local names and codes), not harmonized.
# No coordinates; only district and village are given.
# write_files remaining: NA coordinates (and NA planting_date, N/P/K fertilizer, yield_moisture); unknown
#   (new) variables.

## NEW VARIABLES
# land_type_: land type of the plot as reported (lowland, medium, upland)
# season_local_: season as recorded in the survey (Wet, Wet 2, Dry, Kharif, Maha, Yala)



## NOT PROCESSED
# BaselineVarList_GSR.xls: not read. sheet Variable labels: Variable labels for exported data from Access. Observation records are missing from d until this file is standardized.
# Project details.xlsx: not read. sheet PROJECT: Title of the Study, ...2, Green Super Rice for the Resource Poor of Asia and Africa (GSR) - Phase 1. Observation records are missing from d until this file is standardized.
# tbl00surveyinfo_1.xlsx: not read. sheet tbl00surveyinfo: farmerid, country, institution, respondent, interviewdate, district, village, submergence, drought, salinity, distance, ethnic, .... Observation records are missing from d until this file is standardized.
# tbl00surveyinfo_10.xlsx: not read. sheet tbl00surveyinfo: farmerid, country, institution, respondent, interviewdate, district, village, submergence, drought, salinity_coastal, salinity_inland, distance, .... Observation records are missing from d until this file is standardized.
# tbl00surveyinfo_2.xlsx: not read. sheet tbl00surveyinfo: farmerid, country, institution, respondent, interviewdate, district, village, submergence, drought, salinity, distance, ethnic, .... Observation records are missing from d until this file is standardized.
# tbl00surveyinfo_3.xlsx: not read. sheet tbl00surveyinfo: farmerid, country, institution, respondent, interviewdate, district, village, submergence, drought, salinity, distance, ethnic, .... Observation records are missing from d until this file is standardized.
# tbl00surveyinfo_4.xlsx: not read. sheet tbl00surveyinfo: farmerid, country, institution, respondent, interviewdate, district, village, submergence, drought, salinity, distance, ethnic, .... Observation records are missing from d until this file is standardized.
# tbl00surveyinfo_5.xlsx: not read. sheet tbl00surveyinfo: farmerid, country, institution, respondent, interviewdate, district, village, submergence, drought, salinity, distance, ethnic, .... Observation records are missing from d until this file is standardized.
# tbl00surveyinfo_6.xlsx: not read. sheet tbl00surveyinfo: farmerid, country, institution, respondent, interviewdate, district, village, submergence, drought, salinity, distance, ethnic, .... Observation records are missing from d until this file is standardized.
# tbl00surveyinfo_7.xlsx: not read. sheet tbl00surveyinfo: farmerid, country, institution, respondent, interviewdate, district, village, submergence, drought, salinity_coastal, salinity_inland, distance, .... Observation records are missing from d until this file is standardized.
# tbl00surveyinfo_8.xlsx: not read. sheet tbl00surveyinfo: farmerid, country, institution, respondent, interviewdate, district, village, submergence, drought, salinity_coastal, salinity_inland, distance, .... Observation records are missing from d until this file is standardized.
# tbl00surveyinfo_9.xlsx: not read. sheet tbl00surveyinfo: farmerid, country, institution, respondent, interviewdate, district, village, submergence, drought, salinity_coastal, salinity_inland, distance, .... Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x.xlsx: not read. sheet tbl01hhinfo_x: farmerid, hhmember, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x_1.xlsx: not read. sheet tbl01hhinfo_x: farmerid, hhmember, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x_10.xlsx: not read. sheet tbl01hhinfo_x: farmerid, hhmember, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x_2.xlsx: not read. sheet tbl01hhinfo_x: farmerid, hhmember, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x_3.xlsx: not read. sheet tbl01hhinfo_x: farmerid, hhmember, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x_4.xlsx: not read. sheet tbl01hhinfo_x: farmerid, hhmember, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x_5.xlsx: not read. sheet tbl01hhinfo_x: farmerid, hhmember, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x_6.xlsx: not read. sheet tbl01hhinfo_x: farmerid, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x_7.xlsx: not read. sheet tbl01hhinfo_x: farmerid, hhmember, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x_8.xlsx: not read. sheet tbl01hhinfo_x: farmerid, hhmember, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl01hhinfo_x_9.xlsx: not read. sheet tbl01hhinfo_x: farmerid, hhmember, relation, age, sex, educyear, primoccup, secoccup, comments. Observation records are missing from d until this file is standardized.
# tbl02landholding_x.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, salinity_aman, salinity_boro, tenure, irrigsource, areaws, areaws2, .... Observation records are missing from d until this file is standardized.
# tbl02landholding_x_1.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, salinity_aman, salinity_boro, tenure, irrigsource, areaws, areaws2, .... Observation records are missing from d until this file is standardized.
# tbl02landholding_x_10.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, tenure, tenure_maha, tenure_yala, irrigsource, areaws, areads, .... Observation records are missing from d until this file is standardized.
# tbl02landholding_x_2.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, salinity_aman, salinity_boro, tenure, irrigsource, areaws, areaws2, .... Observation records are missing from d until this file is standardized.
# tbl02landholding_x_3.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, salinity_aman, salinity_boro, tenure, irrigsource, areaws, areaws2, .... Observation records are missing from d until this file is standardized.
# tbl02landholding_x_4.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, salinity_aman, salinity_boro, tenure, irrigsource, areaws, areaws2, .... Observation records are missing from d until this file is standardized.
# tbl02landholding_x_5.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, salinity_aman, salinity_boro, tenure, irrigsource, areaws, areaws2, .... Observation records are missing from d until this file is standardized.
# tbl02landholding_x_6.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, salinity_aman, salinity_boro, tenure, irrigsource, areaws, areaws2, .... Observation records are missing from d until this file is standardized.
# tbl02landholding_x_7.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, tenure, tenure_maha, tenure_yala, irrigsource, areaws, areads, .... Observation records are missing from d until this file is standardized.
# tbl02landholding_x_8.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, tenure, tenure_maha, tenure_yala, irrigsource, areaws, areads, .... Observation records are missing from d until this file is standardized.
# tbl02landholding_x_9.xlsx: not read. sheet tbl02landholding_x: farmerid, parcel, plot, landtype, area, soiltype, tenure, tenure_maha, tenure_yala, irrigsource, areaws, areads, .... Observation records are missing from d until this file is standardized.
# tbl03landuse_x.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, price, note, .... Observation records are missing from d until this file is standardized.
# tbl03landuse_x_1.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, price, note, .... Observation records are missing from d until this file is standardized.
# tbl03landuse_x_10.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, price, comments. Observation records are missing from d until this file is standardized.
# tbl03landuse_x_2.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, price, note, .... Observation records are missing from d until this file is standardized.
# tbl03landuse_x_3.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, price, note, .... Observation records are missing from d until this file is standardized.
# tbl03landuse_x_4.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, price, note, .... Observation records are missing from d until this file is standardized.
# tbl03landuse_x_5.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, prodloc, unit, .... Observation records are missing from d until this file is standardized.
# tbl03landuse_x_6.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, price, note, .... Observation records are missing from d until this file is standardized.
# tbl03landuse_x_7.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, price, comments. Observation records are missing from d until this file is standardized.
# tbl03landuse_x_8.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, price, comments. Observation records are missing from d until this file is standardized.
# tbl03landuse_x_9.xlsx: not read. sheet tbl03landuse_x: farmerid, parcel, plot, landtype, season, croptype, crop_id, crop, area, prod, price, comments. Observation records are missing from d until this file is standardized.
# tbl03landusemixedcrop_.xlsx: not read. sheet tbl03landusemixedcrop_: farmerid, crop, prod, price, crop_id, croptype, area, comments. Observation records are missing from d until this file is standardized.
# tbl03landusemixedcrop__1.xlsx: not read. sheet tbl03landusemixedcrop_: farmerid, crop, prod, price, crop_id, croptype, area, comments. Observation records are missing from d until this file is standardized.
# tbl03landusemixedcrop__2.xlsx: not read. sheet tbl03landusemixedcrop_: farmerid, crop, prod, price, crop_id, croptype, area, comments. Observation records are missing from d until this file is standardized.
# tbl03landusemixedcrop__3.xlsx: not read. sheet tbl03landusemixedcrop_: farmerid, crop, prod, price, crop_id, croptype, area, comments. Observation records are missing from d until this file is standardized.
# tbl03landusemixedcrop__4.xlsx: not read. sheet tbl03landusemixedcrop_: farmerid, crop, prod, price, crop_id, croptype, area, comments. Observation records are missing from d until this file is standardized.
# tbl03landusemixedcrop__5.xlsx: not read. sheet tbl03landusemixedcrop_: farmerid, crop, prod, price, crop_id, croptype, area, comments. Observation records are missing from d until this file is standardized.
# tbl03landusemixedcrop__6.xlsx: not read. sheet tbl03landusemixedcrop_: farmerid, crop, prod, price, crop_id, croptype, area, comments. Observation records are missing from d until this file is standardized.
# tbl03landusemixedcrop__7.xlsx: not read. sheet tbl03landusemixedcrop_: farmerid, crop, prod, price, crop_id, croptype, area, comments. Observation records are missing from d until this file is standardized.
# tbl03landusemixedcrop__8.xlsx: not read. sheet tbl03landusemixedcrop_: farmerid, crop, prod, price, crop_id, croptype, area, comments. Observation records are missing from d until this file is standardized.
# tbl03landusemixedcrop__9.xlsx: not read. sheet tbl03landusemixedcrop_: farmerid, crop, prod, price, crop_id, croptype, area, comments. Observation records are missing from d until this file is standardized.
# tbl04aricevariety_x_1.xlsx: not read. sheet tbl04aricevariety_x: farmerid, season, parcel, plot, subplot, landtype, ricevar, mvtv, cem, area, prod, comments. Observation records are missing from d until this file is standardized.
# tbl04aricevariety_x_10.xlsx: not read. sheet tbl04aricevariety_x: farmerid, season, parcel, plot, subplot, landtype, ricevar, mvtv, cem, area, prod, comments. Observation records are missing from d until this file is standardized.
# tbl04aricevariety_x_2.xlsx: not read. sheet tbl04aricevariety_x: farmerid, season, parcel, plot, subplot, landtype, ricevar, mvtv, cem, area, prod, comments. Observation records are missing from d until this file is standardized.
# tbl04aricevariety_x_3.xlsx: not read. sheet tbl04aricevariety_x: farmerid, season, parcel, plot, subplot, landtype, ricevar, mvtv, cem, area, prod, comments. Observation records are missing from d until this file is standardized.
# tbl04aricevariety_x_4.xlsx: not read. sheet tbl04aricevariety_x: farmerid, season, parcel, plot, subplot, landtype, ricevar, mvtv, cem, area, prod, comments. Observation records are missing from d until this file is standardized.
# tbl04aricevariety_x_5.xlsx: not read. sheet tbl04aricevariety_x: farmerid, season, parcel, plot, subplot, landtype, ricevar, mvtv, cem, area, prod, prodloc, .... Observation records are missing from d until this file is standardized.
# tbl04aricevariety_x_6.xlsx: not read. sheet tbl04aricevariety_x: farmerid, season, parcel, plot, subplot, landtype, ricevar, mvtv, cem, area, prod, comments. Observation records are missing from d until this file is standardized.
# tbl04aricevariety_x_7.xlsx: not read. sheet tbl04aricevariety_x: farmerid, season, parcel, plot, subplot, landtype, ricevar, mvtv, cem, area, prod, comments. Observation records are missing from d until this file is standardized.
# tbl04aricevariety_x_8.xlsx: not read. sheet tbl04aricevariety_x: farmerid, season, parcel, plot, subplot, landtype, ricevar, mvtv, cem, area, prod, comments. Observation records are missing from d until this file is standardized.
# tbl04aricevariety_x_9.xlsx: not read. sheet tbl04aricevariety_x: farmerid, season, parcel, plot, subplot, landtype, ricevar, mvtv, cem, area, prod, comments. Observation records are missing from d until this file is standardized.
# tbl04bdesiredtraits.xlsx: not read. sheet tbl04bdesiredtraits: farmerid, desired01, desired02, desired03, undesired01, undesired02, undesired03, newvariety, newvarietywhy. Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x_1.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x_10.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x_2.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x_3.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x_4.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x_5.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x_6.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x_7.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x_8.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl04breasonsgrowing_x_9.xlsx: not read. sheet tbl04breasonsgrowing_x: farmerid, variety, season, pos_yield, pos_lodge, pos_pest, pos_taste, pos_grain, pos_price, pos_oth, neg_yield, neg_lodge, .... Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x_1.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x_10.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x_2.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x_3.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x_4.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x_5.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, season, variety, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, .... Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x_6.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x_7.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x_8.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_x_9.xlsx: not read. sheet tbl05cropdisposal_x: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl05cropdisposal_xorig.xlsx: not read. sheet tbl05cropdisposal_xorig: farmerid, crop, totalprod, sold, salevalue, seeds, feeds, payment, food, futureuse, otheruses, comments. Observation records are missing from d until this file is standardized.
# tbl06alargestparcel_x.xlsx: not read. sheet tbl06alargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06alargestparcel_x_1.xlsx: not read. sheet tbl06alargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06alargestparcel_x_2.xlsx: not read. sheet tbl06alargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06alargestparcel_x_3.xlsx: not read. sheet tbl06alargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06alargestparcel_x_4.xlsx: not read. sheet tbl06alargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06alargestparcel_x_5.xlsx: not read. sheet tbl06alargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06alargestparcel_x_6.xlsx: not read. sheet tbl06alargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06alargestparcel_x_7.xlsx: not read. sheet tbl06alargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06alargestparcel_x_8.xlsx: not read. sheet tbl06alargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06alargestparcel_x_9.xlsx: not read. sheet tbl06alargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06blargestparcel_x.xlsx: not read. sheet tbl06blargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06blargestparcel_x_1.xlsx: not read. sheet tbl06blargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06blargestparcel_x_2.xlsx: not read. sheet tbl06blargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06blargestparcel_x_3.xlsx: not read. sheet tbl06blargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06blargestparcel_x_4.xlsx: not read. sheet tbl06blargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06blargestparcel_x_5.xlsx: not read. sheet tbl06blargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06blargestparcel_x_6.xlsx: not read. sheet tbl06blargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06blargestparcel_x_7.xlsx: not read. sheet tbl06blargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06blargestparcel_x_8.xlsx: not read. sheet tbl06blargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06blargestparcel_x_9.xlsx: not read. sheet tbl06blargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06clargestparcel_x.xlsx: not read. sheet tbl06clargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06clargestparcel_x_1.xlsx: not read. sheet tbl06clargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06clargestparcel_x_2.xlsx: not read. sheet tbl06clargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06clargestparcel_x_3.xlsx: not read. sheet tbl06clargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06clargestparcel_x_4.xlsx: not read. sheet tbl06clargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06clargestparcel_x_5.xlsx: not read. sheet tbl06clargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06clargestparcel_x_6.xlsx: not read. sheet tbl06clargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06clargestparcel_x_7.xlsx: not read. sheet tbl06clargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06clargestparcel_x_8.xlsx: not read. sheet tbl06clargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06clargestparcel_x_9.xlsx: not read. sheet tbl06clargestparcel_x: farmerid, salineaffected, landtype, cem, soiltype, plantmonth, plantweek, area, season, ricevar, mvtv, tenure, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, laborinput, specify, familymale, familyfemale, hiredmale, hiredfemale, wagemale, wagefemale, contractcost, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x_1.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, laborinput, specify, familymale, familyfemale, hiredmale, hiredfemale, wagemale, wagefemale, contractcost, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x_10.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, laborinput, specify, familymale, familyfemale, hiredmale, hiredfemale, wagemale, wagefemale, contractcost, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x_2.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, laborinput, specify, familymale, familyfemale, hiredmale, hiredfemale, wagemale, wagefemale, contractcost, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x_3.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, laborinput, specify, familymale, familyfemale, hiredmale, hiredfemale, wagemale, wagefemale, contractcost, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x_4.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, laborinput, specify, familymale, familyfemale, hiredmale, hiredfemale, wagemale, wagefemale, contractcost, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x_5.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, ricevar, laborinput, specify, gender, famlyno, famlyday, famlyhr, hiredno, hiredday, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x_6.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, laborinput, specify, familymale, familyfemale, hiredmale, hiredfemale, wagemale, wagefemale, contractcost, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x_7.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, laborinput, specify, familymale, familyfemale, hiredmale, hiredfemale, wagemale, wagefemale, contractcost, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x_8.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, laborinput, specify, familymale, familyfemale, hiredmale, hiredfemale, wagemale, wagefemale, contractcost, .... Observation records are missing from d until this file is standardized.
# tbl06laborinput_x_9.xlsx: not read. sheet tbl06laborinput_x: farmerid, salineaffected, season, laborinput, specify, familymale, familyfemale, hiredmale, hiredfemale, wagemale, wagefemale, contractcost, .... Observation records are missing from d until this file is standardized.
# tbl06matinputs_x.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_x_1.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_x_10.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_x_2.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_x_3.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_x_4.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_x_5.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, ricevar, season, matinputs, specify, kgperunit, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_x_6.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_x_7.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_x_8.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_x_9.xlsx: not read. sheet tbl06matinputs_x: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06matinputs_xorig.xlsx: not read. sheet tbl06matinputs_xorig: farmerid, salineaffected, season, matinputs, specify, unit, pricerent, quantity, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06poweruse_x.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, poweruse, season, hireowned, unit, dayshour, pricerent, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06poweruse_x_1.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, poweruse, season, hireowned, unit, dayshour, pricerent, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06poweruse_x_10.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, poweruse, season, hireowned, unit, dayshour, pricerent, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06poweruse_x_2.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, poweruse, season, hireowned, unit, dayshour, pricerent, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06poweruse_x_3.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, poweruse, season, hireowned, unit, dayshour, pricerent, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06poweruse_x_4.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, poweruse, season, hireowned, unit, dayshour, pricerent, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06poweruse_x_5.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, ricevar, poweruse, season, hireowned, unit, dayshour, pricerent, cost, cropshr, cropshrdenom, .... Observation records are missing from d until this file is standardized.
# tbl06poweruse_x_6.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, poweruse, season, hireowned, unit, dayshour, pricerent, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06poweruse_x_7.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, poweruse, season, hireowned, unit, dayshour, pricerent, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06poweruse_x_8.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, poweruse, season, hireowned, unit, dayshour, pricerent, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06poweruse_x_9.xlsx: not read. sheet tbl06poweruse_x: farmerid, salineaffected, poweruse, season, hireowned, unit, dayshour, pricerent, cost, comments. Observation records are missing from d until this file is standardized.
# tbl06workhours_x.xlsx: not read. sheet tbl06workhours_x: farmerid, salineaffected, season, workhours, comments. Observation records are missing from d until this file is standardized.
# tbl06workhours_x_1.xlsx: not read. sheet tbl06workhours_x: farmerid, salineaffected, season, workhours, comments. Observation records are missing from d until this file is standardized.
# tbl06workhours_x_2.xlsx: not read. sheet tbl06workhours_x: farmerid, salineaffected, season, workhours, comments. Observation records are missing from d until this file is standardized.
# tbl06workhours_x_3.xlsx: not read. sheet tbl06workhours_x: farmerid, salineaffected, season, workhours, comments. Observation records are missing from d until this file is standardized.
# tbl06workhours_x_4.xlsx: not read. sheet tbl06workhours_x: farmerid, salineaffected, season, workhours, comments. Observation records are missing from d until this file is standardized.
# tbl06workhours_x_5.xlsx: not read. sheet tbl06workhours_x: farmerid, salineaffected, season, workhours, comments. Observation records are missing from d until this file is standardized.
# tbl06workhours_x_6.xlsx: not read. sheet tbl06workhours_x: farmerid, salineaffected, season, workhours, comments. Observation records are missing from d until this file is standardized.
# tbl06workhours_x_7.xlsx: not read. sheet tbl06workhours_x: farmerid, salineaffected, season, workhours, comments. Observation records are missing from d until this file is standardized.
# tbl06workhours_x_8.xlsx: not read. sheet tbl06workhours_x: farmerid, salineaffected, season, workhours, comments. Observation records are missing from d until this file is standardized.
# tbl06workhours_x_9.xlsx: not read. sheet tbl06workhours_x: farmerid, salineaffected, season, workhours, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x_1.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x_10.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x_2.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x_3.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x_4.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x_5.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, season, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x_6.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x_7.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x_8.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl07nonricecost_x_9.xlsx: not read. sheet tbl07nonricecost_x: farmerid, nonricecrop, plotsize, seedplant, fertilizer, pesticide, hiredlabor, others, otherspecify, comments. Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, mhhmembers, mworkplace, mdaysworked, mkindpayment, mcashpayment, fhhmembers, fworkplace, fdaysworked, fkindpayment, .... Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x_1.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, mhhmembers, mworkplace, mdaysworked, mkindpayment, mcashpayment, fhhmembers, fworkplace, fdaysworked, fkindpayment, .... Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x_10.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, mhhmembers, mworkplace, mdaysworked, mkindpayment, mcashpayment, fhhmembers, fworkplace, fdaysworked, fkindpayment, .... Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x_2.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, mhhmembers, mworkplace, mdaysworked, mkindpayment, mcashpayment, fhhmembers, fworkplace, fdaysworked, fkindpayment, .... Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x_3.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, mhhmembers, mworkplace, mdaysworked, mkindpayment, mcashpayment, fhhmembers, fworkplace, fdaysworked, fkindpayment, .... Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x_4.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, mhhmembers, mworkplace, mdaysworked, mkindpayment, mcashpayment, fhhmembers, fworkplace, fdaysworked, fkindpayment, .... Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x_5.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, hhmembers, workplace, daysworked, kindpayment, cashpayment, comments. Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x_6.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, mhhmembers, mworkplace, mdaysworked, mkindpayment, mcashpayment, fhhmembers, fworkplace, fdaysworked, fkindpayment, .... Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x_7.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, mhhmembers, mworkplace, mdaysworked, mkindpayment, mcashpayment, fhhmembers, fworkplace, fdaysworked, fkindpayment, .... Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x_8.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, mhhmembers, mworkplace, mdaysworked, mkindpayment, mcashpayment, fhhmembers, fworkplace, fdaysworked, fkindpayment, .... Observation records are missing from d until this file is standardized.
# tbl08afarmnonfarmlabor_x_9.xlsx: not read. sheet tbl08afarmnonfarmlabor_x: farmerid, activitytype, worktype, mhhmembers, mworkplace, mdaysworked, mkindpayment, mcashpayment, fhhmembers, fworkplace, fdaysworked, fkindpayment, .... Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x_1.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x_10.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x_2.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x_3.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x_4.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x_5.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, price, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x_6.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x_7.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x_8.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08banimalsales_x_9.xlsx: not read. sheet tbl08banimalsales_x: farmerid, items, unit, quantity, salevalue, whosold, comments. Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, male, female, both, comments. Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x_1.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, male, female, both, comments. Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x_10.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, husband, son1, son2, other_male, wife, daughter1, daughter2, other_female, .... Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x_2.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, male, female, both, comments. Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x_3.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, male, female, both, comments. Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x_4.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, male, female, both, comments. Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x_5.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, male, female, both, comments. Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x_6.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, male, female, both, comments. Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x_7.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, husband, son1, son2, other_male, wife, daughter1, daughter2, other_female, .... Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x_8.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, husband, son1, son2, other_male, wife, daughter1, daughter2, other_female, .... Observation records are missing from d until this file is standardized.
# tbl08cotherincome_x_9.xlsx: not read. sheet tbl08cotherincome_x: farmerid, incomesource, qty, price, husband, son1, son2, other_male, wife, daughter1, daughter2, other_female, .... Observation records are missing from d until this file is standardized.
# tbl09asset.xlsx: not read. sheet tbl09asset: farmerid, asset_type, asset, qty, unit, presentvalue, yearacqrd. Observation records are missing from d until this file is standardized.
# tbl09borrowing_x.xlsx: not read. sheet tbl09borrowing_x: farmerid, itemborrwed, quantity, unit, itemvalue, source, interestrate, collateral, purpose. Observation records are missing from d until this file is standardized.
# tbl09borrowing_x_1.xlsx: not read. sheet tbl09borrowing_x: farmerid, itemborrwed, quantity, unit, itemvalue, source, interestrate, collateral, purpose. Observation records are missing from d until this file is standardized.
# tbl09borrowing_x_2.xlsx: not read. sheet tbl09borrowing_x: farmerid, itemborrwed, quantity, unit, itemvalue, source, interestrate, collateral, purpose. Observation records are missing from d until this file is standardized.
# tbl09borrowing_x_3.xlsx: not read. sheet tbl09borrowing_x: farmerid, itemborrwed, quantity, unit, itemvalue, source, interestrate, collateral, purpose. Observation records are missing from d until this file is standardized.
# tbl09borrowing_x_4.xlsx: not read. sheet tbl09borrowing_x: farmerid, itemborrwed, quantity, unit, itemvalue, source, interestrate, collateral, purpose. Observation records are missing from d until this file is standardized.
# tbl09borrowing_x_5.xlsx: not read. sheet tbl09borrowing_x: farmerid, itemborrwed, quantity, unit, itemvalue, source, interestrate, collateral, purpose. Observation records are missing from d until this file is standardized.
# tbl09borrowing_x_6.xlsx: not read. sheet tbl09borrowing_x: farmerid, itemborrwed, quantity, unit, itemvalue, source, interestrate, collateral, purpose. Observation records are missing from d until this file is standardized.
# tbl09borrowing_x_7.xlsx: not read. sheet tbl09borrowing_x: farmerid, itemborrwed, quantity, unit, itemvalue, source, interestrate, collateral, purpose. Observation records are missing from d until this file is standardized.
# tbl09borrowing_x_8.xlsx: not read. sheet tbl09borrowing_x: farmerid, itemborrwed, quantity, unit, itemvalue, source, interestrate, collateral, purpose. Observation records are missing from d until this file is standardized.
# tbl09borrowing_x_9.xlsx: not read. sheet tbl09borrowing_x: farmerid, itemborrwed, quantity, unit, itemvalue, source, interestrate, collateral, purpose. Observation records are missing from d until this file is standardized.
# tbl10consumption_x.xlsx: not read. sheet tbl10consumption_x: farmerid, fooditem, normalyear, stressyear, comments. Observation records are missing from d until this file is standardized.
# tbl10consumption_x_1.xlsx: not read. sheet tbl10consumption_x: farmerid, fooditem, normalyear, stressyear, comments. Observation records are missing from d until this file is standardized.
# tbl10consumption_x_2.xlsx: not read. sheet tbl10consumption_x: farmerid, fooditem, normalyear, stressyear, comments. Observation records are missing from d until this file is standardized.
# tbl10consumption_x_3.xlsx: not read. sheet tbl10consumption_x: farmerid, fooditem, normalyear, stressyear, comments. Observation records are missing from d until this file is standardized.
# tbl10consumption_x_4.xlsx: not read. sheet tbl10consumption_x: farmerid, fooditem, normalyear, stressyear, comments. Observation records are missing from d until this file is standardized.
# tbl10consumption_x_5.xlsx: not read. sheet tbl10consumption_x: farmerid, fooditem, normalyear, stressyear, comments. Observation records are missing from d until this file is standardized.
# tbl10consumption_x_6.xlsx: not read. sheet tbl10consumption_x: farmerid, fooditem, normalyear, stressyear, comments. Observation records are missing from d until this file is standardized.
# tbl10consumption_x_7.xlsx: not read. sheet tbl10consumption_x: farmerid, fooditem, normalyear, stressyear, comments. Observation records are missing from d until this file is standardized.
# tbl10consumption_x_8.xlsx: not read. sheet tbl10consumption_x: farmerid, fooditem, normalyear, stressyear, comments. Observation records are missing from d until this file is standardized.
# tbl10consumption_x_9.xlsx: not read. sheet tbl10consumption_x: farmerid, fooditem, normalyear, stressyear, comments. Observation records are missing from d until this file is standardized.
# tbl11adjustment_x.xlsx: not read. sheet tbl11adjustment_x: farmerid, normal, stress, normalmeal, stressmeal, eatless, eatlesswhy, consumeseed, consumeseedwhy, sellmore, sellmorewhy, otherincome, .... Observation records are missing from d until this file is standardized.
# tbl11adjustment_x_1.xlsx: not read. sheet tbl11adjustment_x: farmerid, normal, stress, normalmeal, stressmeal, eatless, eatlesswhy, consumeseed, consumeseedwhy, sellmore, sellmorewhy, otherincome, .... Observation records are missing from d until this file is standardized.
# tbl11adjustment_x_2.xlsx: not read. sheet tbl11adjustment_x: farmerid, normal, stress, normalmeal, stressmeal, eatless, eatlesswhy, consumeseed, consumeseedwhy, sellmore, sellmorewhy, otherincome, .... Observation records are missing from d until this file is standardized.
# tbl11adjustment_x_3.xlsx: not read. sheet tbl11adjustment_x: farmerid, normal, stress, normalmeal, stressmeal, eatless, eatlesswhy, consumeseed, consumeseedwhy, sellmore, sellmorewhy, otherincome, .... Observation records are missing from d until this file is standardized.
# tbl11adjustment_x_4.xlsx: not read. sheet tbl11adjustment_x: farmerid, normal, stress, normalmeal, stressmeal, eatless, eatlesswhy, consumeseed, consumeseedwhy, sellmore, sellmorewhy, otherincome, .... Observation records are missing from d until this file is standardized.
# tbl11adjustment_x_5.xlsx: not read. sheet tbl11adjustment_x: farmerid, normal, stress, normalmeal, stressmeal, eatless, eatlesswhy, consumeseed, consumeseedwhy, sellmore, sellmorewhy, otherincome, .... Observation records are missing from d until this file is standardized.
# tbl11adjustment_x_6.xlsx: not read. sheet tbl11adjustment_x: farmerid, normal, stress, normalmeal, stressmeal, eatless, eatlesswhy, consumeseed, consumeseedwhy, sellmore, sellmorewhy, otherincome, .... Observation records are missing from d until this file is standardized.
# tbl11adjustment_x_7.xlsx: not read. sheet tbl11adjustment_x: farmerid, normal, stress, normalmeal, stressmeal, eatless, eatlesswhy, consumeseed, consumeseedwhy, sellmore, sellmorewhy, otherincome, .... Observation records are missing from d until this file is standardized.
# tbl11adjustment_x_8.xlsx: not read. sheet tbl11adjustment_x: farmerid, normal, stress, normalmeal, stressmeal, eatless, eatlesswhy, consumeseed, consumeseedwhy, sellmore, sellmorewhy, otherincome, .... Observation records are missing from d until this file is standardized.
# tbl11adjustment_x_9.xlsx: not read. sheet tbl11adjustment_x: farmerid, normal, stress, normalmeal, stressmeal, eatless, eatlesswhy, consumeseed, consumeseedwhy, sellmore, sellmorewhy, otherincome, .... Observation records are missing from d until this file is standardized.
# tbl12bstresstolvar_x.xlsx: not read. sheet tbl12bstresstolvar_x: farmerid, stresstolvar, seedsource, experiences. Observation records are missing from d until this file is standardized.
# tbl12bstresstolvar_x_1.xlsx: not read. sheet tbl12bstresstolvar_x: farmerid, stresstolvar, seedsource, experiences. Observation records are missing from d until this file is standardized.
# tbl12bstresstolvar_x_2.xlsx: not read. sheet tbl12bstresstolvar_x: farmerid, stresstolvar, seedsource, experiences. Observation records are missing from d until this file is standardized.
# tbl12bstresstolvar_x_3.xlsx: not read. sheet tbl12bstresstolvar_x: farmerid, stresstolvar, seedsource, experiences. Observation records are missing from d until this file is standardized.
# tbl12bstresstolvar_x_4.xlsx: not read. sheet tbl12bstresstolvar_x: farmerid, stresstolvar, seedsource, experiences. Observation records are missing from d until this file is standardized.
# tbl12bstresstolvar_x_5.xlsx: not read. sheet tbl12bstresstolvar_x: farmerid, stresstolvar, seedsource, experiences. Observation records are missing from d until this file is standardized.
# tbl12bstresstolvar_x_6.xlsx: not read. sheet tbl12bstresstolvar_x: farmerid, stresstolvar, seedsource, experiences. Observation records are missing from d until this file is standardized.
# tbl12bstresstolvar_x_7.xlsx: not read. sheet tbl12bstresstolvar_x: farmerid, stresstolvar, seedsource, experiences. Observation records are missing from d until this file is standardized.
# tbl12bstresstolvar_x_8.xlsx: not read. sheet tbl12bstresstolvar_x: farmerid, stresstolvar, seedsource, experiences. Observation records are missing from d until this file is standardized.
# tbl12bstresstolvar_x_9.xlsx: not read. sheet tbl12bstresstolvar_x: farmerid, stresstolvar, seedsource, experiences. Observation records are missing from d until this file is standardized.
# tbl13adecisions_x.xlsx: not read. sheet tbl13adecisions_x: farmerid, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, .... Observation records are missing from d until this file is standardized.
# tbl13adecisions_x_1.xlsx: not read. sheet tbl13adecisions_x: farmerid, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, .... Observation records are missing from d until this file is standardized.
# tbl13adecisions_x_2.xlsx: not read. sheet tbl13adecisions_x: farmerid, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, .... Observation records are missing from d until this file is standardized.
# tbl13adecisions_x_3.xlsx: not read. sheet tbl13adecisions_x: farmerid, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, .... Observation records are missing from d until this file is standardized.
# tbl13adecisions_x_4.xlsx: not read. sheet tbl13adecisions_x: farmerid, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, .... Observation records are missing from d until this file is standardized.
# tbl13adecisions_x_5.xlsx: not read. sheet tbl13adecisions_x: farmerid, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, .... Observation records are missing from d until this file is standardized.
# tbl13adecisions_x_6.xlsx: not read. sheet tbl13adecisions_x: farmerid, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, .... Observation records are missing from d until this file is standardized.
# tbl13adecisions_x_7.xlsx: not read. sheet tbl13adecisions_x: farmerid, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, .... Observation records are missing from d until this file is standardized.
# tbl13adecisions_x_8.xlsx: not read. sheet tbl13adecisions_x: farmerid, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, .... Observation records are missing from d until this file is standardized.
# tbl13adecisions_x_9.xlsx: not read. sheet tbl13adecisions_x: farmerid, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, .... Observation records are missing from d until this file is standardized.
# tbl13borganization_x.xlsx: not read. sheet tbl13borganization_x: farmerid, husbandwife, organization, position, activity. Observation records are missing from d until this file is standardized.
# tbl13borganization_x_1.xlsx: not read. sheet tbl13borganization_x: farmerid, husbandwife, organization, position, activity. Observation records are missing from d until this file is standardized.
# tbl13borganization_x_2.xlsx: not read. sheet tbl13borganization_x: farmerid, husbandwife, organization, position, activity. Observation records are missing from d until this file is standardized.
# tbl13borganization_x_3.xlsx: not read. sheet tbl13borganization_x: farmerid, husbandwife, organization, position, activity. Observation records are missing from d until this file is standardized.
# tbl13borganization_x_4.xlsx: not read. sheet tbl13borganization_x: farmerid, husbandwife, organization, position, activity. Observation records are missing from d until this file is standardized.
# tbl13borganization_x_5.xlsx: not read. sheet tbl13borganization_x: farmerid, husbandwife, organization, position, activity. Observation records are missing from d until this file is standardized.
# tbl13borganization_x_6.xlsx: not read. sheet tbl13borganization_x: farmerid, husbandwife, organization, position, activity. Observation records are missing from d until this file is standardized.
# tbl13borganization_x_7.xlsx: not read. sheet tbl13borganization_x: farmerid, husbandwife, organization, position, activity. Observation records are missing from d until this file is standardized.
# tbl13borganization_x_8.xlsx: not read. sheet tbl13borganization_x: farmerid, husbandwife, organization, position, activity. Observation records are missing from d until this file is standardized.
# tbl13borganization_x_9.xlsx: not read. sheet tbl13borganization_x: farmerid, husbandwife, organization, position, activity. Observation records are missing from d until this file is standardized.
# tbl13bparticipate_x.xlsx: not read. sheet tbl13bparticipate_x: farmerid, husbandwife, participate. Observation records are missing from d until this file is standardized.
# tbl13bparticipate_x_1.xlsx: not read. sheet tbl13bparticipate_x: farmerid, husbandwife, participate. Observation records are missing from d until this file is standardized.
# tbl13bparticipate_x_2.xlsx: not read. sheet tbl13bparticipate_x: farmerid, husbandwife, participate. Observation records are missing from d until this file is standardized.
# tbl13bparticipate_x_3.xlsx: not read. sheet tbl13bparticipate_x: farmerid, husbandwife, participate. Observation records are missing from d until this file is standardized.
# tbl13bparticipate_x_4.xlsx: not read. sheet tbl13bparticipate_x: farmerid, husbandwife, participate. Observation records are missing from d until this file is standardized.
# tbl13bparticipate_x_5.xlsx: not read. sheet tbl13bparticipate_x: farmerid, husbandwife, participate. Observation records are missing from d until this file is standardized.
# tbl13bparticipate_x_6.xlsx: not read. sheet tbl13bparticipate_x: farmerid, husbandwife, participate. Observation records are missing from d until this file is standardized.
# tbl13bparticipate_x_7.xlsx: not read. sheet tbl13bparticipate_x: farmerid, husbandwife, participate. Observation records are missing from d until this file is standardized.
# tbl13bparticipate_x_8.xlsx: not read. sheet tbl13bparticipate_x: farmerid, husbandwife, participate. Observation records are missing from d until this file is standardized.
# tbl13bparticipate_x_9.xlsx: not read. sheet tbl13bparticipate_x: farmerid, husbandwife, participate. Observation records are missing from d until this file is standardized.
# tbl13ctraining_x.xlsx: not read. sheet tbl13ctraining_x: farmerid, husbandwife, training. Observation records are missing from d until this file is standardized.
# tbl13ctraining_x_1.xlsx: not read. sheet tbl13ctraining_x: farmerid, husbandwife, training. Observation records are missing from d until this file is standardized.
# tbl13ctraining_x_2.xlsx: not read. sheet tbl13ctraining_x: farmerid, husbandwife, training. Observation records are missing from d until this file is standardized.
# tbl13ctraining_x_3.xlsx: not read. sheet tbl13ctraining_x: farmerid, husbandwife, training. Observation records are missing from d until this file is standardized.
# tbl13ctraining_x_4.xlsx: not read. sheet tbl13ctraining_x: farmerid, husbandwife, training. Observation records are missing from d until this file is standardized.
# tbl13ctraining_x_5.xlsx: not read. sheet tbl13ctraining_x: farmerid, husbandwife, training. Observation records are missing from d until this file is standardized.
# tbl13ctraining_x_6.xlsx: not read. sheet tbl13ctraining_x: farmerid, husbandwife, training. Observation records are missing from d until this file is standardized.
# tbl13ctraining_x_7.xlsx: not read. sheet tbl13ctraining_x: farmerid, husbandwife, training. Observation records are missing from d until this file is standardized.
# tbl13ctraining_x_8.xlsx: not read. sheet tbl13ctraining_x: farmerid, husbandwife, training. Observation records are missing from d until this file is standardized.
# tbl13ctraining_x_9.xlsx: not read. sheet tbl13ctraining_x: farmerid, husbandwife, training. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"
Green Super Rice for the Resource Poor of Asia and Africa (GSR) - Phase 1

1. To analyze the patterns of adoption and diffusion of existing improved varieties, and identify constraints to adoption;            2.  To analyze the economics of rice production and farmer livelihood strategies and understand the gender roles in rice production and women's participation in decision making;   3. To estimate the potential impact of improved varieties being developed under the project on rice production, farmer income and poverty reduction; and     4.  To draw implications for technology development, targeting and policy reforms.
"

	uri <- "doi:10.7910/DVN/26194"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "Green Super Rice (GSR)",
		design = "household survey; about 1200 households in Cambodia, Pakistan and Sri Lanka",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		notes = "Yield is farmer-reported production per reported area",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 0.8
	)

	sfx <- c("", paste0("_", c(1:4, 6:10)))

	get_file <- function(s) {
		f4 <- ff[basename(ff) == paste0("tbl04aricevariety_x", s, ".xlsx")]
		f1 <- ff[basename(ff) == paste0("tbl00surveyinfo", s, ".xlsx")]
		r <- carobiner::read.excel(f4)
		r1 <- carobiner::read.excel(f1)
		i <- match(r$farmerid, r1$farmerid)
		data.frame(
			file = s,
			farmerid = r$farmerid,
			country = r1$country[i],
			adm2 = r1$district[i],
			location = r1$village[i],
			season_local_ = trimws(r$season),
			parcel = r$parcel,
			plot = r$plot,
			subplot = r$subplot,
			land_type_ = tolower(trimws(r$landtype)),
			variety = trimws(r$ricevar),
			mvtv = r$mvtv,
			cem = tolower(trimws(r$cem)),
			area = as.numeric(r$area),
			prod = as.numeric(r$prod)
		)
	}
	r <- do.call(rbind, lapply(sfx, get_file))

	d <- data.frame(
		hhid = paste0("f", r$file, "_", r$farmerid),
		country = r$country,
		adm2 = r$adm2,
		location = r$location,
		season_local_ = r$season_local_,
		variety = r$variety,
		field_size = r$area,
		yield = r$prod / r$area
	)
	d$trial_id <- paste(d$hhid, r$parcel, r$plot, r$subplot, sep="_")

	slc <- tolower(r$season_local_)
	d$season <- c("wet", "wet", "dry", "kharif", "wet", "dry")[match(slc, c("wet", "wet 2", "dry", "kharif", "maha", "yala"))]

	lt <- r$land_type_
	lt[lt %in% c("low land", "lawland")] <- "lowland"
	d$land_type_ <- lt

	d$variety_type <- ifelse(r$mvtv == "MV", "improved", "landrace")
	cem <- r$cem
	d$planting_method <- NA
	d$planting_method[cem %in% c("broadcasting", "broardcast")] <- "broadcasting"
	d$planting_method[cem %in% c("tpr", "transplant", "transplanting")] <- "transplanting"
	d$planting_method[cem %in% c("dsr", "wsr", "sowing")] <- "direct seeding"

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	d$planting_date <- as.character(NA)
	d$harvest_date <- as.character(NA)
	d$geo_from_source <- FALSE
	d$longitude <- as.numeric(NA)
	d$latitude <- as.numeric(NA)
	d$irrigated <- as.logical(NA)
	d$N_fertilizer <- as.numeric(NA)
	d$P_fertilizer <- as.numeric(NA)
	d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}
