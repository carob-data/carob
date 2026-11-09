# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI/JICA-related Tanzania Rice Socio Economic Survey 2009 (Nakano & Kajisa), 760 rice households in Morogoro, Mbeya and Shinyanga regions.
# The "Raw Data" folder has 47 household files (hh*.xlsx) and 51 village files; only Code_sheet_Final.doc documents codes. There is no questionnaire, so most column meanings (q511, s04, cf04 ...) are not documented.
# Used: hh0.xlsx (household location codes) and hh5_3.xlsx (rice seed/variety module, one row per household x variety slot s01, 3 slots).
# Region, district, ward and village names from the code sheet lists.
# s03 is the rice variety code (code sheet "Rice Variety"); s02 is the variety name as written by the enumerator.
# s04 is interpreted as the area planted with the variety in acres: it sums to q513 (rice plot area) of hh5_1, and land preparation (hh5_5) uses unit code 1 = acre with the same number. Converted to ha (x 0.4046863).
# Seed amount s08amt x s08conv (kg per unit; units 1 kg, 2 bag, 3 debe, 4 tin) gives kg seed; seed_rate = kg / field_size.
# General codes -9, -8, -2 (not applicable, missing, no information) are set to NA.

## ISSUES
# Rice production is not mapped: hh6_1_cleaned.xlsx q611/q615/q619 (quantity, unit, kg per unit) look like harvest amounts, and hh5_18_cleaned.xlsx has wet season area and harvest by year 2004-2008, but without the questionnaire it is unclear which plot/season they refer to. The human should obtain the questionnaire (Yuko Nakano) before mapping yield. Consider moving to _pending.
# Fertilizer (hh5_7_cleaned.xlsx: product cf02, quantity cf04qty, unit, kg per unit cf04cf) is not mapped; plot link unclear. Only about 80 households used fertilizer (urea, DAP, CAN, NPK, Minjingu, booster).
# hh5_1.xlsx q5114-q5117 look like projected coordinates (e.g. 810721 / 3664040), but the CRS is not documented and northings do not fit UTM south for Tanzania. Not used. Human must georeference the 72 villages (no coordinates).
# s05, s06, s07, s09, s10, s11 in hh5_3 are undocumented (s10 might be seed cost in TZS); not mapped.
# Variety names: code 8 = "Others" (s02 text kept as variety for these), 75 = "Mixed"; code sheet names kept as written (e.g. "Kehogo/Kahogo/Mwasungo").
# Not mapped: all other hh modules (demography hh1, plots hh2_2, other crops hh3, livestock, labour hh5_9-hh5_17, credit, income, extension) and all village files.
# Season/year of the reported crop is not documented (survey 2009, interviews Sep-Dec 2009); planting_date not set.
# write_files: missing longitude/latitude/geo_from_source; new variables.

## NEW VARIABLES
# variety_slot_: variety slot number in the seed module (s01, 1-3)



## NOT PROCESSED
# Project details.xlsx: not read. sheet PROJECT: Title of the Study, ...2, Tanzania Rice Socio Economic Survey. Observation records are missing from d until this file is standardized.
# hh1_cleaned.xlsx: not read. sheet Sheet1: hhid, region, district, ward, village, hhnum, dname, d1, d2, d3, d4, d5, .... Observation records are missing from d until this file is standardized.
# hh1_demograhic.xlsx: not read. sheet Sheet1: hhid, hhsize, adult, eld, men, women, eldmale, eldfmale, boys, girls, child6, slearn, .... Observation records are missing from d until this file is standardized.
# hh10.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q10ab00, q10ab01, q10ab02, q10ab03, q10ab04, q10ab05mm, .... Observation records are missing from d until this file is standardized.
# hh10_2_cleaned.xlsx: not read. sheet Sheet1: region, district, ward, village, hhid, q10ab11, q10ab12text, q10ab12. Observation records are missing from d until this file is standardized.
# hh11_1.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q1111, q112lsname, q112lid, q112lr1, q112lr2, q112lr3, .... Observation records are missing from d until this file is standardized.
# hh11_2.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q1121, q112lpname, q112lp0, q112lp1, q112lp2, q112lp3, .... Observation records are missing from d until this file is standardized.
# hh12.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q12, q12category, q12exname, q12ex0, q12ex1, q12ex2, .... Observation records are missing from d until this file is standardized.
# hh13.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q13, q13aitem, q13a0, q13a1, q13a2. Observation records are missing from d until this file is standardized.
# hh14.xlsx: not read. sheet Sheet1: region, district, ward, village, hhid, q141, q142, q143, q144, q145, q146, q147. Observation records are missing from d until this file is standardized.
# hh2_2_cleaned.xlsx: not read. sheet Sheet1: hhid, region, district, ward, village, q22num, pname, pc0, pc1, pc2, pc3, pc4, .... Observation records are missing from d until this file is standardized.
# hh3_1_cleaned.xlsx: not read. sheet Sheet1: hhid, region, district, ward, village, q31fillup, q3index, q31pid, q31lid, q31cname, q31cid, q31ci1, .... Observation records are missing from d until this file is standardized.
# hh3_2.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q32, q32index, q32pid, q32lid, q32cname, q32cid, .... Observation records are missing from d until this file is standardized.
# hh3_3.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q33fillup, q33index, q33pid, q33lid, q33cname, q33cid, .... Observation records are missing from d until this file is standardized.
# hh4.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q4, rname, rpid, r1, r2, r3, .... Observation records are missing from d until this file is standardized.
# hh5_1_1_cleaned.xlsx: not read. sheet Sheet1: region, district, ward, village, hhid, q511, q512, q513, q514, q515, q516, q517, .... Observation records are missing from d until this file is standardized.
# hh5_1_2.xlsx: not read. sheet Sheet1: hhid, region, district, ward, village, q5114, q5115, q5116, q5117, index. Observation records are missing from d until this file is standardized.
# hh5_10_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q5101, i01, i02, i03, i04qty, i04unit, .... Observation records are missing from d until this file is standardized.
# hh5_11_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q511l, q511l01, q511l02, q511l03, q511l04mnum, q511l04mday, .... Observation records are missing from d until this file is standardized.
# hh5_12_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q512l, q512l01, q512l02, q512l03, q512l04mnum, q512l04mday, .... Observation records are missing from d until this file is standardized.
# hh5_13_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q5131, q5132, q5133, q5134, q513l01, q513l02, .... Observation records are missing from d until this file is standardized.
# hh5_14.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q514l, q514l01, q514l02, q514ir01, q514l03, q514l04mnum, .... Observation records are missing from d until this file is standardized.
# hh5_16_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q516l, q516ma01, q516ma02, q516ma03, q516ma04, q516ma04anim, .... Observation records are missing from d until this file is standardized.
# hh5_17_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q517l, q517l01, q517l02, q517l03, q517l04mnum, q517l04mday, .... Observation records are missing from d until this file is standardized.
# hh5_2_cleaned.xlsx: not read. sheet Sheet1: region, district, ward, village, hhid, q52fillup, ce00, ce01, ce02, ce03dd, ce03mm, ce04dd, .... Observation records are missing from d until this file is standardized.
# hh5_4.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q541, cn01, cn02, cn03, cn04, cn05, .... Observation records are missing from d until this file is standardized.
# hh5_5_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q551, q55ma01, q55ma02, q55ma03, q55ma04, q55ma04anim, .... Observation records are missing from d until this file is standardized.
# hh5_6_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q56fillup, q56l01, q56l02, q56l03, q56l04mnum, q56l04mday, .... Observation records are missing from d until this file is standardized.
# hh5_6_Number of labor (manday for trasplanting).xlsx: not read. sheet Sheet1: hhid, mandaytp. Observation records are missing from d until this file is standardized.
# hh5_8_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q58, of01, of02, of03qty, of03unit_1, of03unit_2, .... Observation records are missing from d until this file is standardized.
# hh5_9_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q59l, q59l01, q59l02, q59l03, q59l04mnum, q59l04mday, .... Observation records are missing from d until this file is standardized.
# hh6_2.xlsx: not read. sheet Sheet1: region, district, ward, village, hhid, q621qty, q621unit, q621cf, q622mm, q622dd, q623, q624a, .... Observation records are missing from d until this file is standardized.
# hh6_2_1.xlsx: not read. sheet Sheet1: region, district, ward, village, hhid, q621qty, q621unit, q621cf, q622mm, q622dd, q623, q624a, .... Observation records are missing from d until this file is standardized.
# hh6_2_2.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q62tr2my, q62tr2var, q62tr221, q62tr222, q62tr223, q62tr224, .... Observation records are missing from d until this file is standardized.
# hh6_3.xlsx: not read. sheet Sheet1: region, district, ward, village, hhid, q631qty, q631unit, q631cf, q632mm, q632dd, q633, q634a, .... Observation records are missing from d until this file is standardized.
# hh6_3_1.xlsx: not read. sheet Sheet1: region, district, ward, village, hhid, q631qty, q631unit, q631cf, q632mm, q632dd, q633, q634a, .... Observation records are missing from d until this file is standardized.
# hh6_3_2.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q63tr2my, q63tr2var, q63tr241, q63tr242, q63tr243, q63tr244, .... Observation records are missing from d until this file is standardized.
# hh6_4.xlsx: not read. sheet Sheet1: region, district, ward, village, hhid, q64fillup, ac01_1, ac01_2, ac01_3, ac01_4, ac02_1, ac02_2, .... Observation records are missing from d until this file is standardized.
# hh7.xlsx: not read. sheet Sheet1: region, district, ward, village, hhid, q71, q72, q73, q74, q7l01_1, q7l01_2, q7l01_3, .... Observation records are missing from d until this file is standardized.
# hh7_1.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q7l01, q7l02, q7l03, q7l04mnum, q7l04mday, q7l04mhrs, .... Observation records are missing from d until this file is standardized.
# hh8_cleaned.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q81, q8index, q8name, q8id, q8et0, q8et1, .... Observation records are missing from d until this file is standardized.
# hh9.xlsx: not read. sheet Sheet1: hhid, id, region, district, ward, village, q9, q9b0, q9name, q9id, q9bname, q9biz, .... Observation records are missing from d until this file is standardized.
# village_100_subsidy_cleaned.xlsx: not read. sheet Sheet1: village, villagename, subsidy, subsidyd. Observation records are missing from d until this file is standardized.
# village_11_cleaned.xlsx: not read. sheet Sheet1: village, district, region, ward, ec0, ec1, ec2, ec3, ec4, ec5, ec6, ec6crop, .... Observation records are missing from d until this file is standardized.
# village_12_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, en0, code, en1, en2, en3, en4, sci22a, sci22b, .... Observation records are missing from d until this file is standardized.
# village_12_stress.xlsx: not read. sheet Sheet1: region, code, en2, en3, biotic, abiotic. Observation records are missing from d until this file is standardized.
# village_13_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, vr0, vr1, vr2, vr3, vr4, vr5, vr6, vr7, .... Observation records are missing from d until this file is standardized.
# village_13_variety characteristics.xlsx: not read. sheet Sheet1: majorcode, varhight, tallvar, midvar, shortvar. Observation records are missing from d until this file is standardized.
# village_21_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, lo0, lo1, l02, lo3, lo4, lo5, lo6. Observation records are missing from d until this file is standardized.
# village_21_location.xlsx: not read. sheet Sheet1: village, distrcap, distdcap, region, district, ward, darrcap, distdarr, dardcap, distdard. Observation records are missing from d until this file is standardized.
# village_22_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, if0, if1, if2, if3, if4, if5, if6, code. Observation records are missing from d until this file is standardized.
# village_22_extoffice.xlsx: not read. sheet Sheet1: village, district, extoffice, extoffice4, extoffice5, extoffice6, extoffice10, extdist. Observation records are missing from d until this file is standardized.
# village_23_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, nh1, nh2. Observation records are missing from d until this file is standardized.
# village_23_noofhh.xlsx: not read. sheet Sheet1: village, region, district, ward, nohh. Observation records are missing from d until this file is standardized.
# village_23_number_of_hh.xlsx: not read. sheet Sheet1: village, region, district, ward, hhinvil, code. Observation records are missing from d until this file is standardized.
# village_24_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, lr1, lr2, lr3a, lr3b, lr4a, lr4b. Observation records are missing from d until this file is standardized.
# village_25_cleaned.xlsx: not read. sheet Sheet1: village, code, region, district, ward, tr1, tr2, tr3. Observation records are missing from d until this file is standardized.
# village_25_number of tracter.xlsx: not read. sheet Sheet1: village, code, region, district, ward, notract. Observation records are missing from d until this file is standardized.
# village_27_cleaned.xlsx: not read. sheet Sheet1: region, district, ward, village, q271, q272, q272unit, q273. Observation records are missing from d until this file is standardized.
# village_28_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, lm0, lm1, lm2, lm3, lm4, code. Observation records are missing from d until this file is standardized.
# village_29_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, wr0, wr1, wr2, wr3, wr4. Observation records are missing from d until this file is standardized.
# village_29_district average wage.xlsx: not read. sheet Sheet1: district, wr1, wr2dist, wr3dist, wr4dist. Observation records are missing from d until this file is standardized.
# village_29_regional average wage.xlsx: not read. sheet Sheet1: region, wr1, wr2reg, wr3reg, wr4reg. Observation records are missing from d until this file is standardized.
# village_29_village wage bird scaring.xlsx: not read. sheet Sheet1: village, region, wr0, wr2, wr3, wr4. Observation records are missing from d until this file is standardized.
# village_29_village wage harvesting.xlsx: not read. sheet Sheet1: village, region, wr0, wr2, wr3, wr4. Observation records are missing from d until this file is standardized.
# village_29_village wage ploughing.xlsx: not read. sheet Sheet1: village, region, wr0, wr2, wr3, wr4. Observation records are missing from d until this file is standardized.
# village_29_village wage transplanting.xlsx: not read. sheet Sheet1: village, region, wr0, wr2, wr3, wr4. Observation records are missing from d until this file is standardized.
# village_29_village wage weeding.xlsx: not read. sheet Sheet1: village, region, wr0, wr2, wr3, wr4. Observation records are missing from d until this file is standardized.
# village_29_village wage.xlsx: not read. sheet Sheet1: village, region, district, ward, wr0, wr1, wr2, wr3, wr4, wr2dist, wr3dist, wr4dist, .... Observation records are missing from d until this file is standardized.
# village_29_wage.xlsx: not read. sheet Sheet1: village, avmwage, avwwage, ravmwage, ravwwage, medmwage, medwwage, rmedmwage, rmedwwage. Observation records are missing from d until this file is standardized.
# village_29_wage_lp.xlsx: not read. sheet Sheet1: village, region, district, ward, mwagelp, wwagelp, medppaddy, avppaddy, rmwagelp, rwwaglpe. Observation records are missing from d until this file is standardized.
# village_30_cleaned.xlsx: not read. sheet Sheet1: village, code, region, district, ward, q31, ir1, ir2, ir3, ir4, ir5, ir6, .... Observation records are missing from d until this file is standardized.
# village_30_irrigation.xlsx: not read. sheet Sheet1: village, irrivil. Observation records are missing from d until this file is standardized.
# village_40_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, crname, cr0, cr1, cr2, cr3, cr4, cr5, code. Observation records are missing from d until this file is standardized.
# village_40_credit access.xlsx: not read. sheet Sheet1: village, saccos, creditlend, credittrade, nmfb, creditacs, creditpriv. Observation records are missing from d until this file is standardized.
# village_40_credit.xlsx: not read. sheet Sheet1: village, saccos, creditpriv, ngo, saccosngo, ngopriv, creditorg, saccossh, ngoprivsh. Observation records are missing from d until this file is standardized.
# village_40_credit1.xlsx: not read. sheet Sheet1: village, saccos1, creditpriv1. Observation records are missing from d until this file is standardized.
# village_50_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, ipname, ip0, ip1, ip2, ip3, ip4. Observation records are missing from d until this file is standardized.
# village_50_inputprice.xlsx: not read. sheet Sheet1: village, region, district, ward, purea, pherb. Observation records are missing from d until this file is standardized.
# village_61_1_cleaned.xlsx: not read. sheet Sheet1: region, district, ward, village, q611m, q611y, q612, q613a, q613b, q613c, q614m, q614y, .... Observation records are missing from d until this file is standardized.
# village_61_2_cleaned.xlsx: not read. sheet Sheet1: village, code, region, district, ward, tr6100, tr6101, tr6102, tr6103, tr6104, tr6105, tr6106, .... Observation records are missing from d until this file is standardized.
# village_61_2_riceprice.xlsx: not read. sheet Sheet1: village, medppaddy, avppaddy. Observation records are missing from d until this file is standardized.
# village_62_1_cleaned.xlsx: not read. sheet Sheet1: region, district, ward, village, q621m, q621y, q622, q623a, q623b, q623c, q624m, q624y, .... Observation records are missing from d until this file is standardized.
# village_62_2_cleaned.xlsx: not read. sheet Sheet1: village, code, region, district, ward, tr6200, tr6201, tr6202, tr6203, tr6204, tr6205, tr6206, .... Observation records are missing from d until this file is standardized.
# village_63_1_cleaned.xlsx: not read. sheet Sheet1: region, district, ward, village, q631m, q631y, q632, q633a, q633b, q633c, q634m, q634y, .... Observation records are missing from d until this file is standardized.
# village_63_2_cleaned.xlsx: not read. sheet Sheet1: village, code, region, district, ward, tr6300, tr6301, tr6302, tr6303, tr6304, tr6305, tr6306, .... Observation records are missing from d until this file is standardized.
# village_64_cleaned.xlsx: not read. sheet Sheet1: village, code, region, district, ward, cr64name, cr640, cr641, cr642, cr643. Observation records are missing from d until this file is standardized.
# village_71_cleaned.xlsx: not read. sheet Sheet1: region, district, ward, village, q71, q721, q722, q723, q73. Observation records are missing from d until this file is standardized.
# village_72_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, ac01, ac02, ac03, ac04, ac05, ac06hr, ac06min, code. Observation records are missing from d until this file is standardized.
# village_72_marketaccess_modified.xlsx: not read. sheet Sheet1: village, avtrader, ricemill, ferdeal, seeddeal, machrepair, region, district, ward, ricemillacs, fertacs, seedacs, .... Observation records are missing from d until this file is standardized.
# village_81_cleaned.xlsx: not read. sheet Sheet1: village, region, district, ward, eg00, eg01, eg02, eg03, code. Observation records are missing from d until this file is standardized.
# village_82_cleaned.xlsx: not read. sheet Sheet1: region, district, ward, village, q81. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"
Tanzania Rice Socio Economic Survey

To investigate the determinants of technology adoption in rice cultivation by focusing on the role of credit in selected Tanzania households.
"

	uri <- "doi:10.7910/DVN/26293"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 10,
		carob_effort = 0.6
	)

	f1 <- ff[basename(ff) == "hh0.xlsx"]
	r1 <- carobiner::read.excel(f1)
	f2 <- ff[basename(ff) == "hh5_3.xlsx"]
	r2 <- carobiner::read.excel(f2)

	# Code_sheet_Final.doc
	regions <- c("1"="Morogoro", "2"="Mbeya", "3"="Shinyanga")
	districts <- c("101"="Kilombero", "102"="Mvomero", "201"="Kyela", "202"="Mbarali", "301"="Shinyanga Rural", "302"="Kahama")
	wards <- c("Lumemo", "Mchombe", "Idete", "Utengule", "Chisano", "Chita", "Mchombe", "Kiberege", "Kisawasawa", "Mkula", "Mvomero", "Hembeti", "Sungaji", "Kanga", "Mtibwa",
		"Ipande", "Mwaya", "Makwale", "Kyela", "Lusongo", "Ikama", "Kasujumele", "Ipinde", "Bujonde", "Ikolo", "Ngana", "Utengule Usangu", "Igurusi", "Mahongole", "Ruiwa", "Mapogolo", "Mapogolo", "Rujewa", "Ubaruku", "Madibira", "Madibira",
		"Mwamala", "Itwangi", "Usanda", "Pandagichiza", "Iselamagazi", "Lyabukande", "Salawe", "Mwakitolyo", "Solwa", "Usule", "Tinde", "Chela", "Busangi", "Ukune", "Bulige", "Ngaya", "Kinaga", "Isagehe", "Kilago", "Kisuke", "Chona", "Lunguya", "Idahina", "Segese", "Mhongolo", "Igwamanoni", "Kinamapula", "Ulowa", "Bugarama", "Ntobo")
	names(wards) <- c(101:115, 201:221, 301:330)
	villages <- c("Michenga", "Njage", "Idete", "Miwangani", "Mpanga", "Chisano", "Mguglwe", "Merera", "Ikule", "Mkasu", "Kisawasawa", "Mkula", "Katurukila",
		"Wami Luhindo", "Hembeti", "Dihombo", "Mkindo", "Mbogo", "Mziha", "Kanga", "Dihinnda", "Lukenge", "Komtonga", "Lungo",
		"Mbula", "Kapamisya", "Mahenge", "Itunge", "Lukama", "Mpunguti", "Kilwa", "Mabunga", "Itope", "Muungano", "Ngana",
		"Luhanga", "Chamoto", "Ilongo", "Wimba Mahango", "Itamba", "Ukwavila", "Igomelo", "Ibohora", "Mapogolo", "Mahango Madibira",
		"Bunonga", "Nyida", "Manyada", "Mwamadilanha", "Mwambasha", "Zobogo", "Ihugi", "Songambele", "Mawemilu", "Mwakatola", "Masekelo",
		"Welezo", "Chela", "Busangi", "Igunda", "Bulige", "Ngaya", "Ubilimbi", "Kishima", "Wame", "Nyamilangano", "Chona", "Lunguya", "Idahina", "Malito", "Nyashimbi", "Iramba", "Kinamapula", "Kangeme", "Bugarama", "Ntobo-A")
	names(villages) <- c(101:124, 201:221, 301:331)
	varieties <- c("Saro5", "IR54", "IR64", "Line88", "TXD85", "Mwangaza", "Klalu", "Others", "Affa Mwanza", "Supa",
		"Supa India/Kilombero/Farmer's India", "Fayarangi", "Zambia", "Kehogo/Kahogo/Mwasungo", "Todogoso", "Wahiwahi", "Supa Zanzibar", "Supa Mbeya", "Supa Shinyanga", "Mwangulu",
		"Supa Masungo", "Rangimbili", "Faya", "Misi", "Morogoro", "Ngome", "Supa Kabangara", "Kalamata/Malamata", "Mabeyenge", "China",
		"Katani", "Kalolo", "Gamti", "Bisholi", "Zaire", "Sogaatibilaga maguta", "Serena", "Mabungo", "Matela", "Nzugimlole",
		"Moshi", "Lugata", "Mwanamwala", "Umano", "Dakawa", "Malomo ga mbiti", "Malomo ga mhuli", "Mavi ya kuku", "Haraka", "Migiha",
		"Mbegubora", "Tuliani", "Maimai", "Bumanha", "Kula na bwana", "Kisegese", "Pawaga", "Kaling'anaula", "Usiniguse", "Udongo wa Songea",
		"Kifuke", "Hodihodi", "India rangi", "Chachacha", "Kavanga", "Bendera", "Jaribu", "Limota", "Rufiji", "Kalimawangu",
		"Belenge", "Mwarabu", "Mwanza", "Ruvu", "Mixed", "Mbawambili", "Supa white", "Moshi wa taa", "TXD88", "Domo la fisi",
		"Supa Malolo", "Shingo ya mwali", "Kizurizuri", "Pamba nyeupe", "Chuba", "Taiwan", "Mchuzi wa jogoo", "Mtunguja", "Supa pamba", "India mpauko",
		"India ya rangi mkia", "Suba mati", "Mwendambio", "Mahiya", "Pijo", "SARO1", "URO", "Fayadume", "SR54", "Mkia wa mbuzi")

	r2 <- r2[!is.na(r2$s03) & r2$s03 > 0, ]
	i <- match(r2$hhid, r1$hhid)

	variety <- varieties[r2$s03]
	other <- r2$s03 == 8 & !is.na(r2$s02)
	variety[other] <- trimws(r2$s02[other])

	area <- ifelse(r2$s04 > 0, r2$s04 * 0.4046863, NA)
	seedkg <- ifelse(r2$s08amt >= 0 & r2$s08conv > 0, r2$s08amt * r2$s08conv, NA)

	d <- data.frame(
		hhid = as.character(r2$hhid),
		country = "Tanzania",
		adm1 = as.vector(regions[as.character(r1$region[i])]),
		adm2 = as.vector(districts[as.character(r1$district[i])]),
		adm3 = as.vector(wards[as.character(r1$ward[i])]),
		location = as.vector(villages[as.character(r1$village[i])]),
		variety_slot_ = as.integer(r2$s01),
		crop = "rice",
		variety = variety,
		field_size = area,
		seed_rate = seedkg / area
	)

	d$trial_id <- paste(d$hhid, d$variety_slot_, sep="-")
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
