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

	r1 <- carobiner::read.excel(ff[basename(ff) == "hh0.xlsx"])
	r2 <- carobiner::read.excel(ff[basename(ff) == "hh5_3.xlsx"])

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
