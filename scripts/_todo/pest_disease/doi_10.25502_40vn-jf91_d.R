# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer-field survey of maize in the 10 regions of Cameroon, mostly 2017 (three survey rounds).
#   About 10 plants per plot. pest_incidence is the FAW count on that plant. Stemborer counts
#   are in stemborer_count_. Co-occurrence of the two is cooccurrence_.
# PltingDate is not a calendar date (values 1 to 100, median 3). age_day is the field age in days
#   (dictionary). planting_date = sampling date minus age_day. Sampling dates are yyyy/m/d, plus
#   14/10/2017, 30/05/2017 and 31/05/2017 (day/month/year).
# FieldSize_sqrm is square metres, stored as field_size in hectares.
# Regions are mapped to GADM French names (Adamawa to Adamaoua, East to Est, Far North to
#   Extrême-Nord, North to Nord, Northwest to Nord-Ouest, South to Sud, Southwest to Sud-Ouest,
#   West to Ouest). location is the village. camp_location_ is the column Location (where the
#   team camped), which usually differs from the village.
# Blank GPS on a plant is filled with the median latitude and longitude of the other plants
#   in the same trial (same farmer, village, field and plot). Points that fall outside Cameroon
#   are kept.
# Pesticide yes/oui versus no/non sets insecticide_used. Product names are kept as written
#   (trade names and a few active ingredients). "emamectin benzoate" matches the vocabulary.
# Fertilizer yes/no is the Fertilizer column. Product names are mostly in product.1.
#   Grades such as 20:10:10 are fertilizer_type NPK, with the original text in fertilizer_grade_.
#   urée is urea. "fiente" is stored as poultry manure and "manure" as farmyard manure.
#   No application rates, so no N/P/K amounts.
# The metadata citation is Fotso Kuate et al. 2019 on S. frugiperda in Cameroon. No DOI is given
#   in the metadata, so publication is NA.
# Read as latin1.

## ISSUES
# 1. Coordinates: some points are not on land or fall in Nigeria or the Central African Republic
#    (Mamfe in Southwest is stored as longitude 23.338). A swap of lat/lon does not fix them.
#    Some plots have no GPS at all, so latitude and longitude stay missing (NA detected).
# 2. No yield. Weediness is only 2, 3 or 4 (dictionary says percent); left as weediness_score_.
# 3. Development stage is 0-7 or veg/flo/Tas/OT with no key. Growth stage left as recorded.
# 4. Pupae is 0-3 or the words ANT and Bug. Kept as text in pupae_record_.
# 5. Recent_Change_in_damage mixes yes/no with notes such as "high infestation" and crop names.
# 6. Insecticide and fertilizer product strings are not all active ingredients. Frequency text
#    was not converted to a number of applications.
# 7. Farmer names are used in hhid. Telephone and national ID were not copied.
# 8. Confirm GADM spelling of adm1, and whether Location should be adm2 rather than camp_location_.
# 9. write_files: missing yield, yield_part, yield_moisture, irrigated, N/P/K, harvest_date;
#    NA latitude/longitude; coordinates not on land or in the wrong country; pest_species
#    Spodoptera frugiperda; insecticide trade names; fertilizer_type agrovert.
#    Variety strings such as "blanche" and "cms 8704" are kept as recorded.

## NOT PROCESSED
# ID, Sampler, Time: row number, enumerator, time of day.
# CNI, telephone: national ID and phone number.
# FAWpresenceabsence, stemborerpresenceabsence: flags; 10 FAW and 3 stemborer rows disagree with the counts.
# PltingDate: numeric codes, not dates (see NOTES).
# meta_maize_survey_database.csv: column dictionary.

## NEW VARIABLES
# survey_round_: survey wave I, II or III
# camp_location_: town where the team camped (column Location)
# insecticide_frequency_: application timing as written (Freq_month)
# fertilizer_frequency_: fertilizer timing as written
# fertilizer_grade_: fertilizer product text (grades, urea, manure)
# stemborer_count_: stemborer count on the plant
# cooccurrence_: both, FAW, SB, or none
# pupae_record_: pupae column as written (0-3, ANT, Bug)
# natural_enemies_: natural enemies named on the plant
# other_insects_: other insects (column Others)
# damage_years_: text for how long the farmer had seen the damage (IncDamDu_yrs): "2 ans", "5 years", "NEVER", "more than 10 years"
# recent_damage_note_: free text on recent change in damage
# surrounding_vegetation_: surrounding vegetation
# surrounding_vegetation_2_: second surrounding-vegetation field
# within_field_vegetation_: vegetation inside the field
# weediness_score_: Weddiness (2, 3 or 4); not treated as a percent
# stem_diameter_cm_: stem diameter; "-" set to missing
# plant_id_: plant number within the plot

carob_script <- function(path) {

"
Fall armyworm in Cameroon: distribution, damage, pesticide use, genetic differentiation and host plants.

Maize farmers in sub-Saharan Africa recently experienced unusual damage in their farms, attributed to the fall armyworm (FAW) Spodoptera frugiperda (J. E. Smith). This pest was first recorded in Africa in 2016, but detailed information on its distribution and damage and farmer’s response in invaded areas are largely lacking. In this study, we determined FAW distribution, genetic diversity, host plants, crop damage, and farmers’ responses. S. frugiperda was recorded in the 10 regions of Cameroon. Average percentage of infested plants and damage severity (on a scale of 1 to 5) were lowest—20.7 ± 7.4% and 2.1 ± 0.1 respectively—in the Sahelian regions and greatest—69.0 ± 4.3% and 3.1 ± 0.1 respectively—in the Western Highlands. Altitude did not influence FAW incidence and severity and its larvae infrequently co-occurred with maize stemborers on the same plants, suggesting possible direct and/or indirect competition between the two groups of maize pests. In response to this new threat to maize production, farmers have opted for the application of synthetic pesticides. Although our experiments were not designed to determine pesticide efficacy, as parameters such as time since application were not considered, our observations suggest lack of a drastic effect on S. frugiperda infestations on maize. There were two haplotypes of FAW co-occurring in Cameroon corresponding to the rice and corn strains and separated by 1.7% sequence divergence, which does not support the existence of cryptic species. S. frugiperda larvae were also recorded on Sorghum bicolor (L.) Moench (10.6%), Solanum tuberosum L. (2.8%), Ipomoea batatas (L.) Lam. (1.9%), Saccharum officinarum L (0.8%), Phaseolus vulgaris L. (0.4%) and Gossypium hirsutum L. (1.9%). This study show that two strains are present in all agroecological zones in Cameroon, and probably in neighboring countries of central Africa sharing the same agroecologies. Management options should therefore consider the use of specific natural enemies and an informed decision of intervention based on strain capture and damage threshold, to avoid pesticide resistance that may arise from inadequate use of chemicals. Further studies should also be undertaken to assess the response of the two S. frugiperda strains to biopesticides and botanical insecticides.
"

	uri <- "doi:10.25502/40vn-jf91/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Agricultural Investments and Market Development Project (PIDMA)",
		design = "Survey of maize fields in the 10 regions of Cameroon; about 10 plants scored per plot",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 1
	)

	f <- ff[basename(ff) == "maize-survey-database.csv"]
	s <- read.csv(f, check.names = TRUE, fileEncoding = "latin1", stringsAsFactors = FALSE)

	rawd <- trimws(s$Date)
	obs <- rep(NA_character_, length(rawd))
	i <- grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$", rawd)
	obs[i] <- as.character(as.Date(rawd[i], "%Y/%m/%d"))
	j <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", rawd)
	obs[j] <- as.character(as.Date(rawd[j], "%d/%m/%Y"))
	planting <- as.character(as.Date(obs) - as.integer(s$age_day))

	reg <- tolower(trimws(s$Region))
	adm_map <- c(
		"adamawa" = "Adamaoua",
		"centre" = "Centre",
		"east" = "Est",
		"far north" = "Extr\u00eame-Nord",
		"littoral" = "Littoral",
		"north" = "Nord",
		"northwest" = "Nord-Ouest",
		"south" = "Sud",
		"southwest" = "Sud-Ouest",
		"west" = "Ouest"
	)
	adm1 <- unname(adm_map[reg])

	blank_token <- function(x) {
		x <- tolower(trimws(x))
		x %in% c("", "0", "/", "no", "non", "na", "inconu", "inconnu")
	}
	village <- trimws(s$Village)
	village[blank_token(village)] <- NA_character_
	okv <- !is.na(village)
	village[okv] <- carobiner::fix_name(village[okv], "title")
	camp <- trimws(s$Location)
	camp[blank_token(camp)] <- NA_character_
	okc <- !is.na(camp)
	camp[okc] <- carobiner::fix_name(camp[okc], "title")

	farmer <- trimws(s$Farmer)
	farmer[farmer %in% c("", "0")] <- "not recorded"
	hhid <- paste(adm1, ifelse(is.na(village), "no village", village), farmer, trimws(as.character(s$FieldNo)), sep = "_")

	variety <- trimws(s$Variety)
	variety[blank_token(variety)] <- NA_character_
	seed <- trimws(s$Seed_source)
	seed[blank_token(seed)] <- NA_character_
	dyears <- trimws(as.character(s$IncDamDu_yrs))
	dyears[is.na(dyears) | dyears == ""] <- NA_character_

	yn <- function(x) {
		x <- tolower(trimws(x))
		ifelse(x %in% c("yes", "oui"), TRUE, ifelse(x %in% c("no", "non"), FALSE, NA))
	}
	ip <- tolower(trimws(s$product))
	ip[blank_token(ip) | ip %in% c("yes", "oui", "unknow", "unknown", "-1")] <- NA_character_
	ip <- gsub("[[:space:]]+", " ", ip)
	ip <- gsub("[/,]", ";", ip)
	ip <- trimws(gsub("[[:space:]]*;[[:space:]]*", ";", ip))
	ip[!is.na(ip) & ip == ""] <- NA_character_
	ins_used <- yn(s$Pesticide)
	ins_used[!is.na(ip)] <- TRUE

	fert_flag <- tolower(trimws(s$Fertilizer))
	prod <- tolower(trimws(s[["product.1"]]))
	prod[prod %in% c("", "0", "no", "non", "inconu", "inconnu")] <- ""
	take <- prod == "" & !fert_flag %in% c("", "yes", "oui", "no", "non")
	prod[take] <- fert_flag[take]
	is_urea <- grepl("urea|uree|urée", prod)
	is_npk <- grepl("[0-9]+[: -][0-9]+[: -][0-9]+", prod)
	is_fiente <- grepl("fiente", prod)
	is_manure <- grepl("manure", prod) & !is_fiente
	is_agrovert <- grepl("agrovert", prod)
	is_foliar <- grepl("foliar", prod)
	ftype <- paste(
		ifelse(is_npk, "NPK", ""),
		ifelse(is_urea, "urea", ""),
		ifelse(is_agrovert, "agrovert", "")
	)
	ftype <- trimws(ftype)
	ftype <- gsub("[[:space:]]+", ";", ftype)
	ftype[!is.na(ftype) & ftype == ""] <- NA_character_
	fert_used <- yn(s$Fertilizer)
	fert_used[is_npk | is_urea | is_agrovert | is_foliar] <- TRUE
	grade <- prod
	grade[prod == ""] <- NA_character_

	freq_i <- trimws(s$Freq_month)
	freq_i[freq_i %in% c("", "0")] <- NA_character_
	freq_f <- trimws(s$Frequency)
	freq_f[freq_f %in% c("", "0")] <- NA_character_

	gs <- trimws(as.character(s[["maize_dev._stage"]]))
	gs[gs == ""] <- NA_character_
	co <- tolower(trimws(s$Cooccurence))
	co[!is.na(co) & co == ""] <- NA_character_
	pup <- trimws(s$Pupae)
	pup[pup == ""] <- NA_character_
	nat <- trimws(s$nat_en)
	nat[nat %in% c("", "0")] <- NA_character_
	oth <- trimws(as.character(s$Others))
	oth[oth %in% c("", "0")] <- NA_character_
	note <- trimws(s$Recent_Change_in_damage)
	note[note %in% c("", "/")] <- NA_character_
	dia <- trimws(as.character(s$Dia_cm))
	dia[dia %in% c("", "-")] <- NA_character_
	sv1 <- trimws(s$SurrVege)
	sv1[sv1 == ""] <- NA_character_
	sv2 <- trimws(s$Surrounding_Vege2)
	sv2[sv2 == ""] <- NA_character_
	sv3 <- trimws(s$Within_Field_veg)
	sv3[sv3 == ""] <- NA_character_

	d <- data.frame(
		country = "Cameroon",
		adm1 = adm1,
		location = village,
		camp_location_ = camp,
		latitude = s$latitude,
		longitude = s$longitude,
		elevation = s$elevation,
		geo_from_source = TRUE,
		hhid = hhid,
		survey_round_ = trimws(s$Survey),
		plot_id = as.character(s$Plot),
		plant_id_ = as.character(s$Plant_no),
		date = obs,
		planting_date = planting,
		field_size = s$FieldSize_sqrm / 10000,
		variety = variety,
		seed_source = seed,
		crop = "maize",
		growth_stage = gs,
		stress = "pests",
		pest_species = "Spodoptera frugiperda",
		pest_incidence = as.integer(s$FAW),
		stemborer_count_ = as.integer(s$stem_borer),
		cooccurrence_ = co,
		pupae_record_ = pup,
		natural_enemies_ = nat,
		other_insects_ = oth,
		insecticide_used = ins_used,
		insecticide_product = ip,
		insecticide_frequency_ = freq_i,
		fertilizer_used = fert_used,
		fertilizer_type = ftype,
		fertilizer_grade_ = grade,
		fertilizer_frequency_ = freq_f,
		foliar_fertilizer_used = ifelse(is_foliar, TRUE, NA),
		OM_used = ifelse(is_fiente | is_manure, TRUE, NA),
		OM_type = ifelse(is_fiente, "poultry manure", ifelse(is_manure, "farmyard manure", NA_character_)),
		damage_years_ = dyears,
		recent_damage_note_ = note,
		surrounding_vegetation_ = sv1,
		surrounding_vegetation_2_ = sv2,
		within_field_vegetation_ = sv3,
		weediness_score_ = s$Weddiness,
		stem_diameter_cm_ = as.numeric(dia),
		trial_id = paste(hhid, s$Plot, sep = "_"),
		on_farm = TRUE,
		is_survey = TRUE,
		stringsAsFactors = FALSE
	)

	med <- function(z) {
		z <- z[!is.na(z)]
		if (!length(z)) NA_real_ else median(z)
	}
	medlat <- tapply(d$latitude, d$trial_id, med)
	medlon <- tapply(d$longitude, d$trial_id, med)
	miss <- is.na(d$latitude) | is.na(d$longitude)
	d$latitude[miss] <- medlat[d$trial_id[miss]]
	d$longitude[miss] <- medlon[d$trial_id[miss]]

	carobiner::write_files(path, meta, d)
}
