# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Four fields had corrupted or missing GPS coordinates (Eyanchang/Mamfe lon=23.338, Afane/Ambam lat=1/lon=2.3857, Nguette/Ntui lat=2/lon=4.4563, Akometan/Mbalmayo NA) and were imputed from other fields in the same villages in this dataset (geo_from_source = FALSE); verify these corrections.
# The last 13 rows (Survey II, North and Far North, Plot = NA, Plant_no = 50) appear to be 50-plant field-level totals rather than single-plant records (flagged with plants_sampled_ = 50); verify whether they should stay in d or be separated.
# Several Cameroon pesticide trade names could not be mapped unambiguously to active ingredients and are kept as recorded in insecticide_product / herbicide_product (with the original text preserved in pesticide_product_raw_).
# Maize development stage mixes numeric codes (0-7) with text (veg, flo, Tas, OT) with no key in the dictionary; Weddiness only takes values 2, 3, or 4 (dictionary says %); Pupae mixes counts (0-3) with ANT and Bug.
# Confirm whether Location (camp town) should map to adm2 rather than camp_location_, and check GADM spellings of adm1.

## NEW
# survey_round_: survey wave (I, II, or III).
# camp_location_: town where the survey team camped (raw column Location).
# plant_id_: plant number within the plot (1-10; NA for the 13 50-plant summary rows).
# plants_sampled_: number of maize plants represented by the row (1 for individual plant records; 50 for the 13 Survey II North/Far North summary rows).
# cooccurrence_: simultaneous pest presence on the plant (both, faw, sb, or none).
# pupae_record_: raw Pupae column (0-3, ANT, or Bug).
# natural_enemies_: natural enemies observed on the plant (text).
# other_insects_: other insects observed on the plant (raw column Others).
# pesticide_product_raw_: original pesticide product string as reported by the farmer.
# insecticide_frequency_: farmer-reported pesticide application frequency (text, raw column Freq_month).
# fertilizer_grade_: original fertilizer product/grade string as reported by the farmer.
# fertilizer_frequency_: farmer-reported fertilizer application frequency (text).
# damage_years_: how long the farmer had noticed damage (text, raw column IncDamDu_yrs).
# recent_damage_note_: farmer response on recent change in damage (mixed yes/no and free text).
# surrounding_vegetation_: primary vegetation surrounding the field.
# surrounding_vegetation_2_: secondary vegetation surrounding the field.
# within_field_vegetation_: vegetation inside the field.
# weediness_score_: field weediness score (2, 3, or 4; dictionary says % but values look ordinal).
# insecticide_product unmapped trade names / local remedies: adama, bastion, bastion super, capi forte 184wg, capsidore, casmal, cofresh, dh2, diadrine, kalao, landrine, magiforce, mamira, mamira super, marcot, mucap, multiparafin, multiperafine, pepper solutio, perfect killer, terazer, wood ash.
# herbicide_product unmapped trade names: antimais, herbimais.
# fertilizer_type values not in terminag: agrovert, sulphate.

## NOTES
# Three-round farmer-field survey of maize across all 10 regions of Cameroon in 2017 (20,363 records; Fotso Kuate et al. 2019, doi:10.1371/journal.pone.0215749), typically 5 plots x 10 plants per field.
# Planting dates are derived as sampling date minus age_day (PltingDate holds non-date integers 1-100). Region names are mapped to French GADM adm1 names. Farmer names are hashed into anonymous hhid values.
# FAW (Spodoptera frugiperda) and stem borer counts are placed in long table dd (linked by record_id and date) instead of splitting stem borer into a custom wide column. Clear herbicides and fungicides reported in the pesticide column are separated into herbicide_product and fungicide_product, and well-known Cameroon insecticide trade names are mapped to active ingredients.

## NOT PROCESSED
# ID, Sampler, Time: row number, enumerator name, and time of day.
# Farmer, CNI, telephone: personally identifiable information (used only to form anonymous hhid).
# FAWpresenceabsence, stemborerpresenceabsence: presence/absence flags redundant with the counts (disagreements occur only on the 13 50-plant summary rows).
# PltingDate: numeric codes (1-100), not calendar dates.
# meta_maize_survey_database.csv: column dictionary only.


carob_script <- function(path) {

"
Fall armyworm in Cameroon: distribution, damage, pesticide use, genetic differentiation and host plants.

Maize farmers in sub-Saharan Africa recently experienced unusual damage in their farms, attributed to the fall armyworm (FAW) Spodoptera frugiperda (J. E. Smith). This pest was first recorded in Africa in 2016, but detailed information on its distribution and damage and farmer's response in invaded areas are largely lacking. In this study, we determined FAW distribution, genetic diversity, host plants, crop damage, and farmers' responses. S. frugiperda was recorded in the 10 regions of Cameroon. Average percentage of infested plants and damage severity (on a scale of 1 to 5) were lowest—20.7 ± 7.4% and 2.1 ± 0.1 respectively—in the Sahelian regions and greatest—69.0 ± 4.3% and 3.1 ± 0.1 respectively—in the Western Highlands. Altitude did not influence FAW incidence and severity and its larvae infrequently co-occurred with maize stemborers on the same plants, suggesting possible direct and/or indirect competition between the two groups of maize pests. In response to this new threat to maize production, farmers have opted for the application of synthetic pesticides. Although our experiments were not designed to determine pesticide efficacy, as parameters such as time since application were not considered, our observations suggest lack of a drastic effect on S. frugiperda infestations on maize. There were two haplotypes of FAW co-occurring in Cameroon corresponding to the rice and corn strains and separated by 1.7% sequence divergence, which does not support the existence of cryptic species. S. frugiperda larvae were also recorded on Sorghum bicolor (L.) Moench (10.6%), Solanum tuberosum L. (2.8%), Ipomoea batatas (L.) Lam. (1.9%), Saccharum officinarum L (0.8%), Phaseolus vulgaris L. (0.4%) and Gossypium hirsutum L. (1.9%). This study show that two strains are present in all agroecological zones in Cameroon, and probably in neighboring countries of central Africa sharing the same agroecologies. Management options should therefore consider the use of specific natural enemies and an informed decision of intervention based on strain capture and damage threshold, to avoid pesticide resistance that may arise from inadequate use of chemicals. Further studies should also be undertaken to assess the response of the two S. frugiperda strains to biopesticides and botanical insecticides.
"

	uri <- "doi:10.25502/40vn-jf91/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; IRAD",
		publication = "doi:10.1371/journal.pone.0215749",
		project = "Agricultural Investments and Market Development Project (PIDMA)",
		design = "Multi-round survey of maize fields in all 10 regions of Cameroon; 5 plots per field, 10 plants per plot",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "maize-survey-database.csv"]
	r <- read.csv(f, check.names=TRUE, fileEncoding="latin1")

	rawd <- trimws(r$Date)
	obs <- rep(NA_character_, length(rawd))
	i <- grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$", rawd)
	obs[i] <- as.character(as.Date(rawd[i], "%Y/%m/%d"))
	j <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", rawd)
	obs[j] <- as.character(as.Date(rawd[j], "%d/%m/%Y"))
	planting <- as.character(as.Date(obs) - as.integer(r$age_day))

	reg <- tolower(trimws(r$Region))
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
	village <- trimws(r$Village)
	village[blank_token(village)] <- NA_character_
	okv <- !is.na(village)
	village[okv] <- carobiner::fix_name(village[okv], "title")
	camp <- trimws(r$Location)
	camp[blank_token(camp)] <- NA_character_
	okc <- !is.na(camp)
	camp[okc] <- carobiner::fix_name(camp[okc], "title")

	farmer <- trimws(r$Farmer)
	farmer[farmer %in% c("", "0")] <- "not recorded"
	hh_key <- paste(adm1, ifelse(is.na(village), "no_village", village), farmer, trimws(as.character(r$FieldNo)), sep="_")
	hhid <- paste0("hh_", as.integer(factor(hh_key, levels=unique(hh_key))))

	variety <- trimws(r$Variety)
	variety[blank_token(variety)] <- NA_character_
	seed <- trimws(r$Seed_source)
	seed[blank_token(seed)] <- NA_character_
	dyears <- trimws(as.character(r$IncDamDu_yrs))
	dyears[is.na(dyears) | dyears == ""] <- NA_character_

	yn <- function(x) {
		x <- tolower(trimws(x))
		ifelse(x %in% c("yes", "oui"), TRUE, ifelse(x %in% c("no", "non"), FALSE, NA))
	}

	# Clean raw pesticide text and classify insecticides, herbicides, and fungicides
	praw <- trimws(r$product)
	praw[blank_token(praw) | tolower(praw) %in% c("yes", "oui", "unknow", "unknown", "-1")] <- NA_character_
	praw <- gsub("[[:space:]]+", " ", praw)

	ins_map <- c(
		"ca\u00efman" = "emamectin benzoate",
		"caiman" = "emamectin benzoate",
		"ca\u00efman b" = "emamectin benzoate",
		"caiman b" = "emamectin benzoate",
		"emacot" = "emamectin benzoate",
		"emamectin benzoate" = "emamectin benzoate",
		"cigogne" = "cypermethrin",
		"cigogne 360" = "cypermethrin",
		"la cigogne" = "cypermethrin",
		"cypercal" = "cypermethrin",
		"cypercal 2" = "cypermethrin",
		"cypercot" = "cypermethrin",
		"dusban" = "chlorpyrifos",
		"piriforce" = "chlorpyrifos",
		"chlorot 480ec" = "chlorpyrifos",
		"pyramine" = "chlorpyrifos",
		"pynfose" = "chlorpyrifos",
		"furadan" = "carbofuran",
		"tiodan" = "endosulfan",
		"bomec 18 ec" = "abamectin",
		"k-optimal" = "acetamiprid;lambda-cyhalothrin",
		"k optimal" = "acetamiprid;lambda-cyhalothrin",
		"k-optimal lamdacythrinet" = "acetamiprid;lambda-cyhalothrin",
		"optimal" = "acetamiprid;lambda-cyhalothrin",
		"pacha" = "acetamiprid;lambda-cyhalothrin",
		"lamida gold" = "imidacloprid;lambda-cyhalothrin",
		"kohinor" = "imidacloprid",
		"kohinor star" = "imidacloprid",
		"gamalin" = "lindane"
	)
	herb_map <- c(
		"gramazone" = "paraquat dichloride",
		"herbicide (gramazone)" = "paraquat dichloride",
		"herbima\u00efs" = "herbimais",
		"antimais" = "antimais"
	)
	fung_map <- c(
		"pencozeb 80wp" = "mancozeb",
		"monchamp 72wp" = "metalaxyl;mancozeb"
	)

	# Parse unique non-NA product strings once for speed
	u_praw <- unique(praw[!is.na(praw)])
	u_ins <- setNames(rep(NA_character_, length(u_praw)), u_praw)
	u_herb <- setNames(rep(NA_character_, length(u_praw)), u_praw)
	u_fung <- setNames(rep(NA_character_, length(u_praw)), u_praw)
	for (s in u_praw) {
		s_norm <- gsub("kohinor,[[:space:]]*star", "kohinor star", tolower(s))
		toks <- trimws(unlist(strsplit(s_norm, "[/,;]")))
		toks <- toks[nzchar(toks)]
		iv <- character()
		hv <- character()
		fv <- character()
		for (tk in toks) {
			if (tk %in% names(herb_map)) {
				hv <- c(hv, unname(herb_map[tk]))
			} else if (tk %in% names(fung_map)) {
				fv <- c(fv, unname(fung_map[tk]))
			} else if (tk %in% names(ins_map)) {
				iv <- c(iv, unlist(strsplit(unname(ins_map[tk]), ";")))
			} else {
				iv <- c(iv, tk)
			}
		}
		if (length(iv)) u_ins[s] <- paste(unique(iv), collapse=";")
		if (length(hv)) u_herb[s] <- paste(unique(hv), collapse=";")
		if (length(fv)) u_fung[s] <- paste(unique(fv), collapse=";")
	}
	ip <- unname(u_ins[praw])
	hp <- unname(u_herb[praw])
	fp <- unname(u_fung[praw])

	ins_used <- yn(r$Pesticide)
	ins_used[!is.na(ip)] <- TRUE
	# Rows where only a herbicide or fungicide was named in product
	only_other <- is.na(ip) & (!is.na(hp) | !is.na(fp))
	ins_used[only_other] <- FALSE

	herb_used <- ifelse(!is.na(hp), TRUE, NA)
	fung_used <- ifelse(!is.na(fp), TRUE, NA)

	fert_flag <- tolower(trimws(r$Fertilizer))
	prod <- tolower(trimws(r[["product.1"]]))
	prod[prod %in% c("", "0", "no", "non", "inconu", "inconnu")] <- ""
	take <- prod == "" & !fert_flag %in% c("", "yes", "oui", "no", "non")
	prod[take] <- fert_flag[take]
	is_urea <- grepl("urea|uree|ur\u00e9e", prod)
	is_npk <- grepl("[0-9]+[: -][0-9]+[: -][0-9]+", prod)
	is_fiente <- grepl("fiente", prod)
	is_manure <- grepl("manure", prod) & !is_fiente
	is_agrovert <- grepl("agrovert", prod)
	is_sulphate <- grepl("sulphate", prod)
	is_foliar <- grepl("foliar", prod)
	ftype <- paste(
		ifelse(is_npk, "NPK", ""),
		ifelse(is_urea, "urea", ""),
		ifelse(is_agrovert, "agrovert", ""),
		ifelse(is_sulphate, "sulphate", "")
	)
	ftype <- trimws(ftype)
	ftype <- gsub("[[:space:]]+", ";", ftype)
	ftype[!is.na(ftype) & ftype == ""] <- NA_character_
	fert_used <- yn(r$Fertilizer)
	fert_used[is_npk | is_urea | is_agrovert | is_sulphate | is_foliar] <- TRUE
	grade <- prod
	grade[prod == ""] <- NA_character_

	freq_i <- trimws(r$Freq_month)
	freq_i[freq_i %in% c("", "0")] <- NA_character_
	freq_f <- trimws(r$Frequency)
	freq_f[freq_f %in% c("", "0")] <- NA_character_

	gs <- trimws(as.character(r[["maize_dev._stage"]]))
	gs[gs == ""] <- NA_character_
	co <- tolower(trimws(r$Cooccurence))
	co[!is.na(co) & co == ""] <- NA_character_
	pup <- trimws(r$Pupae)
	pup[pup == ""] <- NA_character_
	nat <- trimws(r$nat_en)
	nat[nat %in% c("", "0")] <- NA_character_
	oth <- trimws(as.character(r$Others))
	oth[oth %in% c("", "0")] <- NA_character_
	note <- trimws(r$Recent_Change_in_damage)
	note[note %in% c("", "/")] <- NA_character_
	dia <- trimws(as.character(r$Dia_cm))
	dia[dia %in% c("", "-")] <- NA_character_
	sv1 <- trimws(r$SurrVege)
	sv1[sv1 == ""] <- NA_character_
	sv2 <- trimws(r$Surrounding_Vege2)
	sv2[sv2 == ""] <- NA_character_
	sv3 <- trimws(r$Within_Field_veg)
	sv3[sv3 == ""] <- NA_character_

	# Last 13 rows (Plot = NA, Plant_no = 50) are 50-plant field-level totals
	is_agg50 <- is.na(r$Plot) & !is.na(r$Plant_no) & r$Plant_no == 50

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		country = "Cameroon",
		adm1 = adm1,
		location = village,
		camp_location_ = camp,
		latitude = as.numeric(r$latitude),
		longitude = as.numeric(r$longitude),
		elevation = as.numeric(r$elevation),
		geo_from_source = TRUE,
		geo_uncertainty = as.numeric(NA),
		geo_source = NA_character_,
		hhid = hhid,
		survey_round_ = trimws(r$Survey),
		plot_id = ifelse(is.na(r$Plot), NA_character_, as.character(r$Plot)),
		plant_id_ = ifelse(is_agg50, NA_character_, as.character(r$Plant_no)),
		plants_sampled_ = ifelse(is_agg50, 50L, 1L),
		date = obs,
		planting_date = planting,
		field_size = as.numeric(r$FieldSize_sqrm) / 10000,
		variety = variety,
		seed_source = seed,
		crop = "maize",
		growth_stage = gs,
		stress = "pests",
		cooccurrence_ = co,
		pupae_record_ = pup,
		natural_enemies_ = nat,
		other_insects_ = oth,
		pesticide_product_raw_ = praw,
		insecticide_used = ins_used,
		insecticide_product = ip,
		insecticide_frequency_ = freq_i,
		herbicide_used = herb_used,
		herbicide_product = hp,
		fungicide_used = fung_used,
		fungicide_product = fp,
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
		weediness_score_ = as.integer(r$Weddiness),
		stem_diameter = as.numeric(dia),
		trial_id = ifelse(is.na(r$Plot), hhid, paste(hhid, r$Plot, sep="_")),
		on_farm = TRUE,
		is_survey = TRUE
	)

	# Fill within-trial missing coordinates from other plants in the same plot
	med <- function(z) {
		z <- z[!is.na(z)]
		if (!length(z)) NA_real_ else median(z)
	}
	medlat <- tapply(d$latitude, d$trial_id, med)
	medlon <- tapply(d$longitude, d$trial_id, med)
	miss <- is.na(d$latitude) | is.na(d$longitude)
	d$latitude[miss] <- as.numeric(medlat[d$trial_id[miss]])
	d$longitude[miss] <- as.numeric(medlon[d$trial_id[miss]])
	filled_same <- miss & !is.na(d$latitude) & !is.na(d$longitude)
	d$geo_from_source[filled_same] <- FALSE
	d$geo_source[filled_same] <- "imputed from same plot in dataset"

	# Fix 3 corrupted GPS fields and 1 missing field (Akometan) using other fields in the same villages
	# Eyanchang (Mamfe, Southwest): lon 23.338 falls in CAR; other Eyanchang fields are at lon ~9.237
	k1 <- !is.na(d$longitude) & d$longitude > 20
	d$longitude[k1] <- 9.2371
	# Afane (Ambam, South): lat=1, lon=2.3857 is a column shift (2.3857 is lat); other Afane fields are at lon ~11.258
	k2 <- !is.na(d$latitude) & d$latitude == 1
	d$latitude[k2] <- 2.3857
	d$longitude[k2] <- 11.2583
	# Nguette (Ntui, Centre): lat=2, lon=4.4563 is a column shift (4.4563 is lat); other Nguette fields are at lon ~11.5925
	k3 <- !is.na(d$latitude) & d$latitude == 2
	d$latitude[k3] <- 4.4563
	d$longitude[k3] <- 11.5925
	# Akometan (Mbalmayo, Centre): matches Akometam fields in the same dataset (3.4668, 11.5504)
	k4 <- is.na(d$latitude) | is.na(d$longitude)
	d$latitude[k4] <- 3.4668
	d$longitude[k4] <- 11.5504

	imp <- k1 | k2 | k3 | k4
	d$geo_from_source[imp] <- FALSE
	d$geo_uncertainty[imp] <- 5000
	d$geo_source[imp] <- "imputed from same village in dataset"

	# Long table for both pests (Spodoptera frugiperda and stem borer)
	dd <- rbind(
		data.frame(
			record_id = d$record_id,
			date = d$date,
			pest_species = "Spodoptera frugiperda",
			pest_incidence = as.integer(r$FAW)
		),
		data.frame(
			record_id = d$record_id,
			date = d$date,
			pest_species = "stem borer",
			pest_incidence = as.integer(r$stem_borer)
		)
	)
	dd <- dd[!is.na(dd$pest_incidence), ]

	carobiner::write_files(path, meta, d, long=dd)
}
