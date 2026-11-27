# R script for "carob"
# license: GPL (>=3)

## ISSUES
# - Nakuru codebook (01b. Data Dictionary - Nakuru.xlsx) omits value labels for integer codes.
#   Nakuru variety codes 1-11 were decoded by exact cross-match with Combined Dataset.dta, but
#   codes 0, 12, 13, and 14 (appearing in ranksn1/ranksn2/Rejected_variety1/2) have no label in
#   the raw files and are kept as "unknown (Nakuru code <k>)". Based on Nchanji et al. (2021),
#   Nakuru code 13 likely corresponds to Angaza, but this is not confirmed in the raw files.
# - Embu numeric reason codes (rank1reason_sn1..3, rank1reason_sn2..3, reasonworstvar1..3) and
#   gender-impact degree scales (F3/F5/F7/F9/F11/F13/F15 in Embu; By_what_degree* in Nakuru)
#   have no value-label tables in the codebooks and could not be decoded.
# - Nakuru Sub_County, Division, location, and village are unlabelled 0/1 codes; only adm1
#   ("Nakuru") is mapped for Nakuru, whereas Embu village names are mapped to location.
# - Nakuru age in Combined Dataset.dta (and 02b. Nakuru.csv) is an age-group code (1 = <35,
#   2 = >=35), not age in years; age is therefore NA for Nakuru and age_group is recorded for all.

## NEW
# - hh_head_sex_: sex of the household head ("male" / "female").
# - land_title_holder_: person whose name is on the land title deed.
# - seed_buyer_: household member who buys bean seed.
# - seed_financier_: household member who provides money to buy bean seed.
# - bean_seller_: household member who sells beans.
# - missing_traits_: desired bean traits reported as missing in current varieties (Embu).
# - increases_drudgery_: whether evaluated bean variety increases women's drudgery (logical).
# - decreases_drudgery_: whether evaluated bean variety reduces women's drudgery (logical).
# - displaces_women_: whether evaluated bean variety displaces women from activities (logical).
# - maintains_women_employment_: whether evaluated bean variety maintains/increases employment for women (logical).
# - needs_hard_to_access_inputs_: whether evaluated bean variety requires additional inputs difficult for women to access (logical).
# - improves_women_product_: whether evaluated bean variety maintains/increases quantity or quality of women-controlled products (logical).
# - reduces_women_benefit_control_: whether evaluated bean variety reduces women's control of benefits (logical).
# - gender_impact_notes_: open-ended explanation of gender impacts (Nakuru).

## NOT PROCESSED
# - 02b. Nakuru.csv: Enumerator (administrative); Sub_County, Division, location, village, Planting_season, Reason_for_Variety1, Reason_rejected1, By_what_degree, By_what_degree2, By_degree_displace, By_degree_employment, How_difficult_access, How_signi_improve, Degree_ctrl_reduce (unlabelled numeric codes).
# - 02c. Embu.csv: Q1, A01 (administrative); respondent_type, enterprise, relationship, occup, occup_other, active_member, hhtype, land_ownership, season1, season2, varietysn1_dec, sourcesn1_dec, beantype_buysn1, sellsn1_dec, varietysn2_dec, sourcesn2_dec, beantype_buysn2, sellsn2_dec, rank*reason_sn*, reasonworstvar*, F3/F5/F7/F9/F11/F13/F15_* (unlabelled numeric codes); females_over35, females_below35, males_over35, males_below35, femalesagric_*, malesagric_* (age-35 household composition splits).

carob_script <- function(path) {

"
Gender-Responsive Participatory Variety Selection in Kenya: Implications for Common Bean (Phaseolus vulgaris L.) Breeding in Kenya

The data collection aimed to investigate gender-responsive participatory variety selection (PVS) for common bean breeding in Kenya. It was conducted across Embu and Nakuru Counties in 2019 to understand gender differences in varietal and trait preferences among farmers. The study focused on preferences for biofortified released and local bean varieties, covering major topics like yield, maturity, pest resistance, marketability, and cooking time. The Principal Investigators sought to identify gender-specific adoption barriers, potential trade-offs, and socio-economic influences on varietal preferences.

Methodology: The study used a modified gender-responsive PVS tool integrating socio-demographic data with gender gap questions. Data was collected from 93 farmers through systematic random sampling. On-farm trials and demonstrations were conducted to facilitate farmer engagement, and participants ranked their preferred bean varieties. Descriptive statistical analysis was used to highlight gendered differences in preferences, examining factors such as land ownership, age, and education.
"

	uri <- "doi:10.7910/DVN/MO9LPT"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIAT",
		publication = "doi:10.3390/su132313164",
		project = NA,
		design = "gender-responsive PVS survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_LLM = "Gemini 4 Neon",
		carob_date = "2026-08-19",
		carob_completion = 75,
		carob_effort = 3
	)

	f1 <- ff[basename(ff) == "Combined Dataset.dta"]
	f2 <- ff[basename(ff) == "02b. Nakuru.csv"]
	f3 <- ff[basename(ff) == "02c. Embu.csv"]

	r1 <- carobiner::read.dta(f1)
	r2 <- utils::read.csv(f2, na.strings = "")
	r3 <- utils::read.csv(f3, na.strings = "")

	# Row alignment: Combined Dataset.dta ids 1-40 = Embu (r3), ids 41-93 = Nakuru (r2)
	hhid_embu <- as.character(r3$id)
	hhid_nakuru <- as.character(41:93)

	# Embu variety codes (1-25 match Combined Dataset.dta value labels; 99 = other)
	embu_var <- c(
		"1" = "Angaza", "2" = "Nyota", "3" = "Faida", "4" = "Local white",
		"5" = "Local", "6" = "Mwitamania", "7" = "Mama Safi", "8" = "Rose coco",
		"9" = "Yellow bean", "10" = "Katune", "11" = "Raila", "12" = "Duriandu",
		"13" = "Gacuma", "14" = "KAT B1", "15" = "Saitoti", "16" = "Red beans",
		"17" = "Loni", "18" = "Moviki", "19" = "Wairimu", "20" = "Kifam",
		"21" = "Gituru", "22" = "Noe", "23" = "Mukura", "24" = "Canadian wonder",
		"25" = "Mexican", "99" = "other"
	)

	# Nakuru variety codes (1-11 cross-matched from Combined Dataset.dta; others unlabelled)
	nakuru_var <- c(
		"1" = "Mwitamania", "2" = "Wairimu", "3" = "Nyota", "4" = "Kifam",
		"5" = "Gituru", "6" = "Noe", "7" = "Mukura", "8" = "Canadian wonder",
		"9" = "KAT B1", "10" = "Rose coco", "11" = "Mexican"
	)

	decode_var <- function(x, lookup, prefix) {
		k <- as.character(x)
		out <- unname(lookup[k])
		unk <- !is.na(k) & is.na(out)
		if (any(unk)) {
			out[unk] <- paste0("unknown (", prefix, " code ", k[unk], ")")
		}
		out
	}

	# Clean desired missing traits in Embu
	mt_cols <- grep("^missingtraits", names(r3), value = TRUE)
	clean_trait <- function(v) {
		v <- trimws(tolower(as.character(v)))
		v <- gsub("^high yieldig$", "high yielding", v)
		v <- gsub("^high folliage$", "high foliage", v)
		v <- gsub("^drought tolerant$", "drought tolerance", v)
		v <- v[!is.na(v) & nzchar(v)]
		if (length(v) == 0L) NA_character_ else paste(unique(v), collapse = ";")
	}
	embu_missing_traits <- apply(r3[, mt_cols, drop = FALSE], 1, clean_trait)

	# Wide table: 1 row per surveyed household (93 farmers)
	acre_to_ha <- 0.404686
	d <- data.frame(
		hhid = as.character(r1$id),
		country = "Kenya",
		adm1 = as.character(r1$county),
		crop = "common bean",
		is_survey = TRUE,
		on_farm = FALSE,
		survey_year = 2019L,
		sex = tolower(as.character(r1$sex)),
		age = ifelse(r1$county == "Embu", as.numeric(r1$age), NA_real_),
		age_group = as.character(r1$age_2),
		hh_head_sex_ = tolower(gsub("-headed$", "", as.character(r1$hh_head))),
		education = tolower(as.character(r1$educlevel)),
		civil_status = tolower(gsub("Seperated", "Separated", as.character(r1$marital))),
		land_ownedby = tolower(as.character(r1$ownership)),
		land_title_holder_ = tolower(as.character(r1$name_title)),
		seed_source = tolower(as.character(r1$bean_source)),
		seed_buyer_ = tolower(as.character(r1$Who_buys)),
		seed_financier_ = tolower(as.character(r1$Who_provides_money)),
		bean_seller_ = tolower(as.character(r1$Who_sells))
	)

	d_embu <- data.frame(
		hhid = hhid_embu,
		location = trimws(as.character(r3$village)),
		interview_date = NA_character_,
		is_head = ifelse(r3$hhead == 1, TRUE, ifelse(r3$hhead == 2, FALSE, NA)),
		education_years = as.numeric(r3$educ),
		hh_size = as.numeric(r3$hhsize),
		cooperative_member = ifelse(r3$group == 1, TRUE, ifelse(r3$group == 2, FALSE, NA)),
		farmland = as.numeric(r3$land) * acre_to_ha,
		cropland = as.numeric(r3$land_cropped) * acre_to_ha,
		cropland_used = as.numeric(r3$land_beans) * acre_to_ha,
		missing_traits_ = embu_missing_traits
	)

	d_nakuru <- data.frame(
		hhid = hhid_nakuru,
		location = NA_character_,
		interview_date = format(as.Date(r2$Date_interview, format = "%d.%m.%Y"), "%Y-%m-%d"),
		is_head = NA,
		education_years = NA_real_,
		hh_size = NA_real_,
		cooperative_member = NA,
		farmland = as.numeric(r2$Tot_farmsize) * acre_to_ha,
		cropland = as.numeric(r2$Tot_croparea) * acre_to_ha,
		cropland_used = as.numeric(r2$Area_bean) * acre_to_ha,
		missing_traits_ = NA_character_
	)

	d <- merge(d, rbind(d_embu, d_nakuru), by = "hhid", all.x = TRUE, sort = FALSE)

	geo <- data.frame(
		adm1 = c("Embu", "Nakuru"),
		longitude = c(37.6259, 36.0777),
		latitude = c(-0.5922, -0.4573),
		geo_from_source = FALSE,
		geo_uncertainty = c(60722, 91977),
		geo_source = "GADM 4.1, adm1"
	)
	d <- merge(d, geo, by = "adm1", all.x = TRUE, sort = FALSE)
	d <- d[order(as.integer(d$hhid)), ]
	rownames(d) <- NULL

	# Long table: farmer x variety evaluations (grown, known, preferred, rejected, gender_evaluation)
	yn <- function(x) ifelse(x == 1, TRUE, ifelse(x == 2, FALSE, NA))

	make_long_block <- function(hhid, variety, rank_variable, rank_score, season = NA_character_, variety_traits = NA_character_) {
		keep <- !is.na(variety) & nzchar(trimws(variety))
		if (!any(keep)) return(NULL)
		data.frame(
			hhid = hhid[keep],
			rank_variable = rank_variable,
			season = season,
			rank_score = as.integer(rank_score),
			variety = trimws(as.character(variety[keep])),
			variety_traits = trimws(as.character(variety_traits[keep]))
		)
	}

	blocks <- list()

	# 1. Grown varieties: Season 1 (all 93 farmers from Combined Dataset.dta) + Season 2 (Embu)
	for (k in 1:3) {
		blocks[[length(blocks) + 1L]] <- make_long_block(
			as.character(r1$id), as.character(r1[[paste0("variety", k)]]),
			"grown", k, season = "first"
		)
		blocks[[length(blocks) + 1L]] <- make_long_block(
			hhid_embu, decode_var(r3[[paste0("bvariaty", k, "sn2")]], embu_var, "Embu"),
			"grown", k, season = "second"
		)
	}

	# 2. Known varieties (Embu variety1..7)
	for (k in 1:7) {
		blocks[[length(blocks) + 1L]] <- make_long_block(
			hhid_embu, decode_var(r3[[paste0("variety", k)]], embu_var, "Embu"),
			"known", k
		)
	}

	# 3. Preferred (ranked) varieties: Embu season 1 & 2, Nakuru ranksn1 & ranksn2 with reasons
	for (k in 1:4) {
		blocks[[length(blocks) + 1L]] <- make_long_block(
			hhid_embu, decode_var(r3[[paste0("rank", k, "_sn1")]], embu_var, "Embu"),
			"preferred", k, season = "first"
		)
	}
	for (k in 1:3) {
		blocks[[length(blocks) + 1L]] <- make_long_block(
			hhid_embu, decode_var(r3[[paste0("rank", k, "_sn2")]], embu_var, "Embu"),
			"preferred", k, season = "second"
		)
	}
	blocks[[length(blocks) + 1L]] <- make_long_block(
		hhid_nakuru, decode_var(r2$ranksn1, nakuru_var, "Nakuru"),
		"preferred", 1L, variety_traits = r2$Reason1
	)
	blocks[[length(blocks) + 1L]] <- make_long_block(
		hhid_nakuru, decode_var(r2$ranksn2, nakuru_var, "Nakuru"),
		"preferred", 2L, variety_traits = r2$Reason2
	)

	# 4. Rejected (worst) varieties: Embu season 1 & 2, Nakuru Rejected_variety1 & 2 with reasons
	for (k in 1:3) {
		blocks[[length(blocks) + 1L]] <- make_long_block(
			hhid_embu, decode_var(r3[[paste0("worstvariety", k, "_sn1")]], embu_var, "Embu"),
			"rejected", k, season = "first"
		)
		blocks[[length(blocks) + 1L]] <- make_long_block(
			hhid_embu, decode_var(r3[[paste0("worstvariety", k, "_sn2")]], embu_var, "Embu"),
			"rejected", k, season = "second"
		)
	}
	blocks[[length(blocks) + 1L]] <- make_long_block(
		hhid_nakuru, decode_var(r2$Rejected_variety1, nakuru_var, "Nakuru"),
		"rejected", 1L, variety_traits = r2$Reason_rej1
	)
	blocks[[length(blocks) + 1L]] <- make_long_block(
		hhid_nakuru, decode_var(r2$Rejected_variety2, nakuru_var, "Nakuru"),
		"rejected", 2L, variety_traits = r2$Reason_rej2
	)

	# 5. Gender-gap evaluation of best varieties (Embu F1_1..F15_4; Nakuru household-level on ranksn1)
	for (k in 1:4) {
		v <- decode_var(r3[[paste0("F1_", k)]], embu_var, "Embu")
		keep <- !is.na(v) & nzchar(v)
		if (any(keep)) {
			blocks[[length(blocks) + 1L]] <- data.frame(
				hhid = hhid_embu[keep],
				rank_variable = "gender_evaluation",
				season = NA_character_,
				rank_score = as.integer(k),
				variety = v[keep],
				variety_traits = NA_character_,
				increases_drudgery_ = yn(r3[[paste0("F2_", k)]][keep]),
				decreases_drudgery_ = yn(r3[[paste0("F4_", k)]][keep]),
				displaces_women_ = yn(r3[[paste0("F6_", k)]][keep]),
				maintains_women_employment_ = yn(r3[[paste0("F8_", k)]][keep]),
				needs_hard_to_access_inputs_ = yn(r3[[paste0("F10_", k)]][keep]),
				improves_women_product_ = yn(r3[[paste0("F12_", k)]][keep]),
				reduces_women_benefit_control_ = yn(r3[[paste0("F14_", k)]][keep]),
				gender_impact_notes_ = NA_character_
			)
		}
	}

	v_nak <- decode_var(r2$ranksn1, nakuru_var, "Nakuru")
	keep_nak <- !is.na(v_nak) & nzchar(v_nak)
	blocks[[length(blocks) + 1L]] <- data.frame(
		hhid = hhid_nakuru[keep_nak],
		rank_variable = "gender_evaluation",
		season = NA_character_,
		rank_score = 1L,
		variety = v_nak[keep_nak],
		variety_traits = NA_character_,
		increases_drudgery_ = yn(r2$Variety_increase_drudgery[keep_nak]),
		decreases_drudgery_ = yn(r2$Variety_decrease_drudgery[keep_nak]),
		displaces_women_ = yn(r2$Displace_women[keep_nak]),
		maintains_women_employment_ = yn(r2$Employement_Inc_Maint[keep_nak]),
		needs_hard_to_access_inputs_ = yn(r2$input_diff_access[keep_nak]),
		improves_women_product_ = yn(r2$trait_inc_qlnty[keep_nak]),
		reduces_women_benefit_control_ = yn(r2$Affect_ctrl_benefit[keep_nak]),
		gender_impact_notes_ = trimws(as.character(r2$No_explain[keep_nak]))
	)

	# 6. Household livestock (standard animal + heads in long table)
	make_animal_block <- function(hhid, animal, heads) {
		heads <- as.numeric(heads)
		keep <- !is.na(heads)
		if (!any(keep)) return(NULL)
		data.frame(
			hhid = hhid[keep],
			animal = animal,
			heads = heads[keep]
		)
	}
	blocks[[length(blocks) + 1L]] <- make_animal_block(hhid_embu, "cattle", r3$cattle)
	blocks[[length(blocks) + 1L]] <- make_animal_block(hhid_embu, "goat;sheep;donkey", r3$goat_sheep)
	blocks[[length(blocks) + 1L]] <- make_animal_block(hhid_embu, "poultry", r3$poultry)
	blocks[[length(blocks) + 1L]] <- make_animal_block(hhid_nakuru, "cattle", r2$Cattle_owned)
	blocks[[length(blocks) + 1L]] <- make_animal_block(hhid_nakuru, "sheep", r2$Sheep_owned)
	blocks[[length(blocks) + 1L]] <- make_animal_block(hhid_nakuru, "donkey", r2$Donkey_owned)
	blocks[[length(blocks) + 1L]] <- make_animal_block(hhid_nakuru, "chicken", r2$Chicken)

	blocks <- Filter(Negate(is.null), blocks)
	dlong <- do.call(carobiner::bindr, blocks)
	dlong <- dlong[order(as.integer(dlong$hhid), dlong$rank_variable, dlong$season, dlong$rank_score, dlong$animal), ]
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long = dlong)
}
