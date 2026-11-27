# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Coordinates are not in the raw file (CKAN coverage point 2.3232 N, 11.43653 E is far south of these towns). Approximate coordinates for Batchenga (Ndjie, Okongolo) and Mbalmayo (Avebe, Memiam) were geocoded; verify village-level GPS.
# 2. No yield data in this file (yield components are in companion dataset doi:10.25502/zsnk-kk83/d).
# 3. Fertilizer rates (N/P/K) are not reported, and fertilizer status is unknown for the 6 local-innovation pest control treatments (fertilizer_used = NA).
# 4. Application rates/schedules for GREMEC (emamectin benzoate), OMO detergent, Tithonia diversifolia, Thevetia neriifolia, wood ash, and soil treatment are not stated.
# 5. FAWSev, SBSev, and LeafFolderSev range 0-100 (appearing to be % foliar/damage severity) but the exact rating scale is not documented.
# 6. Planting dates are back-calculated from Sampling_date minus WAP/WAS x 7; confirm against trial records.
# 7. 1,837 rows share identical location/field/year/season/date/WAP/treatment/variety/block/method/plant keys (461 exact duplicates excluding ID); kept as recorded pending verification.

## NEW
# agroecosystem_: agroecological zone ("Forest" or "Transition forest-savannah")
# pest_control_method_: FAW control technology applied ("emamectin benzoate (GREMEC)", "OMO detergent", "Tithonia diversifolia leaf", "Thevetia neriifolia leaf", "soil application", "wood ash", "none")
# sampling_method_: plant sampling method ("Destructive" or "Non-destructive")
# plant_number_: plant sequential number within plot/sampling method (1-8 destructive, 9-30 non-destructive)
# insect_stage_: pest life stage or damage observation ("egg mass", "larva", "pupa", "damage")
# organism_role_: ecological role ("pest", "parasitoid", "predator")
# pest_species values not in terminag: leaf folder, cricket

## MISSING
# yield, yield_part, yield_moisture, irrigated, harvest_date, N_fertilizer, P_fertilizer, K_fertilizer

## NOTES
# Plant-level counts of fall armyworm (Spodoptera frugiperda), stem borers, leaf folders, crickets,
#   and natural enemies (parasitoids and predators) on maize in on-farm trials at Batchenga (Ndjie,
#   Okongolo) and Mbalmayo (Avebe, Memiam), Centre Region, Cameroon (2020 season 2 and 2021 season 1).
# Trailing 29,018 empty rows (from Excel 65,535-row export) are removed, leaving 36,517 plant records.
# d holds plant-date records; dd (long) holds all pest counts/severities (pest_species, pest_incidence,
#   pest_severity) and natural-enemy counts (insect_species, insect_abundance) linked by record_id.
# GREMEC active ingredient is emamectin benzoate (per IITA Cameroon FAW trials, e.g. doi:10.1093/jee/toae102).
# Variety names "CMS8704-fertilized" / "CMS8704-unfertilized" are normalized to "CMS8704" with fertilizer_used set.

## NOT PROCESSED
# ID: sequential row number; 29,018 trailing empty rows.


carob_script <- function(path) {

"
Efficacy and cost-effectiveness of different technologies against Spodoptera frugiperda (Lepidoptera: Noctuidae) in the Tropical humid forest of Cameroon

Assess the field biophysical efficacy of different products against FAW and determine the cost-effectiveness of different technologies;
"

	uri <- "doi:10.25502/h02h-bq18/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "CGIAR Research Program on Maize: Coordinating elucidation of science and effectiveness of Local Innovations for Managing Fall armyworm in Africa",
		design = "RCBD; 4-5 blocks on-farm at 2 sites each in Batchenga and Mbalmayo; FAW control technologies and fertilized vs unfertilized maize varieties; destructive (8 plants) and non-destructive (22 plants) sampling at 4, 6, 8, and 11 WAP",
		data_type = "on-farm experiment",
		treatment_vars = "variety;insecticide_used;pest_control_method_",
		response_vars = "pest_incidence;pest_severity;insect_abundance",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "local_innovation_data.csv"]
	r <- read.csv(f, check.names = FALSE, fileEncoding = "latin1")
	r <- r[!is.na(r$Treatment) & nzchar(trimws(r$Treatment)), ]

	# Parse mixed "dd/mm/yyyy" and "dd-Mon-yy" sampling dates without locale dependence
	mon <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06",
		jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")
	rawd <- trimws(r$Sampling_date)
	date <- rep(NA_character_, length(rawd))
	i <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", rawd)
	date[i] <- as.character(as.Date(rawd[i], "%d/%m/%Y"))
	j <- grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2}$", rawd)
	parts <- strsplit(rawd[j], "-", fixed = TRUE)
	dday <- sprintf("%02d", as.integer(vapply(parts, `[`, "", 1)))
	dmon <- unname(mon[tolower(vapply(parts, `[`, "", 2))])
	dyy <- paste0("20", vapply(parts, `[`, "", 3))
	date[j] <- paste(dyy, dmon, dday, sep = "-")

	# Convert WAP/WAS (4, 6, 8, 11) to DAP and derive planting date
	w <- toupper(trimws(r$WAP))
	dap <- as.integer(sub("W(AP|AS)$", "", w)) * 7L
	planting <- as.character(as.Date(date) - dap)

	tr <- trimws(r$Treatment)
	va <- trimws(r$Variety)
	va <- sub("-(un)?fertilized$", "", va, ignore.case = TRUE)

	both <- paste(tr, trimws(r$Variety))
	unfert <- grepl("unfertilized|without fertilizer", both, ignore.case = TRUE)
	fert <- grepl("fertilized|with fertilizer", both, ignore.case = TRUE) & !unfert
	fertilizer_used <- rep(NA, length(tr))
	fertilizer_used[unfert] <- FALSE
	fertilizer_used[fert] <- TRUE

	pcm <- rep("none", length(tr))
	pcm[tr == "Insecticide-GREMEC"] <- "emamectin benzoate (GREMEC)"
	pcm[tr == "Detergent-OMO"] <- "OMO detergent"
	pcm[tr == "Leaf of T. diversifolia"] <- "Tithonia diversifolia leaf"
	pcm[tr == "Leaf of T. neriifolia"] <- "Thevetia neriifolia leaf"
	pcm[tr == "Soil-treatment"] <- "soil application"
	pcm[tr == "Wood ash-treatment"] <- "wood ash"

	loc <- carobiner::fix_name(trimws(r$Loc), "title")
	site <- carobiner::fix_name(trimws(r$Field), "title")
	year <- as.character(r$Year)
	season <- ifelse(r$Cropping_season == 1, "first",
		ifelse(r$Cropping_season == 2, "second", NA_character_))

	# Georeference Batchenga (Lekié) and Mbalmayo (Nyong-et-So'o) sites in Centre Region, Cameroon;
	# Ndjie/Okongolo use Batchenga town coords (4.283 N, 11.650 E, ~10 km uncertainty);
	# Avebe uses Mbalmayo town coords (3.517 N, 11.500 E, ~10 km); Memiam uses village coords (3.467 N, 11.600 E, ~5 km).
	geo <- data.frame(
		site = c("Ndjie", "Okongolo", "Avebe", "Memiam"),
		adm2 = c("Lekié", "Lekié", "Nyong-et-So'o", "Nyong-et-So'o"),
		latitude = c(4.283, 4.283, 3.517, 3.467),
		longitude = c(11.650, 11.650, 11.500, 11.600),
		geo_uncertainty = c(10000, 10000, 10000, 5000),
		geo_source = c("Batchenga commune centroid", "Batchenga commune centroid",
			"Mbalmayo commune centroid", "Memiam village"),
		stringsAsFactors = FALSE
	)
	gm <- match(site, geo$site)

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = paste(loc, site, year, season, sep = "_"),
		country = "Cameroon",
		adm1 = "Centre",
		adm2 = geo$adm2[gm],
		location = loc,
		site = site,
		latitude = geo$latitude[gm],
		longitude = geo$longitude[gm],
		geo_uncertainty = geo$geo_uncertainty[gm],
		geo_source = geo$geo_source[gm],
		geo_from_source = FALSE,
		agroecosystem_ = trimws(r$Agroecosystem),
		planting_date = planting,
		date = date,
		DAP = dap,
		season = season,
		crop = "maize",
		variety = va,
		treatment = tr,
		fertilizer_used = fertilizer_used,
		insecticide_used = tr == "Insecticide-GREMEC",
		# GREMEC 050 WG active ingredient is emamectin benzoate
		insecticide_product = ifelse(tr == "Insecticide-GREMEC", "emamectin benzoate", "none"),
		pest_control_method_ = pcm,
		block_id = as.character(r$Block),
		rep = as.integer(r$Block),
		sampling_method_ = trimws(r$method),
		growth_stage = trimws(r$Crop_Stage),
		plant_number_ = as.integer(r$Plant),
		stress = "pests",
		on_farm = TRUE,
		is_survey = FALSE
	)

	make_obs <- function(vals, species, stage, role, what) {
		keep <- !is.na(vals)
		n <- sum(keep)
		data.frame(
			record_id = d$record_id[keep],
			pest_species = if (role == "pest") rep(species, n) else rep(NA_character_, n),
			insect_species = if (role != "pest") rep(species, n) else rep(NA_character_, n),
			insect_stage_ = rep(stage, n),
			organism_role_ = rep(role, n),
			pest_incidence = if (what == "count") as.integer(vals[keep]) else rep(NA_integer_, n),
			pest_severity = if (what == "severity") as.character(vals[keep]) else rep(NA_character_, n),
			insect_abundance = if (what == "beneficial") as.character(vals[keep]) else rep(NA_character_, n)
		)
	}

	dd <- rbind(
		make_obs(r$NbEggmassFAW, "Spodoptera frugiperda", "egg mass", "pest", "count"),
		make_obs(r$FAWLar, "Spodoptera frugiperda", "larva", "pest", "count"),
		make_obs(r$FAWPup, "Spodoptera frugiperda", "pupa", "pest", "count"),
		make_obs(r$FAWSev, "Spodoptera frugiperda", "damage", "pest", "severity"),
		make_obs(r$NBEggmassSB, "stem borer", "egg mass", "pest", "count"),
		make_obs(r$SBLar, "stem borer", "larva", "pest", "count"),
		make_obs(r$SBPup, "stem borer", "pupa", "pest", "count"),
		make_obs(r$SBSev, "stem borer", "damage", "pest", "severity"),
		make_obs(r$NbLeafFolder, "leaf folder", "larva", "pest", "count"),
		make_obs(r$LeafFolderSev, "leaf folder", "damage", "pest", "severity"),
		make_obs(r$Cricket, "cricket", NA_character_, "pest", "count"),
		make_obs(r$Charops_sp, "Charops sp.", "larva", "parasitoid", "beneficial"),
		make_obs(r$Cotesia_icipe, "Cotesia icipe", "larva", "parasitoid", "beneficial"),
		make_obs(r$Coccygidium_luteum, "Coccygidium luteum", "larva", "parasitoid", "beneficial"),
		make_obs(r$Other_Parasitoids, "other parasitoids", NA_character_, "parasitoid", "beneficial"),
		make_obs(r$Predatory_wasps, "predatory wasp", NA_character_, "predator", "beneficial"),
		make_obs(r$Ear_wigs, "earwig", NA_character_, "predator", "beneficial"),
		make_obs(r$Ants, "ant", NA_character_, "predator", "beneficial"),
		make_obs(r$Predatory_beetle, "predatory beetle", NA_character_, "predator", "beneficial")
	)

	carobiner::write_files(path, meta, d, long = dd)
}
