# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Latitude in Facteurs is unsigned (positive) and is negated for Madagascar
#    (southern hemisphere). Longitude_habitation and Latitude_habitation are
#    swapped in the source and unused because exact plot coordinates exist.
# 2. soil_pH in Facteurs contains Excel-corrupted 5-to-7-digit values on 92
#    columns, whereas the Enherbement_paysans sheet holds clean pH values
#    (4.57-5.68) for all 544 surveys; soil_pH is read from Enherbement_paysans.
# 3. Four surveys have "40cm" entered in Traitement de semences culture 1 (a row
#    spacing entry misplaced into seed treatment); these are set to NA.
# 4. Date de deuxieme desherbage contains invalid entries ("02/29/15", "0.0", and
#    Excel serials 37312/37314 = year 2002); those dates are excluded from
#    weeding_dates.
# 5. Floristique uses code STRLU where Enherbement_paysans uses STRAS (Striga
#    asiatica, syn. S. lutea); STRLU is mapped to Striga asiatica.

## MISSING
# yield, yield_part, yield_moisture, harvest_date, N_fertilizer, P_fertilizer,
# K_fertilizer (weed community survey; nutrient rates not reported).

## NOTES
# 544 weed surveys (136 farmer plots x 4 dates, D1-D4) covering 79 weed species
# in upland rice-based systems in Mandoto district, Vakinankaratra, Madagascar
# (2014-2015; MSc thesis Rakotonirina 2015, FOFIFA/CIRAD). Wide table d holds
# plot-date environmental and management factors; long holds species-level cover
# (%) converted from the 1-9 abundance scale with full botanical names from
# Enherbement_paysans. plant_density is recorded in hills (poquets) per ha.

## NOT PROCESSED
# Facteurs: farmer names, Type_roche, Nombres_satellites, Munsell hue/value/chroma
# classes, Cailloux, Age_parcelle, Couverture_sol (one Excel-date anomaly 44007),
# raw straw sample weights (Poids couverture t/ha mapped to mulch), culture 2
# density/variety/spacing/seed treatment, weeding labor days (Hj/ha), Richesse (all 0).
# Redundant exports duplicating the workbook: MAD-RAK-2015-RIZ-AD-FLO.txt,
# MAD-RAK-2015-RIZ-FAC.txt, MAD-RAK-2015-RIZ-PA-FLO.txt, MAD-RAK-2015-RIZ-AD-DOC.txt.

carob_script <- function(path) {

"
Weed survey in upland rice cropping system in Madagascar (2015)

Characterization of weed communities in rain-fed rice-based cropping systems in the Middle West area of Vakinankaratra in Madagascar. Abundance scale notation 1-9. Intership within DP SPAD (FOFIFA - Cirad). This dataset covers 544 plant surveys for 79 weed species.
"

	uri <- "doi:10.18167/DVN1/NQYPEY"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
		data_organization = "CIRAD;FOFIFA",
		publication = NA,
		project = "DP SPAD;STRADIV",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_cover;weed_species;weed_species_cover",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "MAD-RAK-2015-RIZ-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Facteurs", col_types = "text")
	r2 <- carobiner::read.excel(f, sheet = "Floristique")
	r3 <- carobiner::read.excel(f, sheet = "Enherbement_paysans", col_types = "text")

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))
	get_ep <- function(label) unname(unlist(r3[r3[[1]] == label, 4:ncol(r3)]))

	# 1-9 abundance scale to % cover (MAD-RAK-2015-RIZ-AD-DOC.txt)
	cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	# Split slash-separated intercrops into main crop and intercrops
	crop_tok <- c(
		"arachide" = "groundnut",
		"mais" = "maize",
		"manioc" = "cassava",
		"niebe" = "cowpea",
		"pois de terre" = "bambara groundnut",
		"riz" = "rice",
		"soja" = "soybean"
	)
	crop_src <- tolower(trimws(get_row("Culture_origine")))
	crop_parts <- strsplit(crop_src, "/", fixed = TRUE)
	crop <- unname(crop_tok[vapply(crop_parts, `[`, character(1), 1)])
	intercrops <- vapply(crop_parts, function(p) {
		if (length(p) < 2) return(NA_character_)
		paste(unname(crop_tok[p[-1]]), collapse = ";")
	}, character(1))

	# Previous crop mapping (+ separated combinations)
	prev_tok <- c(
		"arachide" = "groundnut", "mais" = "maize", "manioc" = "cassava",
		"niebe" = "cowpea", "riz" = "rice", "soja" = "soybean",
		"pois de terre" = "bambara groundnut", "jachere" = "none",
		"stylo" = "stylosanthes", "muc" = "velvet bean", "mucuna" = "velvet bean",
		"jachere stylo" = "stylosanthes", "stylo jachere" = "stylosanthes"
	)
	prev_src <- tolower(trimws(get_row("Precedent")))
	previous_crop <- vapply(prev_src, function(x) {
		if (is.na(x) || !nzchar(x)) return(NA_character_)
		x <- gsub("\\s+", " ", gsub("\\s*\\+\\s*", "+", x))
		parts <- trimws(strsplit(x, "+", fixed = TRUE)[[1]])
		paste(unique(unname(prev_tok[parts])), collapse = ";")
	}, character(1), USE.NAMES = FALSE)

	# Helper to parse valid Excel serial dates (2010-2016)
	parse_excel_date <- function(x) {
		out <- rep(NA_character_, length(x))
		ok <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
		if (any(ok)) {
			num <- as.numeric(x[ok])
			valid <- num >= 40000 & num <= 43000
			idx <- which(ok)[valid]
			out[idx] <- as.character(as.Date(num[valid], origin = "1899-12-30"))
		}
		out
	}

	wd1 <- parse_excel_date(get_row("Date de premier desherbage"))
	wd2 <- parse_excel_date(get_row("Date de deuxieme desherbage"))
	wd3 <- parse_excel_date(get_row("Date de troisieme desherbage"))
	weeding_dates <- vapply(seq_along(wd1), function(i) {
		dd <- na.omit(c(wd1[i], wd2[i], wd3[i]))
		if (!length(dd)) NA_character_ else paste(dd, collapse = "; ")
	}, character(1))

	# Management mappings
	seed_trt_raw <- tolower(trimws(get_row("Traitement de semences culture 1")))
	seed_treatment <- ifelse(seed_trt_raw %in% c("insector", "insctor"), "insector",
		ifelse(seed_trt_raw == "non", "none", NA_character_))

	om_raw <- tolower(trimws(get_row("Fumier(non-bovin-porc-porc+bovin)")))
	om_map <- c(
		"non" = "none",
		"bovin" = "cattle dung",
		"bovin+porc" = "cattle dung;animal dung",
		"compost" = "compost",
		"porc + compost" = "animal dung;compost"
	)
	om_type <- unname(om_map[om_raw])

	fert_raw <- trimws(get_row("Engrais (date-type)"))
	fert_used <- fert_raw == "NPK"

	lab_raw <- tolower(trimws(get_row("Labour (non-angady-charrue)")))
	lab_map <- c("non" = "none", "charrue" = "ploughing", "charrue/angady" = "ploughing;hoeing")

	herb_raw <- tolower(trimws(get_row("Avant semis Herbicide type et dose")))
	herb_used <- herb_raw == "glyphosat"

	topo_raw <- tolower(trimws(get_row("Position_topographique")))
	topo_map <- c(
		"haut pente" = "upper slope",
		"haut plat" = "upland flat",
		"milieu pente" = "middle slope",
		"milieu plat" = "middle flat"
	)

	mulch_raw <- trimws(get_row("Type_residus_culture"))
	mulch_type <- ifelse(tolower(mulch_raw) == "non", "none", mulch_raw)

	commune <- trimws(get_row("Commune"))
	wtimes <- as.integer(as.numeric(get_row("Nombres d`intervention de sarclage")))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	irr <- tolower(trimws(get_row("Irrigation")))

	d <- data.frame(
		trial_id = names(r1)[-1],
		hhid = as.character(as.integer(as.numeric(get_row("Numero_agriculteur")))),
		plot_id = trimws(get_row("Code_parcelle")),
		plot_area = as.numeric(get_row("Surface_parcelle(m2)")),
		date = parse_excel_date(get_ep("Date de notation parcelle")),
		planting_date = parse_excel_date(get_row("Date_semis")),
		DAP = as.integer(as.numeric(get_ep("JAS"))),
		country = "Madagascar",
		adm1 = "Vakinankaratra",
		adm2 = "Mandoto",
		adm3 = ifelse(commune == "Ankazo", "Ankazomiriotra", commune),
		location = trimws(get_row("Lieu")),
		longitude = as.numeric(get_row("Longitude")),
		latitude = -as.numeric(get_row("Latitude")),
		elevation = as.numeric(get_row("Altitude")),
		landscape_position = unname(topo_map[topo_raw]),
		land_slope = round(atan(as.numeric(get_row("Pente_%")) / 100) * 180 / pi, 2),
		soil_type = trimws(get_row("Nom_malagasy_sol")),
		soil_color = trimws(get_row("Couleur_code_Munsell_sec")),
		soil_pH = as.numeric(get_ep("PH")),
		crop = crop,
		intercrops = intercrops,
		intercropped = !is.na(intercrops),
		variety = trimws(get_row("Variete culture 1")),
		row_spacing = as.numeric(get_row("Ecartement entre lignes culture 1(Cm)")),
		plant_density = as.numeric(get_row("Densite_culture_1_poq/ha")),
		seed_treatment = seed_treatment,
		previous_crop = previous_crop,
		CA_years = as.numeric(get_row("Age_semis_direct")),
		mulch = as.numeric(get_row("Poids couverture t/ha")) * 1000,
		mulch_type = mulch_type,
		OM_used = om_raw != "non",
		OM_type = om_type,
		fertilizer_used = fert_used,
		fertilizer_type = ifelse(fert_used, "NPK", "none"),
		land_prep_method = unname(lab_map[lab_raw]),
		herbicide_used = herb_used,
		herbicide_product = ifelse(herb_used, "glyphosate", "none"),
		weeding_done = wtimes > 0L,
		weeding_times = wtimes,
		weeding_dates = weeding_dates,
		irrigated = irr != "pluvial",
		irrigation_method = ifelse(irr == "pluvial", "none", NA_character_),
		weed_cover = cover[match(enh, 1:9)],
		stress = "weeds"
	)
	d$geo_from_source <- TRUE
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	## Species lookup from Enherbement_paysans (rows with EPPO codes in col 3, below header row)
	hdr_row <- which(r3[[3]] == "CODE EPPO")
	idx <- which(!is.na(r3[[3]]) & seq_len(nrow(r3)) > hdr_row)
	spp <- data.frame(
		code = trimws(r3[[3]][idx]),
		weed_species = trimws(r3[[1]][idx])
	)
	spp$weed_species[spp$weed_species == "cassia rotundifolia"] <- "Cassia rotundifolia"
	spp <- rbind(spp, data.frame(code = "STRLU", weed_species = "Striga asiatica"))

	## Species-level (long) records
	score <- as.matrix(r2[, -1])
	j <- which(!is.na(score) & score > 0, arr.ind = TRUE)
	codes <- trimws(as.character(r2[[1]][j[, "row"]]))
	sidx <- match(codes, spp$code)

	long <- data.frame(
		trial_id = colnames(score)[j[, "col"]],
		weed_species = spp$weed_species[sidx],
		weed_species_cover = cover[match(as.integer(score[j]), 1:9)]
	)

	carobiner::write_files(path, meta, d, long = long)
}
