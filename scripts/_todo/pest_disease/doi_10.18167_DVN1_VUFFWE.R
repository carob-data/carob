# R script for "carob"
# license: GPL (>=3)

## NOTES
# 476 plot surveys on Mayotte (2019-2020). One row per species with a rating > 0.
# Floristique and Enherbement are the DOC 1-9 notes. weed_species is the 5-letter
# code. French crop names are mapped where the whole label resolves. "qq", "un peu",
# "coupée", and a trailing mulch note are dropped. "avant X" is not treated as a
# current crop. Two trailing Floristique rows are empty and do not become records.

## ISSUES
# Labels that include ylang, a cucurbit family name, listea, pomme cannelle,
# bredes mamaftan, jardin mahorais, terrain nu, avocat ?, or "maïs ou sorgho"
# are left as written. No coordinates. Some Lieu spellings differ and are kept.
# write_files: missing variables: yield, yield_moisture, yield_part, latitude,
# longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# invalid terms: crop: Banane, Manioc, Curcubitacée, Agrumes; Bananier, Agrumes,
# Curcubitacée, Papayer, Manioc; Bananier, Ananas, Avocat ?; Bananier, Goyavier,
# Pomme cannelle; Bananier, Manioc, Agrumes, Ylang; Bredes mamaftan, salade;
# Cocotier, (Listea); Cucurbitacée; Curcubitacées, Ipomoea batatas; Jardin mahorais;
# Jardin Mahorais; Jardin mahorais, Ananas, Banane, Grenade; Maïs ou Sorgho,
# Embrevade, Manioc; Riz, Maïs, Listea; Salade, jardin mahorais; Terrain nu; Ylang;
# Ylang, Agrumes et Bananier; Ylang, Cocotier.


## NOT PROCESSED
# Facteurs not mapped: Num_Relevé, Période, Zone, Climat, Agriculteur, Saison
# (SP/SS), Parcelle, Type_culture, SdC, SdC regroupe. Mayotte 2019 2020 repeats
# the survey. Méta données, FLO, FAC, and PA repeat the workbook or the DOC.


carob_script <- function(path) {

"
Weed survey in different cropping systems in Mayotte island (2019-2020)

Study of weed communities composition and abundance in different cropping systems (tubers, orchards, pastures, food crops) of the Mayotte island. 2019-2020. Abundance scale notation 1-9. This dataset covers 476 plant surveys for 164 weed species.
"

	uri <- "doi:10.18167/DVN1/VUFFWE"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;ground_cover;weed_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "MAY-MAR-2020-DIV-AD.xlsx"]
	r1b <- carobiner::read.excel(f, sheet="Facteurs")
	r1c <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1b[r1b[[1]] == label, -1]))

	# MAY-MAR-2020-DIV-AD-DOC.txt: 1-9 note to % cover, same table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(
		agrumes = "citrus", ail = "garlic", ambrevade = "pigeon pea",
		embrevade = "pigeon pea", ananas = "pineapple", aubergine = "eggplant",
		"aubergine dépôt d'herbe" = "eggplant", banane = "banana",
		bananier = "banana", bananiers = "banana", basilic = "basil",
		brachiaria = "brachiaria", canne = "sugarcane",
		"canne fourragère" = "forage crop", "cannes fourragères" = "forage crop",
		chou = "cabbage", choux = "cabbage", citron = "lemon", coco = "coconut",
		cocotier = "coconut", concombre = "cucumber", courge = "pumpkin",
		courgette = "zucchini", curcuma = "turmeric", curcumin = "turmeric",
		fourrage = "forage crop", gingembre = "ginger", goyaver = "guava",
		goyavier = "guava", grenade = "pomegranate",
		"ipomoea batatas" = "sweetpotato", "jachère" = "none", jachere = "none",
		"jachère inondée" = "none", laitue = "lettuce", salade = "lettuce",
		"maïs" = "maize", mais = "maize", manguier = "mango", manioc = "cassava",
		oignon = "onion", orange = "orange", oranger = "orange",
		"oranger manioc" = "orange_cassava", papaye = "papaya", papayer = "papaya",
		papayers = "papaya", papayes = "papaya", "pastèque" = "watermelon",
		"patate douce" = "sweetpotato", "patates douces" = "sweetpotato",
		piment = "chili pepper", riz = "rice", taro = "taro", tarot = "taro",
		tomate = "tomato", tomates = "tomato",
		"tomate bientôt replantées" = "tomato", vanille = "vanilla"
	)

	map_culture <- function(x) {
		out <- rep(NA_character_, length(x))
		for (i in seq_along(x)) {
			one <- trimws(as.character(x[i]))
			if (is.na(one) || !nzchar(one)) next
			parts <- unlist(strsplit(tolower(one), "\\s*[,;+]\\s*|\\s+et\\s+"))
			parts <- trimws(gsub("[()]", "", parts))
			parts <- parts[nzchar(parts)]
			parts <- parts[!grepl("^avant ", parts)]
			parts <- sub("^qq\\s+", "", parts)
			parts <- sub("^un peu\\s+", "", parts)
			parts <- sub("^et\\s+", "", parts)
			parts <- sub("\\s+mulch$", "", parts)
			parts <- sub("\\s+coup[eé]e?s?$", "", parts)
			parts <- trimws(parts)
			parts <- parts[nzchar(parts)]
			if (length(parts) == 0) {
				out[i] <- one
				next
			}
			mapped <- unname(crop_map[parts])
			if (any(is.na(mapped))) {
				out[i] <- one
			} else {
				out[i] <- paste(unique(mapped), collapse="_")
			}
		}
		out
	}

	src <- as.character(get_row("Culture_origine"))
	crop <- map_culture(src)
	# Date is an Excel serial; the year matches Année.
	obs <- as.numeric(get_row("Date"))
	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	d1b <- data.frame(
		trial_id = names(r1b)[-1],
		crop = crop,
		date = as.character(as.Date(obs, origin="1899-12-30")),
		country = "Mayotte", # geographicCoverage; accepted country name
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	multi <- grepl("[,;+]| et ", tolower(trimws(src)))
	d1b$intercropped <- !is.na(d1b$crop) & (grepl("_", d1b$crop) | multi)
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE
	d1b$geo_from_source <- FALSE

	present <- which(!is.na(r1c[, -1]) & r1c[, -1] > 0, arr.ind=TRUE)
	scores <- as.integer(as.numeric(r1c[, -1][present]))
	d <- data.frame(
		trial_id = names(r1c)[-1][present[, "col"]],
		weed_species = trimws(as.character(r1c[[1]][present[, "row"]])),
		ground_cover = rating_to_cover[match(scores, 1:9)]
	)
	d <- merge(d, d1b, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
