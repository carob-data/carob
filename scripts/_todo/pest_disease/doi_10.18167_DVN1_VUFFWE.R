# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Coordinates are not in the source; village centroids in Mayotte were
#    georeferenced with 2 km uncertainty (4 spelling variants in `Lieu` were
#    harmonized: Dembeni, Mstamboro, Nymabadao, Tsarano).
# 2. Mixed-crop strings in `Culture_origine` list multiple crops without
#    designating a primary crop; the first listed crop is placed in `crop` and
#    remaining crops in `intercrops`. "Jardin mahorais" alone is mapped to
#    `crop = "unknown"` with `intercropped = TRUE`.
# 3. `on_farm` is set to FALSE for surveys labelled "Station" or "Lycée
#    agricole" in `Agriculteur`, and TRUE for all other fields.

## NEW
# weed_family_: botanical family of the weed species
# crop / intercrops values not in terminag: "ylang-ylang" (Cananga odorata),
#   "sugar apple" (Annona squamosa, "pomme cannelle"), "litsea" (Litsea glutinosa).

## MISSING
# yield, yield_moisture, yield_part, planting_date, harvest_date.

## NOTES
# 476 weed surveys across Mayotte (2019-2020) in food crops, orchards, market
# gardening, pineapple, pastures, and fallows. Wide table `d` holds plot-level
# factors; `long` holds 6,757 species x plot presence records with full Latin
# names and families from sheet "Mayotte 2019 2020" and cover (%) from the 1-9
# abundance scale.

## NOT PROCESSED
# Facteurs: Auteur, Num_Relevé, Période, Zone, Climat, Agriculteur (used only
# for `on_farm`), SdC, SdC regroupe (redundant with `Type_culture` -> `land_use`).
# Duplicate text exports (MAY-MAR-2020-DIV-AD-FLO.txt, -FAC.txt, -PA-FLO.txt)
# and documentation (MAY-MAR-2020-DIV-AD-DOC.txt, sheet "Méta données").


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
		project = "Amatrop",
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

	f <- ff[basename(ff) == "MAY-MAR-2020-DIV-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Facteurs")
	r2 <- carobiner::read.excel(f, sheet="Floristique")
	r3 <- carobiner::read.excel(f, sheet="Mayotte 2019 2020", col_names=FALSE)

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	## 1-9 abundance scale to % cover (MAY-MAR-2020-DIV-AD-DOC.txt)
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(
		agrumes = "citrus", ail = "garlic", ambrevade = "pigeon pea",
		embrevade = "pigeon pea", ananas = "pineapple", aubergine = "eggplant",
		"aubergine dépôt d'herbe" = "eggplant", avocat = "avocado",
		banane = "banana", bananier = "banana", bananiers = "banana",
		basilic = "basil", brachiaria = "brachiaria",
		"bredes mamaftan" = "vegetable", canne = "sugarcane",
		"canne fourragère" = "forage crop", "cannes fourragères" = "forage crop",
		chou = "cabbage", choux = "cabbage", citron = "lemon", coco = "coconut",
		cocotier = "coconut", concombre = "cucumber", courge = "pumpkin",
		courgette = "zucchini", cucurbitacée = "vegetable",
		curcubitacée = "vegetable", curcubitacées = "vegetable",
		curcuma = "turmeric", curcumin = "turmeric", fourrage = "forage crop",
		gingembre = "ginger", goyaver = "guava", goyavier = "guava",
		grenade = "pomegranate", "ipomoea batatas" = "sweetpotato",
		"jachère" = "none", jachere = "none", "jachère inondée" = "none",
		laitue = "lettuce", listea = "litsea", "maïs" = "maize", mais = "maize",
		"maïs ou sorgho" = "cereal", manguier = "mango", manioc = "cassava",
		oignon = "onion", orange = "orange", oranger = "orange",
		papaye = "papaya", papayer = "papaya", papayers = "papaya",
		papayes = "papaya", "pastèque" = "watermelon",
		"patate douce" = "sweetpotato", "patates douces" = "sweetpotato",
		piment = "chili pepper", "pomme cannelle" = "sugar apple",
		riz = "rice", salade = "lettuce", taro = "taro", tarot = "taro",
		"terrain nu" = "none", tomate = "tomato", tomates = "tomato",
		"tomate bientôt replantées" = "tomato", vanille = "vanilla",
		ylang = "ylang-ylang"
	)

	parse_crops <- function(src) {
		n <- length(src)
		main <- rep(NA_character_, n)
		inter <- rep(NA_character_, n)
		inter_flag <- rep(FALSE, n)
		prev <- rep(NA_character_, n)
		for (i in seq_len(n)) {
			one <- trimws(as.character(src[i]))
			if (is.na(one) || !nzchar(one)) next
			low <- tolower(one)
			low <- gsub("\\?", "", low)
			low <- gsub("oranger manioc", "oranger, manioc", low)
			parts <- unlist(strsplit(low, "\\s*[,;+]\\s*|\\s+et\\s+"))
			parts <- trimws(gsub("[()]", "", parts))
			parts <- parts[nzchar(parts)]
			is_prev <- grepl("^avant\\s+", parts)
			if (any(is_prev)) {
				p_tok <- sub("^avant\\s+", "", parts[is_prev])
				p_map <- unname(crop_map[p_tok])
				p_map <- unique(p_map[!is.na(p_map)])
				if (length(p_map)) prev[i] <- paste(p_map, collapse=";")
				parts <- parts[!is_prev]
			}
			is_jm <- parts == "jardin mahorais"
			if (any(is_jm)) {
				inter_flag[i] <- TRUE
				parts <- parts[!is_jm]
			}
			parts <- sub("^qq\\s+", "", parts)
			parts <- sub("^un peu\\s+", "", parts)
			parts <- sub("^et\\s+", "", parts)
			parts <- sub("\\s+mulch$", "", parts)
			parts <- sub("\\s+coup[eé]e?s?$", "", parts)
			parts <- trimws(parts)
			parts <- parts[nzchar(parts)]
			if (length(parts) == 0) {
				if (any(is_jm)) main[i] <- "unknown"
				next
			}
			mapped <- unique(unname(crop_map[parts]))
			mapped <- mapped[!is.na(mapped)]
			if (length(mapped) >= 1) main[i] <- mapped[1]
			if (length(mapped) >= 2) {
				inter[i] <- paste(mapped[-1], collapse=";")
				inter_flag[i] <- TRUE
			}
		}
		list(crop=main, intercrops=inter, intercropped=inter_flag, previous_crop=prev)
	}

	src <- as.character(get_row("Culture_origine"))
	cp <- parse_crops(src)

	loc <- trimws(as.character(get_row("Lieu")))
	loc_fix <- c(
		Dembeni = "Dembéni",
		Dzoumogne = "Dzoumogné",
		Mstamboro = "Mtsamboro",
		Nymabadao = "Nyambadao",
		Tsarano = "Tsararano",
		Vahibe = "Vahibé"
	)
	i_fix <- loc %in% names(loc_fix)
	loc[i_fix] <- unname(loc_fix[loc[i_fix]])

	geo <- data.frame(
		location = c(
			"Acoua", "Bambo-Ouest", "Bouyouni", "Coconi", "Combani", "Dapani",
			"Dembéni", "Dzoumogné", "Hajangua", "Hamjago", "Iloni", "Kahani",
			"Kani-Kéli", "Kwalé", "Malamani", "Miréréni", "Moinatrindri",
			"Mronabéja", "Mtsamboro", "Nyambadao", "Ouangani", "Poroani",
			"Tsararano", "Tsimkoura", "Tsingoni", "Vahibé"
		),
		latitude = c(
			-12.7225, -12.9243, -12.7377, -12.8343, -12.7903, -12.9714,
			-12.8461, -12.7163, -12.8661, -12.6890, -12.8450, -12.8237,
			-12.9569, -12.7975, -12.9192, -12.9071, -12.8932,
			-12.9794, -12.6998, -12.8895, -12.8464, -12.8925,
			-12.8355, -12.9370, -12.7872, -12.7906
		),
		longitude = c(
			45.0564, 45.0899, 45.1389, 45.1355, 45.1331, 45.1608,
			45.1775, 45.1163, 45.1986, 45.0758, 45.1906, 45.1296,
			45.1056, 45.1881, 45.1550, 45.1530, 45.0920,
			45.1239, 45.0689, 45.2013, 45.1361, 45.1417,
			45.1713, 45.1365, 45.1028, 45.1781
		)
	)

	lu_map <- c(
		arboriculture = "orchard",
		prairie = "pasture",
		maraichage = "market gardening",
		vivrier = "food crops",
		ananas = "pineapple",
		tubercule = "root and tuber crops",
		jachere = "fallow"
	)
	sais <- trimws(as.character(get_row("Saison")))
	agr <- trimws(as.character(get_row("Agriculteur")))
	enh <- as.integer(as.numeric(get_row("Enherbement")))

	d <- data.frame(
		trial_id = names(r1)[-1],
		plot_id = trimws(as.character(get_row("Parcelle"))),
		date = as.character(as.Date(as.numeric(get_row("Date")), origin="1899-12-30")),
		season = ifelse(sais == "SS", "dry", ifelse(sais == "SP", "wet", NA_character_)),
		country = "Mayotte",
		location = loc,
		crop = cp$crop,
		intercrops = cp$intercrops,
		intercropped = cp$intercropped,
		previous_crop = cp$previous_crop,
		land_use = unname(lu_map[tolower(trimws(as.character(get_row("Type_culture"))))]),
		flooded = grepl("inond", tolower(src)),
		irrigated = FALSE,
		irrigation_method = "none",
		weed_cover = rating_to_cover[match(enh, 1:9)],
		stress = "weeds",
		on_farm = !(agr %in% c("Station", "Lycée agricole")),
		is_survey = TRUE
	)

	d <- merge(d, geo, by="location", all.x=TRUE)
	d$geo_uncertainty <- 2000
	d$geo_source <- "GeoNames / Wikipedia (Mayotte village centroids)"
	d$geo_from_source <- FALSE

	## Species lookup (Latin name + family) from sheet "Mayotte 2019 2020"
	r2_sp <- r2[!is.na(r2[[1]]) & trimws(as.character(r2[[1]])) != "nb occurrences", ]
	codes_valid <- trimws(as.character(r2_sp[[1]]))
	k <- which(!is.na(r3[[3]]) & trimws(as.character(r3[[3]])) %in% codes_valid)
	spp <- data.frame(
		code = trimws(as.character(r3[[3]][k])),
		weed_species = trimws(as.character(r3[[1]][k])),
		weed_family_ = trimws(as.character(r3[[2]][k]))
	)
	spp <- spp[!duplicated(spp$code), ]

	score <- as.matrix(r2_sp[, -1])
	present <- which(!is.na(score) & score > 0, arr.ind=TRUE)
	long <- data.frame(
		trial_id = colnames(score)[present[, "col"]],
		code = codes_valid[present[, "row"]],
		weed_species_cover = rating_to_cover[match(as.integer(as.numeric(score[present])), 1:9)]
	)
	long <- merge(long, spp, by="code", all.x=TRUE)
	long$code <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
