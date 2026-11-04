# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI/BIDS "Livelihood Systems in Bangladesh" panel, 2008 round (62 villages).
# Cost&Return08.xls (and the other data files) are Excel 2.1 (BIFF2) worksheets that readxl cannot open; parsed with a small base-R record reader (read_biff2 below).
# d is Cost&Return08: one row per household x parcel x season crop with inputs, production (kg) and price. Empty rows (no sample/land) dropped.
# land is in ha (codebook); yield = prdkg / land.
# Crop: "crop" code (1-19; MV rice, Indian MV, hybrid, TV rice are rice), rice variety from vrtyrc; labels from the codebook "Documentation of 2008_Bangladesh.xls" (CostRet_value).
# Fertilizer (kg per parcel, codebook): urea (46% N), TSP (19.23% P), MOP (KCl, 49.8% K) -> elemental N, P, K in kg/ha.
# Seedling age at transplanting (agsedlng, days) -> transplanting_days. Costs (Tk) per ha -> BDT/ha. Labour = family + hired person-days (ploughing, transplanting, weeding, harvesting, threshing) per ha.
# Damage dmg1/dmg2 -> stress; pest types -> pest_species_ (labels).
# zila = adm2 (spellings adjusted to GADM), upazila = adm3, village = location.

## ISSUES
# No coordinates; 62 villages need georeferencing; upazila spellings not checked against GADM. No union labels in the 2008 codebook (adm4 not set).
# Transplanting week/month (trnsw, trnsm) and harvest week/month (hrvw, hrvm, undocumented "<none>") have no year; kept as new variables, no planting_date.
#  The months are Bengali calendar months (1 = Baishakh, from mid-April): boro is transplanted in 9-10 (Poush/Magh) and harvested in 1-2, aman transplanted in 4-5 and harvested in 8 (Agrahayan). Converting to dates needs the crop year (2007/08?). Some months > 12 (17, 19, 59, 60, 112) are errors.
# Seed quantity sdq has no unit in the 2008 codebook; assumed kg as in 2000 (seed_rate kg/ha). Check.
# Blank urea/TSP/MOP cells are NA; could be 0 (not applied) or missing. Not resolved.
# Production kg appears to be maunds x 40 + kg (Parcel08 prdbr1 = prdbr1m * 40 + prdbr1k), i.e. a 40 kg maund rather than 37.32 kg; yields may be ~7% high. Check.
# Crop price crpprc is Tk per maund (codebook "Tk/m"); kept as crop_price_maund_ because the maund mass used is unclear.
# qdmgk (crop loss) is undocumented; assumed kg and kept as crop_loss_ (kg/ha). Season code 6 (1 row) is undocumented (season NA).
# Parcel08.xls (all parcels, production by season, soil, land type, irrigation, tenure) has no codebook; not mapped. 99% of Cost&Return rows match a Parcel08 parcel (Sample + plotdid).
# Crop codes 7 "Potato/Sweet potato", 11 "Mixed Rabi crops", 12 "Others" have no single crop (crop NA, label in crop_label_). "Indian MV" and "Hybrid" rice: variety_type "modern"/"hybrid".
# Unmapped: HHDataY08 (household aggregates, income, land), Member08, Housing_Stock_Exp08, pump ownership (irgpump, nopump, price), by-product values.
# write_files: no coordinates; season "aus" not in vocabulary; 4 fully duplicated rows in the source; out of bounds yield (741 t/ha, crop "Others"), seed_rate (13 t/ha), K_fertilizer (1076 kg/ha), transplanting_days (90) left as is, check.

## NEW VARIABLES
# crop_label_: crop label from the codebook (e.g. "MV Rice", "Indian MV", "Pulses")
# transplanting_week_: week of the month of transplanting (1-4)
# transplanting_month_: Bengali calendar month of transplanting (1 = Baishakh, mid-April to mid-May), year not given
# harvest_week_: week of the month of harvest (1-4; undocumented)
# harvest_month_: Bengali calendar month of harvest (undocumented; 1 = Baishakh), year not given
# seedlings_per_hill_: number of seedlings per hill (bundle), coded classes "1", "2/3", "3/4", "4/5", "5+"
# crop_price_maund_: price of the crop, BDT per maund
# crop_loss_: crop loss due to damage, kg/ha
# pest_species_: labelled insect/pest causing damage


carob_script <- function(path) {

"Livelihood Systems in Bangladesh (2008)

a)  Understand the operation of the Bangladesh rural household economy;  b) Analyze the drivers of change in income distribution and poverty;  c)  Prepare technical background papers to facilitating policy dialogues under the project 'Poverty Elimination Through Rice Research Assistance (PETRRA)' in Bangladesh;  d) Assess the impact of rice research on poverty reduction in Bangladesh using the Sustainable Livelihood Framework; and  e)  Estimating parameters on the determinants of rural household incomes for use in poverty mapping for Bangladesh."

	uri <- "doi:10.7910/DVN/26207"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "PETRRA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 45,
		carob_effort = 0.8
	)

	# Excel 2.1 (BIFF2) worksheet reader: INTEGER, NUMBER, LABEL, BOOLERR and FORMULA cell records
	read_biff2 <- function(f) {
		b <- readBin(f, "raw", file.size(f))
		bi <- as.integer(b)
		n <- length(bi)
		pos <- integer(n %/% 8L)
		ids <- integer(n %/% 8L)
		p <- 1L
		k <- 0L
		while (p + 3L <= n) {
			id <- bi[p] + 256L * bi[p + 1L]
			k <- k + 1L
			pos[k] <- p + 4L
			ids[k] <- id
			if (id == 10L) break
			p <- p + 4L + bi[p + 2L] + 256L * bi[p + 3L]
		}
		cell <- ids[1:k] %in% 2:6
		pos <- pos[1:k][cell]
		ids <- ids[1:k][cell]
		u16 <- function(i) bi[i] + 256L * bi[i + 1L]
		rows <- u16(pos) + 1L
		cols <- u16(pos + 2L) + 1L
		num <- rep(NA_real_, length(pos))
		chr <- rep(NA_character_, length(pos))
		i <- ids == 2L
		num[i] <- u16(pos[i] + 7L)
		i <- which(ids %in% c(3L, 6L))
		if (length(i) > 0) {
			idx <- as.vector(outer(0:7, pos[i] + 7L, "+"))
			num[i] <- readBin(b[idx], "double", n=length(i), size=8, endian="little")
		}
		for (j in which(ids == 4L)) {
			nc <- bi[pos[j] + 7L]
			chr[j] <- if (nc > 0) rawToChar(b[(pos[j] + 8L):(pos[j] + 7L + nc)]) else ""
		}
		nr <- max(rows)
		h <- rows == 1L
		hdr <- paste0("V", 1:max(cols))
		hdr[cols[h]] <- chr[h]
		out <- lapply(seq_along(hdr), function(j) {
			i <- which(cols == j & rows > 1L)
			if (all(is.na(chr[i]))) {
				v <- rep(NA_real_, nr - 1L)
				v[rows[i] - 1L] <- num[i]
			} else {
				v <- rep(NA_character_, nr - 1L)
				v[rows[i] - 1L] <- ifelse(is.na(chr[i]), as.character(num[i]), chr[i])
			}
			v
		})
		names(out) <- hdr
		as.data.frame(out, check.names=FALSE)
	}

	fdoc <- ff[basename(ff) == "Documentation of 2008_Bangladesh.xls"]
	# value labels from the codebook: variable name (filled down), code, label
	codes <- function(sheet, var) {
		x <- carobiner::read.excel(fdoc, sheet=sheet, col_names=FALSE)
		nm <- x[[1]]
		nm <- nm[!is.na(nm)][cumsum(!is.na(nm))]
		k <- nm %in% var & !is.na(x[[4]])
		data.frame(code=as.numeric(x[[2]][k]), label=trimws(x[[4]][k]))
	}
	lab <- function(x, cds) {
		cds$label[match(x, cds$code)]
	}

	r <- read_biff2(ff[basename(ff) == "Cost&Return08.xls"])
	r <- r[!is.na(r$Sample) & !is.na(r$land), ]

	# "crop" codes (CostRet_value)
	crops <- data.frame(code=1:19,
		crop = c("wheat", "maize", "jute", "sugarcane", "oilseed", "pulse", NA, "onion", "spices", "vegetable", NA, NA, "fish", "prawn", "tobacco", "rice", "rice", "rice", "rice"),
		variety_type = c(rep(NA, 15), "modern", "modern", "hybrid", "traditional"),
		yield_part = c("grain", "grain", NA, "stems", "seed", "seed", NA, NA, NA, NA, NA, NA, NA, NA, "leaves", "grain", "grain", "grain", "grain")
	)
	stress <- c("pests", "flood", "drought", "hail", "wind", "other")
	ha <- r$land

	d <- data.frame(
		hhid = as.character(r$Sample),
		field_id = as.character(r$plotdid),
		season = c("boro", "rabi", "aus", "aman")[match(r$season, 1:4)],
		crop_label_ = lab(r$crop, codes("CostRet_value", "crop")),
		crop = crops$crop[match(r$crop, crops$code)],
		variety_type = crops$variety_type[match(r$crop, crops$code)],
		yield_part = crops$yield_part[match(r$crop, crops$code)],
		variety = lab(r$vrtyrc, codes("CostRet_value", "vrtyrc")),
		field_size = ha,
		yield = r$prdkg / ha,
		transplanting_week_ = r$trnsw,
		transplanting_month_ = r$trnsm,
		harvest_week_ = r$hrvw,
		harvest_month_ = r$hrvm,
		transplanting_days = r$agsedlng,
		seedlings_per_hill_ = lab(r$nosbunch, codes("CostRet_value", "nosbunch")),
		land_prep_times = as.integer(r$pln),
		seed_rate = r$sdq / ha,
		N_fertilizer = r$urq * 0.46 / ha,
		P_fertilizer = r$phq * 0.1923 / ha,
		K_fertilizer = r$mopq * 0.498 / ha,
		irrigation_source = lab(r$irg, codes("CostRet_value", "irg")),
		weeding_times = as.integer(r$wdn),
		insecticide_times = as.integer(r$ppn),
		labour = rowSums(r[, c("flpl", "hlpl", "flsd", "hlsd", "flwd", "hlwd", "flh", "hlh", "flt", "hlt")], na.rm=TRUE) / ha,
		land_prep_cost = rowSums(r[, c("plcsth", "ptcst")], na.rm=TRUE) / ha,
		seed_cost = r$sdcst / ha,
		fertilizer_cost = rowSums(r[, c("urcst", "phcst", "mopcst", "ofcst")], na.rm=TRUE) / ha,
		OM_cost = r$mncst / ha,
		irrigation_cost = r$irgcsth / ha,
		herbicide_cost = r$hrbcst / ha,
		insecticide_cost = r$inscst / ha,
		crop_price_maund_ = r$crpprc,
		currency = "BDT",
		stress = stress[match(r$dmg1, 1:6)],
		crop_loss_ = r$qdmgk / ha,
		pest_species_ = lab(r$ptyp1, codes("CostRet_value", "ptyp1")),
		hh_size = r$hhsize,
		location = lab(r$village, codes("CostRet_value", "village")),
		adm3 = lab(r$Uz, codes("CostRet_value", "Uz")),
		adm2 = lab(r$Zilla, codes("CostRet_value", "Zilla"))
	)

	s2 <- stress[match(r$dmg2, 1:6)]
	d$stress <- ifelse(is.na(s2), d$stress, ifelse(is.na(d$stress), s2, paste(d$stress, s2, sep=";")))
	p2 <- lab(r$ptyp2, codes("CostRet_value", "ptyp2"))
	d$pest_species_ <- ifelse(is.na(p2), d$pest_species_, ifelse(is.na(d$pest_species_), p2, paste(d$pest_species_, p2, sep="; ")))

	ftype <- cbind(ifelse(is.na(r$urq), NA, "urea"), ifelse(is.na(r$phq), NA, "TSP"), ifelse(is.na(r$mopq), NA, "KCl"))
	d$fertilizer_type <- apply(ftype, 1, function(x) paste(x[!is.na(x)], collapse="; "))
	d$fertilizer_type[d$fertilizer_type == ""] <- NA

	d$irrigated <- d$irrigation_source != "No irrigation"
	d$irrigation_source[d$irrigation_source == "No irrigation"] <- "none"

	gadm <- c("Brahmman Baria"="Brahamanbaria", "Moulovi-Bazar"="Maulvibazar", "Lakhmipur"="Lakshmipur", "Jhalakhati"="Jhalokati", "Perojpur"="Pirojpur", "Jhenaidaha"="Jhenaidah", "Kustia"="Kushtia", "Nababganj"="Nawabganj", "Panchaghar"="Panchagarh", "Netrokona"="Netrakona", "Munsiganj"="Munshiganj")
	i <- d$adm2 %in% names(gadm)
	d$adm2[i] <- gadm[d$adm2[i]]
	d$country <- "Bangladesh"

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
