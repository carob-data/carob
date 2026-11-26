# Shared helper functions for varieties_other scripts

africarice_sites <- function() {
	data.frame(
		country = c(
			"Benin", "Benin", "Benin",
			"Burkina Faso", "Burkina Faso", "Burkina Faso", "Burkina Faso", "Burkina Faso",
			"Central African Republic",
			"Côte d'Ivoire", "Côte d'Ivoire", "Côte d'Ivoire", "Côte d'Ivoire",
			"Ethiopia",
			"Gambia", "Gambia", "Gambia", "Gambia",
			"Ghana", "Ghana",
			"Guinea", "Guinea", "Guinea", "Guinea", "Guinea", "Guinea", "Guinea", "Guinea",
			"Mali", "Mali", "Mali", "Mali", "Mali", "Mali", "Mali",
			"Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria",
			"Senegal", "Senegal", "Senegal",
			"Sierra Leone", "Sierra Leone", "Sierra Leone", "Sierra Leone",
			"Uganda"
		),
		location = c(
			"Kpakpazounme", "Lema", "Sowe",
			"Banfora", "Farako Ba", "Karfiguela", "Lesso", "Vallee du Kou",
			"Boukoko",
			"Bouake", "Gagnoa", "Man", "Mbe",
			"Woreta",
			"Geneir", "Kaiaf", "Lamin Abuko", "Sapu",
			"Nobewan", "Sokwae",
			"Banakalan", "Bordo", "Diankana", "Khayta", "Kilissi", "Kirikine", "Koba", "Seredou",
			"Bougoula", "Kogoni", "Longorola", "Niono", "Samogossoni", "Selingue", "Sikasso",
			"Badeggi", "Edozhigi", "Ibadan", "Jima", "Mokwa", "Ondo", "Sabon Daga",
			"Fanaye", "Ndiaye", "Thiago",
			"Bo", "Kychom", "Rokupr", "Thormoya",
			"Namulonge"
		),
		latitude = c(
			7.970, 7.833, 7.967,
			10.633, 11.095, 10.683, 11.150, 11.383,
			3.900,
			7.690, 6.132, 7.412, 7.870,
			11.920,
			13.450, 13.400, 13.385, 13.555,
			6.620, 7.017,
			10.380, 10.390, 10.450, 9.850, 9.967, 9.900, 9.950, 8.383,
			11.300, 14.733, 11.367, 14.253, 11.350, 11.633, 11.317,
			9.057, 9.067, 7.495, 9.117, 9.295, 7.093, 9.483,
			16.533, 16.192, 16.433,
			7.965, 8.940, 9.017, 9.000,
			0.526
		),
		longitude = c(
			2.230, 2.233, 2.240,
			-4.767, -4.335, -4.817, -4.350, -4.417,
			17.917,
			-5.030, -5.951, -7.554, -5.110,
			37.700,
			-15.567, -15.617, -16.645, -14.900,
			-1.300, -1.650,
			-9.300, -9.306, -9.233, -13.650, -12.950, -13.700, -13.900, -9.300,
			-5.700, -6.033, -5.683, -5.993, -5.733, -8.233, -5.667,
			6.143, 5.967, 3.900, 5.750, 5.054, 4.835, 6.485,
			-15.183, -16.292, -15.733,
			-11.738, -13.100, -12.950, -12.980,
			32.613
		)
	)
}

africarice_norm_country <- function(x) {
	x <- trimws(as.character(x))
	cmap <- c(
		"benin" = "Benin",
		"burkina faso" = "Burkina Faso",
		"central african republic" = "Central African Republic",
		"cote d'ivoire" = "Côte d'Ivoire",
		"côte d'ivoire" = "Côte d'Ivoire",
		"ethiopia" = "Ethiopia",
		"gambia" = "Gambia",
		"ghana" = "Ghana",
		"guinea" = "Guinea",
		"guinea conakry" = "Guinea",
		"mali" = "Mali",
		"nigeria" = "Nigeria",
		"senegal" = "Senegal",
		"sierra leone" = "Sierra Leone",
		"uganda" = "Uganda"
	)
	out <- unname(cmap[tolower(x)])
	ifelse(is.na(out), x, out)
}

africarice_norm_location <- function(x) {
	x <- trimws(as.character(x))
	lmap <- c(
		"farakoba" = "Farako Ba",
		"farako ba" = "Farako Ba",
		"valle du kou" = "Vallee du Kou",
		"vallee du kou" = "Vallee du Kou",
		"longorolla" = "Longorola",
		"longorola" = "Longorola",
		"rokupur" = "Rokupr",
		"rokupr" = "Rokupr",
		"namulongue" = "Namulonge",
		"namulonge" = "Namulonge",
		"banfora" = "Banfora",
		"lamin abuko" = "Lamin Abuko",
		"sabon daga" = "Sabon Daga"
	)
	out <- unname(lmap[tolower(x)])
	ifelse(is.na(out), carobiner::fix_name(x, "title", skip = "du"), out)
}

africarice_add_geo <- function(d) {
	sites <- africarice_sites()
	key_d <- paste(d$country, d$location, sep = "|")
	key_s <- paste(sites$country, sites$location, sep = "|")
	idx <- match(key_d, key_s)
	if (any(is.na(idx))) {
		bad <- unique(key_d[is.na(idx)])
		stop("unmapped AfricaRice station(s): ", paste(bad, collapse = ", "))
	}
	d$latitude <- sites$latitude[idx]
	d$longitude <- sites$longitude[idx]
	d$geo_from_source <- FALSE
	d
}

process_africarice_btf <- function(ff, year, irrigated, drop_pnl_sites = NULL) {
	xlfiles <- sort(grep("\\.xlsx$", ff, value = TRUE))
	d <- lapply(xlfiles, function(f) {
		r <- carobiner::read.excel(f)
		names(r) <- tolower(names(r))
		num <- function(col) {
			if (col %in% names(r)) as.numeric(r[[col]]) else rep(NA_real_, nrow(r))
		}
		raw_variety <- trimws(as.character(if ("genotype" %in% names(r)) r$genotype else r$genotypes))
		is_check <- grepl("\\([Cc]heck\\)", raw_variety) | raw_variety == "LOCAL CHECK"
		variety <- trimws(sub("\\s*\\([Cc]heck\\)", "", raw_variety))
		country <- africarice_norm_country(r$country)
		location <- africarice_norm_location(r$site)
		bn <- basename(f)
		site <- NA_character_
		if (grepl("AfricaRice", bn, ignore.case = TRUE)) site <- "AfricaRice"
		if (grepl("ISRA", bn, fixed = TRUE)) site <- "ISRA"
		pnl <- num("pnl")
		if (!is.null(drop_pnl_sites) && location[1] %in% drop_pnl_sites) {
			pnl <- rep(NA_real_, nrow(r))
		}
		data.frame(
			country = country,
			location = location,
			site = site,
			season = tolower(trimws(as.character(r$season))),
			variety = variety,
			treatment = variety,
			variety_type = ifelse(is_check, "check", NA_character_),
			flowering_days = num("flwr"),
			maturity_days = num("maturity"),
			plant_height = num("plthgt"),
			seed_weight = num("gw1000"),
			panicle_length = pnl,
			tiller_number = num("till"),
			dmy_total = num("biom") * 1000,
			spikelet_fertility = num("fert"),
			# Dictionnary.txt: paddy yield at 14% moisture, t/ha
			yield = as.numeric(r$yield) * 1000
		)
	})
	d <- do.call(rbind, d)

	opt <- c("site", "variety_type", "flowering_days", "maturity_days", "plant_height",
		"seed_weight", "panicle_length", "tiller_number", "dmy_total", "spikelet_fertility")
	for (col in opt) {
		if (all(is.na(d[[col]]))) d[[col]] <- NULL
	}

	d <- africarice_add_geo(d)
	d$crop <- "rice"
	d$planting_date <- as.character(year)
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$yield_isfresh <- FALSE
	d$irrigated <- irrigated
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	site_tag <- if ("site" %in% names(d)) ifelse(is.na(d$site), "", d$site) else ""
	d$trial_id <- as.character(as.integer(as.factor(paste(d$country, d$location, site_tag, d$season))))
	rownames(d) <- NULL
	d
}

process_africarice_blup <- function(ff, year, irrigated) {
	parse_label <- function(label) {
		parts <- trimws(strsplit(label, "-", fixed = TRUE)[[1]])
		inst <- NA_character_
		if (tolower(parts[1]) %in% c("africarice", "isra")) {
			inst <- if (tolower(parts[1]) == "africarice") "AfricaRice" else "ISRA"
			parts <- parts[-1]
		}
		country <- africarice_norm_country(parts[1])
		rest <- parts[-1]
		if (length(rest) > 1 && tolower(rest[1]) %in% c("africarice", "isra")) {
			inst <- if (tolower(rest[1]) == "africarice") "AfricaRice" else "ISRA"
			rest <- rest[-1]
		}
		location <- africarice_norm_location(paste(rest, collapse = "-"))
		data.frame(site_label = label, country = country, location = location, site = inst)
	}

	read_blup <- function(f, varname) {
		r <- carobiner::read.excel(f, col_names = FALSE, .name_repair = "minimal")
		hdr <- trimws(as.character(unlist(r[1, ])))
		variety <- trimws(as.character(r[[1]]))
		drop_lab <- c("Heritability", "Mean", "LSD", "LCS", "CV", "Replicates", "STATISTIC", "Statistic", "Genotype")
		keep <- seq_len(nrow(r)) > 1 & !is.na(variety) & nzchar(variety)
		keep <- keep & !(variety %in% drop_lab)
		keep <- keep & !grepl("^(P-value|Variance|-|Low yields)", variety)
		out <- list()
		for (j in seq_len(ncol(r))[-1]) {
			lab <- hdr[j]
			if (is.na(lab) || !nzchar(lab)) next
			if (grepl("^Combined|highl|Supplementary", lab, ignore.case = TRUE)) next
			vals <- r[[j]][keep]
			if (is.character(vals)) {
				vals <- trimws(vals)
				vals[!nzchar(vals)] <- NA_character_
			}
			piece <- data.frame(
				variety = variety[keep],
				site_label = lab
			)
			piece[[varname]] <- as.numeric(vals)
			out[[length(out) + 1]] <- piece
		}
		x <- do.call(rbind, out)
		x <- x[!is.na(x[[varname]]), , drop = FALSE]
		sites <- do.call(rbind, lapply(unique(x$site_label), parse_label))
		x <- merge(x, sites, by = "site_label", all.x = TRUE)
		x$site_label <- NULL
		x
	}

	fy <- ff[grepl("^BLUP-Yield", basename(ff))]
	fflw <- ff[grepl("^BLUP-Flowering", basename(ff))]
	fh <- ff[grepl("^BLUP-Height", basename(ff))]

	by_cols <- c("variety", "country", "location", "site")
	d <- read_blup(fy[1], "yield")
	if (length(fflw)) d <- merge(d, read_blup(fflw[1], "flowering_days"), by = by_cols, all = TRUE)
	if (length(fh)) d <- merge(d, read_blup(fh[1], "plant_height"), by = by_cols, all = TRUE)
	d <- d[order(d$country, d$location, d$site, d$variety), ]
	rownames(d) <- NULL

	d <- data.frame(
		crop = "rice",
		variety = d$variety,
		treatment = d$variety,
		country = d$country,
		location = d$location,
		site = d$site,
		planting_date = as.character(year),
		yield = as.numeric(d$yield),
		flowering_days = as.numeric(d$flowering_days),
		plant_height = as.numeric(d$plant_height),
		yield_part = "grain",
		irrigated = irrigated,
		on_farm = FALSE,
		is_survey = FALSE
	)
	if (all(is.na(d$site))) d$site <- NULL
	site_tag <- if ("site" %in% names(d)) ifelse(is.na(d$site), "", paste0(" ", d$site)) else ""
	d$trial_id <- paste0(d$location, site_tag, " ", year)
	d <- africarice_add_geo(d)
	d
}
