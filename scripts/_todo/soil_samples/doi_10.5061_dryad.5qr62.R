# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil inorganic N after fertilizer on maize at Yala, Kenya (KE) and Tumbi, Tanzania (TZ). The abstract gives nitrate concentrations in micrograms per gram, so ConcNO3 and ConcNH4 are stored as soil_NO3 and soil_NH4 (mg/kg).
# Weekly files are the 0-15 cm topsoil named in the abstract. Profile files use the depth strings in the tables. 2014_CIAT_DP.csv has no site and no plot, so it is not in the table.
# Treatment numbers are kg N/ha. GLIR and Glir are gliricidia and are not stored as N_fertilizer.
# pH in WeeklyKCl files is stored as soil_pH_KCl because the file name says KCl. Profile files only say pH, so that column is soil_pH and the extract is not named.

## ISSUES
# No coordinates. Some profile depths are deeper than 200 cm. One nitrate value is slightly negative.
# write_files: missing variables: carob_contributor, latitude, longitude. unknown variables: inorganic_N_kg_ha_. out of bounds: depth_top (0, 350), depth_bottom (15, 400), soil_NO3 (-0.004, 86.615), soil_NH4 (0, 72.953).

## NEW VARIABLES
# inorganic_N_kg_ha_: inorganic N stock, kg/ha, from kgNperha.


## NOT PROCESSED
# 2014_CIAT_DP.csv. Stock15cm and Yr1NO3, Yr1NH4.


carob_script <- function(path) {

"
Data from: Effects of fertilizer on inorganic soil N in East Africa maize systems: vertical distributions and temporal dynamics

Fertilizer trials were established on a clayey soil in Yala, Kenya, and on a sandy soil in Tumbi, Tanzania, with application rates of 0-200 kg N/ha/yr. Soil profiles were collected (0-400 cm) annually. Topsoils (0-15 cm) were collected every 3-6 weeks.
"

	uri <- "doi:10.5061/dryad.5qr62"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
		data_organization = "UMD",
		publication = "doi:10.1890/15-1518.1",
		project = NA,
		design = NA,
		data_type = "experiment",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.5
	)

	num <- function(x) as.numeric(as.character(x))
	when <- function(x) {
		x <- trimws(as.character(x))
		d <- as.Date(x, "%d-%b-%y")
		miss <- is.na(d)
		d[miss] <- as.Date(x[miss], "%d/%m/%y")
		as.character(d)
	}
	ends <- function(depth) {
		depth <- gsub("to", "-", depth)
		depth <- gsub("[[:space:]]", "", depth)
		a <- strsplit(depth, "-", fixed=TRUE)
		top <- as.numeric(vapply(a, function(z) z[1], ""))
		bot <- as.numeric(vapply(a, function(z) z[2], ""))
		list(top=top, bot=bot)
	}
	site <- c(KE = "Yala", TZ = "Tumbi")
	country <- c(KE = "Kenya", TZ = "Tanzania")
	nrate <- function(x) {
		x <- trimws(as.character(x))
		x <- sub(" N$", "", x)
		x[!grepl("^[0-9.]+$", x)] <- NA
		num(x)
	}

	one <- function(f, loc, topsoil) {
		r <- read.csv(f, check.names=FALSE)
		code <- if (!is.null(r$Location)) toupper(trimws(r$Location)) else loc
		if (topsoil) {
			top <- rep(0, nrow(r))
			bot <- rep(15, nrow(r))
		} else {
			e <- ends(r$Depth)
			top <- e$top
			bot <- e$bot
		}
		ph <- if ("pH" %in% names(r)) num(r$pH) else rep(NA_real_, nrow(r))
		bd <- if ("BulkD" %in% names(r)) num(r$BulkD) else if ("BD" %in% names(r)) num(r$BD) else rep(NA_real_, nrow(r))
		yr <- if ("Year" %in% names(r)) as.character(r$Year) else rep(NA_character_, nrow(r))
		locname <- unname(site[code])
		trt <- trimws(as.character(r$Treatment))
		data.frame(
			country = unname(country[code]),
			location = locname,
			trial_id = ifelse(is.na(yr), locname, paste(locname, yr, sep="_")),
			plot_id = if ("Plot" %in% names(r)) paste(r$Plot, if ("Block" %in% names(r)) r$Block else "", sep="-") else NA_character_,
			crop = "maize",
			treatment = trt,
			N_fertilizer = nrate(trt),
			date = if ("Date" %in% names(r)) when(r$Date) else NA_character_,
			depth_top = top,
			depth_bottom = bot,
			soil_NO3 = num(r$ConcNO3),
			soil_NH4 = num(r$ConcNH4),
			soil_bd = bd,
			soil_pH_KCl = if (topsoil) ph else NA_real_,
			soil_pH = if (topsoil) NA_real_ else ph,
			inorganic_N_kg_ha_ = if ("kgNperha" %in% names(r)) num(r$kgNperha) else rep(NA_real_, nrow(r)),
			geo_from_source = FALSE,
			is_survey = FALSE,
			on_farm = FALSE
		)
	}

	d <- rbind(
		one(ff[basename(ff) == "WeeklyKCl_KE.csv"], "KE", TRUE),
		one(ff[basename(ff) == "WeeklyKCl_TZ.csv"], "TZ", TRUE),
		one(ff[basename(ff) == "Initial_DP_KETZ_zeroN.csv"], NA, FALSE),
		one(ff[basename(ff) == "Yr2_DP_KETZ_zero200.csv"], NA, FALSE),
		one(ff[basename(ff) == "Yr2_DP_TZ_Glir75.csv"], NA, FALSE)
	)
	d <- d[!is.na(d$country), ]
	d$trial_id[is.na(d$trial_id) | grepl("_NA$", d$trial_id)] <- d$location[is.na(d$trial_id) | grepl("_NA$", d$trial_id)]

	carobiner::write_files(path, meta, d)
}
