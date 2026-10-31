# R script for "carob"
# license: GPL (>=3)

## NOTES
# Split-plot wheat trial of Roelfs with and without the BNI trait at El Batan and Obregon, Mexico. The dataset description states nitrogen rates of 0, 75, 150, and 225 kg/ha as ammonium sulphate.
# Country is Mexico in geographicCoverage. The title names Batan and Obregon. There are no coordinates. geo_from_source is FALSE.
# Soil NO3-N and NH4-N are the columns labeled mg/kg of dry soil. They are stored as soil_NO3 and soil_NH4.
# N_flux is micrograms of N in N2O per square metre per hour, from the N2O metadata sheet. It is not converted to the flux_N2O unit.
# Excel dates use origin 1899-12-30.

## ISSUES
# missing variables: yield, yield_moisture, latitude, longitude, irrigated, K_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variables: bni_component_, sample_position_
# unknown variables: n2o_n_ug_m2_h_
# out of bounds: soil_NH4 (-14.961, 570.723)

## NEW VARIABLES
# bni_component_: BNI, non-BNI, or synthetic nitrification inhibitor, as labeled.
# sample_position_: bed or furrow, where the soil sample was taken.
# n2o_n_ug_m2_h_: N2O flux expressed as nitrogen, micrograms per square metre per hour.

## NOT PROCESSED
# Resin-trap leaching workbooks.
# Chamber volume, air pressure, soil moisture, and the linear-model diagnostics.
# The field-map variety list beyond the variety named on each measurement row.


carob_script <- function(path) {

"
Nitrous oxide emission, leaching and soil mineral N for BNI and non-BNI Roelfs wheat at Batan and Obregon

Soil mineral nitrogen and N2O flux from a split-plot nitrogen-rate trial.
"

	uri <- "doi:10.71682/10549422"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "BNI wheat",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "soil_NO3;soil_NH4",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.5
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | !nzchar(x) | x %in% c("NA", "-")] <- NA_character_
		as.numeric(x)
	}
	excel_date <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | !nzchar(x) | x %in% c("NA", "-")] <- NA_character_
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
		out[iso] <- substring(x[iso], 1, 10)
		slash <- !is.na(x) & grepl("/", x, fixed=TRUE)
		if (any(slash)) {
			p <- strsplit(x[slash], "/", fixed=TRUE)
			dd <- as.integer(vapply(p, function(z) z[1], character(1)))
			mm <- as.integer(vapply(p, function(z) z[2], character(1)))
			yy <- as.integer(vapply(p, function(z) z[3], character(1)))
			out[slash] <- as.character(as.Date(paste(yy, mm, dd, sep="-")))
		}
		serial <- !is.na(x) & !iso & !slash
		if (any(serial)) out[serial] <- as.character(as.Date(as.numeric(x[serial]), origin="1899-12-30"))
		out
	}
	col <- function(df, pattern) {
		hit <- names(df)[grepl(pattern, names(df))]
		if (!length(hit)) return(NULL)
		df[[hit[1]]]
	}

	read_min <- function(f, sheet, location) {
		r <- as.data.frame(carobiner::read.excel(f, sheet=sheet, col_types="text"))
		depth <- as.character(col(r, "DEPTH"))
		parts <- strsplit(depth, "-", fixed=TRUE)
		top <- num(vapply(parts, function(z) z[1], character(1)))
		bot <- num(vapply(parts, function(z) if (length(z) < 2) NA_character_ else z[2], character(1)))
		pos <- toupper(trimws(as.character(col(r, "FURROW"))))
		pos[pos == "B"] <- "bed"
		pos[pos == "F"] <- "furrow"
		no3nm <- names(r)[grepl("NO3", names(r)) & grepl("kg", names(r))]
		nh4nm <- names(r)[grepl("NH4", names(r)) & grepl("kg", names(r))]
		data.frame(
			location = location,
			ensayo = trimws(as.character(col(r, "ENSAYO"))),
			plot = trimws(as.character(col(r, "PLOT"))),
			rep = as.integer(num(col(r, "REPLICATION"))),
			sample_position_ = pos,
			date = excel_date(col(r, "SAMPLING DATE")),
			depth_top = top,
			depth_bottom = bot,
			N_fertilizer = num(col(r, "N RATE")),
			bni_component_ = trimws(as.character(col(r, "BNI"))),
			variety = trimws(as.character(col(r, "VARIETY"))),
			soil_NO3 = num(r[[no3nm[length(no3nm)]]]),
			soil_NH4 = num(r[[nh4nm[length(nh4nm)]]])
		)
	}

	batan <- read_min(ff[grepl("Min_N_Batan", basename(ff))], "MIN N Batan from dataset", "El Batan")
	obregon <- read_min(ff[grepl("Min_N_Obregon", basename(ff))], "Min N Obregon-2025-BNI", "Obregon")
	m <- rbind(batan, obregon)
	m <- m[!is.na(m$plot) & nzchar(m$plot) & !is.na(m$N_fertilizer) & !is.na(m$bni_component_) & nzchar(m$bni_component_), ]
	m$variety[is.na(m$variety) | !nzchar(m$variety)] <- NA_character_
	m <- m[!is.na(m$variety), ]
	m$plot_id <- paste(m$location, m$ensayo, m$plot)
	m$trial_id <- paste(m$location, m$ensayo)
	m$treatment <- paste(m$N_fertilizer, m$bni_component_, m$variety)

	d <- m[!duplicated(m$plot_id), ]
	d <- data.frame(
		country = "Mexico",
		location = d$location,
		trial_id = d$trial_id,
		plot_id = d$plot_id,
		rep = d$rep,
		treatment = d$treatment,
		variety = d$variety,
		bni_component_ = d$bni_component_,
		sample_position_ = d$sample_position_,
		N_fertilizer = d$N_fertilizer,
		crop = "wheat",
		yield_part = "grain",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	long <- data.frame(
		trial_id = m$trial_id,
		plot_id = m$plot_id,
		date = m$date,
		depth_top = m$depth_top,
		depth_bottom = m$depth_bottom,
		soil_NO3 = m$soil_NO3,
		soil_NH4 = m$soil_NH4,
		n2o_n_ug_m2_h_ = NA_real_
	)

	n2of <- ff[grepl("^N2O_", basename(ff))]
	flux <- vector("list", length(n2of))
	for (i in seq_along(n2of)) {
		sh <- NA_character_
		if (grepl("Batan", basename(n2of[i]))) sh <- "Batan_2024_N2O"
		if (grepl("Obregon", basename(n2of[i]))) sh <- "Obregon_2025_N2O"
		r <- as.data.frame(carobiner::read.excel(n2of[i], sheet=sh, col_types="text"))
		loc <- if (grepl("Batan", basename(n2of[i]))) "El Batan" else "Obregon"
		vn <- trimws(as.character(r[["Variety number"]]))
		variety <- rep(NA_character_, length(vn))
		variety[vn == "7"] <- "Roelfs"
		variety[vn == "8"] <- "Roelfs + Lr#n-SA"
		flux[[i]] <- data.frame(
			location = loc,
			ensayo = trimws(as.character(r$ENSAYO)),
			plot = trimws(as.character(r$Plot_No_Act)),
			variety = variety,
			N_fertilizer = num(r[["N-fertilizer rate"]]),
			date = excel_date(r$DATE),
			n2o_n_ug_m2_h_ = num(r$N_flux)
		)
	}
	flux <- do.call(rbind, flux)
	flux <- flux[!is.na(flux$plot) & nzchar(flux$plot) & !is.na(flux$n2o_n_ug_m2_h_) & !is.na(flux$variety) & !is.na(flux$N_fertilizer), ]
	flux$plot_id <- paste(flux$location, flux$ensayo, flux$plot)
	flux$trial_id <- paste(flux$location, flux$ensayo)
	extra <- flux[!duplicated(flux$plot_id), ]
	extra <- extra[!extra$plot_id %in% d$plot_id, ]
	if (nrow(extra)) {
		d <- rbind(d, data.frame(
			country = "Mexico",
			location = extra$location,
			trial_id = extra$trial_id,
			plot_id = extra$plot_id,
			rep = NA_integer_,
			treatment = paste(extra$N_fertilizer, extra$variety),
			variety = extra$variety,
			bni_component_ = NA_character_,
			sample_position_ = NA_character_,
			N_fertilizer = extra$N_fertilizer,
			crop = "wheat",
			yield_part = "grain",
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = FALSE
		))
	}
	long <- rbind(long, data.frame(
		trial_id = flux$trial_id,
		plot_id = flux$plot_id,
		date = flux$date,
		depth_top = NA_real_,
		depth_bottom = NA_real_,
		soil_NO3 = NA_real_,
		soil_NH4 = NA_real_,
		n2o_n_ug_m2_h_ = flux$n2o_n_ug_m2_h_
	))
	long <- long[long$plot_id %in% d$plot_id, ]

	carobiner::write_files(path, meta, d, long=long)
}
