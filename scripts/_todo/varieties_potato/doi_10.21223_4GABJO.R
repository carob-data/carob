# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm yellow-trap counts of three potato tuber moths in Chimborazo, Tungurahua, Cotopaxi, and Bolivar in 2018, and in Cotopaxi and Imbabura in 2019.
# c1 and c2 are two fields. alm is the trap next to storage. Cotopaxi 2019 and Imbabura 2019 have counts only for field 1. Those c2 and storage columns are empty, so they are not stored.
# The columns named latitud and longitud are UTM easting and northing. The utm column is the zone (17 S, 17 N, or 18 N). Latitude and longitude are converted from those values.
# Adults at each evaluation are in the long table, one row per site, date, trap, and species.
# nts is Tecia solanivora, nst is Symmetrischema tangolias, and npo is Phthorimaea operculella. The dictionary capitalizes the species epithet of Tecia.
# Interval temperature, and the day counts below 5 C and above 25 C, come from All_Databases_t, matched on province, farm id, and evaluation date. The dictionary calls those day counts both a count and yes/no. The values run from 0 to 20, so they are stored as day counts.
# Imbabura's fourth column is named Cotacachi. It holds the canton: Cotacachi, Pimampiro, Ibarra, and Otavalo.

## ISSUES
# No variety, yield, fertilizer rate, or irrigation statement.
# Bolivar is stored as Bolívar. Other place spellings are as in the file and should be checked against GADM, including Pujili.
# Some evaluations have no interval temperature.
# write_files: missing yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: farmer_code_, trap_place_, install_date_ (suggested name date), interval_tmean_, interval_tmin_ (suggested name tmin), interval_tmax_ (suggested name tmax), days_below_5c_, days_above_25c_. invalid terms pest_species: Phthorimaea operculella, Symmetrischema tangolias, Tecia solanivora.

## NOT PROCESSED
# Weather_field_experiment.xlsx: hourly temperature and humidity. The weather table has no hour column.
# All_Databases_t_v.xlsx: the same counts already in long form, plus the same temperature columns.
# td and tvar on All_Databases_t. td is another temperature average for the same interval. tvar is a variance labeled as degrees Celsius.
# Trap comment columns.
# Dictionary.xlsx.


## NEW VARIABLES
# farmer_code_: farmer code (codagricultor)
# trap_place_: field 1, field 2, or storage
# install_date_: date the yellow trap was installed
# interval_tmean_: mean temperature between evaluations, degrees Celsius
# interval_tmin_: minimum temperature between evaluations, degrees Celsius
# interval_tmax_: maximum temperature between evaluations, degrees Celsius
# days_below_5c_: days below 5 C between evaluations
# days_above_25c_: days above 25 C between evaluations


carob_script <- function(path) {

"
Dataset for: Distribution of three species of potato moths in Ecuador

A field study was carried out in the main potato-producing provinces of Ecuador to: (1) examine the distribution of three different species of potato tuber moths (PTM), Tecia solanivora (Ts), Symmetrischema tangolias (St) and Phthorimaea operculella (Po), in potato farm fields and adjacent storage units, and (2) update the Insect Life Cycle Modeling (ILCYM) software to further develop integrated pest management (IPM) strategies for the future. Field tests were carried out in the province of Chimborazo, Tungurahua, Cotopaxi and Bolivar in 2018, and in Cotopaxi and Imbabura in 2019. PTM traps were placed in different field sites selected per province that contained pheromones specific for each of the three PTM species. The traps were evaluated and cleaned over a period of about 6 to 8 months in 15-day intervals and data collected regarding the number of PTM adults trapped per device (data separated in sheets according to location with denomination “_moth_evaluation” per province). Data logger devices were also placed in the nearest canton/parish to the selected field sites in order to register soil temperature, and were evaluated over a similar interval of days (data in the “Wheather_field_experiment” sheet). Finally, two aggregate databases were crossed with temperatura, one with horizontal format  (data in \"All_Databases_t\") and another with vertical format (data in \"All_Databases_t_v\")
"

	uri <- "doi:10.21223/4GABJO"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.9
	)

	parse_dmy <- function(x) {
		if (inherits(x, "Date")) return(as.character(x))
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		out <- rep(NA_character_, length(x))
		ok <- !is.na(x)
		out[ok] <- as.character(as.Date(x[ok], "%d-%m-%Y"))
		out
	}

	first_by <- function(id, x) {
		x <- parse_dmy(x)
		id <- as.character(id)
		u <- tapply(x, id, function(z) {
			z <- z[!is.na(z)]
			if (length(z) == 0) NA_character_ else z[1]
		})
		unname(as.character(u[id]))
	}

	# latitud is the easting and longitud is the northing. WGS84.
	utm_to_ll <- function(E, N, zone, south) {
		E <- as.numeric(E)
		N <- as.numeric(N)
		zone <- as.integer(zone)
		r1 <- 6378137.0
		f <- 1/298.257223563
		b <- r1 * (1 - f)
		k0 <- 0.9996
		e2 <- 1 - (b/r1)^2
		ep2 <- e2 / (1 - e2)
		x <- E - 500000
		y <- ifelse(south, N - 10000000, N)
		lon0 <- ((zone - 1) * 6 - 180 + 3) * pi/180
		M <- y / k0
		mu <- M / (r1 * (1 - e2/4 - 3*e2*e2/64 - 5*e2*e2*e2/256))
		e1 <- (1 - sqrt(1 - e2)) / (1 + sqrt(1 - e2))
		phi1 <- mu +
			(3*e1/2 - 27*e1^3/32) * sin(2*mu) +
			(21*e1^2/16 - 55*e1^4/32) * sin(4*mu) +
			(151*e1^3/96) * sin(6*mu) +
			(1097*e1^4/512) * sin(8*mu)
		C1 <- ep2 * cos(phi1)^2
		T1 <- tan(phi1)^2
		N1 <- r1 / sqrt(1 - e2 * sin(phi1)^2)
		R1 <- r1 * (1 - e2) / (1 - e2 * sin(phi1)^2)^1.5
		D <- x / (N1 * k0)
		lat <- phi1 - (N1 * tan(phi1) / R1) * (
			D^2/2 -
			(5 + 3*T1 + 10*C1 - 4*C1^2 - 9*ep2) * D^4/24 +
			(61 + 90*T1 + 298*C1 + 45*T1^2 - 252*ep2 - 3*C1^2) * D^6/720
		)
		lon <- lon0 + (
			D - (1 + 2*T1 + C1) * D^3/6 +
			(5 - 2*C1 + 28*T1 - 3*C1^2 + 8*ep2 + 24*T1^2) * D^5/120
		) / cos(phi1)
		data.frame(latitude = lat * 180/pi, longitude = lon * 180/pi)
	}

	fix <- function(x) carobiner::fix_name(trimws(as.character(x)), "title")

	f1 <- ff[basename(ff) == "All_Databases_t.xlsx"]
	r1 <- carobiner::read.excel(f1)
	temp <- data.frame(
		key = paste(trimws(as.character(r1$provincia)), trimws(as.character(r1$id)), parse_dmy(r1$fevaluacion), sep = "|"),
		interval_tmean_ = as.numeric(r1$tme),
		interval_tmin_ = as.numeric(r1$tmin),
		interval_tmax_ = as.numeric(r1$tmax),
		days_below_5c_ = as.numeric(r1$dbe5),
		days_above_25c_ = as.numeric(r1$dab25)
	)
	temp <- temp[!duplicated(temp$key), ]

	species <- c(nts = "Tecia solanivora", nst = "Symmetrischema tangolias", npo = "Phthorimaea operculella")
	places <- c(c1 = "field 1", c2 = "field 2", alm = "storage")

	one <- function(f) {
		r <- carobiner::read.excel(f)
		year <- sub(".*([0-9]{4})\\.[^.]+$", "\\1", basename(f))
		prov <- trimws(as.character(r$provincia))
		fid <- trimws(as.character(r$id))
		trial_id <- paste(prov, year, fid, sep = "_")
		date <- parse_dmy(r$fevaluacion)
		zone <- trimws(as.character(r$utm))
		south <- grepl("S", zone)
		zone_num <- as.integer(gsub("\\D", "", zone))
		ll <- utm_to_ll(r$latitud, r$longitud, zone_num, south)
		adm1 <- fix(prov)
		adm1[prov == "Bolivar"] <- "Bolívar"
		planting <- rep(NA_character_, nrow(r))
		if ("fsiembra" %in% names(r)) planting <- first_by(fid, r$fsiembra)
		inst <- list()
		if ("finstalacion_c1" %in% names(r)) {
			inst$c1 <- first_by(fid, r$finstalacion_c1)
			inst$c2 <- first_by(fid, r$finstalacion_c2)
			inst$alm <- first_by(fid, r$finstalacion_alm)
		} else {
			oneinst <- first_by(fid, r$finstalacion)
			inst$c1 <- oneinst
			inst$c2 <- oneinst
			inst$alm <- oneinst
		}
		keep <- !duplicated(trial_id)
		site <- data.frame(
			trial_id = trial_id[keep],
			crop = "potato",
			country = "Ecuador",
			adm1 = adm1[keep],
			adm2 = fix(r[[4]])[keep],
			adm3 = fix(r$parroquia)[keep],
			location = fix(r$localidad)[keep],
			farmer_code_ = trimws(as.character(r$codagricultor))[keep],
			elevation = as.numeric(r$altitud)[keep],
			latitude = round(ll$latitude[keep], 5),
			longitude = round(ll$longitude[keep], 5),
			planting_date = planting[keep],
			plot_id = fid[keep],
			stress = "pests",
			on_farm = TRUE,
			is_survey = TRUE,
			geo_from_source = TRUE
		)
		parts <- list()
		for (tr in names(places)) {
			for (sp in names(species)) {
				col <- paste(sp, tr, sep = "_")
				raw <- r[[col]]
				if (is.logical(raw) || all(is.na(raw))) next
				tkey <- paste(prov, fid, date, sep = "|")
				j <- match(tkey, temp$key)
				parts[[length(parts) + 1]] <- data.frame(
					trial_id = trial_id,
					date = date,
					order = as.integer(r$nevaluacion),
					trap_place_ = unname(places[[tr]]),
					install_date_ = inst[[tr]],
					pest_species = unname(species[[sp]]),
					pest_incidence = as.integer(raw),
					interval_tmean_ = temp$interval_tmean_[j],
					interval_tmin_ = temp$interval_tmin_[j],
					interval_tmax_ = temp$interval_tmax_[j],
					days_below_5c_ = temp$days_below_5c_[j],
					days_above_25c_ = temp$days_above_25c_[j]
				)
			}
		}
		long <- do.call(rbind, parts)
		list(site = site, long = long)
	}

	f2 <- ff[basename(ff) %in% c(
		"Bolívar_moth_evaluation2018.xlsx",
		"Chimborazo_moth_evaluation2018.xlsx",
		"Cotopaxi_moth_evaluation2018.xlsx",
		"Cotopaxi_moth_evaluation2019.xlsx",
		"Imbabura_moth_evaluation2019.xlsx",
		"Tungurahua_moth_evaluation2018.xlsx"
	)]
	parts <- lapply(f2, one)
	d <- do.call(rbind, lapply(parts, function(z) z$site))
	long <- do.call(rbind, lapply(parts, function(z) z$long))
	rownames(d) <- NULL
	rownames(long) <- NULL
	d$record_id <- seq_len(nrow(d))
	long$record_id <- d$record_id[match(long$trial_id, d$trial_id)]
	long$trial_id <- NULL

	carobiner::write_files(path, meta, d, long = long)
}
