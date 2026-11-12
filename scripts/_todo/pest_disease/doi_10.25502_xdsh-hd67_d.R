# R script for "carob"
# license: GPL (>=3)

## NOTES
# cgm-data.csv: cassava plants scored for cassava green mite and the predatory mite Typhlodromalus aripo.
# Plants in a block are averaged at each month. Sites in Cameroon, dry season labeled 2016-2017, at 3, 6, or 9 months after planting.
# typhlodromalus_present_ stays yes/no when the plants agree, and is the proportion of plants when they do not. typhlodromalus_30plants_ is the recorded percent; missing plants are left out of the mean.
# pest_incidence is CGM_Abundance (count). CGM_Incidence is kept separately because values reach 1820, so it is not a 0-100 percent.
# T. aripo columns are the predator, not a pest count. ICARDA is an author affiliation in the dataset metadata.

## ISSUES
# No coordinates in the table. Do not use the single CKAN point (coverage_x 03°51.791, coverage_y 011°27.706) for every site.
# Georeference: Bambui, Bertoua, Ekona, Foumbot, Mbalmayo, Meiganga, Meyomessala (Cameroon).
# October vs January is not assigned to 2016 or 2017. The year column is the label "2016-2017".
# CGM_Sev is only 2, 3, or 4 and Hairiness is 2, 3, or 4. The dictionary does not give the scales.
# CGM_Incidence has no unit. Taripo_30plants is filled on 151 of 4520 rows.
# No yield, plot area, planting date, or fertilizer. AEZ codes are not expanded.
# write_files: missing latitude, longitude, yield, yield_moisture, N/P/K, irrigated, planting_date, harvest_date; pest names not in the vocabulary; carob_contributor empty.

## NOT PROCESSED
# cgm-data.csv: ID. Row number.
# data-dictionary.csv: documentation or a codebook, not an observation table.

## NEW VARIABLES
# months_after_planting_: 3, 6, or 9 (column MAP).
# observation_month_: October or January.
# season_year_: the source label 2016-2017.
# cgm_incidence_: CGM_Incidence as recorded; unit not stated.
# typhlodromalus_present_: T_aripo on the plant, averaged as described in NOTES.
# typhlodromalus_percent_: T_aripo_perc; dictionary says percent, values are 0 or 3.333.
# typhlodromalus_30plants_: Taripo_30plants, percent of 30 plants, mostly missing.
# leaf_hairiness_: Hairiness score.
# aez_: agroecological zone code (AEZ II, III, IV, or V).


carob_script <- function(path) {

"
Distribution and impact of the predatory mite Typhlodromalus aripo on cassava green mite in diverse agroecological landscape in Central Africa.

Biological control of Mononychellus tanajoa.
"

	uri <- "doi:10.25502/xdsh-hd67/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; ICARDA",
		publication = NA,
		project = NA,
		design = "multilocation; cassava genotypes in 3 blocks",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "pest_incidence;pest_severity",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "cgm-data.csv"]
	r <- read.csv(f, check.names=FALSE)

	d <- data.frame(
		variety = trimws(r$Genotypes),
		treatment = trimws(r$Genotypes),
		location = carobiner::fix_name(trimws(r$Loc), "title"),
		aez_ = trimws(r$AEZone),
		season = "dry",
		season_year_ = trimws(r$Year),
		observation_month_ = trimws(r$Month),
		months_after_planting_ = as.integer(sub("MAP", "", r$MAP)),
		block_id = as.character(r$Block),
		plant_number_ = as.integer(r$Plant_number),
		crop = "cassava",
		yield_part = "roots",
		pest_species = "Mononychellus tanajoa",
		pest_incidence = as.integer(r$CGM_Abundance),
		cgm_incidence_ = as.numeric(r$CGM_Incidence),
		pest_severity = as.character(r$CGM_Sev),
		typhlodromalus_present_ = r$T_aripo == 1,
		typhlodromalus_percent_ = as.numeric(r$T_aripo_perc),
		typhlodromalus_30plants_ = as.numeric(r$Taripo_30plants),
		leaf_hairiness_ = as.integer(r$Hairiness)
	)

	d$country <- "Cameroon"
	d$geo_from_source <- FALSE
	d$trial_id <- paste(d$location, d$season_year_, sep="_")
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$stress <- "pests"
	# Abstract names the mite Mononychellus tanajoa. CGM_Abundance is the count used as pest_incidence.

	# Average one row per plant to one row per group in `by`.
	# Numbers are means. A yes/no column stays yes/no when the plants agree, otherwise it is the proportion of plants.
	# Text that is all numbers is averaged. Other text that differs is joined with "; ". Full dates are averaged.
	mean_by <- function(x, by) {
		if (!nrow(x)) return(x)
		if ("plant_number_" %in% names(x)) x$plant_number_ <- NULL
		if ("record_id" %in% names(x)) x$record_id <- NULL
		by <- intersect(by, names(x))
		key <- do.call(paste, c(lapply(by, function(v) ifelse(is.na(x[[v]]), "", as.character(x[[v]]))), list(sep = "\r")))
		mean_na <- function(z) {
			z <- as.numeric(z)
			if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
		}
		keep <- !duplicated(key)
		out <- x[keep, , drop = FALSE]
		num <- vapply(x, is.numeric, TRUE)
		logi <- vapply(x, is.logical, TRUE)
		num[names(num) %in% by] <- FALSE
		logi[names(logi) %in% by] <- FALSE
		for (v in names(x)[num]) {
			m <- tapply(x[[v]], key, mean_na)
			m <- unname(m[key[keep]])
			if (is.integer(x[[v]]) && all(is.na(m) | abs(m - round(m)) < 1e-8)) {
				m <- as.integer(round(m))
			}
			out[[v]] <- m
		}
		for (v in names(x)[logi]) {
			same <- tapply(x[[v]], key, function(z) {
				z <- z[!is.na(z)]
				!length(z) || all(z == z[1])
			})
			if (isTRUE(all(same))) {
				m <- tapply(x[[v]], key, function(z) {
					z <- z[!is.na(z)]
					if (!length(z)) NA else z[1]
				})
				out[[v]] <- unname(m[key[keep]])
			} else {
				m <- tapply(as.numeric(x[[v]]), key, mean_na)
				out[[v]] <- unname(m[key[keep]])
			}
		}
		for (v in names(x)) {
			if (v %in% by || num[v] || logi[v]) next
			u <- tapply(as.character(x[[v]]), key, function(z) {
				z <- z[!is.na(z)]
				z <- trimws(z)
				z <- z[nzchar(z)]
				if (!length(z)) return(NA_character_)
				if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", z))) {
					return(as.character(mean(as.Date(z))))
				}
				isn <- grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
				parts <- character()
				if (any(isn)) parts <- c(parts, as.character(mean(as.numeric(z[isn]))))
				codes <- unique(z[!isn])
				parts <- c(parts, codes)
				if (length(parts) == 1) parts else paste(parts, collapse = "; ")
			})
			out[[v]] <- unname(u[key[keep]])
		}
		rownames(out) <- NULL
		out
	}

	d <- mean_by(d, c("variety", "treatment", "location", "aez_", "season", "season_year_", "observation_month_", "months_after_planting_", "block_id", "trial_id", "crop", "pest_species"))

	carobiner::write_files(path, meta, d)
}
