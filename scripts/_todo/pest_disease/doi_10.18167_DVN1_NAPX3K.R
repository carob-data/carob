# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field file only. Cage sheets (oviposition and larval survival) are an insectary trial and are not used.
# Two field sowings (10 Nov 2020 and 14 Jan 2022). Seed treatment is cyantraniliprole (Fortenza) or none.
# Devis is the 1-9 plant score in Data_damage. Plants in a plot on one date are averaged. Presence flags stay yes/no when the plants agree, and become the proportion of plants when they do not.
# Row spacing 0.75 m and within-row spacing 0.24 m, and drip irrigation, are on the Meta_data sheet.
# The dataset has no geographicCoverage and the field sheet has no place name.

## ISSUES
# Repeated plant rows are included in the plot mean.
# write_files: missing on_farm, yield, yield_moisture, yield_part, country, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date.
# unknown variables: larva_present_, damage_present_, larval_instar_. invalid terms: pest_species: fall armyworm.

## NOT PROCESSED
# Data cage Seye.xlsx: insectary oviposition and larval survival.
# Data_seedling: line-level stand counts and plants per ha. They are not the same grain as the plant damage scores.
# Data_damage %plant_attaque: 0 or 100, the same flag as Pres_degat.
# Data_damage Nb_plant_obs: 1 on every row.


carob_script <- function(path) {

"
Maize seed treatment for fall armyworm control

Data on field and semi-field trials of maize seed treament for the control of fall armyworm
"

	uri <- "doi:10.18167/DVN1/NAPX3K"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "pest_severity",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 55,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Data Field Seye.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data_damage")
	treated <- r$Trait_au_fortenza == "Traite au Cyantranilipole"

	d <- data.frame(
		trial_id = paste0("essai_", r$Num_essai),
		crop = "maize",
		treatment = trimws(r$Trait_au_fortenza),
		seed_treatment = ifelse(treated, "cyantraniliprole", "none"),
		insecticide_used = treated,
		rep = as.integer(r$Bloc),
		plot_id = trimws(as.character(r$Parcelle)),
		plant_number_ = as.integer(r$Num_plant),
		planting_date = as.character(as.Date(r$Date_semis)),
		date = as.character(as.Date(r$Date_obs)),
		pest_species = "fall armyworm",
		pest_severity = as.character(as.integer(r$Devis)),
		severity_scale = "1-9",
		larva_present_ = r$Presence_larv == 1,
		damage_present_ = r$Pres_degat == 1,
		larval_instar_ = as.integer(r$Stade_larv),
		row_spacing = 75,
		plant_spacing = 24,
		irrigated = TRUE,
		stress = "pests",
		is_survey = FALSE
	)

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

	d <- mean_by(d, c("trial_id", "treatment", "seed_treatment", "rep", "plot_id", "planting_date", "date", "pest_species", "crop"))

	carobiner::write_files(path, meta, d)
}
