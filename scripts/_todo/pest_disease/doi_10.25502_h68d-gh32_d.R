# R script for "carob"
# license: GPL (>=3)

## NOTES
# cassava-root-rot-data.csv: 45 cassava fields, 5 plants each, South, Centre, and Littoral, Cameroon, October 2022.
# The five plants in a field are averaged. Presence flags stay yes/no when the plants agree, and become the proportion of plants when they do not. Necrosis type and color keep each distinct value.
# Latitude and longitude are the Lat and Long columns (degrees). root_infection is rotting_incidence (percent of roots).
# Necrosis severity is the dictionary 1-5 scale. Weediness is 2 on every row (dictionary: 25% weedy). Soil moisture code is 2 (humid) on every row.
# Field age is months after planting. Dates mix d/m/yyyy (4/10/2022 = 4 Oct) and m/d/yyyy once the day exceeds 12 (10/13/2022 = 13 Oct). Rows that start with 10/ are read as October.
# adm1 uses GADM French names: South -> Sud, Center -> Centre.
# The citation in the metadata has no DOI.

## ISSUES
# Surf_area has no unit (600 to 40000). Values 2501-2504 look like 2500 with a digit added. Not converted to hectares, so yield kg/ha is not computed.
# rotting_incidence is 125 on one row (above 100). Left as recorded.
# Variety Ekoblé is kept with its accent. Blank necrosis color (no color recorded) is stored as missing.
# African root and tuber scale is presence/absence, not a count of insects.
# No fertilizer, irrigation, or planting date. Sampling date is not a harvest date.
# "cassava root rot" is not yet in the disease vocabulary.
# write_files: missing yield, yield_moisture, N/P/K, irrigated, planting_date, harvest_date; possibly out of bounds root_infection; carob_contributor empty.

## NOT PROCESSED
# cassava-root-rot-data.csv: ID. Row number.
# cassava-root-rot-data.csv: Rotting_presence. 0/1 flag repeated by Rotten_root.
# data_dictionary.csv: documentation or a codebook, not an observation table.

## NEW VARIABLES
# field_area_: Surf_area as recorded; unit not stated.
# crop_age_months_: field age in months after planting.
# root_number_: total roots on the plant.
# root_rotten_: rotten roots on the plant.
# bud_necrosis_: bud necrosis present (1) or absent (0).
# necrosis_aerial_: necrosis on the aerial part (1/0).
# necrosis_type_: Wet, Dry, or No necrosis.
# necrosis_color_: Black, Dark brown, or Brown.
# arts_present_: African root and tuber scale present (1) or absent (0).
# soil_moisture_class_: humid (dictionary code 2).


carob_script <- function(path) {

"
Understanding cassava rotting and underlying pathogens across major production regions of Cameroon

Diagnostic and surveillance network established for characterization, monitoring and surveillance of Cassava root rot. The present initiative aims at assessing the situation of cassava rotting and underlying pathogens  in major cassava production regions in Cameroon to inform priority actions.
"

	uri <- "doi:10.25502/h68d-gh32/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = "diagnostic survey; 5 plants per field",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "root_infection;disease_severity",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "cassava-root-rot-data.csv"]
	r <- read.csv(f, check.names=FALSE, fileEncoding="latin1")

	# October 2022 campaign. 4/10/2022 is 4 October. 10/13/2022 can only be 13 October.
	parse_date <- function(x) {
		parts <- strsplit(trimws(x), "/", fixed=TRUE)
		vapply(parts, function(z) {
			a <- as.integer(z[1])
			b <- as.integer(z[2])
			y <- z[3]
			if (a == 10) {
				sprintf("%s-%02d-%02d", y, a, b)
			} else {
				sprintf("%s-%02d-%02d", y, b, a)
			}
		}, character(1))
	}

	region <- trimws(r$Region)
	adm <- c(South="Sud", Center="Centre", Littoral="Littoral")
	variety <- trimws(r$Variety)
	variety <- ifelse(tolower(variety) == "local", "local", variety)

	d <- data.frame(
		country = "Cameroon",
		adm1 = unname(adm[region]),
		location = carobiner::fix_name(trimws(r$Loc), "title"),
		latitude = as.numeric(r$Lat),
		longitude = as.numeric(r$Long),
		elevation = as.numeric(r$Altitude),
		geo_from_source = TRUE,
		date = parse_date(r$Date),
		variety = variety,
		crop = "cassava",
		yield_part = "roots",
		field_area_ = as.numeric(r$Surf_area),
		crop_age_months_ = as.integer(r$Field_age),
		plot_id = as.character(r$FieldNo),
		plant_number_ = as.integer(r$Plant_no),
		root_number_ = as.integer(r$Total_root),
		root_rotten_ = as.integer(r$Rotten_root),
		root_infection = as.numeric(r$rotting_incidence),
		disease = "cassava root rot",
		disease_severity = as.character(r$Necrosis_severity),
		severity_scale = "1-5",
		bud_necrosis_ = r$bud_necrosis == 1,
		necrosis_aerial_ = r$Necrosis_aerial_part == 1,
		necrosis_type_ = trimws(r$Necrosis_type),
		necrosis_color_ = trimws(r$Necrosis_color),
		pest_species = "african root and tuber scale",
		arts_present_ = r$ARTS_presence == 1,
		weed_cover = 25,
		soil_moisture_class_ = "humid",
		stress = "disease;pests"
	)
	# weed_cover 25 and soil_moisture_class_ from dictionary codes, and every row is code 2.
	d$necrosis_color_[!is.na(d$necrosis_color_) & d$necrosis_color_ == ""] <- NA
	d$hhid <- paste(d$location, d$plot_id, sep="_")
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE

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

	d <- mean_by(d, c("plot_id", "location", "date", "variety", "hhid", "trial_id", "country", "adm1", "crop", "disease"))

	carobiner::write_files(path, meta, d)
}
