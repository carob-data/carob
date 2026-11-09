# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field observations at Neufchâteau station, Guadeloupe (geographicCoverage). Coordinates in the file are plant positions such as A02, not longitude and latitude.
# Crop is plantain when Type Genotype is Plantain, and banana otherwise.
# Gaulh is the severity stored in disease_severity. The dictionary tokens non and Non, and pas de PJFN / pas de PJFL, are missing leaf-stage notes and are set to NA before the numbers are read.
# Lesion and necrosis sizes on the same rows are measurements of leaf fragments from these field plants. This is the field file.
# Rows that match on every column kept here, including fragment ids, are dropped. Period code REF is missing and is set to NA.

## ISSUES
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variable: field_ (field_id?)
# unknown variable: status_ (civil_status?)
# unknown variable: sex_latent_days_ (sex?)
# unknown variables: cycle_, period_, yls_, insl_, sev_yls_, incubation_days_, lesion_length_, lesion_width_, fragment_id_, lesion_id_, necrosis_id_
# invalid terms: disease: Pseudocercospora fijiensis

## NOT PROCESSED
# Observer, additional comments, marked-leaf rank, leaf emission rate, and the other stage indexes (Ylst, Ylmst, InstL, InmstL, AsexLP).
# Conidia and perithecia counts and the derived lesion surfaces.
# df_field_dictionary.csv: documentation or a codebook, not an observation table.
# README.txt: documentation or a codebook, not an observation table.


## NEW VARIABLES
# field_: field name inside the station.
# cycle_: banana cycle number.
# period_: observation period number.
# status_: plant status code (V, F, and the other codes in the file).
# yls_: youngest leaf spotted, leaf rank.
# insl_: index of non-spotted leaves, percent.
# sev_yls_: severity of the youngest leaf spotted, Gauhl scale.
# sex_latent_days_: estimated sexual latent period, days.
# incubation_days_: estimated incubation period, days.
# lesion_length_: lesion length on a leaf fragment.
# lesion_width_: lesion width on a leaf fragment.
# fragment_id_: leaf-fragment id. Several fragments can share one plant and date.
# lesion_id_: lesion id on that fragment.
# necrosis_id_: necrosis id on that fragment.


carob_script <- function(path) {

"
Field observations of eleven banana genotypes used to estimate their resistance levels to Pseudocercospora fijiensis in Guadeloupe (2021-2022)

This dataset contains field observations of eleven banana genotypes conducted in 2021 and 2022 at the CIRAD experimental station in Neufchâteau, Guadeloupe (details of the experimental design are provided in the associated publication). These observations were used to estimate different quantitative resistance components of banana to Pseudocercospora fijiensis infectious cycle.
"

	uri <- "doi:10.18167/DVN1/G0ZX4E"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1111/ppa.70062",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "disease_severity",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "df_field.csv"]
	r <- read.csv(f, na.strings=c("NA", ""), check.names=FALSE)
	names(r) <- trimws(names(r))

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "non", "Non", "pas de PJFN", "pas de PJFL", "REF")] <- NA_character_
		as.numeric(x)
	}

	field <- gsub("\u00a0", "", r$Field)
	field <- trimws(field)
	field[field == ""] <- NA_character_
	vtype <- trimws(r[["Type Genotype"]])

	d <- data.frame(
		trial_id = "Neufchateau",
		plot_id = trimws(as.character(r$Coordinates)),
		country = "Guadeloupe",
		location = "Neufchateau",
		field_ = field,
		crop = ifelse(vtype == "Plantain", "plantain", "banana"),
		variety = trimws(r$Genotype),
		variety_type = vtype,
		treatment = trimws(r$Genotype),
		rep = as.integer(num(r$Rep)),
		cycle_ = as.integer(num(r$Cycle)),
		period_ = as.integer(num(r$Period_nb)),
		date = as.character(as.Date(trimws(r$Date), "%d/%m/%Y")),
		status_ = trimws(r$State),
		stress = "disease",
		disease = "Pseudocercospora fijiensis",
		disease_severity = trimws(as.character(r$Gaulh)),
		yls_ = num(r$YLS),
		insl_ = num(r$INSL),
		sev_yls_ = num(r$Sev_YLS),
		sex_latent_days_ = num(r$SexLP),
		incubation_days_ = num(r$IP),
		lesion_length_ = num(r$lesion_length),
		lesion_width_ = num(r$lesion_width),
		fragment_id_ = trimws(as.character(r$ID_fragment)),
		lesion_id_ = trimws(as.character(r$ID_lesion)),
		necrosis_id_ = trimws(as.character(r$ID_necrosis)),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$plot_id[d$plot_id == ""] <- NA_character_
	d$status_[d$status_ == ""] <- NA_character_
	d$disease_severity[d$disease_severity == ""] <- NA_character_
	d$variety_type[d$variety_type == ""] <- NA_character_
	d$fragment_id_[d$fragment_id_ == ""] <- NA_character_
	d$lesion_id_[d$lesion_id_ == ""] <- NA_character_
	d$necrosis_id_[d$necrosis_id_ == ""] <- NA_character_
	# Some rows match on every column kept here, including the fragment ids.
	d <- d[!duplicated(d), ]

	carobiner::write_files(path, meta, d)
}
