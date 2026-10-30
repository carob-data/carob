# R script for "carob"
# license: GPL (>=3)

## NOTES
# Tarwi (Lupinus mutabilis) accessions evaluated at INIA Santa Ana. The abstract names the crop and the site.
# YPLO is kg/ha in the dictionary, so yield is that column. YPLA is yield per plant in g.
# The latitude and longitude in the accession file are decimal degrees. They differ among accessions and are not the Santa Ana trial (the dataset coverage for the trial is about -12.01, -75.22). Those accession coordinates are stored as origin coordinates.
# The trial coordinate is the dataset geographic coverage, one west longitude and one south latitude.
# DH is days to harvest, not a calendar date. TSWHS is hundred-seed weight in g. Pod length and width are cm.

## ISSUES
# No fertilizer rate and no irrigation statement.
# Start, end, and length of season in the file are NDVI metrics, not planting and harvest dates.
# missing variables: carob_contributor
# missing variables: yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variable: origin_latitude_ (latitude?)
# unknown variable: origin_longitude_ (longitude?)
# unknown variable: origin_elevation_ (elevation?)
# unknown variable: yield_plant_g_ (yield?)
# unknown variable: seed_weight_100_g_ (seed_weight?)
# unknown variables: pod_length_cm_, pod_width_cm_, pods_per_plant_, days_to_harvest_
# seed_weight in the vocabulary is thousand-seed weight. TSWHS is hundred-seed weight, so it stays under its own name.

## NOT PROCESSED
# 02_Frequency of QC.xlsx: frequencies of qualitative scores, not one row per accession.
# Qualitative descriptor codes (flower color, stem, seed color, and the rest of the 40 descriptors). They are category codes without the category labels joined.
# sos, eos, los, mgs: NDVI season metrics.
# Cluster.


carob_script <- function(path) {

"
Dataset for: Phenotypic characterization of 142 Tarwi accessions using agro-morphological cualitiative traits.

Tarwi (Lupinus mutabilis S.) is a legume native to the andes that stands out for its nutritional value and potential in food security programs. In the experimental fields of the National Institute of Agrarian Innovation (INIA) - Santa Ana, in the period from 2022 to 2023, 140 accessions from the INIA national collection of Tarwi germoplasm were evaluated, along with 2 reference cultivars, 'INIA 445 Masacan-chino' and 'Andenes 90', selected for their high adaptation and genetic stability. The agromorphological characterization was carried out using 40 qualitative descriptors based on the Lupin descriptors (https://hdl.handle.net/10568/73417).
"

	uri <- "doi:10.21223/RHN72D"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_Data_Tarwi_accesions.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	names(r) <- trimws(names(r))
	variety <- trimws(as.character(r[["PER CODE"]]))

	d <- data.frame(
		trial_id = "SantaAna_tarwi_2022",
		country = "Peru",
		adm1 = "Junin",
		adm2 = "Huancayo",
		location = "Santa Ana",
		# dataset geographic coverage for the Santa Ana trial: one west longitude and one south latitude
		latitude = -12.01177,
		longitude = -75.22156,
		geo_from_source = TRUE,
		crop = "tarwi",
		variety = variety,
		variety_code = variety,
		treatment = variety,
		origin_latitude_ = as.numeric(r$Latitude),
		origin_longitude_ = as.numeric(r$Longitude),
		origin_elevation_ = as.numeric(r$Altitude),
		yield = as.numeric(r$YPLO),
		yield_plant_g_ = as.numeric(r$YPLA),
		yield_part = "seed",
		seed_weight_100_g_ = as.numeric(r$TSWHS),
		pod_length_cm_ = as.numeric(r$PLMS),
		pod_width_cm_ = as.numeric(r$PWMS),
		pods_per_plant_ = as.numeric(r$TNPP),
		days_to_harvest_ = as.numeric(r$DH),
		on_farm = FALSE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
