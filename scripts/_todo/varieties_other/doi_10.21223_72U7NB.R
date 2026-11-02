# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sweetpotato heat screening in the coastal desert of northern Peru, at Piura. The description states the country and the site. Accession collecting-site coordinates are not the trial coordinates and are not used.
# The observation table is the mean across replications. CIP numbers and the other numeric fields use a decimal comma. RYTHA is total storage-root yield in t/ha, so yield is RYTHA * 1000. FYTHA, DMVY, and DMRY are also t/ha. HI is harvest index in percent.
# treat is HS or NHS. The description defines a heat-stress season and a non-heat-stress season. The codes are kept as stored. The design is an alpha lattice, from the description. Harvest DAP is the dap column (the description says 120 days).

## ISSUES
# No trial coordinates.
# Rows are means, so plot and replication in the file are averages and are not plot ids.
# Rows with no CIP number are dropped so the variety treatment is never blank.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: foliage_yield_ (yield?). unknown variable: root_dry_yield_ (yield?). unknown variable: vine_dry_yield_ (yield?). high crop yield: sweetpotato: 134585. NA detected: yield, yield_part.

## NEW VARIABLES
# foliage_yield_: FYTHA, kg/ha, from t/ha.
# root_dry_yield_: DMRY, kg/ha, from t/ha.
# vine_dry_yield_: DMVY, kg/ha, from t/ha.

## NOT PROCESSED
# Morphology file, canopy-trait means, and the Piura weather workbook. The weather file is a station series and is not joined to accessions here.
# Biomass in kg per plot and yield in kg per plant. Plot area is not used to convert those.
# Accession origin latitude and longitude. They are collecting sites, not the Piura trial.


carob_script <- function(path) {

"
Dataset for: Mass screening of sweetpotato germplasm for heat tolerance

This dataset presents the results of a screening for heat stress tolerance of 1973 sweetpotato accessions under field conditions in the coastal desert of Northern Peru (Piura). The experiment was laid out as an alpha lattice design with two replications in two consecutive seasons (2013-2014) characterized by hot and moderate temperatures representing a heat stress and a non-heat stress treatment. Yield performance metrics such as storage root yield, vine yield, total biomass yield, number of marketable roots, harvest index, dry matter content and  normalized difference vegetation index (NDVI) were recorded at harvest at 120 days after planting. The dataset includes selected morhological descriptors and passportdata elements  (elevation of germplasm collecting site, country of origin) originating from the database of CIP genebank . The original datasets of the screened accessions can be found at http://genebank.cipotato.org/gringlobal/search.aspx or https://www.genesys-pgr.org/. Climatic data were recorded (air temperature, air humidity, precipitation, solar radiation, and wind speed) every hour with a HOBObase weather station U30 4.0 (Onset Computer Corporation, MA, USA).
"

	uri <- "doi:10.21223/72U7NB"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "alpha lattice",
		data_type = "experiment",
		treatment_vars = "treatment;variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "harvest_traits_mean_across_rep.csv"]
	r <- utils::read.csv(f, sep=";", dec=",", check.names=FALSE, colClasses=c(CIPNumber="character"))
	num <- function(x) {
		z <- trimws(as.character(x))
		z <- gsub(",", ".", z, fixed=TRUE)
		z[!nzchar(z) | z == "NA"] <- NA_character_
		ok <- is.na(z) | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}
	variety <- trimws(r$CIPNumber)
	variety <- gsub(",", ".", variety, fixed=TRUE)
	d <- data.frame(
		country = "Peru",
		location = "Piura",
		variety = variety,
		treatment = trimws(r$treat),
		DAP = as.integer(num(r$dap)),
		crop = "sweetpotato",
		yield = num(r$rytha) * 1000,
		yield_part = "roots",
		harvest_index = num(r$hi),
		foliage_yield_ = num(r$fytha) * 1000,
		root_dry_yield_ = num(r$dmry) * 1000,
		vine_dry_yield_ = num(r$dmvy) * 1000,
		geo_from_source = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		trial_id = "Piura_sweetpotato_heat"
	)
	d$yield_part[is.na(d$yield)] <- NA_character_
	d <- d[!is.na(d$variety) & nzchar(d$variety) & !is.na(d$treatment), ]
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
