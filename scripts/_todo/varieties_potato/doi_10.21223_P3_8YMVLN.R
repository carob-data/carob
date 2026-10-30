# R script for "carob"
# license: GPL (>=3)

## NOTES
# Harvest_isotopes.csv and Photosynthesis_isototes.csv (the file name is spelled isototes).
# Dictionary: aboveground and tuber biomass are dry grams per plant. Leaf N is g/m2. They are not a yield in kg/ha.
# Treatment C is the control and S is the stress treatment at 50% of the control soil moisture.
# Days after TIO are days after tuber initiation onset.
# Greenhouse at La Molina, Lima, Peru. southLatitude 12.1 and westLongitude 77 are stored as positive edge values. They are used as -12.1 and -77.

## ISSUES
# Biomass is per plant, so yield in kg/ha is not computed.
# No fertilizer and no planting or harvest calendar date. The metadata window is May-September 2013.
# The bounding box gives only the south and west edges, to 0.1 degree.
# The photosynthesis dictionary prints two different units on the intrinsic water-use row. A/gs is stored as recorded.
# write_files: missing variables: carob_contributor; yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: narea_, A_, tio_days_, harvest_number_, agb_g_, tb_g_, leaflet_d13c_, tuber_d13c_, gs_, ags_.

## NOT PROCESSED
# none beyond the dictionary workbooks, which were used for units.

## NEW VARIABLES
# tio_days_: days after tuber initiation onset
# harvest_number_: harvest 1 to 6 in the biomass file
# narea_: leaf N, g/m2
# agb_g_: dry aboveground biomass, g/plant
# tb_g_: dry tuber biomass, g/plant
# leaflet_d13c_: leaflet 13C discrimination, per mil
# tuber_d13c_: tuber 13C discrimination, per mil
# gs_: stomatal conductance, mol H2O m-2 s-1
# A_: net photosynthesis, mmol CO2 m-2 s-1
# ags_: intrinsic water-use efficiency, A/gs as recorded

carob_script <- function(path) {

"
Replication data for: Is Discrimination of 13C in Potato Leaflets and Tubers an Appropriate Trait to Describe Genotype Responses to Restrictive and Well-Watered Conditions?

Greenhouse experiment to compare physiological performance and biomass accumulation among genotypes along the growing period and to test the reliability of 13C discrimination (∆) of leaflet and ∆tuber to determine key mechanisms responsible of the varietal differences in biomass accumulation and tuber yield under different water availability levels.
"

	uri <- "doi:10.21223/P3/8YMVLN"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "agb_g_;tb_g_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	blank <- function(x) {
		y <- trimws(as.character(x))
		y[!nzchar(y) | y == "NA"] <- NA_character_
		y
	}
	num <- function(x) as.numeric(blank(x))

	h <- read.csv(ff[basename(ff) == "Harvest_isotopes.csv"], stringsAsFactors=FALSE, check.names=FALSE)
	p <- read.csv(ff[basename(ff) == "Photosynthesis_isototes.csv"], stringsAsFactors=FALSE, check.names=FALSE)

	dh <- data.frame(
		plot_id = paste("H", blank(h$ID), sep="_"),
		variety = blank(h$Genotype),
		treatment = blank(h$Treatment),
		tio_days_ = num(h[["Days after TIO"]]),
		harvest_number_ = num(h$Harvest),
		narea_ = num(h$Narea),
		agb_g_ = num(h$AGB),
		tb_g_ = num(h$TB),
		leaflet_d13c_ = num(h$DeltaLeaflet),
		tuber_d13c_ = num(h$DeltaT),
		stringsAsFactors = FALSE
	)
	dp <- data.frame(
		plot_id = paste("P", blank(p$ID), sep="_"),
		variety = blank(p$Genotype),
		treatment = blank(p$Treatment),
		tio_days_ = num(p[["Days after TIO"]]),
		gs_ = num(p$gs),
		A_ = num(p$A),
		ags_ = num(p[["A/gs"]]),
		stringsAsFactors = FALSE
	)
	d <- carobiner::bindr(dh, dp)
	d$crop <- "potato"
	d$country <- "Peru"
	d$adm1 <- "Lima"
	d$location <- "La Molina"
	d$latitude <- -12.1
	d$longitude <- -77
	d$geo_from_source <- TRUE
	d$geo_uncertainty <- 12000
	d$geo_source <- "Dataverse bounding box edges southLatitude 12.1 and westLongitude 77, signs applied"
	d$stress <- ifelse(d$treatment == "S", "drought", "none")
	d$trial_id <- "LaMolina_greenhouse_2013"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE

	carobiner::write_files(path, meta, d)
}
