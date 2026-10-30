# R script for "carob"
# license: GPL (>=3)

## NOTES
# cgm-data.csv: cassava plants scored for cassava green mite and the predatory mite Typhlodromalus aripo.
# One row per plant. Sites in Cameroon, dry season labeled 2016-2017, at 3, 6, or 9 months after planting.
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

## NEW VARIABLES
# months_after_planting_: 3, 6, or 9 (column MAP).
# observation_month_: October or January.
# season_year_: the source label 2016-2017.
# cgm_incidence_: CGM_Incidence as recorded; unit not stated.
# typhlodromalus_present_: T_aripo 1/0, presence on the plant.
# typhlodromalus_percent_: T_aripo_perc; dictionary says percent, values are 0 or 3.333.
# typhlodromalus_30plants_: Taripo_30plants, percent of 30 plants, mostly missing.
# leaf_hairiness_: Hairiness score.
# aez_: agroecological zone code (AEZ II, III, IV, or V).
# plant_number_: plant within the block.


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
	r <- read.csv(f, check.names=FALSE, stringsAsFactors=FALSE)

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
		leaf_hairiness_ = as.integer(r$Hairiness),
		stringsAsFactors = FALSE
	)

	d$country <- "Cameroon"
	d$geo_from_source <- FALSE
	d$trial_id <- paste(d$location, d$season_year_, sep="_")
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$stress <- "pests"
	# Abstract names the mite Mononychellus tanajoa. CGM_Abundance is the count used as pest_incidence.

	carobiner::write_files(path, meta, d)
}
