# R script for "carob"
# license: GPL (>=3)

## NOTES
# Harvest sheet of Data_CWR_Sweetpotato2018.xlsx. fw and dw are kept as recorded. The sheet does not name the unit, so they are not converted to kg/ha.
# The abstract states a completely randomized design at San Ramon. Coordinates are the bounding box, already signed: southLatitude -11.1275, westLongitude -75.356389.
# Publication doi:10.3389/fpls.2020.567507 is the paper DOI in the metadata.

## ISSUES
# Fresh and dry weights are not a per-hectare yield. Plot area is not in the harvest sheet.
# No fertilizer. dat is kept as recorded and is not labeled as a calendar date.
# write_files: missing variables: carob_contributor; yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: dat_, fw_, dw_.

## NOT PROCESSED
# Foliar area, Gravimetric hum, Leaf temp, Stomatal cond, Spad, and Isotopes sheets: repeated leaf measurements, not joined to the harvest rows here.
# Weather_data_CRW_Sweepotato2018.xlsx and Material_list_Sweetpotato2018.xlsx.

## NEW VARIABLES
# dat_: value in the dat column
# fw_: fresh weight as recorded
# dw_: dry weight as recorded

carob_script <- function(path) {

"
Replication data for: Potential Short-term Memory Induction as a Promising Method for Increasing Drought Tolerance in Sweetpotato Crop Wild Relatives [Ipomoea series Batatas (Choisy) D. F. Austin]

Potted experiment to develop water stress resilience and productivity in 59 accessions of sweetpotato crop wild relatives (eight plants per accession) through short-term memory induction (four plants in potencial conditions and four underwent a water stress priming process as non-primed and primed treatment, respectively) using ecophysiological traits (Chl_SPAD, foliar area, leaf-minus-air temperature and leaf isotopic discrimination). Experiment was carried out durind July-December 2018 in a completely randomized desing at San Ramón CIP experimental station (Junin-Peru).
"

	uri <- "doi:10.21223/WNZR93"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = "doi:10.3389/fpls.2020.567507",
		project = NA,
		design = "completely randomized",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "fw_;dw_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Data_CWR_Sweetpotato2018.xlsx"]
	r <- carobiner::read.excel(f, sheet="Harvest")
	txt <- function(x) {
		y <- trimws(as.character(x))
		y[!nzchar(y)] <- NA_character_
		y
	}

	num <- function(x) {
		y <- trimws(as.character(x))
		y[!nzchar(y) | y %in% c("NA", ".")] <- NA_character_
		as.numeric(y)
	}
	d <- data.frame(
		crop = "sweetpotato",
		plot_id = paste(txt(r$ID), txt(r$Individual), sep="_"),
		variety_code = txt(r[["Accession number"]]),
		variety = txt(r[["Accession number"]]),
		treatment = txt(r$Treatment),
		dat_ = num(r$dat),
		fw_ = num(r$fw),
		dw_ = num(r$dw),
		country = "Peru",
		adm1 = "Junin",
		location = "San Ramon",
		latitude = -11.1275,
		longitude = -75.356389,
		geo_from_source = TRUE,
		geo_source = "Dataverse bounding box",
		trial_id = "SanRamon_2018",
		stress = "drought",
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = NA
	)

	carobiner::write_files(path, meta, d)
}
