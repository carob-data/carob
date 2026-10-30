# R script for "carob"
# license: GPL (>=3)

## NOTES
# The IJPP extract sheet is blast scores for 25 seed parents against five Pyricularia grisea isolates.
# Column headers are "Pyricularia grisea 45" and so on. The abstract calls the isolates Pg 45, Pg 53, Pg 56, Pg 118 and Pg 119, and the fungus Magnaporthe grisea.
# Scores with a decimal are kept as printed. The abstract names ICMR 06444 as the resistant check and ICMB 95444 as the susceptible check.
# The screen is a greenhouse test (abstract). Dataverse place is Patancheru, Telangana, field code BP 07. There is no bounding box.

## ISSUES
# The ANOVA sheet is for 160 lines (breeding-line DF 159), 5 pathotypes and 3 replications (total DF 2399). Those plot scores are not in the files. This script has the 25-line extract only.
# The extract has no replicate, year, or scale definition. Collection dates in Dataverse are 2014-06-14 to 2016-11-25 and were not used as a planting date.
# No coordinates, yield, or fertilizer.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date; unknown pathotype_; invalid disease blast; invalid pathogen Pyricularia grisea.

## NOT PROCESSED
# ANOVA workbook: source of variation, DF, SS, MSS, F value, F probability. Summary statistics, not plot scores.
# IJPP extract column Mean: the mean of the five isolate scores on the same row.

## NEW VARIABLES
# pathotype_: isolate code from the column header (Pg 45, Pg 53, Pg 56, Pg 118, Pg 119).

carob_script <- function(path) {

"
Evaluation of designated hybrid seed parents of Pearl millet for blast resistance

This database includes the research work carried out on evaluation of 160 Pearl millet seed parents designated in 1981 to 2011. The 160 B-lines were screened for for blast resistance under greenhouse conditions along with a resistant (ICMR 06444) and a susceptible (ICMB 95444) check against five pathotype-isolates (Pg 45, Pg 53, Pg 56, Pg 118 and Pg 119) of Magnaporthe grisea. Identified blast resistant B lines can be used as a parent for development of blast resistant hybrids in future.        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/R4TFQF"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;pathotype_",
		response_vars = "disease_severity",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "IJPP extract evaluation of designated hybrid seed parents of Pearl millet for blast resistance.xlsx"]
	r <- carobiner::read.excel(f, na="NA")

	cols <- c("Pyricularia grisea 45", "Pyricularia grisea 53", "Pyricularia grisea 56", "Pyricularia grisea 118", "Pyricularia grisea 119")
	codes <- c("Pg 45", "Pg 53", "Pg 56", "Pg 118", "Pg 119")
	n <- nrow(r)
	variety <- trimws(r$Entry)
	vtype <- rep("breeding line", n)
	vtype[variety %in% c("ICMR 06444", "ICMB 95444")] <- "check"

	d <- data.frame(
		variety = rep(variety, length(cols)),
		variety_pedigree = rep(trimws(r$Pedigree), length(cols)),
		variety_type = rep(vtype, length(cols)),
		pathotype_ = rep(codes, each=n),
		disease_severity = as.character(unlist(r[cols])),
		stringsAsFactors = FALSE
	)
	d$treatment <- paste(d$variety, d$pathotype_, sep="_")
	d$crop <- "pearl millet"
	d$disease <- "blast"
	d$pathogen <- "Pyricularia grisea"
	d$stress <- "disease"
	d$country <- "India"
	d$adm1 <- "Telangana"
	d$location <- "Patancheru"
	d$site <- "ICRISAT Research Field - BP 07"
	d$trial_id <- "1"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
