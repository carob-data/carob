# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Only the 25-line mean extract (IJPP extract) is provided, not the replicate-level scores for all 160 B-lines summarized in the ANOVA file (160 lines x 5 pathotypes x 3 reps).
# Greenhouse screening experiment (per AGENTS.md, greenhouse variety-trait screenings are acceptable); no exact sowing date is reported (Dataverse collection window is 2014-06-14 to 2016-11-25).

## NEW
# pathotype_: Magnaporthe/Pyricularia grisea pathotype-isolate code (Pg 45, Pg 53, Pg 56, Pg 118, Pg 119)
# disease: "blast" (pearl millet foliar blast) not yet in values_disease.csv
# pathogen: "Pyricularia grisea" (anamorph of Magnaporthe grisea) not yet in values_pathogen.csv

## MISSING
# yield, yield_part, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date

## NOTES
# Greenhouse blast screening of designated pearl millet hybrid seed parents (B-lines) against five Pyricularia grisea pathotype-isolates at ICRISAT Patancheru (BP 07).
# Associated publication (found via web search, no DOI): Yella Goud et al. (2016), Indian Journal of Plant Protection 44(1): 83-87.
# Scores are on the standard 1-9 blast severity scale (resistant <= 3; susceptible check ICMB 95444 scores 7-9; resistant check ICMR 06444 scores 1-3).
# Typo "IICMB 97222" in row 4 of the Excel sheet is corrected to "ICMB 97222" (confirmed in Yella Goud et al. 2016).
# Coordinates are estimated for ICRISAT Patancheru from the Dataverse Google Maps link (geo_from_source = FALSE, geo_uncertainty = 1000 m).

## NOT PROCESSED
# ANOVA Evaluation of designated hybrid seed parents of Pearl millet for blast resistance.xlsx: ANOVA summary statistics only (DF, SS, MSS, F), not plot observations.
# IJPP extract ...xlsx: Mean column (arithmetic mean of the five pathotype scores per line).

carob_script <- function(path) {

"
Evaluation of designated hybrid seed parents of Pearl millet for blast resistance

This database includes the research work carried out on evaluation of 160 Pearl millet seed parents designated in 1981 to 2011. The 160 B-lines were screened for for blast resistance under greenhouse conditions along with a resistant (ICMR 06444) and a susceptible (ICMB 95444) check against five pathotype-isolates (Pg 45, Pg 53, Pg 56, Pg 118 and Pg 119) of Magnaporthe grisea. Identified blast resistant B lines can be used as a parent for development of blast resistant hybrids in future.        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/R4TFQF"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;pathotype_",
		response_vars = "disease_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 60,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "IJPP extract evaluation of designated hybrid seed parents of Pearl millet for blast resistance.xlsx"]
	r <- carobiner::read.excel(f, na = "NA")

	cols <- c("Pyricularia grisea 45", "Pyricularia grisea 53", "Pyricularia grisea 56", "Pyricularia grisea 118", "Pyricularia grisea 119")
	codes <- c("Pg 45", "Pg 53", "Pg 56", "Pg 118", "Pg 119")
	n <- nrow(r)
	variety <- trimws(r$Entry)
	# Fix spelling typo in row 4 ("IICMB 97222" -> "ICMB 97222", confirmed in Yella Goud et al. 2016)
	variety[variety == "IICMB 97222"] <- "ICMB 97222"
	vtype <- rep("breeding line", n)
	vtype[variety == "ICMR 06444"] <- "resistant check"
	vtype[variety == "ICMB 95444"] <- "susceptible check"

	d <- data.frame(
		variety = rep(variety, length(cols)),
		variety_pedigree = rep(trimws(r$Pedigree), length(cols)),
		variety_type = rep(vtype, length(cols)),
		pathotype_ = rep(codes, each = n),
		disease_severity = as.character(unlist(r[cols]))
	)
	d$treatment <- paste(d$variety, d$pathotype_, sep = "_")
	d$crop <- "pearl millet"
	d$disease <- "blast"
	d$pathogen <- "Pyricularia grisea"
	d$severity_scale <- "1-9"
	d$stress <- "disease"
	d$country <- "India"
	d$adm1 <- "Telangana"
	d$location <- "ICRISAT Patancheru"
	d$site <- "BP 07"
	# ICRISAT Patancheru campus coordinates from Dataverse Google Maps link
	d$longitude <- 78.2720
	d$latitude <- 17.5182
	d$geo_uncertainty <- 1000
	d$geo_source <- "ICRISAT Patancheru (Dataverse Google Maps link)"
	d$geo_from_source <- FALSE
	d$trial_id <- "1"
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
