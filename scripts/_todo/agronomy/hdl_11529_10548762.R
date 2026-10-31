# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm rice-wheat scenarios at Karnal, India, 2014-2017, as named in the dataset description.
# Sc1 conventional tillage without residue. Sc2 conventional tillage with residue. Sc3 reduced tillage with residue and the recommended fertilizer. Sc4 is described as reduced or zero tillage with residue and the recommended fertilizer. Sc5 is zero tillage with residue, the recommended fertilizer, GreenSeeker and a tensiometer. Sc6 is Sc5 plus Nutrient Expert.
# Plot rows are replicate 1 to 3 and scenario 1 to 6. Sc1-Sc6 summary rows are not kept.
# Grain yield is already kg/ha.
# The fertilizer column is an energy total, about 12,000-15,000, not a nutrient rate.

## ISSUES
# Sc4 is labeled both reduced tillage and zero tillage, so land preparation is empty for that scenario.
# Recommended fertilizer rates are not given as kilograms. No coordinates.
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# Energy, greenhouse-gas, and economic columns. They are calculated.
# System and RW energy sheets.
# The PDF of the paper.


carob_script <- function(path) {

"
Intensive tillage operations, indiscriminate use of irrigation water, chemical fertilizers, and pesticides and crop biomass burning have made the conventional rice-wheat (RW) system highly energy intensive and inefficient. In the recent past, portfolios of climate-smart agricultural practices (CSAP) have been promoted as a potential alternative to improve the energy efficiency in conventional RW system. Therefore, to evaluate the energy input-output relation, energy flow and economic efficiency in various combinations of crop management options, a 3-year (2014-2017) on-farm study was conducted at Karnal, India. Various portfolio of management practices; Sc1-Business as usual (BAU) or Conventional tillage (CT) without residue, Sc2-CT with residue, Sc3-Reduce tillage (RT) with residue + recommended dose of fertilizer (RDF), Sc4-RT/Zero tillage (ZT) with residue + RDF, Sc5-ZT with residue + RDF + GreenSeeker + Tensiometer, Sc6-Sc5 + Nutrient expert were investigated.
"

	uri <- "hdl:11529/10548762"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[grepl("xlsx$", basename(ff))]
	label <- c(
		"1" = "Sc1 conventional tillage without residue",
		"2" = "Sc2 conventional tillage with residue",
		"3" = "Sc3 reduced tillage with residue and recommended fertilizer",
		"4" = "Sc4 reduced or zero tillage with residue and recommended fertilizer",
		"5" = "Sc5 zero tillage with residue, recommended fertilizer, GreenSeeker and tensiometer",
		"6" = "Sc6 Sc5 plus Nutrient Expert"
	)
	prep <- c("1"="conventional", "2"="conventional", "3"="reduced tillage", "5"="none", "6"="none")

	one <- function(sheet, crop) {
		r <- carobiner::read.excel(f, sheet=sheet)
		y <- as.numeric(r$`Grain yield`)
		sc <- as.character(r$Scenarios)
		keep <- as.character(r$Rep) %in% c("1", "2", "3") & sc %in% names(label) & !is.na(y)
		sc <- sc[keep]
		data.frame(
			country = "India",
			location = "Karnal",
			trial_id = paste("Karnal", crop, "2014-2017"),
			crop = crop,
			rep = as.integer(as.character(r$Rep[keep])),
			treatment = unname(label[sc]),
			land_prep_method = unname(prep[sc]),
			residue_prevcrop_used = sc != "1",
			yield = y[keep],
			yield_part = "grain",
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	d <- carobiner::bindr(one("Rice", "rice"), one("Wheat", "wheat"))
	d$record_id <- as.integer(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
