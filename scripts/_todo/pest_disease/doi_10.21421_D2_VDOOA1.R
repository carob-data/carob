# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sheet PercentSM is percent sterility mosaic incidence for 29 genotypes, 2 reps, 2007 and 2008, at 8 sites.
# The location columns were stacked. "Banglore" is Bangalore.
# Coordinates are the degree-minute pairs in the description and in the Dataverse geographicUnit text. geo_from_source is TRUE.
# adm1 is the Dataverse state, except Dholi (see ISSUES).
# Disease is sterility mosaic. The abstract names Pigeonpea sterility mosaic virus.

## ISSUES
# No yield, fertilizer, irrigation, or planting day. Dataverse coverage is 2003-01-14 to 2009-12-25; this sheet is only 2007 and 2008.
# Artificial versus natural inoculation is not a column. BP 14B and BIL 7 are named in the metadata but the sheet has one Patancheru column.
# Dataverse puts Dholi in Gujarat. The same metadata gives 25°59'N, 85°35'E, so adm1 was left empty for Dholi.
# Publication DOI 10.1007/s10327-015-0585-z is in the Dataverse metadata. No RIS file was added.
# "sterility mosaic" and "Pigeonpea sterility mosaic virus" are not in the vocabularies.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; invalid disease sterility mosaic; invalid pathogen Pigeonpea sterility mosaic virus.

## NOT PROCESSED
# None. The sheet's year, genotype, replication, and eight incidence columns are all used.

carob_script <- function(path) {

"
Data on multi-environment field testing to identify broad, stable resistance to sterility mosaic disease of Pigeonpea

Sterility mosaic disease (SMD) caused by Pigeonpea sterility mosaic virus and vectored by the eriophyid mite is a serious disease of pigeonpea in almost all pigeonpea-growing areas. In this context, total 976 Pigeonpea genotypes including germplasm accessions and breeding lines was evaluated for SMD resistance under artificial epiphytotic conditions in a disease nursery at ICRISAT, Patancheru during 2003/2004. Based on this evaluation, a set of 166 Pigeonpea genotypes with high levels of SMD resistance was selected and again evaluated for 3 years during 2004/2005–2006/2007 in a disease nursery at Patancheru. Finally, a Pigeonpea Sterility Mosaic Disease Nursery (PSMDN) of the 28 genotypes with the most resistance against SMD was established for multi-environment evaluation. At Bangalore, Dholi and Rahuri locations, all genotypes were susceptible to SMD with mean disease incidence of 71.1, 50.4 and 32.6 % respectively. However, most of the genotypes were resistant at four locations, Akola, Badnapur, Patancheru, and Vamban, and moderately resistant at Coimbatore. The GGE biplot analysis explained about 67.26 % of total variation and identified four genotypes (ICPLs 20094, 20106, 20098, 20115) as the most stable and resistant to SMD.         Experiment locations on Google Map :       ICRISAT - BP 14B         ICRISAT - BIL 7         Akola          Badnapur         Bangalore         Coimbatore         Dholi         Rahuri         Vamban
"

	uri <- "doi:10.21421/D2/VDOOA1"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICRISAT",
		publication = "doi:10.1007/s10327-015-0585-z",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "disease_incidence",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Multi-environment field testing to identify stable resistance.xlsx"]
	r <- carobiner::read.excel(f, na="NA")

	sites <- c("Akola", "Badnapur", "Banglore", "Coimbatore", "Dholi", "Rahuri", "Patancheru", "Vamban")
	one <- function(site) {
		data.frame(
			planting_date = as.character(r$Year),
			variety = trimws(r$Genotype),
			rep = as.integer(r[["Replication number"]]),
			location = site,
			disease_incidence = as.character(r[[site]])
		)
	}
	d <- do.call(rbind, lapply(sites, one))
	d$location[d$location == "Banglore"] <- "Bangalore"
	d$treatment <- d$variety
	d$crop <- "pigeon pea"
	d$disease <- "sterility mosaic"
	d$pathogen <- "Pigeonpea sterility mosaic virus"
	d$stress <- "disease"
	d$country <- "India"
	d$trial_id <- paste(d$location, d$planting_date, sep="_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE

	# degree-minutes from the description docx and Dataverse geographicUnit
	geo <- data.frame(
		location = c("Akola", "Badnapur", "Bangalore", "Coimbatore", "Dholi", "Rahuri", "Patancheru", "Vamban"),
		adm1 = c("Maharashtra", "Maharashtra", "Karnataka", "Tamil Nadu", NA_character_, "Maharashtra", "Telangana", "Tamil Nadu"),
		latitude = c(20 + 42/60, 19 + 23/60, 12 + 58/60, 10 + 59/60, 25 + 59/60, 19 + 23/60, 17 + 31/60, 10 + 25/60),
		longitude = c(76 + 59/60, 75 + 43/60, 77 + 35/60, 76 + 57/60, 85 + 35/60, 74 + 42/60, 78 + 15/60, 76 + 49/60)
	)
	d <- merge(d, geo, by="location", all.x=TRUE, sort=FALSE)

	carobiner::write_files(path, meta, d)
}
