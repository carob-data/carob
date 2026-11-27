# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Source metadata gives 10°25'N, 76°49'E for Vamban, which falls in Kerala (~230 km west of Vamban, Tamil Nadu; apparent typo for 78°E); replaced with NPRC Vamban (Pudukkottai, Tamil Nadu) coordinates with geo_from_source = FALSE. Source latitude for Badnapur (19°23'N) also duplicates Rahuri's latitude (~60 km south of Badnapur town) but stays in Maharashtra and was kept as reported.
# Artificial vs natural pathogen inoculation varies across sites/nurseries (per metadata) but is not reported per site in the workbook.
# No yield, fertilizer, irrigation, or exact planting/harvest dates are in the data file (check Sharma et al. 2015 Methods behind paywall for any missing management details).

## NEW
# disease: sterility mosaic
# pathogen: Pigeonpea sterility mosaic virus

## MISSING
# yield, yield_part, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date

## NOTES
# Multi-environment sterility mosaic disease (SMD) screening of 28 resistant pigeonpea genotypes + susceptible check ICP 2376 across 8 Indian locations in 2007 and 2008 (2 replications; Sharma et al. 2015, doi:10.1007/s10327-015-0585-z).
# The 8 location columns of percent SMD incidence are reshaped to rows; "Banglore" is normalized to "Bangalore".
# Dataverse erroneously lists Dholi under Gujarat; corrected to Bihar matching the source coordinates (25°59'N, 85°35'E) and Sharma et al. (2015).

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
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
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
			disease_incidence = as.character(round(as.numeric(r[[site]]), 2))
		)
	}
	d <- do.call(rbind, lapply(sites, one))
	d$location[d$location == "Banglore"] <- "Bangalore"
	# Abstract notes 28 resistant lines; the 29th entry ICP 2376 is the susceptible check (Sharma et al. 2015)
	d$variety_type <- ifelse(d$variety == "ICP 2376", "check", NA_character_)
	d$treatment <- d$variety
	d$crop <- "pigeon pea"
	d$disease <- "sterility mosaic"
	d$pathogen <- "Pigeonpea sterility mosaic virus"
	d$stress <- "disease"
	d$country <- "India"
	d$trial_id <- paste(d$location, d$planting_date, sep="_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	# Degree-minute coordinates from metadata docx and Dataverse geographicUnit, except Vamban
	# (source 10°25'N, 76°49'E falls in Kerala; replaced with NPRC Vamban, Pudukkottai, Tamil Nadu)
	geo <- data.frame(
		location = c("Akola", "Badnapur", "Bangalore", "Coimbatore", "Dholi", "Rahuri", "Patancheru", "Vamban"),
		adm1 = c("Maharashtra", "Maharashtra", "Karnataka", "Tamil Nadu", "Bihar", "Maharashtra", "Telangana", "Tamil Nadu"),
		latitude = c(20 + 42/60, 19 + 23/60, 12 + 58/60, 10 + 59/60, 25 + 59/60, 19 + 23/60, 17 + 31/60, 10.368),
		longitude = c(76 + 59/60, 75 + 43/60, 77 + 35/60, 76 + 57/60, 85 + 35/60, 74 + 42/60, 78 + 15/60, 78.931),
		geo_from_source = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
		geo_uncertainty = c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 2000),
		geo_source = c(rep("ICRISAT Dataverse metadata", 7), "NPRC Vamban, Pudukkottai, Tamil Nadu")
	)
	d <- merge(d, geo, by="location", all.x=TRUE, sort=FALSE)

	carobiner::write_files(path, meta, d)
}
