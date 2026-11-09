# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The previous draft assigned coordinates and set geo_from_source to TRUE:
# Palmira 3.53782, -76.2968 and Darien 3.932135, -76.4938. They are not in the
# phenotypic file. Confirm them before using them.
# 100-seed weight was previously multiplied by 10. The source column is kept as recorded.
# The GBS VCF (5820 SNPs) is not processed here.
# Rows with missing yield were previously dropped. They are kept.


carob_script <- function(path) {

"
These datasets contain phenotypic and genotypic data of a panel of elite Andean breeding lines of common bean (Phaseolus vulgaris L.) from CIAT. This population has been tested in twelve yield trials carried out in Palmira and Darien (Colombia) between 2013 and 2018 to assess its performance under irrigated, drought and variable soil P conditions.
"

	uri <- "doi:10.7910/DVN/XCD67U"
	group <- "genetics"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIAT",
		publication = NA,
		project = NA,
		design = "alpha lattice",
		data_type = "experiment",
		treatment_vars = "variety;irrigated",
		response_vars = "yield",
		carob_completion = 25,
		carob_effort = 0,
		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		notes = "",
		
	)

	f1 <- ff[basename(ff) == "VEF_raw_phenotypic_data.csv"]
	r <- read.csv(f1)

	# The year token is in the trial name (13 = 2013 through 18 = 2018).
	year <- ifelse(grepl("13", r$Trial), "2013",
		ifelse(grepl("14", r$Trial), "2014",
		ifelse(grepl("15", r$Trial), "2015",
		ifelse(grepl("16", r$Trial), "2016",
		ifelse(grepl("17", r$Trial), "2017", "2018")))))

	d <- data.frame(
		country = "Colombia",
		location = ifelse(grepl("Pal", r$Trial), "Palmira", "Darien"),
		crop = "common bean",
		irrigated = grepl("Irrigated", r$Management),
		rep = ifelse(r$Replicate == "R1", 1L, ifelse(r$Replicate == "R2", 2L, 3L)),
		variety = r$Line,
		treatment = r$Management,
		yield = r$Yd,
		seed_weight = r$X100SdW,
		trial_id = r$Unique.Identifier,
		planting_date = year,
		on_farm = TRUE,
		is_survey = FALSE,
		inoculated = FALSE,
		yield_part = "seed"
	)

	f2 <- ff[basename(ff) == "VEF_meteoro.csv"]
	r1 <- read.csv(f2)
	wh <- data.frame(
		date = as.character(as.Date(carobiner::eng_months_to_nr(r1$Date), "%d-%m-%y")),
		station_name = ifelse(grepl("Pal", r1$Season), "Palmira", "Darien"),
		tmax = r1$Maximum.temperature,
		tmin = r1$Minimum.temperature,
		rhum = rowMeans(r1[, c("Relative.humidity.07.H", "Relative.humidity.13.H", "Relative.humidity.19.H")]),
		prec = r1$Total.precipitation,
		rhmn = r1$Minimum.relative.humidity,
		vapr = r1$Total.evaporation
	)

	carobiner::write_files(path, meta, d, wth = wh)
}
