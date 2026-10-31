# R script for "carob"
# license: GPL (>=3)

## NOTES
# Grain Yield (Kg/ha) matches the plot yield that was adjusted to 10% moisture, scaled to hectares. yield_moisture is 10.
# p0 is the treatment without phosphorus. The phosphorus rate for p1 is not in the file, so P_fertilizer is 0 for p0 and left empty for p1.
# The yield file records pigeon pea only. Soybean is the intercrop named in the treatment, and it is not a separate yield row.
# Country Malawi is the geographic coverage of the dataset. Golomoti and Nsipe are the sites in the file. There are no coordinates.
# Rows after the trial records are blank and are dropped.

## ISSUES
# The phosphorus rate for p1 is not in the file.
# No planting date, harvest date, or coordinates.
# write_files: missing variables: carob_contributor, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# 001_pigeonpeaProductivityParameters_PPBNF.csv (biological nitrogen fixation and soil mineral N).
# 002_pigeonpeaProductivityParameters_Nodulation.csv.


carob_script <- function(path) {

"
On-farm pigeon pea, sole and intercropped with inoculated soybean, with and without phosphorus fertilizer. Sites are Golomoti and Nsipe in Malawi. Africa RISING.
"

	uri <- "doi:10.7910/DVN/TROBCF"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "LUANAR",
		publication = NA,
		project = "Africa RISING",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "003_pigeonpeaProductivityParameters_PPgrainYield.csv"]
	r <- read.csv(f, check.names=FALSE)
	r <- r[r$Site %in% c("Golomoti", "Nsipe"), ]

	inter <- r[["Cropping system"]] == "pigeonpea/soybean"
	dm <- r[["total dry matter yield (kgha-1"]]

	d <- data.frame(
		country = "Malawi",
		trial_id = r$Site,
		location = r$Site,
		plot_id = paste(r[["Farmer ID"]], r$Rep),
		rep = as.integer(r$Rep),
		crop = "pigeon pea",
		variety = r$Variety,
		yield_part = "seed",
		treatment = r[["Treatment Description"]],
		intercropped = inter,
		intercrops = ifelse(inter, "soybean", NA_character_),
		P_fertilizer = ifelse(r[["p level"]] == "p0", 0, NA_real_),
		yield = r[["Grain Yield (Kg/ha)"]],
		yield_moisture = 10,
		dmy_residue = r[["dry biomass (kgha-1)"]],
		dmy_total = dm,
		harvest_index = r[["Harvest Index"]] * 100,
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
