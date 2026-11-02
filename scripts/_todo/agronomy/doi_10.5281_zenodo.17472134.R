# R script for "carob"

## NOTES
# Raimat (Lleida) vineyard trial 2024: cover crop (C) vs tillage (LL), deficit vs full irrigation.
# Harvest is bunch count and kg on three vines per plot; mean kg/vine stored as yield_per_plant_.
# Harvest date Excel serial 45532 = 2024-08-28. Physiological files (porometer, stem water potential, neutron probe, FIPAR) not processed.

## ISSUES
# Vine spacing / plants per ha not in the files; cannot convert kg/vine to yield kg/ha.
# Fertilizer not reported. Cover-crop species not named.
# Plot coordinates not in the files; Raimat winery 41.677, 0.450 hardcoded (geo_from_source=FALSE). Confirm.
# Long records (stem water potential, gas exchange, soil water) not processed.
# write_files: yield is NA (kg/ha unknown); N/P/K and planting_date not in source; IRTA not in org vocab.

## NEW VARIABLES
# yield_per_plant_: mean bunch yield of three sampled vines (kg/vine)
# irrigation_deficit_: TRUE if deficit irrigation, FALSE if full (100%) irrigation

carob_script <- function(path) {

"
Field data collection in a vineyard experiment with cover crop and tillage during 2024

This dataset correponds to physiological measurements conducted in a vineyard trial with different treatments including cover crop and tillage, as well as full and deficit irrigation. Measurements include stem water potential, leaf gas exchanges, harvest and also soil water content measured with neutron probe.
"

	uri <- "doi:10.5281/zenodo.17472134"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "IRTA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "cover_crop_used;irrigation_deficit_",
		response_vars = "yield_per_plant_",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 30,
		carob_effort = 0.8
	)

	f <- ff[grepl("Cosecha_2024", basename(ff))]
	r <- carobiner::read.excel(f, skip=11)
	# 12 plots; drop trailing summary rows
	r <- r[!is.na(r[[1]]) & r[[1]] %in% 1:12, ]

	pes <- cbind(as.numeric(r[[5]]), as.numeric(r[[7]]), as.numeric(r[[9]]))
	n_bunch <- cbind(as.numeric(r[[4]]), as.numeric(r[[6]]), as.numeric(r[[8]]))

	subtrt <- as.character(r[[3]])
	full_irr <- grepl("1|100", subtrt)

	d <- data.frame(
		trial_id = "Raimat_2024",
		plot_id = as.character(r[[1]]),
		treatment = paste(r[[2]], subtrt, sep="_"),
		cover_crop_used = r[[2]] == "C",
		irrigated = TRUE,
		irrigation_deficit_ = !full_irr,
		yield_per_plant_ = rowMeans(pes, na.rm=TRUE),
		harvest_date = "2024-08-28",
		crop = "grape",
		country = "Spain",
		adm1 = "Catalonia",
		location = "Raimat",
		latitude = 41.677,
		longitude = 0.450,
		geo_from_source = FALSE,
		on_farm = TRUE,
		is_survey = FALSE,
		yield_part = "fruit"
	)

	# yield kg/ha unknown without vine density
	d$yield <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}
