# R script for "carob"
# license: GPL (>=3)

## NOTES
# Long-term conservation-agriculture experiment. The title places it in eastern India, so the country is India. The workbook has no district and no coordinates.
# Info-sheet codes: ZT zero tillage, PB permanent beds, CT conventional tillage. CT also mentions puddling in rice. The row is a whole cropping system, so land preparation is stored as conventional and puddling is not added as a second method.
# The Info sheet spells Miaze and Permanenet. The data sheet spells Maize-chickpea and the tillage code is PB.
# Yield columns are maize-equivalent yield in t/ha, not the grain yield of one component crop. They are stored in kg/ha under new names.
# Cost, gross return, and net return are labeled Rs/ha. The numbers are about 200 to 630, which is small for rupees per hectare. They are stored as printed.
# Sheet3 is empty. The PDF is the paper and is not a data table.

## ISSUES
# Maize-equivalent yield is not the grain yield of one crop, so crop and yield are not filled.
# The season label is not a controlled season name.
# Costs labeled Rs/ha are about 200 to 630 and are stored as printed.
# write_files: missing variables: crop, yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: cropping_system_ (crop?). unknown variable: season_label_ (season?). unknown variables: kharif_mey_kg_ha_, rabi_mey_kg_ha_, system_mey_kg_ha_, kharif_rainfall_mm_, rabi_rainfall_mm_, system_rainfall_mm_, kharif_labour_days_, rabi_labour_days_, system_labour_days_, kharif_cost_rs_ha_, rabi_cost_rs_ha_, system_cost_rs_ha_, kharif_gross_return_rs_ha_, rabi_gross_return_rs_ha_, system_gross_return_rs_ha_, kharif_net_return_rs_ha_, rabi_net_return_rs_ha_, system_net_return_rs_ha_, kharif_bc_ratio_, rabi_bc_ratio_, system_bc_ratio_, kharif_input_water_mm_, rabi_input_water_mm_, system_input_water_mm_, kharif_iwp_kg_m3_, rabi_iwp_kg_m3_, system_iwp_kg_m3_, kharif_protein_t_ha_, rabi_protein_t_ha_, system_protein_t_ha_. contributor:

## NEW VARIABLES
# cropping_system_: cropping system named on the data sheet.
# season_label_: season label such as 2013-14.
# kharif_mey_kg_ha_: kharif maize-equivalent yield, t/ha times 1000.
# rabi_mey_kg_ha_: rabi maize-equivalent yield, t/ha times 1000.
# system_mey_kg_ha_: system maize-equivalent yield, t/ha times 1000.
# kharif_rainfall_mm_: rainfall received in kharif, mm.
# rabi_rainfall_mm_: rainfall received in rabi, mm.
# system_rainfall_mm_: rainfall received by the system, mm.
# kharif_input_water_mm_: rainfall plus irrigation in kharif, mm.
# rabi_input_water_mm_: rainfall plus irrigation in rabi, mm.
# system_input_water_mm_: rainfall plus irrigation for the system, mm.
# kharif_labour_days_: kharif labour, mandays.
# rabi_labour_days_: rabi labour, mandays.
# system_labour_days_: system labour, mandays.
# kharif_cost_rs_ha_: kharif cost of production, as printed under Rs/ha.
# rabi_cost_rs_ha_: rabi cost of production, as printed under Rs/ha.
# system_cost_rs_ha_: system cost of production, as printed under Rs/ha.
# kharif_gross_return_rs_ha_: kharif gross return, as printed under Rs/ha.
# rabi_gross_return_rs_ha_: rabi gross return, as printed under Rs/ha.
# system_gross_return_rs_ha_: system gross return, as printed under Rs/ha.
# kharif_net_return_rs_ha_: kharif net return, as printed under Rs/ha.
# rabi_net_return_rs_ha_: rabi net return, as printed under Rs/ha.
# system_net_return_rs_ha_: system net return, as printed under Rs/ha.
# kharif_bc_ratio_: kharif benefit-cost ratio.
# rabi_bc_ratio_: rabi benefit-cost ratio.
# system_bc_ratio_: system benefit-cost ratio.
# kharif_iwp_kg_m3_: kharif irrigation-water productivity, kg/m3.
# rabi_iwp_kg_m3_: rabi irrigation-water productivity, kg/m3.
# system_iwp_kg_m3_: system irrigation-water productivity, kg/m3.
# kharif_protein_t_ha_: kharif protein, t/ha as printed.
# rabi_protein_t_ha_: rabi protein, t/ha as printed.
# system_protein_t_ha_: system protein, t/ha as printed.

## NOT PROCESSED
# The code column CS, because the cropping-system name is stored.
# The year index column. The season label is stored.
# Sheet3.
# The PDF of the paper.


carob_script <- function(path) {

"
Sustainability, productivity, profitability and nutritional diversity of six cropping systems under conservation agriculture: a long term study in eastern India.

Intensive rice-wheat under conventional tillage is the main system of the Eastern Gangetic Plains. This workbook reports kharif, rabi, and whole-system maize-equivalent yield, rainfall, labour, costs, returns, water, and protein for six cropping systems and three tillage methods.
"

	uri <- "hdl:11529/10548911"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method",
		response_vars = "kharif_mey_kg_ha_;rabi_mey_kg_ha_;system_mey_kg_ha_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.6
	)

	f <- ff[grepl("xlsx$", basename(ff))]
	r <- carobiner::read.excel(f, sheet="Data")
	nm <- names(r)
	col <- function(pat) {
		i <- grep(pat, nm)
		if (length(i) != 1) stop(paste("column", pat))
		r[[i]]
	}
	num <- function(z) {
		if (is.numeric(z)) return(z)
		z <- trimws(as.character(z))
		z[z %in% c("", "NA", "n/a")] <- NA_character_
		z[!is.na(z) & grepl("[[:alpha:]]", z)] <- NA_character_
		as.numeric(z)
	}
	lp_code <- trimws(as.character(col("^Tillage$")))
	lp <- c(ZT="none", PB="permanent beds", CT="conventional")
	land <- unname(lp[lp_code])
	sys <- tolower(trimws(as.character(col("^Cropping System$"))))
	season <- trimws(as.character(col("^Year1$")))

	d <- data.frame(
		country = "India",
		trial_id = season,
		rep = as.integer(num(col("^Replication$"))),
		land_prep_method = land,
		cropping_system_ = sys,
		season_label_ = season,
		kharif_mey_kg_ha_ = num(col("^Kharif Maize Equivalent")) * 1000,
		rabi_mey_kg_ha_ = num(col("^Rabi Maize Equivalent")) * 1000,
		system_mey_kg_ha_ = num(col("^System Maize Equivalent")) * 1000,
		kharif_rainfall_mm_ = num(col("^Rainfall received Kharif$")),
		rabi_rainfall_mm_ = num(col("^Rainfall received Rabi$")),
		system_rainfall_mm_ = num(col("^Rainfall received System$")),
		kharif_labour_days_ = num(col("^Kharif labour")),
		rabi_labour_days_ = num(col("^Rabi labour")),
		system_labour_days_ = num(col("^System labour")),
		kharif_cost_rs_ha_ = num(col("^Kharif Cost")),
		rabi_cost_rs_ha_ = num(col("^Rabi Cost")),
		system_cost_rs_ha_ = num(col("^System Cost")),
		kharif_gross_return_rs_ha_ = num(col("^Kharif Gross")),
		rabi_gross_return_rs_ha_ = num(col("^Rabi Gross")),
		system_gross_return_rs_ha_ = num(col("^System Gross")),
		kharif_net_return_rs_ha_ = num(col("^Kharif Net")),
		rabi_net_return_rs_ha_ = num(col("^Rabi Net")),
		system_net_return_rs_ha_ = num(col("^System Net")),
		kharif_bc_ratio_ = num(col("^Kharif B:C")),
		rabi_bc_ratio_ = num(col("^Rabi B:C")),
		system_bc_ratio_ = num(col("^System B:C")),
		kharif_input_water_mm_ = num(col("^Input water .* Kharif$")),
		rabi_input_water_mm_ = num(col("^Input water .* Rabi$")),
		system_input_water_mm_ = num(col("^Input water .* System$")),
		kharif_iwp_kg_m3_ = num(col("^Iwp .* Kharif$")),
		rabi_iwp_kg_m3_ = num(col("^Iwp .* Rabi$")),
		system_iwp_kg_m3_ = num(col("^Iwp .* System$")),
		kharif_protein_t_ha_ = num(col("^Kharif Protein")),
		rabi_protein_t_ha_ = num(col("^Rabi Protein")),
		system_protein_t_ha_ = num(col("^System Protein")),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d <- d[!is.na(d$land_prep_method) & !is.na(d$cropping_system_), ]

	carobiner::write_files(path, meta, d)
}
