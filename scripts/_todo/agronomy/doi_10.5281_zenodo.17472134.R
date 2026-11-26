# R script for "carob"
# license: GPL (>=3)

## NOTES
# Raimat (Lleida, Catalonia, Spain) Chardonnay vineyard experiment 2024 (ET4DROUGHT): cover crop (C) vs tillage (LL) crossed with full (1 / 100%) vs deficit irrigation (12 plots).
# Harvest (ET4DROUGHT_IRTA_Cosecha_2024.xls, 2024-08-28): 3 vines (Cep 1..3) per plot with bunch count (NombreRaims) and bunch fresh weight (Pes, kg). Plot mean kg/vine and bunches/vine are stored in d, and individual vine harvest records plus repeated stem water potential (7 dates from ET4DROUGHT_IRTA_Potencial stem 2024.xlsx, pressure chamber bars -> MPa as -bar/10) are preserved in the long table linked via record_id.
# Variety "Chardonnay" comes from the companion ET4DROUGHT_IRTA_Intercepcio radiacio_2024.xls header ("Vinya Chardonnay Raimat 2024").
# Estimated Raimat winery coordinates (41.677, 0.450) with geo_from_source = FALSE and geo_uncertainty = 2000 m.

## ISSUES
# Vine spacing / plant density is not stated in the files, so kg/vine cannot be converted to yield in kg/ha (yield omitted rather than leaving an all-NA placeholder).
# Fertilizer rates, planting date (perennial vineyard), yield moisture, and cover-crop species are not reported.
# "IRTA" is not in terminag values_organization.csv.
# write_files: missing carob_contributor, yield, yield_moisture, K/N/P_fertilizer, planting_date; invalid organization IRTA; unknown yield_per_plant_, bunches_per_vine_, irrigation_deficit_, vine_yield_kg_, vine_bunches_, stem_water_potential_MPa_.

## NOT PROCESSED
# ET4DROUGHT_IRTA_Dosel vegetativo_2024.xls: canopy geometry measurements (1 date).
# ET4DROUGHT_IRTA_Intercepcio radiacio_2024.xls: multi-point PAR interception sheets (FIPAR).
# ET4DROUGHT_IRTA_2024_*_porometro.xlsx: raw porometer sensor logs (7 dates, 96-107 instrument columns, mixed crop sheet names).
# ET4DROUGHT_IRTA_Sondaneutronesvinya_2024.xls: raw neutron-probe calibration and counts across depths.
# desktop.ini and readme.txt files.

## NEW VARIABLES
# yield_per_plant_: mean grape bunch fresh weight across 3 sampled vines per plot (kg/vine).
# bunches_per_vine_: mean grape bunch count across 3 sampled vines per plot (count/vine).
# irrigation_deficit_: logical, TRUE for deficit irrigation and FALSE for full (100%) irrigation.
# vine_yield_kg_: individual vine bunch fresh weight (kg) in long table.
# vine_bunches_: individual vine bunch count in long table.
# stem_water_potential_MPa_: midday stem water potential (MPa, converted from positive pressure-chamber bar readings as -bar / 10) in long table.


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
		project = "ET4DROUGHT",
		design = NA,
		data_type = "experiment",
		treatment_vars = "cover_crop_used;land_prep_method;irrigation_deficit_",
		response_vars = "yield_per_plant_;bunches_per_vine_;stem_water_potential_MPa_",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "ET4DROUGHT_IRTA_Cosecha_2024.xls"]
	r <- carobiner::read.excel(f, skip=11)
	# 12 plots; drop trailing summary rows
	r <- r[!is.na(r[[1]]) & r[[1]] %in% 1:12, ]

	pes <- cbind(as.numeric(r[[5]]), as.numeric(r[[7]]), as.numeric(r[[9]]))
	n_bunch <- cbind(as.numeric(r[[4]]), as.numeric(r[[6]]), as.numeric(r[[8]]))

	trt_soil <- trimws(as.character(r[[2]]))
	subtrt <- trimws(as.character(r[[3]]))
	full_irr <- grepl("1|100", subtrt)

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = "Raimat_2024",
		plot_id = as.character(as.integer(r[[1]])),
		treatment = paste(trt_soil, subtrt, sep="_"),
		cover_crop_used = trt_soil == "C",
		land_prep_method = ifelse(trt_soil == "LL", "tillage", "none"),
		irrigated = TRUE,
		irrigation_deficit_ = !full_irr,
		yield_per_plant_ = rowMeans(pes, na.rm=TRUE),
		bunches_per_vine_ = rowMeans(n_bunch, na.rm=TRUE),
		harvest_date = "2024-08-28",
		crop = "grape",
		variety = "Chardonnay",
		yield_part = "fruit",
		yield_isfresh = TRUE,
		country = "Spain",
		adm1 = "Catalonia",
		location = "Raimat",
		latitude = 41.677,
		longitude = 0.450,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		on_farm = TRUE,
		is_survey = FALSE
	)

	# Individual vine harvest observations (3 vines per plot)
	l_vines <- do.call(rbind, lapply(1:3, function(v) {
		data.frame(
			record_id = d$record_id,
			date = "2024-08-28",
			order = as.integer(v),
			vine_yield_kg_ = pes[, v],
			vine_bunches_ = n_bunch[, v]
		)
	}))

	# Midday stem water potential across 7 dates (pressure chamber readings in bar -> -bar/10 MPa)
	fs <- ff[basename(ff) == "ET4DROUGHT_IRTA_Potencial stem 2024.xlsx"]
	date_sheets <- c("2024-05-28", "2024-06-27", "2024-07-03", "2024-07-18", "2024-07-30", "2024-08-19", "2024-09-13")
	swp_list <- lapply(date_sheets, function(sh) {
		x <- carobiner::read.excel(fs, sheet=sh, skip=10)
		x <- x[!is.na(x[[1]]) & x[[1]] %in% 1:12, ]
		if (!nrow(x)) return(NULL)
		rid <- d$record_id[match(as.character(as.integer(x[[1]])), d$plot_id)]
		# Columns 4..6 hold the 3 measured stems (pressure chamber bars)
		do.call(rbind, lapply(1:3, function(k) {
			val <- as.numeric(x[[3 + k]])
			data.frame(
				record_id = rid,
				date = sh,
				order = as.integer(k),
				stem_water_potential_MPa_ = -val / 10
			)
		}))
	})
	l_swp <- do.call(rbind, swp_list)
	l_swp <- l_swp[!is.na(l_swp$record_id) & !is.na(l_swp$stem_water_potential_MPa_), ]

	dlong <- carobiner::bindr(l_vines, l_swp)
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
