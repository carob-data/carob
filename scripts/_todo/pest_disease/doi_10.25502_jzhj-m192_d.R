# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Grain yield in kg/ha is not directly harvested per plot: t_h_kernel_factor (stored in yield_est_) is a yield-component estimate calculated from kernels per cob assuming 5 ears per sample row and a fixed kernel-per-bushel factor (90,000 in rainy seasons, 110,000 in dry seasons; doi:10.1093/jee/toae235), while Yield (grain_ear_weight_) is measured ear grain weight in g (Experiment 1 only). Decide whether yield_est_ should be mapped to yield.
# 2. Fertilizer rates (290.4 kg N/ha, 19.21 kg P/ha, 36.51 kg K/ha) are calculated from doi:10.1093/jee/toae235 Methods (11 g/plant NPK 20-10-10 at 2 WAP + 11 g/plant urea at 4 WAP, at 50 x 50 cm = 40,000 plants/ha assuming 1 plant/hill after thinning); verify plants/hill in the full text.
# 3. In Experiment 4, raw column Average equals the mean of Observation1-4 rather than all 7 non-missing observations; all weekly observations (Observation1-8) are preserved in the long table dd.
# 4. SB_damage values range 0-90 (% stem borer damage?); exact scale is not stated in the dictionary.
# 5. Cob_length (up to 31.5) and Cob_Width (up to 6) units are not stated (likely cm).
# 6. One GPS typo in Experiment 2 (ID 131, Long = 44.46291 between 11.46286 and 11.46292) was corrected to 11.46291; Experiments 1 and 4 lack row GPS and use the CKAN coverage point (3.86318 N, 11.46177 E, Nkolbisson).

## NEW
# season_label_: original trial season label ("Rainy 1", "Rainy 2", "Dry 1", "Dry 2")
# defoliation_class_: FAW visual defoliation class ("0%", "1-25%", "26-50%", "51-75%", ">75%", "control")
# plant_number_: tagged plant number within defoliation class (1-20)
# grain_ear_weight_: measured grain weight per ear, g (Yield column, Experiment 1 only)
# yield_est_: kernel-count bushel-formula estimated grain yield, kg/ha (t_h_kernel_factor)
# kernel_rows_: number of kernel rows per cob (Num_of_Rows_per_cob)
# kernels_per_row_: number of kernels per row (Num_of_kernel_per_row)
# kernels_per_cob_: number of kernels per cob (Num_of_kernels_per_cob)
# cob_length_: cob length, unit not stated (likely cm; Cob_length)
# cob_width_: cob width, unit not stated (likely cm; Cob_Width)
# sample_length_: length of sampled row, m (8 m; Length_of_sample_area)

## MISSING
# yield, yield_moisture, harvest_date

## NOTES
# Four rainfed seasonal trials of maize (variety ACR 06 TZL COMP4C4F2) at IITA Nkolbisson, Cameroon
#   (2020-2021; published in Abang et al. 2024, doi:10.1093/jee/toae235).
# In each trial, 20 plants were tagged per natural FAW defoliation class (Scale 1-5) plus an
#   insecticide-protected Control (480 plants total; 117 trailing empty rows removed).
# d holds plant-level measurements (combining the two non-overlapping Plant_height columns);
#   dd (long) holds weekly FAW defoliation scores (Observation1-8, order 1-8) and stem borer damage
#   (SB_damage) linked by record_id.

## NOT PROCESSED
# Average: summary of Observation1-8 (or 1-4 in Exp 4); individual weekly observations are in dd.
# g_ha, Grain_yield: intermediate steps of the bushel-formula estimate (t_h_kernel_factor = Grain_yield x 60 = g_ha x 480).
# Number_of_ears (constant 5), Conver_sample_area_to_ha (constant 500), Kernel_size (90000 or 110000): formula constants.


carob_script <- function(path) {

"
Don’t panic, maize plants can compensate significant damage by the Fall Armyworm and recover under optimum crop production conditions

Assessing how maize plants damage at different injury levels can recover and reproduce
"

	uri <- "doi:10.25502/jzhj-m192/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = "doi:10.1093/jee/toae235",
		project = "Coordinating elucidation of science and effectiveness of Local Innovations for Managing Fall armyworm in Africa",
		design = "4 seasonal trials at IITA Nkolbisson; 5 natural FAW defoliation classes (0%, 1-25%, 26-50%, 51-75%, >75%) plus insecticide-protected control; 20 tagged plants per class",
		data_type = "on-station experiment",
		treatment_vars = "insecticide_used;defoliation_class_",
		response_vars = "plant_height;seed_weight;pest_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "plant-recovery-from-faw-damage_complete-data-set-kf.csv"]
	r <- read.csv(f, check.names = FALSE, fileEncoding = "latin1")
	r <- r[!is.na(r$Experiment) & nzchar(trimws(r$Experiment)), ]

	# Plant_height is split across two non-overlapping columns (19 for Exp 1/3/4, 26 for Exp 2)
	ht <- r[[19]]
	ht[is.na(ht)] <- r[[26]][is.na(ht)]

	seas <- trimws(r$Season)
	tr <- trimws(r$Scale)
	defol_map <- c(
		"One (0%)" = "0%",
		"Two (1-25%)" = "1-25%",
		"Three (26-50%)" = "26-50%",
		"Four (51-75%)" = "51-75%",
		"Five (>75%)" = ">75%",
		"Control" = "control"
	)

	# Fix single longitude typo (44.46291 -> 11.46291) and fill missing Exp 1/4 GPS from CKAN coverage (03°51.791'N, 011°27.706'E)
	lon <- as.numeric(r$Long)
	lon[!is.na(lon) & lon > 20] <- 11.46291
	lat <- as.numeric(r$Lat)
	miss <- is.na(lat) | is.na(lon)
	lat[miss] <- 3 + 51.791 / 60
	lon[miss] <- 11 + 27.706 / 60

	# Management from Abang et al. (2024, doi:10.1093/jee/toae235): variety ACR 06 TZL COMP4C4F2,
	# 50 x 50 cm spacing (40,000 plants/ha), 11 g/plant NPK 20-10-10 at 2 WAP + 11 g/plant urea at 4 WAP,
	# rainfed across rainy and dry seasons, Control kept FAW-free with insecticide.
	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = paste(trimws(r$Experiment), r$Year, seas, sep = "_"),
		country = "Cameroon",
		adm1 = "Centre",
		adm2 = "Mfoundi",
		location = "Nkolbisson",
		latitude = lat,
		longitude = lon,
		elevation = as.numeric(r$Altitude),
		geo_from_source = TRUE,
		planting_date = as.character(as.Date(r$Sowing_date, "%d/%m/%Y")),
		season = ifelse(grepl("Rainy", seas), "wet", ifelse(grepl("Dry", seas), "dry", NA_character_)),
		season_label_ = seas,
		crop = "maize",
		variety = "ACR 06 TZL COMP4C4F2",
		treatment = tr,
		defoliation_class_ = unname(defol_map[tr]),
		insecticide_used = tr == "Control",
		fertilizer_used = TRUE,
		fertilizer_type = "NPK; urea",
		N_fertilizer = 290.4,
		P_fertilizer = round(44 / 2.29, 2),
		K_fertilizer = round(44 / 1.2051, 2),
		irrigated = FALSE,
		row_spacing = as.numeric(r$Row_spacing),
		plant_spacing = 50,
		plant_number_ = as.integer(r$Plant),
		plant_height = as.numeric(ht),
		seed_weight = as.numeric(r$Weight_per_grain) * 1000,
		grain_ear_weight_ = as.numeric(r$Yield),
		yield_est_ = as.numeric(r$t_h_kernel_factor),
		kernel_rows_ = as.numeric(r$Num_of_Rows_per_cob),
		kernels_per_row_ = as.numeric(r$Num_of_kernel_per_row),
		kernels_per_cob_ = as.numeric(r$Num_of_kernels_per_cob),
		cob_length_ = as.numeric(r$Cob_length),
		cob_width_ = as.numeric(r$Cob_Width),
		sample_length_ = as.numeric(r$Length_of_sample_area),
		stress = "pests",
		on_farm = FALSE,
		is_survey = FALSE,
		yield_part = "grain"
	)

	# Long table: weekly FAW defoliation observations (Observation1-8) plus stem borer damage (SB_damage)
	pieces <- vector("list", 9)
	for (i in 1:8) {
		v <- r[[paste0("Observation", i)]]
		keep <- !is.na(v)
		pieces[[i]] <- data.frame(
			record_id = d$record_id[keep],
			order = as.integer(i),
			pest_species = "Spodoptera frugiperda",
			pest_severity = as.character(v[keep]),
			severity_scale = "0-100"
		)
	}
	sb_keep <- !is.na(r$SB_damage)
	pieces[[9]] <- data.frame(
		record_id = d$record_id[sb_keep],
		order = NA_integer_,
		pest_species = "stem borer",
		pest_severity = as.character(r$SB_damage[sb_keep]),
		severity_scale = NA_character_
	)
	dd <- do.call(rbind, pieces)

	carobiner::write_files(path, meta, d, long = dd)
}
