# R script for "carob"
# license: GPL (>=3)

## NOTES
# Four sowing dates at Nkolbisson (IITA Cameroon station), 2020-2021. Treatment is an imposed
#   defoliation class (Scale), including a separate Control, 20 plants per class.
# Sowing_date is day/month/year (28/10/2020 cannot be month-first).
# Season stored as wet/dry; the original "Rainy 1/2" and "Dry 1/2" labels are in season_label_.
# The column Yield equals kernels per cob x weight per grain, i.e. grams of grain on that ear,
#   not kg/ha (the dictionary calls it kg/ha). seed_weight is weight per grain x 1000.
#   Grain_yield, g_ha and t_h_kernel_factor are formulas of kernel number (Grain_yield = g_ha x 8,
#   t_h_kernel_factor = Grain_yield x 60) and do not use grain weight, so they are not a yield.
# Plant_height is stored twice (never both filled); the non-missing value is used. Unit cm.
# Row spacing in the file is 50 cm. Sample-row length is 8 m.
# GPS is filled on the second experiment only. Other rows use the dataset coverage point
#   03°51.791'N, 011°27.706'E. One longitude 44.46291 sits between 11.46286 and 11.46292
#   and was corrected to 11.46291.
# 117 trailing rows with no experiment, season or scale were dropped.

## ISSUES
# 1. No kg/ha yield is reported. Confirm whether the 8 m x 50 cm sample and the constant
#    "Number_of_ears" = 5 can be used as a harvest area. They look like a protocol constant.
# 2. N/P/K, irrigation and harvest date are not in the files (the title says "optimum" conditions).
# 3. What Observation1-8 are (leaf positions or repeated scores) is not defined; only Average is kept.
# 4. SB_damage scale is not defined (values 0-90). Scale "One (0%)" and "Control" both occur.
# 5. write_files: missing yield, yield_moisture, irrigated, N/P/K, harvest_date;
#    pest_species Spodoptera frugiperda is not in the vocabulary. yield_part is grain.
# 6. "Spodoptera frugiperda" is not yet in the pest vocabulary. The defoliation was imposed.

## NOT PROCESSED
# plant-recovery-from-faw-damage_complete-data-set-kf.csv, Observation1-8: eight damage scores per plant; Average is their summary and the dictionary does not define each column.
# Grain_yield, g_ha, t_h_kernel_factor: kernel-count formulas, not measured t/ha or g/ha (see NOTES).
# Number_of_ears: 5 on every filled row (sample size, not a stand count).
# Conver_sample_area_to_ha: constant 500; the hectare factor is not explained.
# Kernel_size: 90000 or 110000; meaning not defined.
# ID: row number.

## NEW VARIABLES
# season_label_: original season text (Rainy 1, Rainy 2, Dry 1, Dry 2)
# leaf_damage_pct_: Average damage score for the plant (0-100); same number as pest_severity
# stemborer_damage_: SB_damage, scale not documented
# grain_g_ear_: grain weight of the ear in grams (the Yield column)
# kernel_rows_: number of kernel rows per cob
# kernels_per_row_: kernels per row
# kernels_per_cob_: kernels per cob
# cob_length_: cob length, unit not stated (values up to 31.5)
# cob_width_: cob width, unit not stated (values up to 6)
# sample_length_m_: length of the sampled row (8 m in the file)
# plant_id_: plant number within the defoliation class

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
		publication = NA,
		project = "Coordinating elucidation of science and effectiveness of Local Innovations for Managing Fall armyworm in Africa",
		design = "Defoliation classes (0%, 1-25%, 26-50%, 51-75%, >75%) plus a control; 20 plants per class; four sowing dates",
		data_type = "on-station experiment",
		treatment_vars = "treatment",
		response_vars = "plant_height;seed_weight;pest_severity",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 72,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "plant-recovery-from-faw-damage_complete-data-set-kf.csv"]
	r <- read.csv(f, check.names = FALSE)
	r <- r[trimws(r$Experiment) != "" & !is.na(r$Experiment), ]

	h1 <- r[[19]]
	h2 <- r[[26]]
	ht <- h1
	ht[is.na(ht)] <- h2[is.na(ht)]

	seas <- trimws(r$Season)
	lon <- r$Long
	# single bad GPS; see NOTES
	lon[!is.na(lon) & lon > 20] <- 11.46291
	lat <- r$Lat
	miss <- is.na(lat) | is.na(lon)
	# dataset coverage point, 03°51.791'N, 011°27.706'E
	lat[miss] <- 3 + 51.791 / 60
	lon[miss] <- 11 + 27.706 / 60

	d <- data.frame(
		country = "Cameroon",
		location = "Nkolbisson",
		adm1 = "Centre",
		latitude = lat,
		longitude = lon,
		elevation = r$Altitude,
		geo_from_source = TRUE,
		planting_date = as.character(as.Date(r$Sowing_date, "%d/%m/%Y")),
		season = ifelse(grepl("Rainy", seas), "wet", ifelse(grepl("Dry", seas), "dry", NA_character_)),
		season_label_ = seas,
		treatment = trimws(r$Scale),
		plant_id_ = as.character(r$Plant),
		crop = "maize",
		stress = "pests",
		pest_species = "Spodoptera frugiperda",
		pest_severity = ifelse(is.na(r$Average), NA_character_, as.character(round(r$Average, 2))),
		leaf_damage_pct_ = r$Average,
		stemborer_damage_ = r$SB_damage,
		plant_height = ht,
		seed_weight = r$Weight_per_grain * 1000,
		grain_g_ear_ = r$Yield,
		kernel_rows_ = r$Num_of_Rows_per_cob,
		kernels_per_row_ = r$Num_of_kernel_per_row,
		kernels_per_cob_ = r$Num_of_kernels_per_cob,
		cob_length_ = r$Cob_length,
		cob_width_ = r$Cob_Width,
		row_spacing = r$Row_spacing,
		sample_length_m_ = r$Length_of_sample_area,
		trial_id = paste(trimws(r$Experiment), r$Year, seas, sep = "_"),
		on_farm = FALSE,
		is_survey = FALSE,
		yield_part = "grain"
	)

	carobiner::write_files(path, meta, d)
}
