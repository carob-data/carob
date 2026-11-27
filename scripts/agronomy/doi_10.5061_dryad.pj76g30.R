# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Plot yields are not in the data file (only country-crop-treatment means appear in the paper).
# Two Nigerian soybean site codes (NG_FAN, 12 rows; NG_KAR, 5 rows) could not be matched to Wortmann's companion OFRA datasets (doi:10.5061/dryad.5t065bb, doi:10.5061/dryad.tt6h5h1) and have NA location and coordinates.
# Cassava storage-root flour nutrient concentrations (39 rows) are stored alongside grain crops in grain_* columns because terminag only has root_N, root_K, and root_Na (not root_P, root_Ca, etc.).
# Four rows have Contr1Macro2MgZnSB3 = NA and are dropped.

## MISSING
# yield, yield_moisture, harvest_date.

## NOTES
# Grain (10 cereal/pulse crops) and cassava flour nutrient concentrations from 70 OFRA fertilizer trials in Mali, Niger, Nigeria, and Tanzania (2015-2016; Wortmann et al. 2018, doi:10.2134/agronj2018.04.0274).
# Locations, coordinates, and elevations for 32 of 34 Plot_code site prefixes (806 of 823 rows) were matched to Wortmann's companion OFRA datasets (doi:10.5061/dryad.5t065bb, verified by identical trial-mean soil chemistry, and doi:10.5061/dryad.tt6h5h1; Kirkissoye georeferenced directly as Niamey V irrigated rice perimeter).
# Units follow the Readme sheet: grain/root % -> mg/g (* 10), ppm -> mg/g (/ 1000); soil TotN g/kg -> mg/kg (* 1000), TotCarbon and Clay/Silt/Sand g/kg -> % (/ 10).
# Elemental fertilizer rates follow the Readme sheet: NPK = 90/15/20 kg/ha (30/15/20 for common bean; 0/15/20 PK for other pulses); MgSZnB adds 10 Mg, 15 S, 2.5 Zn, 0.5 B kg/ha to the macronutrient treatment.

carob_script <- function(path) {

"Fertilizer application can affect nutrient concentrations of edible plant products. Data from 70 crop-nutrient response trials conducted in Mali, Niger, Nigeria, and Tanzania were used to evaluate nutrient application effects on nutrient concentrations for grain of five pulse and five cereal crops and for storage roots of cassava (Manihot esculenta L.).

Treatments per trial were ≥12 but this study was limited to: no fertilizer applied; macronutrients applied (NPK or PK); and the macronutrient treatment plus Mg, S, Zn, and B applied (MgSZnB). Dried grain or cassava flour samples were analyzed for concentrations of all essential soil nutrients except for Ni and Cl. Concentrations of N and K were positively correlated with concentrations of most other nutrients.

The concentrations were relatively low overall for cowpea (Vigna unguiculata L.) and pigeonpea (Cajanus cajan L.) compared with other pulse crops and for maize (Zea mays L.) compared with other cereal crops. Application of NPK or PK had little effect on nutrient concentrations except for increased mean cereal grain concentrations for N, Ca, Mg, S, Zn, Cu, and B.

Bean (Phaseolus vulgaris L.), maize and rice (Oryza sativa L.) grain concentrations were reduced by MgSZnB for N, K, S, Cu, Mn, and B.There were no or inconsistent effects of MgSZnB on other crop-nutrient concentrations. Nutrient concentrations are not reduced by NPK for non-legumes or PK for pulses but MgSZnB often reduced bean and cereal nutrient concentrations with greater reductions for immobile compared with mobile nutrients."

	uri <- "doi:10.5061/dryad.pj76g30"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
		data_organization = "UNL;ABU;INRAN;TARI;IER",
		publication = "doi:10.2134/agronj2018.04.0274",
		project = "Optimization of Fertilizer Recommendations in Africa",
		data_type = "experiment",
		response_vars = "grain_N;grain_P;grain_K;grain_Ca;grain_Mg;grain_S;grain_B;grain_Cu;grain_Fe;grain_Mn;grain_Mo;grain_Na;grain_Zn",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;Mg_fertilizer;Zn_fertilizer;B_fertilizer",
		carob_contributor = "Cedric Ngakou",
		carob_effort = NA,
		carob_date = "2024-06-29",
		notes = NA
	)

	f <- ff[basename(ff) == "Grain Nutrient Conc Study for Tropical Africa 2018.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Data")
	r <- r[!is.na(r$Contr1Macro2MgZnSB3), ]

	# Extract standardized site key from Plot_code (Niger places site in token 3; other countries in token 2)
	pc <- toupper(gsub("[[:space:]]+", "", trimws(as.character(r$Plot_code))))
	parts <- strsplit(pc, "[_-]")
	site_key <- ifelse(
		r$Country == "Niger",
		paste0("NE_", vapply(parts, function(x) x[3], "")),
		vapply(parts, function(x) paste(head(x, 2), collapse = "_"), "")
	)

	crop <- tolower(trimws(r$Crop))
	crop <- carobiner::replace_values(crop, c("bean", "pigeonpea"), c("common bean", "pigeon pea"))

	trt_code <- as.integer(r$Contr1Macro2MgZnSB3)
	is_pulse_noN <- crop %in% c("cowpea", "groundnut", "pigeon pea", "soybean")
	trt_name <- ifelse(
		trt_code == 1L, "control",
		ifelse(trt_code == 2L, ifelse(is_pulse_noN, "PK", "NPK"), "MgSZnB")
	)

	# Fertilizer rates (elemental kg/ha, per Readme sheet)
	has_macro <- trt_code %in% c(2L, 3L)
	has_micro <- trt_code == 3L
	n_rate <- ifelse(has_macro, ifelse(is_pulse_noN, 0, ifelse(crop == "common bean", 30, 90)), 0)

	d <- data.frame(
		country = trimws(r$Country),
		site_key_ = site_key,
		trial_id = paste(site_key, gsub(" ", "_", crop), r$Year, sep = "_"),
		plot_id = trimws(r$Plot_code),
		planting_date = as.character(as.integer(r$Year)),
		crop = crop,
		treatment = trt_name,
		N_fertilizer = n_rate,
		P_fertilizer = ifelse(has_macro, 15, 0),
		K_fertilizer = ifelse(has_macro, 20, 0),
		Mg_fertilizer = ifelse(has_micro, 10, 0),
		S_fertilizer = ifelse(has_micro, 15, 0),
		Zn_fertilizer = ifelse(has_micro, 2.5, 0),
		B_fertilizer = ifelse(has_micro, 0.5, 0),
		on_farm = TRUE,
		irrigated = NA,
		is_survey = FALSE,

		# Soil (Readme: Mehlich-3 except TotN/TotCarbon; g/kg converted to mg/kg or %)
		soil_pH = as.numeric(r$pH),
		soil_B = as.numeric(r$B...21),
		soil_Cu = as.numeric(r$CU),
		soil_Fe = as.numeric(r$FE),
		soil_Mn = as.numeric(r$MN),
		soil_P = as.numeric(r$m3P),
		soil_P_method = "Mehlich3",
		soil_S = as.numeric(r$S...26),
		soil_Zn = as.numeric(r$Zn...27),
		soil_Ca_exch = as.numeric(r$ExCa),
		soil_Mg_exch = as.numeric(r$ExMg),
		soil_K_exch = as.numeric(r$ExK),
		soil_N_total = as.numeric(r$TotN) * 1000,
		soil_C_total = as.numeric(r$TotCarbon) / 10,
		soil_clay = as.numeric(r$Clay) / 10,
		soil_silt = as.numeric(r$Silt) / 10,
		soil_sand = as.numeric(r$Sand) / 10,

		# Grain / cassava flour nutrient concentrations (Readme: % -> mg/g [*10], ppm -> mg/g [/1000])
		grain_B = as.numeric(r$B...7) / 1000,
		grain_Ca = as.numeric(r$Ca) * 10,
		grain_Cu = as.numeric(r$Cu) / 1000,
		grain_Fe = as.numeric(r$Fe) / 1000,
		grain_K = as.numeric(r$K) * 10,
		grain_Mg = as.numeric(r$Mg) * 10,
		grain_Mn = as.numeric(r$Mn) / 1000,
		grain_Na = as.numeric(r$Na) / 1000,
		grain_P = as.numeric(r$P) * 10,
		grain_S = as.numeric(r$S...16) * 10,
		grain_Zn = as.numeric(r$Zn...17) / 1000,
		grain_N = as.numeric(r$N) * 10,
		grain_Mo = as.numeric(r$Mo) / 1000
	)

	# Site coordinates & elevations matched from Wortmann's companion OFRA datasets
	# (doi:10.5061/dryad.5t065bb verified by exact trial-mean soil chemistry, and doi:10.5061/dryad.tt6h5h1)
	geo <- data.frame(
		site_key_ = c(
			"ML_BOU", "ML_CIN", "ML_FIN", "ML_KOL", "ML_LON", "ML_NIO", "ML_SAM",
			"NE_ANG", "NE_BEN", "NE_KAG", "NE_KIR", "NE_KON", "NE_MAG", "NE_MAR", "NE_SAY",
			"NG_BAU", "NG_BIL", "NG_DD", "NG_GIR", "NG_GW", "NG_KD", "NG_KN", "NG_SAM", "NG_ZAR", "NG_ZMF",
			"TZ_CHA", "TZ_DAR", "TZ_ILO", "TZ_KAR", "TZ_KWE", "TZ_MLI", "TZ_SEL", "TZ_UYO", "TZ_VUN1", "TZ_VUN2"
		),
		location = c(
			"Bougouni", "Cinzana", "Finkolo", "Kolombada", "Longorola", "Niono", "Samanko",
			"Angoual Mata", "Bengou", "Kagara", "Kirkissoye", "Konni", "Magaria", "Maradi", "Say",
			"Bauchi", "Billiri", "Dandume", "Girei", "Giwa", "Samaru", "Rogo", "Samaru", "Zaria", "Tsafe",
			"Chambezi", "Dareda", "Ilonga", "Karangai", "Kwedizinga", "Mlingano", "Selian", "Uyole", "Vundemanyinyi", "Vundemanyinyi"
		),
		latitude = c(
			11.3870, 13.2790, 11.2740, 12.6940, 11.3840, 14.2808, 12.5380,
			13.3191, 11.9790, 13.3633, 13.4783, 13.8250, 12.9750, 13.4590, 13.1160,
			10.2630,  9.9311, 11.4330,  9.4123, 11.0943, 11.1784, 11.4092, 11.1784, 11.1766, 11.9284,
			-6.5470, -3.7660, -6.7830, -3.5180, -5.4180, -5.1380, -3.3600, -8.9180, -4.9660, -4.9660
		),
		longitude = c(
			-7.4770, -5.9310, -5.3040, -7.0150, -5.6620, -5.9509, -8.0700,
			 7.1856,  3.5580,  7.0073,  2.1064,  5.2890,  8.9170,  7.1040,  2.3550,
			 9.9920, 11.2188,  7.1167, 12.5735,  7.5396,  7.6117,  7.8136,  7.6117,  7.6116,  6.9387,
			38.9150, 35.5240, 37.0370, 36.8820, 38.5160, 38.8520, 36.6320, 33.5170, 38.9760, 38.9760
		),
		elevation = c(
			329, 290, 450, 309, 340, 280, 329,
			359, 170, 398, 184, 270, 400, 360, 175,
			543, 399, 690, 223, 700, 698, 690, 698, 700, 600,
			 39, 1635, 490, 980, 350,  80, 1410, 1783, 121, 121
		),
		geo_source = c(
			rep("doi:10.5061/dryad.5t065bb", 5), "doi:10.5061/dryad.tt6h5h1", "doi:10.5061/dryad.5t065bb",
			"doi:10.5061/dryad.tt6h5h1", "doi:10.5061/dryad.5t065bb", "doi:10.5061/dryad.tt6h5h1",
			"Kirkissoye irrigated rice perimeter, Niamey V",
			rep("doi:10.5061/dryad.5t065bb", 3), "doi:10.5061/dryad.tt6h5h1",
			"doi:10.5061/dryad.5t065bb", rep("doi:10.5061/dryad.tt6h5h1", 9),
			rep("doi:10.5061/dryad.5t065bb", 10)
		)
	)

	d <- merge(d, geo, by = "site_key_", all.x = TRUE, sort = FALSE)
	# Tanzania Bean at Uyole in doi:10.5061/dryad.5t065bb (exact soil match) is at -8.958, 33.308, 1780 m
	i_uyobe <- d$site_key_ == "TZ_UYO" & d$crop == "common bean"
	d$latitude[i_uyobe] <- -8.9580
	d$longitude[i_uyobe] <- 33.3080
	d$elevation[i_uyobe] <- 1780
	d$site_key_ <- NULL

	d$geo_from_source <- ifelse(is.na(d$latitude), NA, FALSE)
	d$geo_uncertainty <- ifelse(is.na(d$latitude), NA_real_, 5000)

	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("common bean", "cowpea", "groundnut", "pigeon pea", "soybean")] <- "seed"
	d$yield_part[d$crop == "cassava"] <- "roots"

	carobiner::write_files(path, meta, d)
}


