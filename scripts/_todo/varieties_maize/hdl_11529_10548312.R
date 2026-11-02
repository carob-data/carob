# R script for "carob"
# license: GPL (>=3)

## NOTES
# Three TAMASA maize variety trials in Nigeria. Grain and cob yields are already kg/ha.
# In NG_VT_Validation_SS_2016.xlsx the variable sheet says CY_ha is grain yield and GY_ha is cob yield. The other two files say the opposite. Each file is mapped from its own variable sheet.
# Calibration metadata lists four coordinate pairs and puts N on the longitude label and E on the latitude label. The pairs are not tied to LERE, SMR, BUK and DBT, so they are not stored. The northern Guinea savanna metadata gives one point for each of Doguwa, Ikara and Lere.
# Hundred-seed weight is in grams. seed_weight is that value times 10. Harvest index in the files is a ratio. The vocabulary unit is percent, so the ratio is times 100.
# Density is stored as plant_density. The variable sheet says "number" and does not say per hectare. The values are 26666 and above.

## ISSUES
# Calibration coordinates are not attached to the location codes. The Sudan savanna sheet has no place column.
# No fertilizer rates and no grain moisture. The northern Guinea savanna points are one coordinate per local government area.
# write_files: , yield_moisture, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: cob_yield_kg_ha_, tops_weight_kg_ha_, days_to_tassel_, kernels_per_cob_, biomass_tasselling_kg_ha_. duplicate coordinates: fewer coordinates than locations. NA detected: yield, latitude, longitude.

## NEW VARIABLES
# cob_yield_kg_ha_: cob yield after drying, kg/ha.
# tops_weight_kg_ha_: above-ground biomass with cobs at harvest, kg/ha. Fresh or dry is not stated.
# biomass_tasselling_kg_ha_: above-ground biomass without cobs at tasselling, kg/ha.
# days_to_tassel_: days from planting to 50 percent tassels.
# kernels_per_cob_: kernels averaged over five cobs.


## NOT PROCESSED
# Plot-layout sheets and the data-history sheets.


carob_script <- function(path) {

"
TAMASA Nigeria maize variety calibration and on-farm validation, 2016

Variety calibration trials at BUK, Dambatta, Samaru and Lere. The dry-season trial was under full irrigation from 23 March to 20 June 2016. On-farm validation in the northern Guinea savanna (Doguwa, Ikara and Lere) and in the Sudan savanna (Kura, Bunkure and Garun Mallam).
"

	uri <- "hdl:11529/10548312"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IITA",
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.6
	)

	num <- function(x) as.numeric(as.character(x))
	blank <- function(x, n) rep(NA, n)

	f1 <- ff[basename(ff) == "NG_VT_Calibration_2016.xlsx"]
	a <- carobiner::read.excel(f1, sheet="Data")
	n <- nrow(a)
	d1 <- data.frame(
		trial_id = "dry-season calibration",
		country = "Nigeria",
		adm1 = "Kano and Kaduna",
		location = a$Location,
		latitude = blank(0, n),
		longitude = blank(0, n),
		geo_from_source = FALSE,
		on_farm = FALSE,
		irrigated = TRUE,
		planting_date = "2016-03-23",
		harvest_date = "2016-06-20",
		plot_id = paste(a$Location, a$Rep, a$Variety, sep="_"),
		rep = as.integer(a$Rep),
		crop = "maize",
		variety = a$Variety,
		plant_density = blank(0, n),
		yield = num(a$GY_ha),
		yield_part = "grain",
		cob_yield_kg_ha_ = num(a$CY_ha),
		tops_weight_kg_ha_ = blank(0, n),
		harvest_index = blank(0, n),
		seed_weight = num(a$hun_SW) * 10,
		emergence_days = num(a$DTE),
		flowering_days = num(a$DTS),
		maturity_days = num(a$DTPM),
		days_to_tassel_ = num(a$DTT),
		kernels_per_cob_ = num(a$No_Kernel),
		biomass_tasselling_kg_ha_ = num(a$BM_Tasselling),
		is_survey = FALSE
	)

	f2 <- ff[basename(ff) == "NG_VT_NGS_2016 (1).xlsx"]
	b <- carobiner::read.excel(f2, sheet="Revised")
	lat <- c(Doguwa = 10.77841, Ikara = 11.14099, Lere = 10.54297)
	lon <- c(Doguwa = 8.60288, Ikara = 8.23207, Lere = 8.45094)
	n <- nrow(b)
	hi <- num(b$HI)
	d2 <- data.frame(
		trial_id = "northern Guinea savanna",
		country = "Nigeria",
		adm1 = blank("", n),
		location = b$LGA,
		latitude = unname(lat[b$LGA]),
		longitude = unname(lon[b$LGA]),
		geo_from_source = TRUE,
		on_farm = TRUE,
		irrigated = blank(TRUE, n),
		planting_date = "2016-06-20",
		harvest_date = "2016-11-25",
		plot_id = paste(b$LGA, b[["Farm ID"]], b$Variety, b$Density, sep="_"),
		rep = rep(NA_integer_, n),
		crop = "maize",
		variety = b$Variety,
		plant_density = num(b$Density),
		yield = num(b$GY_ha),
		yield_part = "grain",
		cob_yield_kg_ha_ = num(b$CY_ha),
		tops_weight_kg_ha_ = num(b$TW_Ha),
		harvest_index = ifelse(hi <= 1.5, hi * 100, hi),
		seed_weight = blank(0, n),
		emergence_days = blank(0, n),
		flowering_days = blank(0, n),
		maturity_days = blank(0, n),
		days_to_tassel_ = blank(0, n),
		kernels_per_cob_ = blank(0, n),
		biomass_tasselling_kg_ha_ = blank(0, n),
		is_survey = FALSE
	)

	f3 <- ff[basename(ff) == "NG_VT_Validation_SS_2016.xlsx"]
	c <- carobiner::read.excel(f3, sheet="DATA")
	# Twelve rows are repeated in full. The repeats are dropped.
	c <- c[!duplicated(c), ]
	n <- nrow(c)
	hi <- num(c$HI)
	d3 <- data.frame(
		trial_id = "Sudan savanna",
		country = "Nigeria",
		adm1 = "Kano",
		location = blank("", n),
		latitude = blank(0, n),
		longitude = blank(0, n),
		geo_from_source = FALSE,
		on_farm = TRUE,
		irrigated = blank(TRUE, n),
		planting_date = "2015-06-20",
		harvest_date = "2015-11-25",
		plot_id = paste(c[["Plot No"]], c$Variety, c$Density, sep="_"),
		rep = rep(NA_integer_, n),
		crop = "maize",
		variety = c$Variety,
		plant_density = num(c$Density),
		yield = num(c$CY_ha),
		yield_part = "grain",
		cob_yield_kg_ha_ = num(c$GY_ha),
		tops_weight_kg_ha_ = num(c$TW_Ha),
		harvest_index = ifelse(hi <= 1.5, hi * 100, hi),
		seed_weight = blank(0, n),
		emergence_days = blank(0, n),
		flowering_days = blank(0, n),
		maturity_days = blank(0, n),
		days_to_tassel_ = blank(0, n),
		kernels_per_cob_ = blank(0, n),
		biomass_tasselling_kg_ha_ = blank(0, n),
		is_survey = FALSE
	)

	d <- rbind(d1, d2, d3)
	d$adm1[is.na(d$adm1)] <- NA_character_
	carobiner::write_files(path, meta, d)
}
