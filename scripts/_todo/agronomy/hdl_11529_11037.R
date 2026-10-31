# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer fields in southern Bangladesh, 2012-13. Nitrogen rate and variety were imposed. Other management varied by farmer.
# Grain yield is t/ha, stored as kg/ha. Plant population is per m2, stored as plants/ha.
# Sowing dates are month/day/year. 1/5/13 is 5 January 2013: the DAYS column is the exact day count from 15 December 2012.
# Soil total N is percent in the metadata, stored as mg/kg (times 10000).
# Salinity columns are labeled dS/m2. The values are stored as soil_EC in mS/cm, which equals dS/m. The four stages are in the long table.
# District spellings in the file are PUTALIKHALI, BARISAL, KHULNA, and SHATKIRA.
# UJIRPUR is stored as Uzirpur. SADAR is stored with its district name.
# Recorded rainfall is 1 to 24 mm.

## ISSUES
# write_files: missing variables: yield_moisture, K_fertilizer, P_fertilizer, harvest_date. time/depth: no time/depth variables in long records?
# The long table uses growth_stage because the salinity readings have no date. get_data warns that the R analysis files are restricted.

## NOT PROCESSED
# Analysis scripts. Those files are restricted.
# Growing degree days, soil moisture, and groundwater depth.
# Sowing-date class, nitrogen-recommendation class, environment code, and land type.
# Mixed soil-texture classes that are not one texture.
# Carbon to nitrogen ratio, which is computed from organic C and total N.
# MetaData.csv, which is the column dictionary.


carob_script <- function(path) {

"
Untangling crop management and environmental influences on wheat yield variability in Bangladesh: An application of non-parametric approaches

In South Asia, wheat is typically grown in favorable environments, although policies promoting intensification in Bangladesh's stress-prone coastal zone have resulted in expanded cultivation in this non-traditional area. Yield, crop management, and environmental observations were collected from 422 wheat fields in the 2012-13 season across six production environments in southern Bangladesh. Nutrient rates and genotypes were imposed. Management of other production factors varied from farmer to farmer.
"

	uri <- "hdl:11529/11037"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Final_DATA_year_02.csv"]
	r <- read.csv(f, check.names=FALSE)
	names(r) <- trimws(names(r))

	dist <- c(PUTALIKHALI="Patuakhali", BARISAL="Barisal", KHULNA="Khulna", SHATKIRA="Satkhira")
	div <- c(Patuakhali="Barisal", Barisal="Barisal", Khulna="Khulna", Satkhira="Khulna")
	adm2 <- unname(dist[as.character(r$DIST)])
	tex <- tolower(trimws(as.character(r$ST)))
	tex[tex == "mostly clay"] <- "clay"
	tex[tex == "mostly silty clay loam"] <- "silty clay loam"
	tex[tex == "mostly silt loam"] <- "silty loam"
	tex[tex == "predominatly silty caly"] <- "silty clay"
	tex[grepl("mixed", tex)] <- NA_character_

	adm3 <- carobiner::fix_name(as.character(r$UPZ), "title")
	adm3[adm3 == "Ujirpur"] <- "Uzirpur"
	sadar <- !is.na(adm3) & adm3 == "Sadar"
	adm3[sadar] <- paste(adm2[sadar], "Sadar")
	location <- carobiner::fix_name(as.character(r$VILL), "title")
	location <- gsub("(sanka)", "(Sanka)", location, fixed=TRUE)

	d <- data.frame(
		country = "Bangladesh",
		adm1 = unname(div[adm2]),
		adm2 = adm2,
		adm3 = adm3,
		location = location,
		latitude = as.numeric(r$LAT),
		longitude = as.numeric(r$LONG),
		geo_from_source = TRUE,
		trial_id = paste(r$FARMER, "rabi_2012"),
		plot_id = as.character(seq_len(nrow(r))),
		crop = "wheat",
		variety = trimws(as.character(r$GEN)),
		treatment = paste(trimws(as.character(r$GEN)), r$N_RATE),
		N_fertilizer = as.numeric(r$N_RATE),
		planting_date = as.character(as.Date(r$DATE_SOW, format="%m/%d/%y")),
		yield = as.numeric(r$GY) * 1000,
		yield_part = "grain",
		plant_density = as.numeric(r$PLANT_POP) * 10000,
		maturity_days = as.numeric(r$Maturity_days),
		irrigation_number = as.integer(r$Irrigation),
		weeding_times = as.integer(r$Weeding),
		rain = as.numeric(r$RAINFALL),
		soil_pH = as.numeric(r$pH),
		soil_SOC = as.numeric(r$SOC),
		soil_N_total = as.numeric(r$TN) * 10000,
		soil_texture = tex,
		on_farm = TRUE,
		is_survey = FALSE,
		irrigated = TRUE
	)
	d$record_id <- as.integer(seq_len(nrow(d)))

	sal <- data.frame(
		SAL_PLANT = "planting",
		SAL_CRI = "crown root",
		SAL_FLR = "flowering",
		SAL_GRFILL = "grain filling"
	)
	long <- NULL
	for (nm in names(sal)) {
		x <- data.frame(
			record_id = d$record_id,
			growth_stage = unname(sal[[nm]]),
			soil_EC = as.numeric(r[[nm]])
		)
		long <- if (is.null(long)) x else rbind(long, x)
	}
	long <- long[!is.na(long$soil_EC), ]

	carobiner::write_files(path, meta, d, long=long)
}
