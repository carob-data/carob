# R script for "carob"
# license: GPL (>=3)

## NOTES
# Drought trial at Jodhpur. Country, Rajasthan, Jodhpur, and the coordinates are on the Minimal sheet.
# Installation: split plot in RCBD, 3 blocks, 50 plants, plot 6 m2, 0.20 m between plants, 0.60 m between rows. Spacing is stored in cm. The description says 4.8 by 1.2 m2. The installation area matches 50 plants at that spacing, so 6 m2 is used.
# The description says normal irrigation was eight times and deficit irrigation five times. Both are irrigated. irrigation_number is 8 or 5.
# Planting day in that same description is 5 November 2012. The Crop_management dates are empty. End date 2014 is the project window, not a harvest day.
# TTYNA is t/ha. The dictionary formula is total tuber weight per plot divided by plot size, times 10. Yield is TTYNA * 1000.
# AVDM is percent dry matter. yield_moisture is 100 minus AVDM.
# Variety is the accession name when the material list has one (Basadre, Baseko), otherwise the institutional number. Kufri Pukhraj and Kufri Surya are the controls named in the description.
# plant_density, stem_density, and tuber_density use the harvested plant count and the 6 m2 plot. Stem number and tubers per plant are the fieldbook means.

## ISSUES
# The description calls the layout a strip plot. The Installation sheet says Split Plot with Plots in RCBD (SPRCBD). The installation wording is stored.
# No fertilizer rate and no harvest date.
# write_files: missing K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variable: leaf_number_ (not leaf_N; that is leaf nitrogen). unknown variables: plants_emerged_pct_.

## NOT PROCESSED
# Organoleptic.xlsx. Counts of panelists in appearance, taste, and texture classes for 2013-14, not a plot score.
# Summary sheet.
# Dictionary.xls. The units used above.
# NTP. Every plot has 50 tubers planted, the installation count.


## NEW VARIABLES
# plants_emerged_pct_: percent of plants emerged.
# leaf_number_: leaves per plant at 60 days.


carob_script <- function(path) {

"
Dataset for: Replication data for: Evaluation of advanced potatoes clones for drought tolerance in arid zone in Rajasthan, India.

Eight CIP-bred potato clones (V1 = 392745.7, V2 = 392780.1, V3 = 397006.18, V4 = 399101.1, V5 = 301029.18, V6 = 380583.8, V7 = 388972.22, V8 = 391580.30) were evaluated along with two control varieties Kufri Pukhraj (early bulking) and Kufri Surya (heat tolerant). Sprouted seed tubers of 40-45 mm size were planted on 5 November 2012 at 60 x 20 cm distance in a plot size of 4.8 × 1.2 m2. The field experiment was laid out in strip plot design with two irrigation regimes: I1 = Normal Irrigation (eight irrigations) and I2 = Deficit irrigation (five irrigations) as horizontal factor and ten clones/varieties as vertical factor. Organoleptic evaluation by panels: to evaluate the appearance, taste and texture of boiled samples by clone and control variety. There were total 58 participants: 40 men and 18 women.
"

	uri <- "doi:10.21223/P3/T41TZD"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Split Plot with Plots in RCBD (SPRCBD)",
		data_type = "experiment",
		treatment_vars = "variety;irrigation_number",
		response_vars = "yield;plant_height;yield_moisture",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "PTDrought012012_JODHPUR.xlsx"]
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	r1 <- carobiner::read.excel(f, sheet="Minimal")
	r2 <- carobiner::read.excel(f, sheet="Material_List", na=c("", "NA"))
	v <- setNames(trimws(as.character(r1$Value)), trimws(r1$Factor))
	clean <- function(x) {
		x <- gsub("\u00a0", " ", as.character(x), fixed=TRUE)
		x <- trimws(x)
		x[!nzchar(x)] <- NA_character_
		x
	}
	key <- clean(r2$Accession_Number)
	nm <- clean(r2$Accession_Name)
	instn <- clean(r$INSTN)
	i <- match(instn, key)
	variety <- nm[i]
	variety[is.na(variety)] <- instn[is.na(variety)]
	parent <- function(col) {
		p <- clean(r2[[col]][i])
		p
	}
	factor <- clean(r$FACTOR)
	irrigation_number <- ifelse(factor == "normal irrigation", 8L, 5L)
	plot_area <- 6
	nph <- as.numeric(r$NPH)

	d <- data.frame(
		trial_id = v[["Trial_name"]],
		crop = "potato",
		variety = variety,
		variety_code = instn,
		treatment = paste(variety, factor),
		female_parent = parent("Female_AcceNumb"),
		male_parent = parent("Male_AcceNumb"),
		rep = as.integer(r$REP),
		plot_id = as.character(r$PLOT),
		irrigation_number = irrigation_number,
		irrigated = TRUE,
		yield = as.numeric(r$TTYNA) * 1000,
		yield_part = "tubers",
		yield_isfresh = TRUE,
		yield_moisture = 100 - as.numeric(r$AVDM),
		plant_height = as.numeric(r$PLAHE_EV1),
		plants_emerged_pct_ = as.numeric(r$PPE),
		leaf_number_ = as.numeric(r$LEAF_NUMBER_AT_60_DAYS),
		plot_area = plot_area,
		plant_spacing = 20,
		row_spacing = 60,
		plant_density = nph / plot_area * 10000,
		stem_density = as.numeric(r$SNPP_EV1) * nph / plot_area * 10000,
		tuber_density = as.numeric(r$TNTPL) * nph / plot_area * 10000,
		country = v[["Country"]],
		adm1 = carobiner::fix_name(v[["Admin1"]], "title"),
		adm2 = carobiner::fix_name(v[["Admin2"]], "title"),
		location = "Jodhpur",
		latitude = as.numeric(v[["Latitude"]]),
		longitude = as.numeric(v[["Longitude"]]),
		planting_date = "2012-11-05",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
