# R script for "carob"
# license: GPL (>=3)

## NOTES
# Low-ODAP grasspea lines at El Batan, Mexico, 2017. Geographic coverage is Mexico, Estado de Mexico, Texcoco. The protocol places the trial in lote G5 at El Batan.
# The protocol is an alpha design with two replicates. Land preparation is narrow permanent beds. Fertilizer is 40 kg P2O5/ha as triple superphosphate and 20 kg N/ha as urea. P is P2O5/2.29. K is 0 because those are the only products listed. Irrigation is listed as temporal, so the trial is rainfed.
# Grain yield is Yield_12%H2O, already kg/ha, at 12 percent moisture. Biomass Yield_Dry is dry kg/ha. Flowering and maturity in the yield legend are days after emergence.
# Entry names, accessions, and origins are the protocol table in BV105-17-Grasspea.docx. Entry 25 is vetch.
# Spike counts per m2 are stored per ha.

## ISSUES
# No coordinates.
# Plot harvest area is below 1 m2 for some plots. The nominal plot in the protocol is 6 m2. The sheet area is kept.
# Some grain yields are missing.
# NDVI files are not in this table.
# write_files: missing variables: latitude, longitude, planting_date, harvest_date. unknown variable: flowering_dae_ (DAE?). unknown variable: maturity_dae_ (DAE?). out of bounds: plot_area (0.15, 13.3), plot_length (0.2, 4.7). NA detected: yield.

## NEW VARIABLES
# flowering_dae_: days from emergence to flowering. The yield legend says after emergence.
# maturity_dae_: days from emergence to maturity. The yield legend says after emergence.


## NOT PROCESSED
# DAT-BV105-17-NDVI_H.xls and DAT-BV105-17-NDVI_V.xls. Repeated NDVI. The horizontal file does not have a simple plot header.
# Grain production rates and kernel rate. They are calculated from yield and duration.
# Subsample fresh and dry weights used to compute yield and biomass.


carob_script <- function(path) {

"
Several varieties of Low-ODAP grass pea were evaluated. Some of the variables measured were Total biomass weight (g), Fresh weigth of the subsample (g), Dry weight of the subsample (g) and Harvest area (m²). The experiment was conducted in El Batán, México.
"

	uri <- "hdl:11529/10548680"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = "alpha design",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_total",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.6
	)

	# Entry list in BV105-17-Grasspea.docx
	acc <- c("299", "368", "384", "243", "269", "303", "311", "659", "275", "728", "1118", "1123", "56", "65", "22", "1095", "18", "17", "25", "651", "991", "973", "bio520", "Sel190", "Vetch")
	origin <- c("Morocco", "Pakistan", "Nepal", "Turkey", "Algeria", "Morocco", "Bangladesh", "Bangladesh", "Bulgaria", "Bangladesh", "Bangladesh", "Bangladesh", "Germany", "Greece", "Greece", "Bangladesh", "Greece", "Greece", "Greece", "Bangladesh", "Bangladesh", "Bangladesh", NA, NA, NA)
	key <- data.frame(trt=1:25, variety=acc, variety_origin=origin)

	fy <- ff[basename(ff) == "DAT-BV105-17-Yield_Grasspea.xlsx"]
	fb <- ff[basename(ff) == "DAT-BV105-17-Biomass_Grasspea.xlsx"]
	r <- carobiner::read.excel(fy, sheet="Calculations")
	b <- carobiner::read.excel(fb, sheet="Calculation")
	ib <- match(paste(r$Rep, r$Trt), paste(b$Rep, b$Trt))
	ik <- match(as.integer(r$Trt), key$trt)

	variety <- key$variety[ik]
	d <- data.frame(
		country = "Mexico",
		adm1 = "Estado de Mexico",
		location = "El Batan",
		site = "G5",
		trial_id = "El Batan 2017",
		rep = as.integer(r$Rep),
		plot_id = paste(r$Rep, variety),
		crop = ifelse(variety == "Vetch", "vetch", "grass pea"),
		variety = variety,
		variety_code = as.character(r$Trt),
		variety_origin = key$variety_origin[ik],
		variety_type = ifelse(variety == "Vetch", NA_character_, "low-ODAP"),
		treatment = variety,
		yield_part = "seed",
		yield = as.numeric(r$`Yield_12%H₂O`),
		yield_moisture = 12,
		dmy_total = as.numeric(b$Yield_Dry[ib]),
		seed_weight = as.numeric(r$Thou),
		spike_density = as.numeric(r$`Spikes/m²`) * 10000,
		grains_per_spike = as.numeric(r$`Grains/spike`),
		flowering_dae_ = as.numeric(r$Flowering),
		maturity_dae_ = as.numeric(r$Maturity),
		plot_area = as.numeric(r$Area),
		plot_width = as.numeric(r$WidthArea),
		plot_length = as.numeric(r$LengthArea),
		plant_density = as.numeric(r$Count) / as.numeric(r$AreaCount) * 10000,
		land_prep_method = "permanent beds",
		N_fertilizer = 20,
		P_fertilizer = 40 / 2.29,
		K_fertilizer = 0,
		fertilizer_type = "TSP;urea",
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$record_id <- as.integer(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
