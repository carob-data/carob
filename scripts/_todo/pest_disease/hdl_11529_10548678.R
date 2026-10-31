# R script for "carob"
# license: GPL (>=3)

## NOTES
# Herbicide comparison in common bean at El Batan, Mexico, 2017. BV101-17-C-Bean.docx states an RCBD with 2 replications, variety Pinto Saltillo, rainfed (temporal), narrow permanent beds.
# The protocol applies 25 kg N/ha as urea and 40 kg P2O5/ha as triple superphosphate before sowing, and 1 kg/ha borax, which it states is about 120 g B/ha.
# Yield_12%H2O is already kg/ha at 12% moisture. Thou is the dry thousand-kernel weight.
# Flowering and maturity are days after emergence. The legend gives harvest as 18/9/2017. The day is 18, so the date is 18 September 2017.
# Treatment 1 is an unsprayed control and treatment 2 is weed free without herbicide. The protocol lists ten treatments. The workbook has treatments 1 to 7.
# Product doses in the protocol are g or mL of product per ha, not kg of active ingredient. Spellings in the protocol (Fomesafan, Flauzifop, Imizathapyr) are stored as the usual active-ingredient names.

## ISSUES
# No coordinates or planting date in the workbook or the protocol. Potassium is not in the protocol.
# write_files: missing variables: latitude, longitude, K_fertilizer, planting_date. unknown variable: flowering_dae_ (DAE?). unknown variable: maturity_dae_ (DAE?). invalid terms: herbicide_product: fomesafen, imazethapyr.

## NEW VARIABLES
# flowering_dae_: days from emergence to flowering.
# maturity_dae_: days from emergence to maturity.

## NOT PROCESSED
# Subsample weights used to compute yield, grain moisture amount, and the two grain-filling rates.
# Grains per square metre.
# Test weight (P.H.). The legend does not state the unit.
# Product doses. They are g or mL of product per ha, not kg of active ingredient.


carob_script <- function(path) {

"
Evaluation of herbicides for weed control in beans in El Batán, México (2017)

Effectiveness of 10 herbicides were compared during 2017 in El Batán, México. The results will be used to make recommendations to maize farmers for weed control.
"

	uri <- "hdl:11529/10548678"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

		meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "BV101C/17",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "DAT-BV107-17-Yield_Bean.xlsx"]
	r <- carobiner::read.excel(f, sheet="Calculations")
	r <- r[!is.na(r$Trt), ]

	# Active ingredients from the treatment table in BV101-17-C-Bean.docx.
	prod <- c(
		"1" = "none",
		"2" = "none",
		"3" = "pendimethalin",
		"4" = "fomesafen",
		"5" = "fluazifop-p-butyl",
		"6" = "fomesafen;fluazifop-p-butyl",
		"7" = "imazethapyr",
		"8" = "imazethapyr",
		"9" = "imazethapyr",
		"10" = "bentazon"
	)
	lab <- c(
		"1" = "untreated",
		"2" = "weed free",
		"3" = "pendimethalin",
		"4" = "fomesafen",
		"5" = "fluazifop-p-butyl",
		"6" = "fomesafen;fluazifop-p-butyl",
		"7" = "imazethapyr 500",
		"8" = "imazethapyr 400",
		"9" = "imazethapyr 300",
		"10" = "bentazon"
	)
	code <- as.character(r$Trt)
	yname <- names(r)[grepl("^Yield_12", names(r))]

	d <- data.frame(
		country = "Mexico",
		location = "El Batan",
		site = "Lote J9",
		trial_id = "BV101-17",
		rep = as.integer(r$Rep),
		treatment = unname(lab[code]),
		herbicide_product = unname(prod[code]),
		herbicide_used = !code %in% c("1", "2"),
		crop = "common bean",
		variety = "Pinto Saltillo",
		yield_part = "seed",
		yield = as.numeric(r[[yname]]),
		yield_moisture = 12,
		seed_weight = as.numeric(r$Thou),
		plot_area = as.numeric(r$Area),
		row_spacing = 75,
		seed_rate = 50,
		plant_density = 150000,
		flowering_dae_ = as.numeric(r$Flowering),
		maturity_dae_ = as.numeric(r$Maturity),
		harvest_date = "2017-09-18",
		N_fertilizer = 25,
		P_fertilizer = 40 / 2.29,
		B_fertilizer = 0.12,
		fertilizer_type = "urea;TSP",
		stress = "weeds",
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$weeding_done <- NA
	d$weeding_done[d$treatment == "weed free"] <- TRUE
	d$weeding_done[d$treatment == "untreated"] <- FALSE

	carobiner::write_files(path, meta, d)
}
