# R script for "carob"
# license: GPL (>=3)

## NOTES
# Herbicide comparison in wheat at El Batan, Mexico, 2016. The protocol in BV101-16-B-Wheat.docx states an RCBD with 2 replications, variety Zinc Mutus, rainfed (temporal), 150 kg N/ha as urea and 40 kg P2O5/ha as triple superphosphate at planting.
# Yield_12%H2O is already kg/ha at 12% moisture. Biomass in the legend is dry biomass, kg/ha.
# P2O5 is divided by 2.29. Potassium is not in the protocol.
# Flowering and maturity are days after emergence, not after planting.
# Treatment 1 is an unsprayed control and treatment 2 is weed free without herbicide. Product doses are g or ml of product per ha, not kg of active ingredient.

## ISSUES
# No coordinates, potassium rate, or planting and harvest dates in the workbook or the protocol.
# write_files: missing variables: latitude, longitude, K_fertilizer, planting_date, harvest_date. unknown variable: flowering_dae_ (DAE?). unknown variable: maturity_dae_ (DAE?). invalid terms: herbicide_product: flucarbazone-sodium, prosulfuron, triasulfuron.

## NEW VARIABLES
# flowering_dae_: days from emergence to flowering.
# maturity_dae_: days from emergence to maturity.

## NOT PROCESSED
# Lodging: the English legend says a count and the Spanish legend says a percent.
# Weed scores (Scoring_27/06/16, H.ACH_30, H.ANGO_30, P_sco_30). The sheet does not define the scale.
# Calculated rates, straw, and test weight.
# Subsample weights used to compute yield.


carob_script <- function(path) {

"
Evaluation of herbicides for weed control in wheat in El Batan, Mexico (2016)

Effectiveness of 14 herbicides were compared during 2016 in El Batan, Mexico. The results will be used to make recommendations to wheat farmers for weed control.
"

	uri <- "hdl:11529/10548670"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIMMYT",
		publication = NA,
		project = "BV101B/16",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "DAT-BV101_WHEAT-16.xlsx"]
	r <- carobiner::read.excel(f, sheet="Calculations")
	r <- r[!is.na(r$Trt), ]

	# Active ingredients from the treatment table in BV101-16-B-Wheat.docx.
	prod <- c(
		"1" = "none",
		"2" = "none",
		"3" = "prosulfuron",
		"4" = "triasulfuron",
		"5" = "prosulfuron;triasulfuron",
		"6" = "dicamba;2,4-D",
		"7" = "2,4-D",
		"8" = "fenoxaprop-p-ethyl",
		"9" = "fluroxypyr",
		"10" = "flucarbazone-sodium;clodinafop",
		"11" = "clodinafop",
		"12" = "pinoxaden",
		"13" = "mesosulfuron-methyl;iodosulfuron",
		"14" = "carfentrazone"
	)
	lab <- c(
		"1" = "untreated",
		"2" = "weed free",
		"3" = "prosulfuron",
		"4" = "triasulfuron",
		"5" = "prosulfuron;triasulfuron",
		"6" = "dicamba;2,4-D",
		"7" = "2,4-D",
		"8" = "fenoxaprop-p-ethyl",
		"9" = "fluroxypyr",
		"10" = "flucarbazone-sodium;clodinafop",
		"11" = "clodinafop",
		"12" = "pinoxaden",
		"13" = "mesosulfuron-methyl;iodosulfuron",
		"14" = "carfentrazone"
	)
	code <- as.character(r$Trt)
	hi <- as.numeric(r$HI)
	hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)

	d <- data.frame(
		country = "Mexico",
		adm1 = "Estado de Mexico",
		adm2 = "Texcoco",
		location = "El Batan",
		site = "Lote J9",
		trial_id = "BV101-16",
		rep = as.integer(r$Rep),
		treatment = unname(lab[code]),
		herbicide_product = unname(prod[code]),
		herbicide_used = !code %in% c("1", "2"),
		crop = "wheat",
		variety = "Zinc Mutus",
		yield_part = "grain",
		yield = as.numeric(r[["Yield_12%H2O"]]),
		yield_moisture = 12,
		dmy_total = as.numeric(r$Biomass),
		harvest_index = hi,
		plant_height = as.numeric(r$Height),
		seed_weight = as.numeric(r$Thou),
		spike_density = as.numeric(r[["Spikes/m2"]]) * 10000,
		plant_density = as.numeric(r[["Plants/m²"]]) * 10000,
		plot_area = as.numeric(r$Area),
		row_spacing = 24,
		seed_rate = 110,
		flowering_dae_ = as.numeric(r$Flowering),
		maturity_dae_ = as.numeric(r$Maturity),
		N_fertilizer = 150,
		P_fertilizer = 40 / 2.29,
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
