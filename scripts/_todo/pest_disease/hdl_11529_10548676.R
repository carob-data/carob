# R script for "carob"
# license: GPL (>=3)

## NOTES
# Herbicide comparison in maize at El Batan, Mexico, 2017. The yield legend in DAT-BV101-17-Yield_Maize.xlsx names 14 treatments and says Yield_12%H2O is kg/ha at 12% moisture. Yield_dry is dry grain, kg/ha.
# Flowering, tasseling, silking, and maturity in the legend are days after emergence.
# Treatment 1 is an unsprayed control and treatment 2 is weed free. Active ingredients are taken from the legend product names. Acetachlor, S-metalachlor, and Carfentrazole are the legend spellings of acetochlor, S-metolachlor, and carfentrazone.
# The protocol docx listed in the repository was not in the downloaded files, so fertilizer, design, and irrigation are not taken from it.
# Harvest date 24/11/17 is the harvest date in the yield legend. Country and state are the dataset title and geographic coverage.

## ISSUES
# No coordinates or fertilizer rates in the yield workbook.
# write_files: missing variables: latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date. unknown variable: flowering_dae_ (DAE?). unknown variable: tasseling_dae_ (DAE?). unknown variable: silking_dae_ (DAE?). unknown variable: maturity_dae_ (DAE?). unknown variables: ear_density_per_ha_, lodging_plants_per_ha_. invalid terms: herbicide_product: acetochlor.

## NEW VARIABLES
# flowering_dae_: days from emergence to flowering.
# tasseling_dae_: days from emergence to tasseling.
# silking_dae_: days from emergence to silking.
# maturity_dae_: days from emergence to maturity.
# ear_density_per_ha_: good ears per hectare.
# lodging_plants_per_ha_: lodged plants per hectare.

## NOT PROCESSED
# Weed counts and weed dry weight.
# Phytotoxicity scores. The legend calls the same score both disease damage and phytotoxicity, on a 0-5 scale.
# Subsample weights used to compute yield.
# Lodging counts that are already stored per hectare.


carob_script <- function(path) {

"
Evaluation of herbicides for weed control in maize in El Batán, México (2017)

Effectiveness of 14 herbicides were compared during 2017 in El Batán, México. The results will be used to make recommendations to maize farmers for weed control.
"

	uri <- "hdl:11529/10548676"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "BV101-17",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "DAT-BV101-17-Yield_Maize.xlsx"]
	r <- carobiner::read.excel(f, sheet="Calculations_Maize")
	r <- r[!is.na(r$Trt), ]

	# Active ingredients from the legend in this workbook.
	prod <- c(
		"1" = "none",
		"2" = "none",
		"3" = "atrazine",
		"4" = "acetochlor",
		"5" = "acetochlor;atrazine",
		"6" = "atrazine;s-metolachlor",
		"7" = "halosulfuron-methyl",
		"8" = "isoxaflutole;thiencarbazone-methyl",
		"9" = "mesotrione",
		"10" = "tembotrione",
		"11" = "dicamba;atrazine",
		"12" = "atrazine;2,4-D;dicamba",
		"13" = "atrazine;carfentrazone",
		"14" = "nicosulfuron"
	)
	lab <- c(
		"1" = "untreated",
		"2" = "weed free",
		"3" = "atrazine",
		"4" = "acetochlor",
		"5" = "acetochlor;atrazine",
		"6" = "atrazine;s-metolachlor",
		"7" = "halosulfuron-methyl",
		"8" = "isoxaflutole;thiencarbazone-methyl",
		"9" = "mesotrione",
		"10" = "tembotrione",
		"11" = "dicamba;atrazine",
		"12" = "atrazine;2,4-D;dicamba",
		"13" = "atrazine;carfentrazone",
		"14" = "nicosulfuron"
	)
	code <- as.character(as.integer(r$Trt))
	y12 <- grep("^Yield_12", names(r), value=TRUE)[1]
	ears <- grep("^GoodEars/m", names(r), value=TRUE)[1]

	d <- data.frame(
		country = "Mexico",
		adm1 = "Estado de Mexico",
		location = "El Batan",
		trial_id = "BV101-17",
		rep = as.integer(r$Rep),
		treatment = unname(lab[code]),
		herbicide_product = unname(prod[code]),
		herbicide_used = !code %in% c("1", "2"),
		crop = "maize",
		yield_part = "grain",
		yield = as.numeric(r[[y12]]),
		yield_moisture = 12,
		plant_height = as.numeric(r$Height),
		seed_weight = as.numeric(r$Thou),
		plot_area = as.numeric(r$Area),
		flowering_dae_ = as.numeric(r$Flowering),
		tasseling_dae_ = as.numeric(r$Tasseling),
		silking_dae_ = as.numeric(r$Silking),
		maturity_dae_ = as.numeric(r$Maturity),
		ear_density_per_ha_ = as.numeric(r[[ears]]) * 10000,
		lodging_plants_per_ha_ = as.numeric(r[["Lodging/ha"]]),
		harvest_date = "2017-11-24",
		stress = "weeds",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$weeding_done <- NA
	d$weeding_done[d$treatment == "weed free"] <- TRUE
	d$weeding_done[d$treatment == "untreated"] <- FALSE

	carobiner::write_files(path, meta, d)
}
