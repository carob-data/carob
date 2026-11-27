# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The protocol (BV101-17-C-Bean.docx) lists 10 treatments (including 3 doses of Pivot/imazethapyr and Basagran/bentazon), but the yield workbook only contains treatments 1 to 7 (14 plots).
# Exact planting date is not reported; planting_date is set to "2017" (harvest was 2017-09-18 and maturity was 94-97 DAE, implying emergence around mid-June 2017).
# Herbicide doses in the protocol are product rates (g or mL/ha), not kg a.i./ha; kept in herbicide_product_dose_.
# The repository description erroneously states "recommendations to maize farmers", whereas the title, protocol, and data are for common bean.

## NEW
# flowering_dae_: days from emergence to flowering (days).
# maturity_dae_: days from emergence to maturity (days).
# grain_test_weight_: hectoliter test weight (kg/hL).
# herbicide_product_dose_: commercial herbicide product dose from protocol (g or mL product/ha).
# herbicide_product values not in terminag: fomesafen, imazethapyr.

## MISSING
# K_fertilizer: potassium application is not mentioned in the workbook or protocol.

## NOTES
# Herbicide efficacy trial in rainfed common bean (cv. Pinto Saltillo) at CIMMYT El Batan (Lote J9), Mexico, 2017 (protocol BV101-17-C-Bean.docx).
# RCBD with 2 replicates on narrow permanent beds (0.75 m), 25 kg N/ha (urea), 40 kg P2O5/ha (TSP), and 1 kg/ha borax (~0.12 kg B/ha) pre-sowing; seeded at 50 kg/ha (150,000 seeds/ha).
# Coordinates are estimated for the CIMMYT El Batan research station (Texcoco, Mexico).
# Standardized yield (12% H2O) is mapped to yield and dry grain yield to yield_dry.

## NOT PROCESSED
# DAT-BV107-17-Yield_Bean.xlsx (Calculations): Grains/m2, DaysGrain, GrainRateA, GrainRateB, KernelRate (derived yield-component and grain-filling rates).


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
		treatment_vars = "herbicide_product;herbicide_used;weeding_done",
		response_vars = "yield;seed_weight",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 88,
		carob_effort = 0
	)

	# Treatments, active ingredients, and product doses (g or mL/ha) from BV101-17-C-Bean.docx
	key <- data.frame(
		trt = 1:10,
		treatment = c(
			"untreated check", "weed free", "Prowl (pendimethalin)", "Flex BIW (fomesafen)",
			"Fusilade (fluazifop-p-butyl)", "Fusiflex (fomesafen;fluazifop-p-butyl)",
			"Pivot 500 (imazethapyr)", "Pivot 400 (imazethapyr)",
			"Pivot 300 (imazethapyr)", "Basagran (bentazon)"
		),
		herbicide_product = c(
			"none", "none", "pendimethalin", "fomesafen", "fluazifop-p-butyl",
			"fomesafen;fluazifop-p-butyl", "imazethapyr", "imazethapyr",
			"imazethapyr", "bentazon"
		),
		herbicide_product_dose_ = c(
			"0", "0", "2631", "1000", "1000", "1000", "500", "400", "300", "2000"
		),
		herbicide_used = c(FALSE, FALSE, rep(TRUE, 8)),
		weeding_done = c(FALSE, TRUE, rep(FALSE, 8))
	)

	f <- ff[basename(ff) == "DAT-BV107-17-Yield_Bean.xlsx"]
	r <- carobiner::read.excel(f, sheet="Calculations")
	r <- r[!is.na(r$Trt), ]
	yname <- grep("^Yield_12", names(r), value=TRUE)[1]

	d <- data.frame(
		country = "Mexico",
		adm1 = "México",
		adm2 = "Texcoco",
		location = "El Batán",
		site = "Lote J9",
		# CIMMYT El Batan research station, Texcoco
		longitude = -98.848,
		latitude = 19.533,
		geo_from_source = FALSE,
		geo_uncertainty = 1000,
		geo_source = "CIMMYT El Batan research station",
		trial_id = "BV101C-17",
		plot_id = sprintf("%d%02d", as.integer(r$Rep), as.integer(r$Trt)),
		rep = as.integer(r$Rep),
		trt = as.integer(r$Trt),
		crop = "common bean",
		variety = "Pinto Saltillo",
		planting_date = "2017",
		harvest_date = "2017-09-18",
		yield_part = "seed",
		yield = as.numeric(r[[yname]]),
		yield_moisture = 12,
		yield_isfresh = TRUE,
		yield_dry = as.numeric(r$Yield_Dry),
		seed_weight = as.numeric(r$Thou),
		grain_test_weight_ = as.numeric(r[["P.H."]]),
		plot_area = as.numeric(r$Area),
		row_spacing = 75,
		seed_rate = 50,
		seed_density = 150000,
		flowering_dae_ = as.numeric(r$Flowering),
		maturity_dae_ = as.numeric(r$Maturity),
		N_fertilizer = 25,
		P_fertilizer = 40 / 2.29,
		B_fertilizer = 0.12,
		fertilizer_type = "urea;TSP;Borax",
		land_prep_method = "permanent beds",
		stress = "weeds",
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)
	d <- merge(d, key, by="trt", sort=FALSE)
	d$trt <- NULL

	carobiner::write_files(path, meta, d)
}
