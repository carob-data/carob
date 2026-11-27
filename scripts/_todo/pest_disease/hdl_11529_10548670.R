# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Lodging is described as a plant count in the English legend and as "% de plantas acamadas" in the Spanish legend; values (0 to 80 in increments of 5) indicate percentages and are stored in lodging_ (%).
# Exact planting and harvest dates are not recorded in the workbook or protocol; only the 2016 summer season is known (phytotoxicity was scored on 2016-06-27), so planting_date and harvest_date are set to "2016".
# Herbicide doses in the protocol (BV101-16-B-Wheat.docx) are product rates (g or mL/ha), not kg a.i./ha; kept in herbicide_product_dose_.
# Weed counts (H.ACH_30, H.ANGO_30) and dry weight (P_sco_30) at 30 DAS do not specify the sampled quadrat area or whether values are per quadrat or summed across quadrats, so they are stored unscaled in the long table.

## NEW
# heading_dae_: days from emergence to heading/tasseling (days).
# flowering_dae_: days from emergence to flowering (days).
# maturity_dae_: days from emergence to maturity (days).
# herbicide_product_dose_: commercial herbicide product dose from protocol (g or mL product/ha; semicolon-separated for tank mixes).
# grain_test_weight_: grain test weight / hectoliter weight (g/L).
# lodging_: lodged plants (%).
# phytotoxicity_score_: visual herbicide phytotoxicity score (1-6 scale, recorded 2016-06-27).
# weed_count_: weed plant count per sampled quadrat at 30 DAS (count).
# weed_dry_weight_: weed dry weight per sampled quadrat at 30 DAS (g).
# herbicide_product values not in terminag: prosulfuron, triasulfuron, flucarbazone-sodium.

## MISSING
# K_fertilizer: potassium application is not mentioned in the workbook or protocol.

## NOTES
# Herbicide efficacy trial in rainfed wheat (cv. Zinc Mutus) at CIMMYT El Batan (Lote J9), Mexico, 2016 (protocol BV101-16-B-Wheat.docx).
# RCBD with 2 replicates and 14 treatments (untreated check, weed-free check, and 12 herbicide treatments) on narrow permanent zero-till beds (0.75 m), 150 kg N/ha (urea) and 40 kg P2O5/ha (TSP) pre-sowing.
# Coordinates are estimated for the CIMMYT El Batan research station (Texcoco, Mexico).
# Yield_12%H2O (12% moisture) is mapped to yield and Yield_Dry to yield_dry; 30-DAS weed counts/biomass and 2016-06-27 phytotoxicity scores are stored in the long table.

## NOT PROCESSED
# DAT-BV101_WHEAT-16.xlsx (Calculations): Grains/m2, WStem, WGrain/spike, DaysGrain, BioRate, GrainRateA, GrainRateB, VegRate, KernelRate (derived yield-component ratios and growth rates).
# DAT-BV101_WHEAT-16.xlsx (Calculations): Plants/m2 (all values are NA).


carob_script <- function(path) {

"
Evaluation of herbicides for weed control in wheat in El Batán, México (2016)

Effectiveness of 14 herbicides were compared during 2016 in El Batán, México. The results will be used to make recommendations to wheat farmers for weed control.
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
		treatment_vars = "herbicide_product;herbicide_used;weeding_done",
		response_vars = "yield;dmy_total;dmy_residue;harvest_index;plant_height;seed_weight;spike_density;grains_per_spike",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "DAT-BV101_WHEAT-16.xlsx"]
	r <- carobiner::read.excel(f, sheet="Calculations")
	r <- r[!is.na(r$Trt), ]

	# Treatments, active ingredients, and product doses (g or mL/ha) from BV101-16-B-Wheat.docx
	key <- data.frame(
		trt = 1:14,
		treatment = c(
			"untreated check", "weed free", "Peak (prosulfuron)", "Amber (triasulfuron)",
			"Peak+Amber (prosulfuron;triasulfuron)", "Banvel 12-24 (dicamba;2,4-D)",
			"Esteron (2,4-D)", "Puma (fenoxaprop-p-ethyl)", "Starane (fluroxypyr)",
			"Everest+Topik (flucarbazone-sodium;clodinafop)", "Topik (clodinafop)",
			"Axial (pinoxaden)", "Sigma (mesosulfuron-methyl;iodosulfuron)",
			"Veloz (carfentrazone)"
		),
		herbicide_product = c(
			"none", "none", "prosulfuron", "triasulfuron", "prosulfuron;triasulfuron",
			"dicamba;2,4-D", "2,4-D", "fenoxaprop-p-ethyl", "fluroxypyr",
			"flucarbazone-sodium;clodinafop", "clodinafop", "pinoxaden",
			"mesosulfuron-methyl;iodosulfuron", "carfentrazone"
		),
		herbicide_product_dose_ = c(
			"0", "0", "30", "10", "26.3;6.66", "1000", "500", "1000", "400",
			"30;750", "750", "600", "500", "1000"
		),
		herbicide_used = c(FALSE, FALSE, rep(TRUE, 12)),
		weeding_done = c(FALSE, TRUE, rep(FALSE, 12))
	)

	d <- data.frame(
		record_id = as.integer(seq_len(nrow(r))),
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
		trial_id = "BV101B-16",
		plot_id = sprintf("%d%02d", as.integer(r$Rep), as.integer(r$Trt)),
		rep = as.integer(r$Rep),
		trt = as.integer(r$Trt),
		crop = "wheat",
		variety = "Zinc Mutus",
		planting_date = "2016",
		harvest_date = "2016",
		yield_part = "grain",
		yield = as.numeric(r[["Yield_12%H2O"]]),
		yield_moisture = 12,
		yield_isfresh = TRUE,
		yield_dry = as.numeric(r$Yield_Dry),
		dmy_total = as.numeric(r$Biomass),
		dmy_residue = as.numeric(r$Straw),
		harvest_index = as.numeric(r$HI),
		plant_height = as.numeric(r$Height),
		seed_weight = as.numeric(r$Thou),
		spike_density = as.numeric(r[["Spikes/m2"]]) * 10000,
		grains_per_spike = as.numeric(r[["Grains/spike"]]),
		grain_test_weight_ = as.numeric(r[["Test weight"]]),
		lodging_ = as.numeric(r$Lodging),
		plot_area = as.numeric(r$Area),
		row_spacing = 24,
		seed_rate = 110,
		heading_dae_ = as.numeric(r$Tasseling),
		flowering_dae_ = as.numeric(r$Flowering),
		maturity_dae_ = as.numeric(r$Maturity),
		N_fertilizer = 150,
		P_fertilizer = 40 / 2.29,
		fertilizer_type = "urea;TSP",
		land_prep_method = "permanent beds",
		stress = "weeds",
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)
	d <- merge(d, key, by="trt", sort=FALSE)
	d$trt <- NULL

	# Long table: 30-DAS weed counts/biomass (protocol: 30 DDS) and phytotoxicity score (2016-06-27)
	l_broad <- data.frame(
		record_id = d$record_id,
		DAP = 30L,
		date = NA_character_,
		weed_species = "broadleaf total",
		weed_count_ = as.numeric(r$H.ACH_30),
		weed_dry_weight_ = NA_real_,
		phytotoxicity_score_ = NA_real_
	)
	l_narrow <- data.frame(
		record_id = d$record_id,
		DAP = 30L,
		date = NA_character_,
		weed_species = "narrowleaf total",
		weed_count_ = as.numeric(r$H.ANGO_30),
		weed_dry_weight_ = NA_real_,
		phytotoxicity_score_ = NA_real_
	)
	l_dry <- data.frame(
		record_id = d$record_id,
		DAP = 30L,
		date = NA_character_,
		weed_species = "all weeds",
		weed_count_ = NA_real_,
		weed_dry_weight_ = as.numeric(r$P_sco_30),
		phytotoxicity_score_ = NA_real_
	)
	l_phyto <- data.frame(
		record_id = d$record_id,
		DAP = NA_integer_,
		date = "2016-06-27",
		weed_species = NA_character_,
		weed_count_ = NA_real_,
		weed_dry_weight_ = NA_real_,
		phytotoxicity_score_ = as.numeric(r[["Scoring_27/06/16"]])
	)
	long <- rbind(l_broad, l_narrow, l_dry, l_phyto)

	carobiner::write_files(path, meta, d, long=long)
}
