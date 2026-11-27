# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Harvest date in the Yield Legend (row 76) is stored as Excel serial 43078 (2017-12-09), which appears to be 12/09/2017 (12 Sep 2017) misparsed as MM/DD/YY (the same day/month swap occurs in the Legend for 03/07/17, 07/08/17, 11/08/17, and 05/09/17, and 12 Sep 2017 matches 111 DAE maturity after late-May emergence). harvest_date is set to "2017-09-12"; human review recommended.
# The Scoring column is labeled "Damages for diseases" in the English legend and "Danos por fitotoxicidad" in the Spanish legend (1-6 scale, 1 = same as control to 6 = plants dying); per protocol item 5 it is stored in phytotoxicity_score_.
# Weed count methods specify 2 quadrats per plot (each 0.50 m x 1.50 m bed width = 0.75 m2) without stating whether recorded counts and dry weights are per quadrat or summed over both quadrats, so values are kept unscaled in the long table.
# Exact planting date is not stated; planting_date is set to "2017".
# Herbicide doses in BV101-17-B-Wheat.docx are product rates (g or mL/ha), not kg a.i./ha; kept in herbicide_product_dose_.

## NEW
# heading_dae_: days from emergence to heading/tasseling (days).
# flowering_dae_: days from emergence to flowering (days).
# maturity_dae_: days from emergence to maturity (days).
# grain_test_weight_: hectoliter test weight (kg/hL).
# lodging_: lodged plants (%).
# herbicide_product_dose_: commercial herbicide product dose from protocol (g or mL product/ha; semicolon-separated for tank mixes).
# weed_count_: weed plant count per sampled quadrat on observation date (count).
# weed_dry_weight_: total weed dry weight per sampled quadrat on observation date (g).
# phytotoxicity_score_: visual herbicide phytotoxicity score (1-6 scale, recorded 2017-06-29).
# herbicide_product values not in terminag: prosulfuron, triasulfuron, flucarbazone-sodium.

## MISSING
# K_fertilizer: potassium application is not mentioned in the workbook or protocol.

## NOTES
# Herbicide efficacy trial in rainfed wheat (BV232-2016-Ent 13, GID 6569899) at CIMMYT El Batan (Lote J8), Mexico, 2017 (protocol BV101-17-B-Wheat.docx).
# RCBD with 2 replicates and 14 treatments (unweeded check, weed-free check, and 12 herbicide treatments) on wide permanent beds (1.5 m), 150 kg N/ha (urea), 40 kg P2O5/ha (TSP), and 1.5 kg/ha borax (~0.18 kg B/ha) pre-sowing.
# Coordinates are estimated for the CIMMYT El Batan research station (Texcoco, Mexico).
# Standardized yield (12% H2O) is mapped to yield and dry grain yield to yield_dry; repeated weed species counts (3 dates), weed dry weight (3 dates), and phytotoxicity scores (2017-06-29) are in long.

## NOT PROCESSED
# DAT-BV101-17-Yield_Wheat.xlsx (Calculations): Grains/m2, WGrain/spike, DaysGrain, GrainRateA, GrainRateB, KernelRate (derived yield-component ratios and grain-filling rates).


carob_script <- function(path) {

"
Evaluation of herbicides for weed control in wheat in El Batán, México (2017)

Effectiveness of 14 herbicides were compared during 2017 in El Batán, México. The results will be used to make recommendations to wheat farmers for weed control.
"

	uri <- "hdl:11529/10548677"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "BV101B/17",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "herbicide_product;herbicide_used;weeding_done",
		response_vars = "yield;plant_height;seed_weight;spike_density;grains_per_spike",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 88,
		carob_effort = 0
	)

	# Treatments, active ingredients, and product doses (g or mL/ha) from BV101-17-B-Wheat.docx
	key <- data.frame(
		trt = 1:14,
		treatment = c(
			"unweeded check", "weed free", "Peak (prosulfuron)", "Amber (triasulfuron)",
			"Peak+Amber (prosulfuron;triasulfuron)", "Banvel 12-24 (dicamba;2,4-D)",
			"Esteron (2,4-D)", "Puma (fenoxaprop-p-ethyl)", "Starane (fluroxypyr)",
			"Everest+Topik (flucarbazone-sodium;clodinafop)", "Topik (clodinafop)",
			"Axial (pinoxaden)", "Sigma (mesosulfuron-methyl;iodosulfuron-methyl-sodium)",
			"Veloz (carfentrazone)"
		),
		herbicide_product = c(
			"none", "none", "prosulfuron", "triasulfuron", "prosulfuron;triasulfuron",
			"dicamba;2,4-D", "2,4-D", "fenoxaprop-p-ethyl", "fluroxypyr",
			"flucarbazone-sodium;clodinafop", "clodinafop", "pinoxaden",
			"mesosulfuron-methyl;iodosulfuron-methyl-sodium", "carfentrazone"
		),
		herbicide_product_dose_ = c(
			"0", "0", "30", "10", "26.3;6.66", "1000", "500", "1000", "400",
			"30;750", "750", "600", "500", "1000"
		),
		herbicide_used = c(FALSE, FALSE, rep(TRUE, 12)),
		weeding_done = c(FALSE, TRUE, rep(FALSE, 12))
	)

	f <- ff[basename(ff) == "DAT-BV101-17-Yield_Wheat.xlsx"]
	r <- carobiner::read.excel(f, sheet="Calculations")
	r <- r[!is.na(r$Rep) & !is.na(r$Trt), ]

	y12 <- grep("^Yield_12", names(r), value=TRUE)[1]
	spks <- grep("^Spikes/m", names(r), value=TRUE)[1]

	d <- data.frame(
		record_id = as.integer(seq_len(nrow(r))),
		country = "Mexico",
		adm1 = "México",
		adm2 = "Texcoco",
		location = "El Batán",
		site = "Lote J8",
		# CIMMYT El Batan research station, Texcoco
		longitude = -98.848,
		latitude = 19.533,
		geo_from_source = FALSE,
		geo_uncertainty = 1000,
		geo_source = "CIMMYT El Batan research station",
		trial_id = "BV101B-17",
		plot_id = sprintf("%d%02d", as.integer(r$Rep), as.integer(r$Trt)),
		rep = as.integer(r$Rep),
		trt = as.integer(r$Trt),
		crop = "wheat",
		variety = "BV232-2016-Ent 13",
		variety_code = "6569899",
		planting_date = "2017",
		# Legend row 76 has Excel serial 43078 (2017-12-09), a DD/MM/YY -> MM/DD/YY swap of 12/09/2017
		harvest_date = "2017-09-12",
		yield_part = "grain",
		yield = as.numeric(r[[y12]]),
		yield_moisture = 12,
		yield_isfresh = TRUE,
		yield_dry = as.numeric(r$Yield_Dry),
		plant_height = as.numeric(r$Height),
		seed_weight = as.numeric(r$Thou),
		spike_density = as.numeric(r[[spks]]) * 10000,
		grains_per_spike = as.numeric(r[["Grains/spike"]]),
		grain_test_weight_ = as.numeric(r[["P.H."]]),
		lodging_ = as.numeric(r$Lodging),
		heading_dae_ = as.numeric(r$Tasseling),
		flowering_dae_ = as.numeric(r$Flowering),
		maturity_dae_ = as.numeric(r$Maturity),
		plot_area = as.numeric(r$Area),
		row_spacing = 20,
		seed_rate = 110,
		N_fertilizer = 150,
		P_fertilizer = 40 / 2.29,
		B_fertilizer = 0.18,
		fertilizer_type = "urea;TSP;Borax",
		land_prep_method = "wide permanent beds",
		irrigated = FALSE,
		stress = "weeds",
		on_farm = FALSE,
		is_survey = FALSE
	)
	d <- merge(d, key, by="trt", sort=FALSE)
	d$trt <- NULL

	# Harmonize minor spelling variants in field-book weed species codes
	fix_sp <- function(x) {
		m <- c(
			"H del C" = "H. Cancer", "H de Cancer" = "H. Cancer", "H.Cancer" = "H. Cancer",
			"H.Mora" = "H. Mora", "H.ANCHA" = "broadleaf total",
			"H.ANGOSTA" = "narrowleaf total", "CIPERACEAS" = "sedges total"
		)
		idx <- match(x, names(m))
		ifelse(is.na(idx), x, unname(m[idx]))
	}

	# Long table part 1: Weed counts across 3 dates
	f1 <- ff[basename(ff) == "DAT-BV101-17-WeedCounts_Wheat.xlsx"]
	wc_sheets <- c("20-Jun-17" = "2017-06-20", "03-Jul-17" = "2017-07-03", "07-Ago-17" = "2017-08-07")
	long_list <- list()
	for (sh in names(wc_sheets)) {
		w <- carobiner::read.excel(f1, sheet=sh, skip=2, na=c(".", ""))
		pid <- sprintf("%03d", as.integer(w$PARC))
		rid <- d$record_id[match(pid, d$plot_id)]
		sp_cols <- setdiff(names(w), "PARC")
		for (sp in sp_cols) {
			val <- as.numeric(w[[sp]])
			keep <- !is.na(rid) & !is.na(val)
			if (!any(keep)) next
			long_list[[length(long_list) + 1]] <- data.frame(
				record_id = rid[keep],
				date = unname(wc_sheets[sh]),
				weed_species = fix_sp(trimws(sp)),
				weed_count_ = val[keep],
				weed_dry_weight_ = NA_real_,
				phytotoxicity_score_ = NA_real_
			)
		}
	}

	# Long table part 2: Weed dry weight across 3 dates (2 side-by-side rep blocks in WeedDryWeight)
	f2 <- ff[basename(ff) == "DAT-BV101-17-WeedDryWeight_Wheat.xlsx"]
	wd <- carobiner::read.excel(f2, sheet="WeedDryWeight", col_names=FALSE, na=c(".", ""))
	wd_dates <- c("2017-06-23", "2017-07-07", "2017-08-11")
	wd_body <- wd[8:nrow(wd), ]
	for (blk in list(c(1, 2, 3, 4), c(6, 7, 8, 9))) {
		pid <- sprintf("%03d", as.integer(wd_body[[blk[1]]]))
		rid <- d$record_id[match(pid, d$plot_id)]
		for (k in 1:3) {
			val <- as.numeric(wd_body[[blk[k + 1]]])
			keep <- !is.na(rid) & !is.na(val)
			if (!any(keep)) next
			long_list[[length(long_list) + 1]] <- data.frame(
				record_id = rid[keep],
				date = wd_dates[k],
				weed_species = "all weeds",
				weed_count_ = NA_real_,
				weed_dry_weight_ = val[keep],
				phytotoxicity_score_ = NA_real_
			)
		}
	}

	# Long table part 3: Phytotoxicity score on 2017-06-29
	long_list[[length(long_list) + 1]] <- data.frame(
		record_id = d$record_id,
		date = "2017-06-29",
		weed_species = NA_character_,
		weed_count_ = NA_real_,
		weed_dry_weight_ = NA_real_,
		phytotoxicity_score_ = as.numeric(r[["Scoring_29/06/17"]])
	)

	long <- do.call(rbind, long_list)
	carobiner::write_files(path, meta, d, long=long)
}
