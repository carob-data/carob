# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The Scoring columns are labeled "Damages for diseases" in the English legend and "Danos por fitotoxicidad" in the Spanish legend (0-5 scale, 0 = same as control to 5 = plants dying); per protocol item 5 (Escoring de fitotoxicidad) they are stored in phytotoxicity_score_.
# Weed count methods specify 2 quadrats per plot (each 0.50 m x 1.50 m bed width = 0.75 m2) without stating whether recorded counts and dry weights are per quadrat or summed over both quadrats, so values are kept unscaled in the long table.
# Exact planting date is not stated; repository metadata lists 2017-06 to 2017-12, whereas Maturity (183 DAE) and Harvest (2017-11-24) imply emergence in late May 2017. planting_date is set to "2017".
# Raw ASI column is computed as Tasseling - Silking (-2); stored in asi as Silking - Tasseling (+2) to match standard definition.
# Herbicide doses in BV101-17-A-Maize.docx are product rates (g or mL/ha), not kg a.i./ha; kept in herbicide_product_dose_.

## NEW
# tasseling_dae_: days from emergence to tasseling (days).
# silking_dae_: days from emergence to silking (days).
# flowering_dae_: days from emergence to flowering (days).
# maturity_dae_: days from emergence to maturity (days).
# good_ear_density_: number of undamaged (<50% damaged) maize ears per ha (ears/ha).
# bad_ear_density_: number of damaged (>50% damaged) maize ears per ha (ears/ha).
# lodging_density_: total lodged plants per ha (plants/ha).
# root_lodging_density_: root-lodged plants per ha (plants/ha).
# stalk_lodging_density_: stalk-lodged plants per ha (plants/ha).
# herbicide_product_dose_: commercial herbicide product dose from protocol (g or mL product/ha; semicolon-separated for tank mixes).
# weed_count_: weed plant count per sampled quadrat on observation date (count).
# weed_dry_weight_: total weed dry weight per sampled quadrat on observation date (g).
# phytotoxicity_score_: visual herbicide phytotoxicity score (0-5 scale, mean of 7 observations/plot).
# herbicide_product values not in terminag: acetochlor.

## MISSING
# K_fertilizer: potassium application is not mentioned in the workbook or protocol.

## NOTES
# Herbicide efficacy trial in rainfed maize (cv. CHLHW09035) at CIMMYT El Batan (Lote J8), Mexico, 2017 (protocol BV101-17-A-Maize.docx).
# RCBD with 2 replicates and 14 treatments (untreated check, weed-free check, and 12 herbicide treatments) on wide permanent beds (1.5 m), 150 kg N/ha (urea), 40 kg P2O5/ha (TSP), and 1.5 kg/ha borax (~0.18 kg B/ha) pre-sowing.
# Coordinates are estimated for the CIMMYT El Batan research station (Texcoco, Mexico).
# Standardized yield (12% H2O) and dry yield are in d; repeated weed species counts (3 dates), weed dry weight (3 dates), and phytotoxicity scores (3 dates) are in long.

## NOT PROCESSED
# DAT-BV101-17-Yield_Maize.xlsx (Calculations_Maize): Grains/m2, %GoodEars (derived from already standardized yield, seed_weight, and ear densities).


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
		project = "BV101A/17",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "herbicide_product;herbicide_used;weeding_done",
		response_vars = "yield;cob_density;plant_height;seed_weight",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 88,
		carob_effort = 0
	)

	# Treatments, active ingredients, and product doses (g or mL/ha) from BV101-17-A-Maize.docx
	key <- data.frame(
		trt = 1:14,
		treatment = c(
			"untreated check", "weed free", "Gesaprim (atrazine)", "Surpass (acetochlor)",
			"Gesaprim+Surpass (acetochlor;atrazine)", "Primagram gold (atrazine;s-metolachlor)",
			"Sempra (halosulfuron-methyl)", "Adengo (isoxaflutole;thiencarbazone-methyl)",
			"Calistto (mesotrione)", "Laudis (tembotrione)", "Marvel (dicamba;atrazine)",
			"Gesaprim+Banvel (atrazine;2,4-D;dicamba)", "Gesaprim+Veloz (atrazine;carfentrazone)",
			"Sanson (nicosulfuron)"
		),
		herbicide_product = c(
			"none", "none", "atrazine", "acetochlor", "acetochlor;atrazine",
			"atrazine;s-metolachlor", "halosulfuron-methyl", "isoxaflutole;thiencarbazone-methyl",
			"mesotrione", "tembotrione", "dicamba;atrazine", "atrazine;2,4-D;dicamba",
			"atrazine;carfentrazone", "nicosulfuron"
		),
		herbicide_product_dose_ = c(
			"0", "0", "1388", "2000", "1111;2000", "4000", "100", "250",
			"415", "300", "3000", "1111;1000", "1111;1000", "1000"
		),
		herbicide_used = c(FALSE, FALSE, rep(TRUE, 12)),
		weeding_done = c(FALSE, TRUE, rep(FALSE, 12))
	)

	f <- ff[basename(ff) == "DAT-BV101-17-Yield_Maize.xlsx"]
	r <- carobiner::read.excel(f, sheet="Calculations_Maize")
	r <- r[!is.na(r$Trt), ]

	y12 <- grep("^Yield_12", names(r), value=TRUE)[1]
	ears_tot <- grep("^Ears/m", names(r), value=TRUE)[1]
	ears_good <- grep("^GoodEars/m", names(r), value=TRUE)[1]
	ears_bad <- grep("^BadEars/m", names(r), value=TRUE)[1]
	area <- as.numeric(r$Area)

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
		trial_id = "BV101A-17",
		plot_id = sprintf("%d%02d", as.integer(r$Rep), as.integer(r$Trt)),
		rep = as.integer(r$Rep),
		trt = as.integer(r$Trt),
		crop = "maize",
		variety = "CHLHW09035",
		planting_date = "2017",
		harvest_date = "2017-11-24",
		yield_part = "grain",
		yield = as.numeric(r[[y12]]),
		yield_moisture = 12,
		yield_isfresh = TRUE,
		yield_dry = as.numeric(r$Yield_dry),
		cob_density = as.numeric(r[[ears_tot]]) * 10000,
		good_ear_density_ = as.numeric(r[[ears_good]]) * 10000,
		bad_ear_density_ = as.numeric(r[[ears_bad]]) * 10000,
		plant_height = as.numeric(r$Height),
		seed_weight = as.numeric(r$Thou),
		# Raw ASI is Tasseling - Silking (-2); standard ASI is Silking - Tasseling (+2)
		asi = as.numeric(r$Silking) - as.numeric(r$Tasseling),
		lodging_density_ = as.numeric(r[["Lodging/ha"]]),
		root_lodging_density_ = as.numeric(r[["Lodging A"]]) / area * 10000,
		stalk_lodging_density_ = as.numeric(r[["Lodging D"]]) / area * 10000,
		plot_area = area,
		row_spacing = 75,
		seed_density = 55000,
		tasseling_dae_ = as.numeric(r$Tasseling),
		silking_dae_ = as.numeric(r$Silking),
		flowering_dae_ = as.numeric(r$Flowering),
		maturity_dae_ = as.numeric(r$Maturity),
		N_fertilizer = 150,
		P_fertilizer = 40 / 2.29,
		B_fertilizer = 0.18,
		fertilizer_type = "urea;TSP;Borax",
		land_prep_method = "wide permanent beds",
		stress = "weeds",
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)
	d <- merge(d, key, by="trt", sort=FALSE)
	d$trt <- NULL

	# Harmonize minor spelling variants in field-book weed species codes
	fix_sp <- function(x) {
		m <- c(
			"Bromo" = "Bromos", "Elev" = "Elve", "Lechu" = "Lech",
			"Hidel C" = "H. Cancer", "H.ANCHA" = "broadleaf total",
			"H.ANGOSTA" = "narrowleaf total", "CIPERACEAS" = "sedges total"
		)
		idx <- match(x, names(m))
		ifelse(is.na(idx), x, unname(m[idx]))
	}

	# Long table part 1: Weed counts across 3 dates
	f1 <- ff[basename(ff) == "DAT-BV101-17-WeedCounts_Maize.xlsx"]
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

	# Long table part 2: Weed dry weight across 3 dates (Excel serials in row 5: 42909, 42923, 42958)
	f2 <- ff[basename(ff) == "DAT-BV101-17-WeedDryWeight_Maize.xlsx"]
	wd_raw <- carobiner::read.excel(f2, sheet="PBiomasa", col_names=FALSE, na=c(".", ""))
	wd_dates <- as.character(as.Date(as.numeric(unlist(wd_raw[5, 3:5])), origin="1899-12-30"))
	wd_body <- wd_raw[7:nrow(wd_raw), ]
	pid <- sprintf("%03d", as.integer(wd_body[[2]]))
	rid <- d$record_id[match(pid, d$plot_id)]
	for (k in 1:3) {
		val <- as.numeric(wd_body[[k + 2]])
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

	# Long table part 3: Phytotoxicity scores across 3 dates from Calculations_Maize
	sc_cols <- c("Scoring 03-07-17" = "2017-07-03", "Scoring 01-09-17" = "2017-09-01", "Scoring 15-09-17" = "2017-09-15")
	for (sc in names(sc_cols)) {
		val <- as.numeric(r[[sc]])
		keep <- !is.na(val)
		if (!any(keep)) next
		long_list[[length(long_list) + 1]] <- data.frame(
			record_id = d$record_id[keep],
			date = unname(sc_cols[sc]),
			weed_species = NA_character_,
			weed_count_ = NA_real_,
			weed_dry_weight_ = NA_real_,
			phytotoxicity_score_ = val[keep]
		)
	}

	long <- do.call(rbind, long_list)
	carobiner::write_files(path, meta, d, long=long)
}
