# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 17 plot records have harvest_date within 45 days of planting_date (likely year/month typos in raw Date_Planted/Date_Harvested); kept as reported for review.
# Some cassava plant_density, stem_density, fwy_stems, and maize cob_density values exceed terminag valid_max (kept as reported).
# Additional plot-level blocks in onfarm2017_all_locations_datafile_rft.csv (sprouting/germination, 15 WAP weed biomass, total weed density 4-14 WAP, herbicide efficacy 4-12 WAP) are listed in NOT PROCESSED and can be added to long if needed.

## NEW
# marketable_root_density_: number of marketable cassava roots harvested per hectare (roots/ha; raw column NoOKRootsha).
# herbicide_product values not in terminag: cyprosulfamide, diflufenican, flufenacet, flurtamone, pyroxasulfone.

## MISSING
# N_fertilizer, P_fertilizer, K_fertilizer, irrigated, yield_moisture (for cassava).

## NOTES
# 2017 multi-location on-farm cassava and cassava-maize intercrop weed-management trials across four Nigerian states (Abia, Benue, Ogun, Oyo) under the Sustainable Weed Management Technologies for Cassava Systems in Nigeria project.
# Herbicides are mapped directly from Pretreat and Posttreat trade names (verified against onfarm2017_all_locations_weedspecies_rft.csv) to active ingredients instead of fragile regex replacements on Treatment codes.
# Intercrop plots are split into separate cassava and maize rows sharing plot_id; weed species densities at 4, 6, 8, 10, and 12 WAP (converted from plants/m2 to plants/ha) are placed in long table dlong linked by record_id rather than merged wide.

## NOT PROCESSED
# Onfarm2017_All_Locations_DataFile_Rft.csv: sprouting/germination counts & percentages (CasSprt_* 4-8 WAP, MzGerm_* 2-6 WAP), weed dry biomass at 15 WAP (Wdbiomass_BLf/Grs/Sdg_gm2, WdBiomass15_gm2), plot-level total weed density (Wddenm2_04-14WAP), and herbicide weed control efficacy percentages (TWControl_*, Broadleaf_*, Grass_*, Sedge_* 4-12 WAP) — deferred until main CSV is downloaded and verified.
# codelist_onfarm2017_all_locations_weedspecies_rft.csv, metadata_onfarm2017_all_locations_datafile_rft.csv: documentation/codebooks only.


carob_script <- function(path) {

"
Cassava Weed Management Data - On farm trials 2017

The 'Sustainable Weed Management Technologies for Nigeria' was a 5-year project that was developed and assessed with smallholder farmer participation modern, relevant and appropriate cassava weed management technologies suitable for sustainable intensification in major agro-ecological (humid rainforest, forest transition savanna and southern Guinea savanna) and socio-economic conditions of Nigeria. An important goal of the project was to help smallholder cassava growers achieve sustainable increases in their productivity and incomes through the development and adoption of improved weed control methods. The project evaluated enhanced cassava agronomy, including judicious, safe use of herbicides, toward improved weed management, across 4 states in Nigeria where cassava is central to food security and livelihoods of 4.5 million farm families.

Though Nigeria is still the global leader in the overall production of cassava with about 50 million tons on 3.8 million hectares, average yields in Nigeria are only about half of those in leading countries in Asia, and less than half of those typical from researcher-run trials in Nigeria. Diverse factors are responsible for low productivity on about 4.5 million cassava farms, but poor weed management is generally among the principal factors. Weed control in the humid tropics is always a challenge, but compared to most other field crops, weed control in cassava systems is much more demanding. The crop is in the field for a long time (12 to 18 months), and is sown at wide spacing, resulting in ample opportunity for weeds to occupy space under the cassava canopy and reduce productivity. Although weeds are one of the most important constraints to improving cassava productivity; for high yields, good weed control needs to be coupled with improved varieties sown in the right densities at the right time. Adequate plant nutrition and pest control are also important; however, such inputs will not result in better yields if weeds are not controlled.

Hand weeding is the predominant weed control practice on smallholder cassava farms. Conventionally, farmers weed cassava three times, but in cassava farms where perennial weeds, such as Imperata, are predominant, extra hoe weeding may be required. Weeding takes 50 to 80% of the total labor budget. Up to 200-500 hours of labor for mostly women and children per ha are needed to prevent economic cassava root losses in Nigeria. IITA and its partners are therefore, through this project conducted research that developed innovative weed management practices, combining improved varieties, proper planting dates, plant populations, and plant nutrition, all coupled to intercropping and tillage options, through well-focused trials in the three agro-ecologies where cassava dominates in Nigeria. Herbicides, meeting globally accepted conventions and safety thresholds appropriate for smallholders, were tested for efficacy and economic merit. Multi-location on-station/off-station trials were followed with participatory farmer evaluations. Extension manuals and other tools for farmer and applicator learning were developed.

Results from this project showed that with appropriate weed management couple with best cassava agronomy cassava growers in can more than double the national yield average in Nigeria.
"

	uri <- "doi:10.25502/7k15-wy47/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Sustainable Weed Management Technologies for Cassava Systems in Nigeria",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "herbicide_used;herbicide_product;weeding_done;intercrops",
		response_vars = "yield;weed_density",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 55,
		carob_effort = 0
	)

	f1 <- ff[tolower(basename(ff)) == "onfarm2017_all_locations_datafile_rft.csv"]
	f2 <- ff[tolower(basename(ff)) == "onfarm2017_all_locations_weedspecies_rft.csv"]
	if (length(f1) == 0) {
		stop("Missing main data file onfarm2017_all_locations_datafile_rft.csv (IITA CKAN download failed with HTTP 503)")
	}
	r1 <- read.csv(f1[1], check.names=FALSE)
	r2 <- read.csv(f2[1], check.names=FALSE)

	# Helper to resolve minor header differences between metadata dictionary and CSV
	pick_col <- function(df, candidates) {
		hit <- candidates[candidates %in% names(df)]
		if (length(hit)) df[[hit[1]]] else rep(NA_real_, nrow(df))
	}

	# Map pre- and post-emergence trade names to active ingredients
	pre_map <- c(
		"Lagon" = "isoxaflutole;aclonifene",
		"Fierce" = "flumioxazin;pyroxasulfone",
		"Primextra" = "atrazine;s-metolachlor",
		"Gardoprim" = "s-metolachlor;terbuthylazine",
		"SencorPlus" = "indaziflam;metribuzin",
		"MerlinTotal" = "isoxaflutole;indaziflam",
		"Movon" = "flufenacet;diflufenican;flurtamone",
		"Untreated" = "none",
		"FarmerPractise" = "none"
	)
	post_map <- c(
		"MaisterPower" = "foramsulfuron;iodosulfuron-methyl-sodium;thiencarbazone-methyl;cyprosulfamide",
		"Maister61WG" = "foramsulfuron;iodosulfuron-methyl-sodium",
		"Fusilade+Cobra" = "fluazifop-p-butyl;lactofen",
		"SelectMax+Cobra" = "clethodim;lactofen",
		"Roundup" = "glyphosate",
		"Shorthandledhoe" = "none",
		"Untreated" = "none",
		"FarmerPractise" = "none"
	)

	pre_ai <- unname(pre_map[trimws(r1$Pretreat)])
	post_ai <- unname(post_map[trimws(r1$Posttreat)])
	comb_ai <- mapply(function(a, b) {
		parts <- c(a, b)
		parts <- parts[!is.na(parts) & parts != "none"]
		if (!length(parts)) return("none")
		paste(unique(unlist(strsplit(parts, ";"))), collapse=";")
	}, pre_ai, post_ai, USE.NAMES=FALSE)

	# Herbicide timing in days after planting (PostApplWAT is weeks after planting; 0 = not applied)
	wat <- as.numeric(r1$PostApplWAT)
	h_timing <- ifelse(!is.na(wat) & wat > 0, as.character(as.integer(round(wat * 7))), NA_character_)
	is_hoe <- trimws(r1$Posttreat) == "Shorthandledhoe" | grepl("FarmerPractise", r1$Pretreat) | grepl("FarmerPractise", r1$Posttreat)

	base <- data.frame(
		unique_id_ = trimws(as.character(r1$UniqueID)),
		country = "Nigeria",
		adm1 = trimws(r1$Location),
		location = trimws(r1$Site),
		trial_id = paste(trimws(r1$Location), trimws(r1$Site), sep="_"),
		plot_id = as.character(r1$PlotNo),
		treatment = trimws(r1$Treatment),
		latitude = as.numeric(r1$Latitude),
		longitude = as.numeric(r1$Longitude),
		geo_from_source = TRUE,
		herbicide_used = comb_ai != "none",
		herbicide_product = comb_ai,
		herbicide_timing = h_timing,
		weeding_done = is_hoe,
		weeding_method = ifelse(is_hoe, "manual", NA_character_),
		soil_pH_KCl = as.numeric(pick_col(r1, c("pH_KCl"))),
		soil_SOC = as.numeric(pick_col(r1, c("OC", "OC_pct"))),
		# % N converted to mg/kg (* 10000)
		soil_N = as.numeric(pick_col(r1, c("N", "N_pct"))) * 10000,
		soil_P = as.numeric(pick_col(r1, c("P", "Meh_P_ppm"))),
		soil_P_method = "Mehlich",
		# Exchangeable Ca in cmol(+)/kg
		soil_Ca_exch = as.numeric(pick_col(r1, c("Ca", "CMOL_perKg"))),
		soil_sand = as.numeric(r1$SAND_pct),
		soil_silt = as.numeric(r1$SILT_pct),
		soil_clay = as.numeric(r1$CLAY_pct),
		on_farm = TRUE,
		is_survey = FALSE,
		stress = "weeds"
	)

	is_inter <- grepl("Intercrop", r1$Cropsystem, ignore.case=TRUE)

	# Cassava rows (all plots)
	d_cas <- base
	d_cas$crop <- "cassava"
	d_cas$intercrops <- ifelse(is_inter, "maize", "none")
	d_cas$planting_date <- as.character(as.Date(r1$Date_Planted_Cas, "%m/%d/%Y"))
	d_cas$harvest_date <- as.character(as.Date(r1$Date_Harvested_Cas, "%m/%d/%Y"))
	d_cas$plant_density <- as.numeric(r1$Standha_Cas)
	d_cas$stem_density <- as.numeric(pick_col(r1, c("Stemha_Cas")))
	d_cas$fwy_stems <- as.numeric(r1$WtStemtha) * 1000
	d_cas$marketable_root_density_ <- as.numeric(r1$NoOKRootsha)
	d_cas$yield <- as.numeric(r1$RootYldtha) * 1000
	d_cas$yield_part <- "roots"
	d_cas$yield_isfresh <- TRUE

	# Maize rows (intercropped plots only)
	d_mze <- base[is_inter, , drop=FALSE]
	d_mze$crop <- "maize"
	d_mze$intercrops <- "cassava"
	d_mze$planting_date <- as.character(as.Date(r1$Date_Planted_Mz[is_inter], "%m/%d/%Y"))
	d_mze$harvest_date <- as.character(as.Date(r1$Date_Harvested_Mz[is_inter], "%m/%d/%Y"))
	d_mze$plant_density <- as.numeric(r1$Standha_Mz[is_inter])
	d_mze$cob_density <- as.numeric(r1$MzCobCount_ha[is_inter])
	d_mze$yield <- as.numeric(r1$MzGrainYld_tha[is_inter]) * 1000
	d_mze$yield_part <- "grain"
	# Dictionary: Maize grain yield in t/ha @ 12% moisture content
	d_mze$yield_moisture <- 12
	d_mze$yield_isfresh <- FALSE

	d <- carobiner::bindr(d_cas, d_mze)
	d$record_id <- seq_len(nrow(d))

	# Weed species density time series (4-12 WAP) in long format, linked to cassava plot records
	cas_rid <- setNames(d$record_id[d$crop == "cassava"], d$unique_id_[d$crop == "cassava"])
	wap_cols <- c("Wdden_m2_04WAP", "Wdden_m2_06WAP", "Wdden_m2_08WAP", "Wdden_m2_10WAP", "Wdden_m2_12WAP")
	dap_vals <- c(28L, 42L, 56L, 70L, 84L)
	r2_rid <- unname(cas_rid[trimws(as.character(r2$UniqueID))])
	keep_r2 <- !is.na(r2_rid)
	dlong <- do.call(rbind, lapply(seq_along(wap_cols), function(k) {
		data.frame(
			record_id = r2_rid[keep_r2],
			DAP = dap_vals[k],
			weed_species = trimws(as.character(r2$WeedSpecies[keep_r2])),
			# Convert plants/m2 to plants/ha (* 10000)
			weed_density = as.numeric(r2[[wap_cols[k]]][keep_r2]) * 10000
		)
	}))
	dlong <- dlong[!is.na(dlong$weed_density), ]
	d$unique_id_ <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
