# R script for "carob"
# license: GPL (>=3)

## NOTES
# Individual-plant field trial of 21 species (17 cover crops, 2 spontaneous weeds, 2 banana cultivars) at CIRAD Neufchâteau station, Capesterre-Belle-Eau, Guadeloupe in 2013 (Damour, Tardy, Dorel & Moreau, 2020, Data in Brief, doi:10.1016/j.dib.2020.105890).
# Each species was grown in 4 randomized blocks with one plant per 16 m2 plot (plot_area = 16 m2; plant_density = 625 plants/ha), harvested destructively at flowering (short-cycle annuals) or after ~6 months.
# Code "P" is Momordica charantia (French Creole: paroka / pomme coolie, Cucurbitaceae), the second spontaneous species alongside Bidens pilosa (verified from Damour et al., 2020); mapped to "bitter gourd".
# Dry weights (g/plant) are converted to kg/ha using 10000 / 16 / 1000 (= 0.625) alongside plant_density = 625 plants/ha so per-plant weights remain recoverable. Aboveground dry biomass (BMa) is stored in both dmy_total and yield (yield_part = "aboveground biomass", yield_moisture = 0).
# LNC (%) is leaf nitrogen concentration (variables.txt mislabels it "Leaf dry matter content", while LDMC is mg/g; values are 2.36-6.07%), converted (* 10) to mg/g for leaf_N.
# Fertilizer: 50 g urea (46% N) per plant = 14.375 kg N/ha; rainfed (irrigated = FALSE); no P or K applied (0 kg/ha). Coordinates (16°05'N, 61°35'W) from the data paper.
# Planting date is computed as collection date minus DAS (harvest_days).

## ISSUES
# Low-growing creeping cover crops (e.g. Arachis pintoi) had standing vegetative heights of 7-9 cm (< terminag plant_height min of 10 cm); kept as measured. Small annuals (Bidens pilosa, cowpea, marigold) have < 100 kg/ha aboveground dry biomass because each plot had a single plant on 16 m2 (625 plants/ha), triggering the low yield check.
# Five species (Arachis pintoi, Bidens pilosa, Centrosema pascuorum, Mucuna deeringiana, Neonotonia wightii, Pueraria phaseoloides) are not yet in terminag's crop list and are kept as Latin binomials.
# Nodule activity (Nod) uses categorical codes (0, 2, 3) whose exact scale is not defined in variables.txt; retained as nodule_score_.

## NEW VARIABLES
# plant_role_: functional role in the banana cropping system ("cover crop", "spontaneous", or "banana")
# crown_width1_: maximal crown width (cm)
# crown_width2_: crown width perpendicular to crown_width1_ (cm)
# crown_area_: crown projected area (m2)
# dmy_leaf_blade_: leaf blade dry weight per ha (kg/ha)
# dmy_petiole_: petiole dry weight per ha (kg/ha)
# leaf_area_: total leaf blade area per plant (m2)
# LSA_: leaf-to-soil area ratio, LA / crown_area (m2/m2)
# SLA_: specific leaf area with petioles (m2/kg)
# LDMC_: leaf dry matter content (mg/g)
# rooting_depth_: maximal rooting depth (cm)
# rooting_depth_med_: median rooting depth (cm)
# rooting_width_: maximal rooting width (cm)
# nodule_score_: categorical root nodule activity code (0, 2, 3)
# root_diameter_: mean root diameter (mm)
# SRL_: specific root length (m/g)
# RLD_: root length density (cm/cm3)
# RWD_: root weight density (g/cm3)
# RID_0_20_: root impact density in 0-20 cm soil layer (/dm2)
# RID_20_40_: root impact density in 20-40 cm soil layer (/dm2)
# RID_40_60_: root impact density in 40-60 cm soil layer (/dm2)
# RID_60_80_: root impact density in 60-80 cm soil layer (/dm2)

## NOT PROCESSED
# data_CC_indiv_plant.txt: subsample leaf weights (DWb_we, DWp_we, DWleaf_we) and derived ratios (PBR, LMFab, LMFa, LDMCps, SLAb, SLAb_ps, SLAps, LARab_ps, LARaps, LARab, LARa) recoverable from primary weights/areas.
# data_CC_indiv_plant_variables.txt: variable dictionary/codebook, not an observation table.


carob_script <- function(path) {

"
A dataset on above- and below-ground traits of 17 cover crops, two wild plant species and two banana cultivars, cultivated individually

This dataset describes 21 species of cover crops (17 sp.), wild plant species (2 sp.) and bananas (2 sp.), each repeated four times and cultivated individually under non-limiting conditions, on 16m² plots. Data were acquired at flowering (annuals with short cycle) or after six months of growth (other species/varieties). The traits and variables measured refer to whole plant (7 variables), leaves (21 variables), and roots (12 variables). This dataset is made available to enable comparisons between datasets, extended analysis and meta-analysis on cover crops
"

	uri <- "doi:10.18167/DVN1/HIEXNF"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.dib.2020.105890",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "crop;variety",
		response_vars = "yield;dmy_total;plant_height;leaf_N;rooting_depth_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "data_CC_indiv_plant.txt"]
	r <- read.delim(f, na.strings=c("NA", "na", ""), check.names=FALSE)
	names(r) <- make.unique(trimws(names(r)))
	r$esp <- trimws(r$esp)
	r <- r[!is.na(r$esp) & nzchar(r$esp), ]

	# Table 1 of Damour et al. (2020), doi:10.1016/j.dib.2020.105890; code P = Momordica charantia (spontaneous Cucurbitaceae)
	sp <- data.frame(
		esp = c("AP", "B925", "Bcav", "BD", "BP", "BR", "CCG", "CP", "CS", "CZ", "DL", "GS", "MD", "N", "NW", "P", "PN", "PP", "RC", "SG", "TP"),
		crop = c("Arachis pintoi", "banana", "banana", "brachiaria", "Bidens pilosa", "brachiaria", "pigeon pea",
			"Centrosema pascuorum", "crotalaria", "crotalaria", "lablab", "gliricidia", "Mucuna deeringiana",
			"cowpea", "Neonotonia wightii", "bitter gourd", "bahia grass", "Pueraria phaseoloides", "castor bean",
			"stylosanthes", "marigold"),
		variety = c("Arachis pintoi", "Cirad 925", "Cavendish", "Brachiaria decumbens", "Bidens pilosa",
			"Brachiaria ruziziensis", "Cajanus cajan Guadeloupe", "Centrosema pascuorum", "Crotalaria spectabilis",
			"Crotalaria zanzibarica", "Dolichos lablab", "Gliricidia sepium", "Mucuna deeringiana",
			"Vigna unguiculata David", "Neonotonia wightii", "Momordica charantia", "Paspalum notatum",
			"Pueraria phaseoloides", "Ricinus communis", "Stylosanthes guianensis", "Tagetes patula"),
		plant_role_ = c("cover crop", "banana", "banana", "cover crop", "spontaneous", "cover crop", "cover crop",
			"cover crop", "cover crop", "cover crop", "cover crop", "cover crop", "cover crop",
			"cover crop", "cover crop", "spontaneous", "cover crop", "cover crop", "cover crop",
			"cover crop", "cover crop")
	)
	r <- merge(r, sp, by="esp", all.x=TRUE)

	# 1 plant per 16 m2 plot: g/plant * (10000 / 16) / 1000 -> kg/ha
	to_ha <- 10000 / 16 / 1000
	collected <- as.Date(trimws(r$date), "%d/%m/%Y")
	das <- as.integer(r$DAS)
	block <- trimws(r$block)

	d <- data.frame(
		trial_id = "Neufchateau_2013",
		plot_id = paste(r$esp, block, sep="_"),
		rep = as.integer(sub("^B", "", block)),
		country = "Guadeloupe",
		location = "Neufchateau",
		# Coordinates from Damour et al. (2020): 16°05'N, 61°35'W
		latitude = 16 + 5/60,
		longitude = -(61 + 35/60),
		geo_from_source = TRUE,
		plot_area = 16,
		plant_density = 10000 / 16,
		crop = r$crop,
		variety = r$variety,
		plant_role_ = r$plant_role_,
		treatment = r$variety,
		planting_date = as.character(collected - das),
		harvest_date = as.character(collected),
		harvest_days = das,
		plant_height = as.numeric(r$H),
		crown_width1_ = as.numeric(r$W1),
		crown_width2_ = as.numeric(r$W2),
		crown_area_ = as.numeric(r$A),
		dmy_stems = as.numeric(r$DWs) * to_ha,
		dmy_leaves = as.numeric(r$DWleaf) * to_ha,
		dmy_leaf_blade_ = as.numeric(r$DWb) * to_ha,
		dmy_petiole_ = as.numeric(r$DWp) * to_ha,
		dmy_total = as.numeric(r$BMa) * to_ha,
		yield = as.numeric(r$BMa) * to_ha,
		yield_part = "aboveground biomass",
		yield_moisture = 0,
		# LNC is leaf N concentration (%) -> * 10 for mg/g
		leaf_N = as.numeric(r$LNC) * 10,
		leaf_area_ = as.numeric(r$LA),
		LSA_ = as.numeric(r$LSA),
		SLA_ = as.numeric(r$SLA),
		LDMC_ = as.numeric(r$LDMC),
		rooting_depth_ = as.numeric(r$RDmax),
		rooting_depth_med_ = as.numeric(r$RDmed),
		rooting_width_ = as.numeric(r$RWmax),
		nodule_score_ = as.character(r$Nod),
		root_diameter_ = as.numeric(r$Diam),
		SRL_ = as.numeric(r$SRL),
		RLD_ = as.numeric(r$RLD),
		RWD_ = as.numeric(r$RWD),
		RID_0_20_ = as.numeric(r[["RID0-20"]]),
		RID_20_40_ = as.numeric(r[["RID20-40"]]),
		RID_40_60_ = as.numeric(r[["RID40-60"]]),
		RID_60_80_ = as.numeric(r[["RID60-80"]]),
		# 50 g urea (46% N) per plant on 16 m2 plot; no P or K applied
		fertilizer_used = TRUE,
		fertilizer_type = "urea",
		N_fertilizer = 50 * 0.46 * (10000 / 16) / 1000,
		P_fertilizer = 0,
		K_fertilizer = 0,
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
