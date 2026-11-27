# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Nicaglobale.xlsx repeats each of the 49 plots 4 times (Cartesian product of
#    April and October fruit-count subsets); plot-level environmental attributes
#    are identical across the 4 rows and are deduplicated by plot_id, while full
#    tree-level CBB fruit counts come from brocaarbre.csv.
# 2. Spatial coordinates X and Z in Nicaglobale.xlsx are UTM Zone 16N (WGS84,
#    EPSG:32616) easting/northing in meters and are converted to WGS84 longitude
#    and latitude (landing within El Tuma - La Dalia, Matagalpa, Nicaragua).
# 3. Three (plant_id, date) pairs appear twice in antarbre.xlsx with differing
#    ant counts; visit_ distinguishes them.
# 4. brocaarbre.csv mixes capitalized and lowercase farm_id labels; these are
#    standardized to title case.

## NEW
# farm_practice_: coffee farm management system (Conventional, Organic, Sustainable)
# shade_: mean canopy shade cover in 2017 (%)
# shade_tree_species_: central shade tree species in plot
# shade_tree_richness_: shade tree species richness in plot (count)
# shade_tree_dbh_sum_: sum of shade tree diameters at breast height in plot (m)
# shade_tree_basal_area_: total shade tree basal area in plot (m2)
# coffee_age_: mean age of coffee plants in plot (years)
# tmin_mean_: mean plot minimum air temperature (deg C)
# tmax_mean_: mean plot maximum air temperature (deg C)
# temp_diurnal_range_: diurnal temperature range 06:00-18:00 on 9-10 Jul 2017 (deg C)
# cbb_trap_count_: coffee berry borers trapped in 6 days in April 2018 (count/plot)
# cbb_berry_damage_: sum of CBB berry damage scores on 100 berries in Oct 2018
# cbb_berry_presence_: % of berries containing CBB inside in Oct 2018 (%)
# berry_weight_100_: weight of 100 coffee berries (g)
# plant_id_: sampled tree identifier within plot
# fruits_sampled_: number of mature coffee fruits sampled on tree
# visit_: sampling visit index for duplicate tree-date entries in antarbre
# pest_species value not yet in terminag: Hypothenemus hampei

## MISSING
# yield, yield_part, yield_moisture, irrigated, planting_date, harvest_date,
# N_fertilizer, P_fertilizer, K_fertilizer.

## NOTES
# Coffee agroforestry pest-regulation survey across 49 plots in 3 farms
# (Conventional, Organic, Sustainable) in El Tuma - La Dalia, Matagalpa,
# Nicaragua (2017-2018; Bagny Beilhe et al. 2020). Wide table d holds plot-level
# agroforestry, microclimate, and CBB summary variables (Nicaglobale.xlsx); long
# holds tree-level CBB fruit infestation (brocaarbre.csv) and predatory ant counts
# (antarbre.xlsx, stored in insect_species / insect_abundance) linked by record_id.

## NOT PROCESSED
# Nicaglobale.xlsx: derived diversity indices (Simpglob2/3, shanglob2/3, shan_tree),
# aggregated ant counts/ratios (redundant with antarbre.xlsx), April/October fruit
# summaries (redundant with brocaarbre.csv), fruiting branch counts (n_rmfruct17,
# n_ndfruct17, n_rmmort17).
# Script_BagnyBeilhe_et_al_2020.R, ExplicationNicaglobal.docx, Readme.txt:
# analysis script and documentation.

carob_script <- function(path) {

"
Raw data: Bagny Beilhe et al 2020: \"Pest-regulating networks of the coffee berry borer (Hypothenemus hampei) in agroforestry systems. Crop Protection, 131 : 10 p.\"

Données collectées à La Dalia, Nicaragua, de 2017 à 2018. Données issues de 3 systèmes agroforestiers à base de caféiers avec des niveaux de diversification végétale différents et trois types de pratiques (conventionnelles, durables et organiques). Dans chaque ferme 20 parcelles ont été suivies. Données collectées durant une année et demi pour suivre la dynamique des fruits scolytés dans les parcelles, les fourmis arboricoles, l'évolution de la température, de l'ombrage, du rendement et des dégâts associés aux scolytes dans les parcelles.
"

	uri <- "doi:10.18167/DVN1/QVTOBA"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.cropro.2019.105036",
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence;insect_species;insect_abundance",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	# WGS84 UTM Zone 16N (EPSG:32616) inverse to decimal longitude/latitude
	utm16n_to_lonlat <- function(E, N) {
		a <- 6378137.0
		f <- 1 / 298.257223563
		e2 <- f * (2 - f)
		ep2 <- e2 / (1 - e2)
		k0 <- 0.9996
		lon0 <- -87 * pi / 180
		x <- E - 500000.0
		M <- N / k0
		e1 <- (1 - sqrt(1 - e2)) / (1 + sqrt(1 - e2))
		mu <- M / (a * (1 - e2/4 - 3*e2^2/64 - 5*e2^3/256))
		phi1 <- mu + (3*e1/2 - 27*e1^3/32)*sin(2*mu) + (21*e1^2/16 - 55*e1^4/32)*sin(4*mu) +
			(151*e1^3/96)*sin(6*mu) + (1097*e1^4/512)*sin(8*mu)
		N1 <- a / sqrt(1 - e2 * sin(phi1)^2)
		T1 <- tan(phi1)^2
		C1 <- ep2 * cos(phi1)^2
		R1 <- a * (1 - e2) / (1 - e2 * sin(phi1)^2)^(3/2)
		D <- x / (N1 * k0)
		lat <- phi1 - (N1 * tan(phi1) / R1) * (D^2/2 - (5 + 3*T1 + 10*C1 - 4*C1^2 - 9*ep2)*D^4/24 +
			(61 + 90*T1 + 298*C1 + 45*T1^2 - 252*ep2 - 3*C1^2)*D^6/720)
		lon <- lon0 + (D - (1 + 2*T1 + C1)*D^3/6 +
			(5 - 2*C1 + 28*T1 - 3*C1^2 + 8*ep2 + 24*T1^2)*D^5/120) / cos(phi1)
		data.frame(longitude = lon * 180 / pi, latitude = lat * 180 / pi)
	}

	month_year <- function(x) {
		mon <- c(january="01", february="02", march="03", april="04", may="05", june="06",
			july="07", august="08", september="09", october="10", november="11", december="12")
		mm <- tolower(gsub("[0-9]", "", x))
		yy <- gsub("[^0-9]", "", x)
		paste0("20", yy, "-", unname(mon[mm]))
	}

	f1 <- ff[basename(ff) == "Nicaglobale.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet = "Nicaglobale", na = c("", "NA"))
	u1 <- r1[!duplicated(r1$plot_id), ]
	rownames(u1) <- NULL

	ll <- utm16n_to_lonlat(as.numeric(u1$X), as.numeric(u1$Z))
	farm_clean <- carobiner::fix_name(trimws(as.character(u1$farm_id)), "title")
	plot_r <- 7 # 7 m radius circular plot (~153.9 m2; Script_BagnyBeilhe_et_al_2020.R uses 153 m2)

	d <- data.frame(
		record_id = seq_len(nrow(u1)),
		trial_id = paste0(farm_clean, "_", trimws(as.character(u1$plot_id))),
		plot_id = trimws(as.character(u1$plot_id)),
		country = "Nicaragua",
		adm1 = "Matagalpa",
		location = "El Tuma - La Dalia",
		longitude = round(ll$longitude, 6),
		latitude = round(ll$latitude, 6),
		geo_from_source = TRUE,
		crop = "coffee",
		farm_practice_ = farm_clean,
		plot_area = round(pi * plot_r^2, 1),
		plant_density = round(as.numeric(u1$density) / 153 * 10000),
		tree_density = round(as.numeric(u1$nbtree1) / (pi * plot_r^2) * 10000),
		shade_ = as.numeric(u1$shade),
		shade_tree_species_ = gsub("_", " ", trimws(as.character(u1$tree_id)), fixed = TRUE),
		shade_tree_richness_ = as.integer(as.numeric(u1$richesp)),
		shade_tree_dbh_sum_ = as.numeric(u1$DBH),
		shade_tree_basal_area_ = as.numeric(u1$treesurface),
		coffee_age_ = as.numeric(u1$age),
		tmin_mean_ = as.numeric(u1$temp_min),
		tmax_mean_ = as.numeric(u1$temp_max),
		temp_diurnal_range_ = as.numeric(u1$Ampli),
		cbb_trap_count_ = as.numeric(u1$Broca),
		cbb_berry_damage_ = as.numeric(u1$damage),
		cbb_berry_presence_ = as.numeric(u1$presence),
		berry_weight_100_ = as.numeric(u1$weight),
		stress = "pests",
		on_farm = TRUE,
		is_survey = TRUE
	)

	## Tree-level CBB fruit observations (brocaarbre.csv)
	f2 <- ff[basename(ff) == "brocaarbre.csv"]
	r2 <- read.csv(f2, sep = ";", check.names = FALSE)
	k2 <- match(trimws(as.character(r2$plot_id)), d$plot_id)
	l_cbb <- data.frame(
		record_id = d$record_id[k2],
		trial_id = d$trial_id[k2],
		plant_id_ = trimws(as.character(r2$plant_id)),
		date = month_year(trimws(as.character(r2$date))),
		pest_species = "Hypothenemus hampei",
		pest_incidence = as.integer(r2$fruit_CBB),
		fruits_sampled_ = as.integer(r2$fruit_nb)
	)

	## Tree-level ant observations (antarbre.xlsx; predatory/arboreal insects, not pests)
	f3 <- ff[basename(ff) == "antarbre.xlsx"]
	r3 <- carobiner::read.excel(f3, sheet = "antarbre")
	visit <- ave(seq_len(nrow(r3)), r3$plant_id, r3$date, FUN = seq_along)
	idcols <- c("plant_id", "date", "plant_id2", "farm_id", "plot_id")
	spp <- r3[, !(names(r3) %in% idcols)]
	n <- nrow(r3)
	k3 <- match(trimws(as.character(r3$plot_id)), d$plot_id)

	l_ant <- data.frame(
		record_id = rep(d$record_id[k3], ncol(spp)),
		trial_id = rep(d$trial_id[k3], ncol(spp)),
		plant_id_ = rep(trimws(as.character(r3$plant_id)), ncol(spp)),
		date = rep(month_year(trimws(as.character(r3$date))), ncol(spp)),
		visit_ = rep(as.integer(visit), ncol(spp)),
		insect_species = rep(gsub("_", " ", names(spp), fixed = TRUE), each = n),
		insect_abundance = as.character(as.integer(unlist(spp, use.names = FALSE)))
	)

	long <- carobiner::bindr(l_cbb, l_ant)

	carobiner::write_files(path, meta, d, long = long)
}
