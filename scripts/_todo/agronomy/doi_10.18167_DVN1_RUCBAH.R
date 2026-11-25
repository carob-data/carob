# R script for "carob"
# license: GPL (>=3)

## NOTES
# Litter-bag decomposition, vegetation cover, detritivorous macrofauna, earthworm community, and plot microclimate experiment across 6 organic and 6 conventional banana plantations in two bioclimatic zones of Martinique (Dec 2020 - Mar 2021; El Jaouhari, Damour & Coulis, 2024, Applied Soil Ecology 204:105697, doi:10.1016/j.apsoil.2024.105697).
# Coordinates are estimated at bioclimatic zone level from El Jaouhari et al. (2024) because exact farm GPS coordinates are not published (Southern zone / Vertisol near Le Vauclin: 14.545 N, -60.838 E for Bioban and Petite Grenade; Central zone / Ferrisol near Saint-Joseph / Lézarde station: 14.663 N, -60.996 E for Banabio; geo_from_source = FALSE, geo_uncertainty = 5000 m).
# Treatments: farming_ (organic vs conventional), mesh_ (small vs large mesh litter bags), and glyphosate_ (TRUE for G+ glyphosate-contaminated banana leaf litter, FALSE for G-).
# Decomposition variables from Decomposition.csv: initial and final ash-free dry litter mass (g/bag), ash-free mass loss (g/bag), decomposition fraction (decomp = mass_loss / mass_i, 0.14-0.98), and faunal_contribution_ (large-mesh decomp minus plot mean of small-mesh decomp). Soil codes "verti"/"niti" are normalized to "Vertisol"/"Ferrisol" per labels and temperature files.
# Field-level vegetation cover (Plant variables.csv: %soil_cover -> ground_cover, %shadow -> canopy_shade_; note conventional Banabio codes C-EX1..3 normalized to C-EXC1..3), earthworm community (Earthworm's community.csv), and macroarthropod community (Macroarthropod's community.csv) are preserved at sample level in long linked via trial_id.
# Daily plot microclimate (Temperature & humidity.csv, 2020-12-15 to 2021-03-15) is passed via wth.

## ISSUES
# This is a litter-decomposition and soil-fauna study in established banana fields; crop yield, fertilizer rates, irrigation, and banana planting/harvest dates were not measured (triggers missing variables for yield, yield_part, yield_moisture, irrigated, N/P/K_fertilizer, planting_date, harvest_date).
# Coordinates are bioclimatic-zone estimates (Le Vauclin vs Saint-Joseph/Lézarde); exact plantation coordinates are withheld for grower privacy.

## NEW VARIABLES
# farming_: plantation management system ("organic" or "conventional")
# mesh_: litter-bag mesh size ("small" or "large")
# glyphosate_: TRUE when banana leaf litter was glyphosate-contaminated (G+), FALSE for G-
# litter_mass_initial_: initial ash-free dry litter mass per bag (g)
# litter_mass_final_: final ash-free dry litter mass per bag (g)
# litter_mass_loss_: ash-free dry litter mass loss over 3 months (g)
# decomposition_: proportion of initial ash-free litter mass decomposed (0-1)
# faunal_contribution_: macrofaunal contribution to decomposition (large-mesh minus plot mean small-mesh decomposition)
# sample_code_: within-field sampling unit identifier in plant and fauna tables
# canopy_shade_: overhead canopy shade cover from hemispherical photographs (%)
# fauna_group_: soil fauna group ("earthworm" or "macroarthropod")
# fauna_class_: taxonomic class (macroarthropods)
# fauna_order_: taxonomic order (macroarthropods)
# fauna_family_: taxonomic family (macroarthropods)
# fauna_species_: species or morphospecies name
# fauna_abundance_: count of individuals in the sample

## NOT PROCESSED
# Decomposition.csv: intermediate moisture/ash calculation columns (initial_mass, part_humid, mass_dry_i, ash_rate_i, part_ash_i, final_dry_mass, ash_rate_f, part_ash_f) already summarized in ash-free mass_i, mass_f, and mass_loss.
# Temperature & humidity.csv: Temp_range (= Temp_max - Temp_min).
# Decomposition_labels.csv, Plant variables_labels.csv, Decomposer's community_labels.csv, Temperature & humidity_labels.csv: codebooks/label dictionaries, not observation tables.


carob_script <- function(path) {

"
Dataset on the effect of organic farming on microbial and macrofaunal litter decomposition in banana fields

Data from a field experiment in the banana agroecosystems of Martinique (Lesser Antilles). Six organic and six conventional banana plantations located in two bioclimatic zones of the island were studied. To measure the microbial and macrofaunal contribution to decomposition, 264 bags of small- and large-mesh litter were placed and left in the field for three months. The chemical composition and decomposition of glyphosate-contaminated and glyphosate-free banana leaf litter were studied. Biotic (plant cover, abundance and composition of detritivorous macrofauna) and abiotic (microclimatic variables) characteristics were also measured.
"

	uri <- "doi:10.18167/DVN1/RUCBAH"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.apsoil.2024.105697",
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "farming_;mesh_;glyphosate_",
		response_vars = "decomposition_;litter_mass_loss_;faunal_contribution_;ground_cover",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0.6
	)

	f_dec <- ff[basename(ff) == "Decomposition.csv"]
	f_plt <- ff[basename(ff) == "Plant variables.csv"]
	f_ew  <- ff[basename(ff) == "Earthworm's community.csv"]
	f_ma  <- ff[basename(ff) == "Macroarthropod's community.csv"]
	f_wth <- ff[basename(ff) == "Temperature & humidity.csv"]

	r <- read.csv(f_dec, sep=";", na.strings=c("NA", ""), check.names=FALSE)
	names(r) <- trimws(names(r))

	soil_raw <- tolower(trimws(as.character(r$soil)))
	soil_std <- ifelse(soil_raw == "verti", "Vertisol", ifelse(soil_raw == "niti", "Ferrisol", trimws(as.character(r$soil))))
	# Bioclimatic-zone coordinates from El Jaouhari et al. (2024): Vertisol (south, Le Vauclin) vs Ferrisol (central, Saint-Joseph/Lézarde)
	lat <- ifelse(soil_std == "Vertisol", 14.545, 14.663)
	lon <- ifelse(soil_std == "Vertisol", -60.838, -60.996)

	d <- data.frame(
		trial_id = trimws(as.character(r$code_field)),
		plot_id = trimws(as.character(r$code_LB)),
		country = "Martinique",
		location = carobiner::fix_name(trimws(as.character(r$plantation)), "title"),
		latitude = lat,
		longitude = lon,
		geo_from_source = FALSE,
		geo_uncertainty = 5000,
		crop = "banana",
		date = "2020-12-15",
		farming_ = tolower(trimws(as.character(r$agriculture))),
		mesh_ = ifelse(r$mesh_size == "S", "small", ifelse(r$mesh_size == "L", "large", trimws(as.character(r$mesh_size)))),
		glyphosate_ = trimws(as.character(r$litter_glypho)) == "G+",
		soil_type = soil_std,
		litter_mass_initial_ = as.numeric(r$mass_i),
		litter_mass_final_ = as.numeric(r$mass_f),
		litter_mass_loss_ = as.numeric(r$mass_loss),
		decomposition_ = as.numeric(r$decomp),
		faunal_contribution_ = as.numeric(r$faunal_contrib),
		station_id = trimws(as.character(r$code_field)),
		on_farm = TRUE,
		is_survey = FALSE
	)
	d$treatment <- paste(d$farming_, d$mesh_, ifelse(d$glyphosate_, "glyphosate", "no glyphosate"))

	# Long 1: Plant cover & canopy shade (Plant variables.csv; normalize C-EX1..3 -> C-EXC1..3 to match Decomposition.csv)
	rp <- read.csv(f_plt, sep=";", na.strings=c("NA", ""), check.names=FALSE)
	names(rp) <- trimws(names(rp))
	pf_id <- trimws(as.character(rp$code_field))
	pf_id <- sub("^C-EX([123])$", "C-EXC\\1", pf_id)
	long_plt <- data.frame(
		trial_id = pf_id,
		sample_code_ = trimws(as.character(rp$code_sample)),
		ground_cover = as.numeric(rp[["%soil_cover"]]),
		canopy_shade_ = as.numeric(rp[["%shadow"]])
	)

	# Long 2: Earthworm community
	rew <- read.csv(f_ew, sep=";", na.strings=c("NA", ""), check.names=FALSE)
	names(rew) <- trimws(names(rew))
	long_ew <- data.frame(
		trial_id = trimws(as.character(rew$code_field)),
		sample_code_ = trimws(as.character(rew$code_sample)),
		fauna_group_ = "earthworm",
		fauna_species_ = trimws(as.character(rew$species_name)),
		fauna_abundance_ = as.integer(rew$abundance)
	)

	# Long 3: Macroarthropod community
	rma <- read.csv(f_ma, sep=";", na.strings=c("NA", ""), check.names=FALSE)
	names(rma) <- trimws(names(rma))
	long_ma <- data.frame(
		trial_id = trimws(as.character(rma$code_field)),
		sample_code_ = trimws(as.character(rma$code_sample)),
		fauna_group_ = "macroarthropod",
		fauna_class_ = trimws(as.character(rma$class)),
		fauna_order_ = trimws(as.character(rma$order)),
		fauna_family_ = trimws(as.character(rma$family)),
		fauna_species_ = trimws(as.character(rma$species)),
		fauna_abundance_ = as.integer(rma$abundance)
	)

	dlong <- carobiner::bindr(long_plt, long_ew, long_ma)

	# Daily microclimate weather (Dec 2020 - Mar 2021)
	rw <- read.csv(f_wth, sep=";", na.strings=c("NA", ""), check.names=FALSE)
	names(rw) <- trimws(names(rw))
	w_soil <- trimws(as.character(rw$soil))
	wth <- data.frame(
		station_id = trimws(as.character(rw$code_plot)),
		station_name = carobiner::fix_name(trimws(as.character(rw$plantation)), "title"),
		country = "Martinique",
		latitude = ifelse(w_soil == "Vertisol", 14.545, 14.663),
		longitude = ifelse(w_soil == "Vertisol", -60.838, -60.996),
		geo_from_source = FALSE,
		date = as.character(as.Date(rw$date, "%d/%m/%Y")),
		tmin = as.numeric(rw$Temp_min),
		tmax = as.numeric(rw$Temp_max),
		temp = as.numeric(rw$Temp_mean),
		rhmn = as.numeric(rw$hum_min),
		rhmx = as.numeric(rw$hum_max),
		rhum = as.numeric(rw$hum_mean)
	)

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
