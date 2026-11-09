# R script for "carob"
# license: GPL (>=3)

## NOTES
# Summer and winter are two trials at one site. Plot codes (L#_P# or L#_T#) carry cc_mixture and nb_cc_species.
# There is no _Cover_crop observation unit. Fresh mass on a species-code unit (kg/m2 x 10000) is that species' yield, summed for the plot. Mass on _weed units and on bare L#_T# units is weed biomass. Dry mass is fresh times the sample dry-matter percent. weed_biomass is the dry mass.
# Seedlot codes: GUIAB Guizotia abyssinica, PHSAU Vigna radiata, CNAEN Canavalia ensiformis, PESGL Pennisetum glaucum, AVESG Avena strigosa, BRSCA Brassica carinata, VICVI Vicia villosa.
# Fertilizer_N_amount_kg_ha on the Trial row is applied to every plot of that season. P and K are not in the itk file.
# Meteo.csv is semicolon-separated. Position is (longitude, latitude, elevation). RAD is not labeled; values near 2600 are stored as srad in kJ m-2. Dates are d/m/yyyy (the summer sowing date in the itk file is 2019-11-12).
# Country is geographicCoverage Réunion. City in that record is Saint-Pierre. Coordinates are from Meteo.csv.
# Irrigation_frequency_days is 3, so the trials were irrigated. The count of irrigations is not given.

## ISSUES
# Unsown plots have weed biomass and no cover-crop yield. P and K are not in the itk file. Soil chemistry and seed-lot sowing density are not joined.
# Guizotia is stored as "niger " (vocabulary token). Avena strigosa is not in the crop list. Pearl millet fresh mass is high for a grain-yield limit.
# write_files: missing K_fertilizer, P_fertilizer; unknown variables species_number_; untrimmed characters crop; invalid terms crop Avena strigosa; invalid terms intercrops Avena strigosa; high crop yield pearl millet 51625; NA detected yield; out of bounds plant_height (1, 241.5).

## NEW VARIABLES
# species_number_: number of cover-crop species in the mixture (0 to 4)

## NOT PROCESSED
# Soil observations (pH, Corg, Norg, exchangeable cations, CEC, P)
# Specific leaf area, C:N, leaf and stem dry-matter fractions
# Seed_sowing_density_kgha (varies by seed lot)
# NEARPLOT.csv (spatial neighbours)
# Readme.xlsx: documentation or a codebook, not an observation table.
# Summer_design.csv: design, layout, or protocol sheet, not plot or survey records.
# Summer_seedlot.csv: not read. columns: unit_code;lot_code;Description. Observation records are missing from d until this file is standardized.
# Winter_design.csv: design, layout, or protocol sheet, not plot or survey records.
# Winter_seedlot.csv: not read. columns: unit_code;lot_code;Description. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"
Experimental data set of cover crop mixture and weed control in tropical summer and winter in Reunion Island.

Weed pressure is a main biotic constraint in tropical agriculture. Cover crop mixtures have increased in popularity to limit weed growth through competition for shared resources but the relation between cover crop diversity and weed suppression is still under debate. This study aimed to assess the impact of increasing cover crops diversity (one to four species) on weed control during two 4-month growing seasons (tropical summer and winter) in Reunion Island. Weed control was expressed in terms of ground cover by weeds and weed aboveground dry mass and linked to cover crop traits in the mixtures.
"

	uri <- "doi:10.18167/DVN1/WPGRAM"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;species_number_",
		response_vars = "yield;weed_biomass;ground_cover",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 45,
		carob_effort = 1
	)

	code_crop <- c(
		GUIAB="niger ", PHSAU="mung bean", CNAEN="jack bean", PESGL="pearl millet",
		AVESG="Avena strigosa", BRSCA="carinata", VICVI="vetch"
	)
	# "niger " keeps the trailing space used in the crop vocabulary.
	# AVESG is Avena strigosa (black oat), not Avena sativa. BRSCA is Brassica carinata.
	mix_parts <- function(level) {
		level <- as.character(level)
		crop <- rep("none", length(level))
		inter <- rep("none", length(level))
		ok <- !is.na(level) & nzchar(level) & substr(level, 1, 1) != "0"
		bits <- strsplit(sub("^[0-9]_", "", level[ok]), "_")
		one <- function(z) {
			if ("GUIAB" %in% z) {
				rest <- z[z != "GUIAB"]
				list(crop="niger ", inter=rest)
			} else {
				list(crop=z[1], inter=z[-1])
			}
		}
		parsed <- lapply(bits, one)
		crop[ok] <- vapply(parsed, function(p) {
			if (p$crop == "niger ") return(p$crop)
			unname(code_crop[p$crop])
		}, character(1))
		inter[ok] <- vapply(parsed, function(p) {
			if (length(p$inter) == 0) return("none")
			paste(unname(code_crop[p$inter]), collapse="; ")
		}, character(1))
		data.frame(crop=crop, intercrops=inter)
	}

	plot_of <- function(unit) sub("_(weed|GUIAB|PHSAU|CNAEN|PESGL|AVESG|BRSCA|VICVI)$", "", unit)
	sum_known <- function(z) if (all(is.na(z))) NA_real_ else sum(z, na.rm=TRUE)

	build <- function(season, treat_bn, obs_bn, itk_bn) {
		f1 <- ff[basename(ff) == treat_bn]
		r1 <- read.csv(f1, sep=";", check.names=FALSE)
		f2 <- ff[basename(ff) == obs_bn]
		r2 <- read.csv(f2, sep=";", check.names=FALSE)
		f3 <- ff[basename(ff) == itk_bn]
		r3 <- carobiner::read.excel(f3)
		n_rate <- as.numeric(r3$Value[r3$Variable == "Fertilizer_N_amount_kg_ha" & r3$unit_code == "Trial"][1])
		sow <- as.character(r3$date[r3$Variable == "Seed_sowing_method" & r3$unit_code == "Trial"][1])
		har <- as.character(r3$date[r3$Variable == "Harvest_method" & r3$unit_code == "Trial"][1])

		# Fresh mass is on species units (GUIAB, PHSAU, CNAEN, PESGL) and on _weed units. There is no _Cover_crop unit.
		mass <- r2[r2$Variable == "Aboveground_fresh_mass_kg_m2", ]
		dm <- r2[r2$Variable == "Aboveground_sample_dry_matter_content_%", ]
		u <- data.frame(
			unit = mass$UNIT_CODE,
			plot = plot_of(mass$UNIT_CODE),
			kind = ifelse(grepl("_(GUIAB|PHSAU|CNAEN|PESGL|AVESG|BRSCA|VICVI)$", mass$UNIT_CODE), "cover", "weed"),
			fresh = as.numeric(mass$Value) * 10000
		)
		u <- merge(u, data.frame(unit=dm$UNIT_CODE, dm=as.numeric(dm$Value)), by="unit", all.x=TRUE)
		u$dry <- u$fresh * u$dm / 100
		cov <- u[u$kind == "cover", ]
		wed <- u[u$kind == "weed", ]
		cover <- merge(aggregate(fresh ~ plot, cov, sum_known), aggregate(dry ~ plot, cov, sum_known), by="plot")
		cover$yield <- cover$fresh
		cover$yield_moisture <- (1 - cover$dry / cover$fresh) * 100
		cover <- cover[, c("plot", "yield", "yield_moisture")]
		weed <- aggregate(dry ~ plot, wed, sum_known)
		names(weed)[2] <- "weed_biomass"

		d <- data.frame(plot_id=unique(u$plot), trial_id=season)
		d <- merge(d, cover, by.x="plot_id", by.y="plot", all.x=TRUE)
		d <- merge(d, weed, by.x="plot_id", by.y="plot", all.x=TRUE)

		mix <- r1[r1$factor == "cc_mixture", c("unit_code", "factor_level")]
		nsp <- r1[r1$factor == "nb_cc_species", c("unit_code", "factor_level")]
		names(mix) <- c("plot_id", "mixture")
		names(nsp) <- c("plot_id", "nsp")
		d <- merge(d, mix, by="plot_id", all.x=TRUE)
		d <- merge(d, nsp, by="plot_id", all.x=TRUE)
		parts <- mix_parts(ifelse(is.na(d$mixture), d$nsp, d$mixture))
		d$crop <- parts$crop
		d$intercrops <- parts$intercrops
		d$species_number_ <- as.integer(sub("sp", "", d$nsp))
		d$treatment <- ifelse(is.na(d$mixture), d$nsp, d$mixture)
		d$mixture <- NULL
		d$nsp <- NULL
		d$N_fertilizer <- n_rate
		d$planting_date <- sow
		d$harvest_date <- har
		d$block_id <- sub("_.*", "", d$plot_id)

		ht <- r2[r2$Variable %in% c("Plant_height_cm", "Plant_coverage_%"), ]
		long <- data.frame(
			trial_id = season,
			plot_id = plot_of(ht$UNIT_CODE),
			date = as.character(as.Date(ht$OBS_DATE, "%d/%m/%Y")),
			plant_height = ifelse(ht$Variable == "Plant_height_cm", as.numeric(ht$Value), NA_real_),
			ground_cover = ifelse(ht$Variable == "Plant_coverage_%" & !grepl("_weed$", ht$UNIT_CODE), as.numeric(ht$Value), NA_real_),
			weed_cover = ifelse(grepl("_weed$", ht$UNIT_CODE) & ht$Variable == "Plant_coverage_%", as.numeric(ht$Value), NA_real_)
		)
		long <- long[long$plot_id %in% d$plot_id, ]
		list(d=d, long=long)
	}

	s1 <- build("summer", "Summer_treatment.csv", "Summer_observation.csv", "Summer_itk.xlsx")
	s2 <- build("winter", "Winter_treatment.csv", "Winter_observation.csv", "Winter_itk.xlsx")
	d <- rbind(s1$d, s2$d)
	long <- rbind(s1$long, s2$long)

	d$country <- "Réunion"
	d$location <- "Saint-Pierre"
	d$yield_part <- "aboveground biomass"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$land_prep_method <- "rotovating"
	# itk lists rototiller and rotovator. Both are rotary tillage.

	f4 <- ff[basename(ff) == "Meteo.csv"]
	r4 <- read.csv(f4, sep=";", check.names=FALSE)
	pos <- gsub("[()]", "", r4$Position)
	sp <- strsplit(pos, ",")
	wth <- data.frame(
		station_name = "Saint-Pierre",
		date = as.character(as.Date(r4$Date, "%d/%m/%Y")),
		prec = as.numeric(r4$PPT),
		srad = as.numeric(r4$RAD),
		tmin = as.numeric(r4$Tmin),
		temp = as.numeric(r4$Tmean),
		tmax = as.numeric(r4$Tmax),
		ETP = as.numeric(r4$ETP),
		longitude = as.numeric(vapply(sp, `[`, character(1), 1)),
		latitude = as.numeric(vapply(sp, `[`, character(1), 2)),
		elevation = as.numeric(vapply(sp, `[`, character(1), 3))
	)
	d$longitude <- wth$longitude[1]
	d$latitude <- wth$latitude[1]
	d$elevation <- wth$elevation[1]
	d$geo_from_source <- TRUE

	carobiner::write_files(path, meta, d, long=long, wth=wth)
}
