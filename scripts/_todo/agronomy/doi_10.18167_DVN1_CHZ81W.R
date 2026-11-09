# R script for "carob"
# license: GPL (>=3)

## NOTES
# Crop harvests (carrot, lettuce, tomato) joined to fertilizer amounts and management dates.
# Carrot and lettuce weights are kg fresh per 8 m2 bed. Tomato weights are kg fresh per plot; the fertilization readme states the cropped beds total 48 m2.
# NPK 10-10-20 is treated as percent N, P2O5 and K2O. Urea is 46% N and potassium sulfate is 41.5% K (terminag SOP).
# Organic waste is t raw matter/ha in OM_amount (kg/ha). Its N and P stay in the product, not in N_fertilizer.
# Tomato yield is one weekly harvest (H1-H5), not the sum of the five harvests.
# Publication DOI is the Dataverse publicationIDNumber 10.1016/j.dib.2026.112456.

## ISSUES
# No coordinates. Irrigation is not stated. Sewage sludge and poultry litter are stored as sludge and poultry manure. digestate is not in the OM list.
# Some beds have no harvest weight. Lettuce and tomato cycles are shorter than 45 days.
# K from 10-10-20 plus potassium sulfate reaches 405 kg/ha. Carrot moisture reaches 98.5%.
# write_files: missing geo_from_source, latitude, longitude, irrigated; out of bounds yield_moisture (36, 98.532), K_fertilizer (0, 405.04); invalid terms OM_type digestate; datespan 1134 records with harvest_date within 45 days of planting_date; NA detected yield.

## NEW VARIABLES
# None beyond terminag names. Treatment acronyms are kept in treatment (SS sewage sludge, PL poultry litter, D digestate, SF synthetic; 100 or 200 is the dose relative to crop P need).

## NOT PROCESSED
# 1_DATA_SOIL: annual soil chemistry at several depths (not one value per crop row)
# 2_DATA_FERTILIZATION data_CHEM_PROPERTIES: batch nutrient contents of the waste products
# Carrot and lettuce dry-weight samples, except carrot and lettuce root/leaf moisture joined at plot level
# Tomato dry-weight samples (several harvests per plot)
# Pest and disease presence scores on sample plants
# 1_DATA_SOIL_SOERE-PRO_SENEGAL.xlsx: not read. sheet README (of 2): Column names, Description, Values / unit. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"
Replication Data for: A dataset of soil properties and crop observations from a long-term organic fertilization trial in Sub-Sahelian market gardening

Recycling the growing stock of organic waste products (OWP) from cities, factories, and farms is a key challenge for sustainable agriculture. However, it must be done with awareness of potential long-term environmental and health risks. The SOERE PRO observatory was created in this context, including the trial in Sangalkam, Dakar region (Senegal), where these data were collected. Since 2016, four fertiliser types - one mineral (synthetic) and three organic - were applied on three crops a year (tomato, lettuce, carrot). A multi-variable dataset was consistently documented, including soil physico-chemical parameters measured at three depth once a year, OWP characterization, crop parameters, and management activities. All soil, OWP and vegetables samples are stored in a sample bank in Dakar, also described in the dataset, and available for additional analyses. The dataset has been shared in a Dataverse repository for free (re)use in meta-analyses, process modelling and other environmental studies or new projects (the trial is still running), in particular for understanding tropical ecosystem dynamics (nutrients, contaminants, biodiversity), and guiding sustainable land management practices.
"

	uri <- "doi:10.18167/DVN1/CHZ81W"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.dib.2026.112456",
		project = "SOERE PRO",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 55,
		carob_effort = 1.2
	)

	num <- function(x) {
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x <- gsub(",", ".", x, fixed=TRUE)
		x[x %in% c("", "NA", "na", "<QL")] <- NA
		as.numeric(x)
	}
	as_iso <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) {
			return(as.character(as.Date(x)))
		}
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "na", "no_thinning")] <- NA
		as.character(as.Date(as.numeric(x), origin="1899-12-30"))
	}

	f1 <- ff[basename(ff) == "2_DATA_FERTILIZATION_SOERE-PRO_SENEGAL.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="data_AMOUNT")
	f2 <- ff[basename(ff) == "3_DATA_MANAGEMENT_SOERE-PRO_SENEGAL.xlsx"]
	r2 <- carobiner::read.excel(f2, sheet="data_MANAGEMENT")

	# File names Sewage_sludge and Poultry_litter. digestate is not in the OM vocabulary.
	om_name <- c(
		Sewage_sludge = "sludge",
		Poultry_litter = "poultry manure",
		Digestate = "digestate",
		Synthetic_fertilizer = "none"
	)
	r1$OM_type <- om_name[as.character(r1$Fertilizer_type)]
	r1$OM_amount <- ifelse(r1$OM_type == "none", 0, num(r1$Amount_applied_OWP) * 1000)
	r1$OM_used <- r1$OM_amount > 0
	r1$OM_type[r1$OM_type == "none" | !r1$OM_used] <- "none"
	npk <- num(r1$Amount_applied_chem_10_10_20)
	urea <- num(r1$Amount_applied_chem_urea)
	sop <- num(r1$Amount_applied_chem_K2SO4)
	r1$N_fertilizer <- npk * 0.10 + urea * 0.46
	r1$P_fertilizer <- npk * 0.10 / 2.29
	r1$K_fertilizer <- npk * 0.20 / 1.2051 + sop * 0.415
	ft <- rep("none", nrow(r1))
	ft[npk > 0] <- "NPK"
	ft[urea > 0 & ft == "none"] <- "urea"
	ft[urea > 0 & ft != "none"] <- paste(ft[urea > 0 & ft != "none"], "urea", sep="; ")
	ft[sop > 0 & ft == "none"] <- "SOP"
	ft[sop > 0 & ft != "none"] <- paste(ft[sop > 0 & ft != "none"], "SOP", sep="; ")
	r1$fertilizer_type <- ft

	r2$planting_date <- ifelse(r2$Crop == "Carrot",
		as_iso(r2$Date_sowing),
		as_iso(r2$Date_transplanting_or_thinning))
	r2$harvest_date <- as_iso(r2$Harvest_date)
	# Carrot is sown in the plot. Lettuce and tomato planting_date is the transplant date (sown in a nursery).
	# Cultivars from the management readme: tomato Mongal, lettuce Eden, carrot Pamela +.
	r2$variety <- c(Tomato="Mongal", Lettuce="Eden", Carrot="Pamela +")[as.character(r2$Crop)]

	f3 <- ff[basename(ff) == "4_DATA_CROP_CARROT_SOERE-PRO_SENEGAL.xlsx"]
	r3 <- carobiner::read.excel(f3, sheet="data_CARROT_harvest")
	d_car <- data.frame(
		Year = r3$Year,
		Cycle = r3$Cycle,
		Crop = "Carrot",
		Treatment = r3$Treatment,
		Plot = r3$Plot,
		bed = as.character(r3$Cropping_bed),
		crop = "carrot",
		yield = num(r3$Tot_weight_carrots) / 8 * 10000,
		yield_part = "roots",
		yield_marketable = num(r3$Weight_diam_sup_2cm) / 8 * 10000,
		fwy_residue = num(r3$Total_weight_leaves) / 8 * 10000
	)
	r4 <- carobiner::read.excel(f3, sheet="data_CARROT_dry_weight")
	mc <- data.frame(
		Year = r4$Year, Cycle = r4$Cycle, Plot = r4$Plot,
		yield_moisture = num(r4$Perc_hum_carrot_root)
	)
	d_car <- merge(d_car, mc, by=c("Year", "Cycle", "Plot"), all.x=TRUE)

	f4 <- ff[basename(ff) == "5_DATA_CROP_LETTUCE_SOERE-PRO_SENEGAL.xlsx"]
	rl <- carobiner::read.excel(f4, sheet="data_LETTUCE_harvest ")
	if (nrow(rl) == 0) rl <- carobiner::read.excel(f4, sheet="data_LETTUCE_harvest")
	d_let <- data.frame(
		Year = rl$Year,
		Cycle = rl$Cycle,
		Crop = "Lettuce",
		Treatment = rl$Treatment,
		Plot = rl$Plot,
		bed = as.character(rl$Cropping_bed),
		crop = "lettuce",
		yield = num(rl$Tot_weight_lettuce) / 8 * 10000,
		yield_part = "leaves",
		yield_marketable = num(rl$Weight_headed) / 8 * 10000,
		fwy_residue = NA_real_
	)
	r5 <- carobiner::read.excel(f4, sheet="data_LETTUCE_dry_weight")
	ml <- data.frame(
		Year = r5$Year, Cycle = r5$Cycle, Plot = r5$Plot,
		yield_moisture = num(r5$Perc_hum_lettuce_leaves)
	)
	d_let <- merge(d_let, ml, by=c("Year", "Cycle", "Plot"), all.x=TRUE)

	f5 <- ff[basename(ff) == "6_DATA_CROP_TOMATO_SOERE-PRO_SENEGAL.xlsx"]
	r6 <- carobiner::read.excel(f5, sheet="data_TOMATO_harvest")
	tom_kg <- num(r6$Cat1_tot_weight) + num(r6$Cat2_weight) + num(r6$Cat3_weight) + num(r6$Cat4_weight) + num(r6$Cat5_weight)
	d_tom <- data.frame(
		Year = r6$Year,
		Cycle = r6$Cycle,
		Crop = "Tomato",
		Treatment = r6$Treatment,
		Plot = r6$Plot,
		bed = as.character(r6$Harvest_nb),
		crop = "tomato",
		yield = tom_kg / 48 * 10000,
		yield_part = "fruit",
		yield_marketable = num(r6$Cat1_tot_weight) / 48 * 10000,
		fwy_residue = NA_real_,
		yield_moisture = NA_real_,
		plant_density = num(r6$Nb_plants) / 48 * 10000,
		harvest_date_obs = as_iso(r6$Date_harvest)
	)

	d <- rbind(
		data.frame(d_car, plant_density=NA_real_, harvest_date_obs=NA_character_),
		data.frame(d_let, plant_density=NA_real_, harvest_date_obs=NA_character_),
		d_tom
	)

	fert <- r1[, c("Year", "Cycle", "Crop", "Treatment", "OM_type", "OM_amount", "OM_used", "N_fertilizer", "P_fertilizer", "K_fertilizer", "fertilizer_type")]
	d <- merge(d, fert, by=c("Year", "Cycle", "Crop", "Treatment"), all.x=TRUE)
	mgmt <- r2[, c("Year", "Cycle", "Crop", "planting_date", "harvest_date", "variety")]
	d <- merge(d, mgmt, by=c("Year", "Cycle", "Crop"), all.x=TRUE)
	d$harvest_date[!is.na(d$harvest_date_obs)] <- d$harvest_date_obs[!is.na(d$harvest_date_obs)]
	d$harvest_date_obs <- NULL

	d$treatment <- d$Treatment
	d$plot_id <- paste(d$Plot, d$bed, sep="_")
	d$rep <- as.integer(sub(".*_", "", d$Plot))
	d$trial_id <- paste("Sangalkam", d$Cycle, sep="_")
	d$country <- "Senegal"
	d$location <- "Sangalkam"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	# country and location from geographicCoverage (Senegal, Sangalkam).

	d$Year <- NULL
	d$Cycle <- NULL
	d$Crop <- NULL
	d$Treatment <- NULL
	d$Plot <- NULL
	d$bed <- NULL

	carobiner::write_files(path, meta, d)
}
