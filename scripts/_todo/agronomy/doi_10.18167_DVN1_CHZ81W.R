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

	f_amt <- ff[basename(ff) == "2_DATA_FERTILIZATION_SOERE-PRO_SENEGAL.xlsx"]
	amt <- carobiner::read.excel(f_amt, sheet="data_AMOUNT")
	f_man <- ff[basename(ff) == "3_DATA_MANAGEMENT_SOERE-PRO_SENEGAL.xlsx"]
	man <- carobiner::read.excel(f_man, sheet="data_MANAGEMENT")

	# File names Sewage_sludge and Poultry_litter. digestate is not in the OM vocabulary.
	om_name <- c(
		Sewage_sludge = "sludge",
		Poultry_litter = "poultry manure",
		Digestate = "digestate",
		Synthetic_fertilizer = "none"
	)
	amt$OM_type <- om_name[as.character(amt$Fertilizer_type)]
	amt$OM_amount <- ifelse(amt$OM_type == "none", 0, num(amt$Amount_applied_OWP) * 1000)
	amt$OM_used <- amt$OM_amount > 0
	amt$OM_type[amt$OM_type == "none" | !amt$OM_used] <- "none"
	npk <- num(amt$Amount_applied_chem_10_10_20)
	urea <- num(amt$Amount_applied_chem_urea)
	sop <- num(amt$Amount_applied_chem_K2SO4)
	amt$N_fertilizer <- npk * 0.10 + urea * 0.46
	amt$P_fertilizer <- npk * 0.10 / 2.29
	amt$K_fertilizer <- npk * 0.20 / 1.2051 + sop * 0.415
	ft <- rep("none", nrow(amt))
	ft[npk > 0] <- "NPK"
	ft[urea > 0 & ft == "none"] <- "urea"
	ft[urea > 0 & ft != "none"] <- paste(ft[urea > 0 & ft != "none"], "urea", sep="; ")
	ft[sop > 0 & ft == "none"] <- "SOP"
	ft[sop > 0 & ft != "none"] <- paste(ft[sop > 0 & ft != "none"], "SOP", sep="; ")
	amt$fertilizer_type <- ft

	man$planting_date <- ifelse(man$Crop == "Carrot",
		as_iso(man$Date_sowing),
		as_iso(man$Date_transplanting_or_thinning))
	man$harvest_date <- as_iso(man$Harvest_date)
	# Carrot is sown in the plot. Lettuce and tomato planting_date is the transplant date (sown in a nursery).
	# Cultivars from the management readme: tomato Mongal, lettuce Eden, carrot Pamela +.
	man$variety <- c(Tomato="Mongal", Lettuce="Eden", Carrot="Pamela +")[as.character(man$Crop)]

	f_car <- ff[basename(ff) == "4_DATA_CROP_CARROT_SOERE-PRO_SENEGAL.xlsx"]
	rc <- carobiner::read.excel(f_car, sheet="data_CARROT_harvest")
	d_car <- data.frame(
		Year = rc$Year,
		Cycle = rc$Cycle,
		Crop = "Carrot",
		Treatment = rc$Treatment,
		Plot = rc$Plot,
		bed = as.character(rc$Cropping_bed),
		crop = "carrot",
		yield = num(rc$Tot_weight_carrots) / 8 * 10000,
		yield_part = "roots",
		yield_marketable = num(rc$Weight_diam_sup_2cm) / 8 * 10000,
		fwy_residue = num(rc$Total_weight_leaves) / 8 * 10000
	)
	rdc <- carobiner::read.excel(f_car, sheet="data_CARROT_dry_weight")
	mc <- data.frame(
		Year = rdc$Year, Cycle = rdc$Cycle, Plot = rdc$Plot,
		yield_moisture = num(rdc$Perc_hum_carrot_root)
	)
	d_car <- merge(d_car, mc, by=c("Year", "Cycle", "Plot"), all.x=TRUE)

	f_let <- ff[basename(ff) == "5_DATA_CROP_LETTUCE_SOERE-PRO_SENEGAL.xlsx"]
	rl <- carobiner::read.excel(f_let, sheet="data_LETTUCE_harvest ")
	if (nrow(rl) == 0) rl <- carobiner::read.excel(f_let, sheet="data_LETTUCE_harvest")
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
	rdl <- carobiner::read.excel(f_let, sheet="data_LETTUCE_dry_weight")
	ml <- data.frame(
		Year = rdl$Year, Cycle = rdl$Cycle, Plot = rdl$Plot,
		yield_moisture = num(rdl$Perc_hum_lettuce_leaves)
	)
	d_let <- merge(d_let, ml, by=c("Year", "Cycle", "Plot"), all.x=TRUE)

	f_tom <- ff[basename(ff) == "6_DATA_CROP_TOMATO_SOERE-PRO_SENEGAL.xlsx"]
	rt <- carobiner::read.excel(f_tom, sheet="data_TOMATO_harvest")
	tom_kg <- num(rt$Cat1_tot_weight) + num(rt$Cat2_weight) + num(rt$Cat3_weight) + num(rt$Cat4_weight) + num(rt$Cat5_weight)
	d_tom <- data.frame(
		Year = rt$Year,
		Cycle = rt$Cycle,
		Crop = "Tomato",
		Treatment = rt$Treatment,
		Plot = rt$Plot,
		bed = as.character(rt$Harvest_nb),
		crop = "tomato",
		yield = tom_kg / 48 * 10000,
		yield_part = "fruit",
		yield_marketable = num(rt$Cat1_tot_weight) / 48 * 10000,
		fwy_residue = NA_real_,
		yield_moisture = NA_real_,
		plant_density = num(rt$Nb_plants) / 48 * 10000,
		harvest_date_obs = as_iso(rt$Date_harvest)
	)

	d <- rbind(
		data.frame(d_car, plant_density=NA_real_, harvest_date_obs=NA_character_),
		data.frame(d_let, plant_density=NA_real_, harvest_date_obs=NA_character_),
		d_tom
	)

	fert <- amt[, c("Year", "Cycle", "Crop", "Treatment", "OM_type", "OM_amount", "OM_used", "N_fertilizer", "P_fertilizer", "K_fertilizer", "fertilizer_type")]
	d <- merge(d, fert, by=c("Year", "Cycle", "Crop", "Treatment"), all.x=TRUE)
	mgmt <- man[, c("Year", "Cycle", "Crop", "planting_date", "harvest_date", "variety")]
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
