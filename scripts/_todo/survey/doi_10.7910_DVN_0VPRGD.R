# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rice Monitoring Survey South Asia (RMS-SA), interviews 2014 (India March-June, Bangladesh July, Nepal June) about the 2013 kharif/aman season
#   ("Did your household plant rice in 2013 Kharif season?") and, for Bangladesh, the following boro season.
# Records: household x plot x crop/variety.
# India and Nepal: "Crop" files (crop, share of the plot, rice variety, planting method, production, stresses) joined with
#   "PlotList" (plot size and unit, tenure, land type, irrigation). Crop area = plot area (ha) x prop_plot_area.
#   Plot size converted to ha with the definitions in the coding manual: acre (2.471 acre = 1 ha), decimal (1/100 acre),
#   bisswa (32 bisswa = 1 acre), bigha (20 bisswa), ha.
#   Production converted to kg with the unit definitions (bags of 40-100 kg, Gon = 170.66 kg, Mani = 10.6 kg).
# Bangladesh: "Aman_rice" and "Boro_rice" files (plot x variety) with the provider's plot area (area_ha) and variety area
#   (rice_aman_area = area_ha x prop_plot_area; for boro aprop_plot_area x area_ha), district, upazila (thana) and village names.
#   Plot tenure and land type from "PlotList".

## ISSUES
# NEPAL_2014_* Crop, PlotList and VarietyList are identical to the NEPAL_2011_* files of doi:10.7910/DVN/VVGCH7 (only included here).
# No coordinates. India: district, tahsil and village are only numeric codes ("unlabeled" in the coding manual), so only adm1 (state) is set.
#   State codes in Crop/PlotList (1-4) matched to the names in the household file: 1 Bihar, 2 Odisha, 3 Uttar Pradesh, 4 West Bengal
#   (from the cross-tabulation of household State_ID names with the VID codes).
#   Nepal: district and VDC are numeric codes (District_ID 4-71, VDC_ID); location = village_name from the household file (many spelling variants).
# Plot size units "Katha (1 Katha=0.2 ha)", "Dhur (0.01ha)", "Ropani (0.3ha/1.5 Katha)" and "Other" not converted: the stated sizes
#   disagree with the unit codes (500, 48, 8) and with common values, so field_size/yield are NA for these plots (plot_size_local_, plot_unit_ kept).
#   This affects most Nepal plots (2992 of 3851 in katha).
# India prop_plot_area has values > 1 (up to 5): kept, crop area may be wrong for those records.
# planting_date: only the year 2013 for kharif/aman records; boro (2013-2014) left NA.
# Fallow records dropped. "Other (specify)" and "Fodder" (species unknown) crops -> crop NA.
# Variety lists (seed source, replenishment, traits, threshing, drying, sales/consumption), household (demography, opinions,
#   demonstrations, minikits, time preference), animal and asset files not processed.
# Bangladesh "irrigation_times" (number of irrigations; boro up to 28880) not processed.
# Remaining write_files messages: missing latitude/longitude; new variables (trailing _) reported as unknown;
#   yield out of bounds / high rice yield (up to 2.1 million kg/ha; production or plot size probably in other units than reported, or tiny prop_plot_area);
#   duplicate records (same household, plot and variety listed twice); NA yield (unconverted units, missing production);
#   NA yield_part for NA crops.

## NEW VARIABLES
# survey_month_: month and year of the household interview
# plot_size_local_: plot size in the reported (local) unit
# plot_unit_: unit of plot size as reported
# prop_plot_area_: proportion of the plot under this crop or variety
# submerged_: crop was submerged during the season
# submergence_damage_: % crop damage due to submergence
# salinity_damage_: % crop damage due to salinity
# drought_: crop suffered from drought
# drought_damage_: % crop damage due to drought
# irrigated_percent_: % of the plot area irrigated


carob_script <- function(path) {

"
Rice Monitoring Survey: South Asia

Rice Monitoring Survey: South Asia (RMS-SA) Project was IRRI implemented with support from the Bill and Melinda Gates Foundation (BMGF) to monitor rice system that captures varietal turnovers over time. The project consisted of (a) household surveys of about 9,000 rice farmers in India, Bangladesh, and Nepal, (b) rice seeds collection from 20 % of the sample households, and (c) DNA fingerprinting of the sampled rice seeds.
"

	uri <- "doi:10.7910/DVN/0VPRGD"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "IRRI",
		publication = NA,
		project = "RMS-SA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 0.6
	)

	rd <- function(name) carobiner::read.excel(ff[basename(ff) == name], guess_max=100000)

	ha <- c("Acre (2.471 Acre = 1 ha)"=1/2.471, "Decimal (100 Decimal = 1 Acre"=0.01/2.471, "Decimal (100 Decimal = 1 Acre)"=0.01/2.471,
		"Bisswa (32 Bisswa=1 acre)"=1/(32*2.471), "Bigha (20 Bisswa = 1 Bigha)"=20/(32*2.471), "ha (1 ha=4 Bigha, 80 Bisswa)"=1, "ha"=1,
		"Bigha (1 Bigha = 33 decimal)"=0.33/2.471)
	kg <- c("kgs"=1, "40 kg bag"=40, "40 kg bag (1 mann)"=40, "40 kg bag (Mann)"=40, "50 kg bag"=50, "60 kg bag"=60, "60 kg bag (1 Muri)"=60,
		"75 kg bag"=75, "80 kg bag"=80, "100 kg bag"=100, "Gon (170.66 Kg)"=170.66, "Mani (10.6kg)"=10.6)
	crops <- c("Rice"="rice", "Maize"="maize", "Sugarcane"="sugarcane", "Peagion Pea"="pigeon pea", "Peagion pea"="pigeon pea")
	pmeth <- c("Transplanting"="transplanting", "Broadcasting"="direct seeding", "Direct (line) seeding"="direct seeding")
	tenure <- function(x) {
		x <- tolower(x)
		x[x %in% c("other", "other (specify)")] <- "other"
		x
	}
	irsrc <- function(x) {
		x <- tolower(x)
		x[grepl("no irrigation", x)] <- "none"
		x <- gsub("deisel", "diesel", gsub("sallow", "shallow", x))
		x
	}
	yn <- function(x) c(Yes=TRUE, No=FALSE)[x]
	variety <- function(v, nc) {
		i <- v %in% c("Others", "Other (specify)") & !is.na(nc)
		v[i] <- nc[i]
		v[v %in% c("Unknown/ don't know", "Dont know")] <- NA
		v
	}

	## India and Nepal: crop x plot
	plots <- function(fcrop, fplot, fhh, keys, country) {
		r <- rd(fcrop)
		p <- rd(fplot)
		h <- rd(fhh)
		r <- r[!(r$crop_which %in% "Fallow"), ]
		states <- c("Bihar", "Odisha", "Uttar Pradesh", "West Bengal")
		if (country == "India") h$State_ID <- match(h$State_ID, states)
		i <- match(do.call(paste, r[keys]), do.call(paste, p[keys]))
		j <- match(do.call(paste, r[keys[-length(keys)]]), do.call(paste, h[keys[-length(keys)]]))
		size <- p$plot_size[i] * ha[p$unit_plot[i]]
		area <- size * r$prop_plot_area
		data.frame(
			country = country,
			hhid = do.call(paste, c(country, r[keys[-length(keys)]], sep="-")),
			field_id = as.character(r$Plot_ID),
			survey_month_ = substr(h$starting_time[j], 4, 10),
			season = "kharif",
			planting_date = "2013",
			crop = unname(crops[r$crop_which]),
			variety = variety(r$variety_name_rice, r$not_coded),
			planting_method = unname(pmeth[r$method_planting]),
			prop_plot_area_ = r$prop_plot_area,
			field_size = area,
			plot_size_local_ = p$plot_size[i],
			plot_unit_ = p$unit_plot[i],
			yield = r$proct_total * kg[r$unit_prod] / area,
			submerged_ = yn(r$was_submerged),
			submergence_damage_ = r$prop_damage,
			salinity_damage_ = ifelse(r$salinity_dam %in% "No", 0, r$salinity_damage),
			drought_ = yn(r$drogh_kha),
			drought_damage_ = r$prot_dam,
			land_tenure = tenure(p$tenure[i]),
			landscape_position = tolower(p$land_type[i]),
			irrigation_source = irsrc(p$irrigation_source[i]),
			irrigated_percent_ = p$irrigation_percent[i],
			adm1 = if (country == "India") states[r$State_ID] else NA,
			location = if (country == "Nepal") h$village_name[j] else NA
		)
	}
	d1 <- plots("INDIA_2014_Crop_IN.xls", "INDIA_2014_PlotList_IN.xls", "INDIA_2014_Household_IN.xls",
		c("State_ID", "District_ID", "Tahsil_ID", "Village_ID", "Household_number", "Plot_ID"), "India")
	d2 <- plots("NEPAL_2014_Crop_NP.xls", "NEPAL_2014_PlotList_NP.xls", "NEPAL_2014_Household_NP.xls",
		c("Region_ID", "District_ID", "VDC_ID", "Household_ID", "Plot_ID"), "Nepal")

	## Bangladesh: aman and boro rice, plot x variety
	keys <- c("Division_ID", "District_ID", "Thana_ID", "Village_ID", "Household_ID")
	p <- rd("BANGLADESH_2014_PlotList_BD.xls")
	bd <- function(r, season, year) {
		i <- match(do.call(paste, r[c(keys, "Plot_ID")]), do.call(paste, p[c(keys, "Plot_ID")]))
		area <- r$area_ha * r$prop_plot_area
		data.frame(
			country = "Bangladesh",
			adm1 = gsub(" Division$", "", r$division_name),
			adm2 = r$district_name,
			adm3 = r$thana_name,
			location = r$village,
			hhid = do.call(paste, c("Bangladesh", r[keys], sep="-")),
			field_id = as.character(r$Plot_ID),
			season = season,
			planting_date = year,
			crop = "rice",
			variety = variety(r$variety_name_rice, r$not_coded),
			planting_method = unname(pmeth[r$method_planting]),
			prop_plot_area_ = r$prop_plot_area,
			field_size = area,
			yield = r$proct_total * kg[r$unit_prod] / area,
			submerged_ = yn(r$was_submerged),
			submergence_damage_ = r$prop_damage,
			salinity_damage_ = ifelse(r$salinity_dam %in% "No", 0, r$salinity_damage),
			drought_ = yn(r$drogh_kha),
			drought_damage_ = r$prot_dam,
			land_tenure = tenure(p$tenure[i]),
			landscape_position = tolower(p$land_type[i]),
			irrigation_source = irsrc(r$irrigation_source)
		)
	}
	d3 <- bd(rd("BANGLADESH_2014_Aman_rice_BD.xls"), "aman", "2013")
	r <- rd("BANGLADESH_2014_Boro_rice_BD.xls")
	i <- !(names(r) %in% c("area_ha", "rice_boro_area"))
	names(r)[i] <- sub("^ab?", "", names(r)[i])
	d4 <- bd(r, "boro", NA)
	d <- carobiner::bindr(d1, d2, d3, d4)
	d$field_size[d$field_size %in% 0] <- NA
	d$yield[is.na(d$field_size)] <- NA
	d$irrigated <- ifelse(is.na(d$irrigation_source), NA, d$irrigation_source != "none")
	d$irrigation_source[d$irrigation_source %in% "none"] <- NA
	d$yield_part <- unname(c(rice="grain", maize="grain", "pigeon pea"="grain", sugarcane="stems")[d$crop])
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
