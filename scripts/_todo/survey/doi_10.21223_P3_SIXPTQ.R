# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline household survey in Ethiopia, August to December 2010. Country is the Dataverse geographic coverage.
# Big-plot latitude and longitude are stored as hours, minutes, and seconds (BPLG/BPLS/BPLM and BPLOG/BPLOS/BPLOM) and were converted to decimal degrees. Ethiopia is north and east of the origin, and the hour values in the file are positive.
# Potato plot area is labelled m2. It is not a yield. Variety used_1 is the variety column. Inorganic fertilizer is a yes/no question, not a rate.

## ISSUES
# Region, zone, and woreda are codes. They are stored as recorded and are not expanded to names.
# Sexagesimal values were kept only when latitude hours were 3-15, longitude hours were 30-48, and minutes and seconds were below 60. Other GPS cells were left missing.
# No kg/ha yield and no fertilizer rate.
# Some converted coordinates fall in Eritrea rather than Ethiopia. They were left as recorded.
# write_files: missing carob_contributor. unknown variables: region_code_, zone_code_, woreda_code_, kebele_code_, crop_type_, farm_type_, the area columns, variety_2_, variety_3_, fertilizer_inorganic_. coordinates in wrong country (Ethiopia/Eritrea). NA detected: latitude, longitude.

## NOT PROCESSED
# MODULE B household composition, MODULE C resources, MODULE D crop activity, MODULE E technical assistance, MODULE F control methods, MODULE G sweetpotato management, MODULE H vitamin A foods, MODULE I diet diversity: .dta and .sav copies, and the dictionaries and PDFs. Module F is used only for potato area, varieties, and the inorganic-fertilizer question.

## NEW VARIABLES
# region_code_, zone_code_, woreda_code_, kebele_code_, crop_type_, farm_type_.
# potato_area_belg_m2_, potato_area_meher_m2_.
# variety_2_, variety_3_.
# fertilizer_inorganic_: answer as recorded.


carob_script <- function(path) {

"
Dataset for: Tackling Food Insecurity and Malnutrition through Diversification: Exploiting the Potential of Potato and Sweetpotato to Reduce Food Insecurity and Dependence on Cereals in SNNPR and Tigray. Better Potato for a Better Life (BPBL) Baseline Survey

The baseline survey is designed with an aim of determining the level of community involvement in potato and sweetpotato production, consumption and marketing and as a means of improving livelihoods of the communities in SNNPR and Tigray regions. It looks at the general household demographic profile, livelihood, income, production and marketing realities and constraints affecting the potato and sweetpotato yields in the two regions prior to CIPUSAID project intervention. Food insecurity is the major problem of smallholder farmers in many areas of rural Ethiopia. The data is expected to set benchmark values to serve as initial measure to assess the outcomes or impact of the 'Better Potato for a Better Life' project interventions and will be used as a reference by the project and the donor, USAID. The data from the baseline survey will be incorporated directly into the programmatic decision making process to ensure that best strategies are employed during implementation of the project. The baseline survey was conducted from August 2010 to December 2010 in the two project intervention regions namely, Southern Nations, Nationalities and Peoples Region (SNNPR) and Tigray. The survey was carried out in the eleven administrative zones, twenty two woredas (districts) and forty four kebeles/tabias (Peasant Associations-PAs) of these two regions. The first criterion used was potato and sweetpotato potential producing areas as sampling frame. These areas were identified in collaboration with partner institutions Bureau of Agriculture from both regions, GOAL Ethiopia and Vita-RT. Different administrative levels were considered for stratification which included region, zone, woreda and kebele/tabia (the lowest administrative unit). All the woredas in which the project is being implemented in the two regions and the eleven zones were considered during the baseline study. The sample size distribution between the two regions was done proportionally with respect to the potato and sweetpotato production potential across all project intervention woredas and kebeles/tabias. In each of the project kebeles/tabias, twenty four participating farm households were interviewed. These were selected randomly from the list of project participating households if their total number was more than twenty four. In case there were less than twenty four participating farmers in a selected intervention kebele/tabia, all participating  farmers plus additional farmers who were selected to participate the next season were interviewed to meet the total twenty four samples of households. Another nearby kebele/tabia was selected randomly as non-participant from which twelve farmers were  selected randomly for the interview. In case the farmer targeted for an interview was not found at home during the visit, a spouse was recommended to answer on behalf of the farmer. If both of them were not able to be located during the day, additional reserve samples were drawn to meet the required number of households to be interviewed. Data entry and cleaning was done from December 2010 - August 2011 and data analysis and report writing was done September 2011 - December 2011.
"

	uri <- "doi:10.21223/P3/SIXPTQ"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.45
	)

	num <- function(x) {
		x <- trimws(as.character(haven::zap_labels(x)))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	chr <- function(x) {
		x <- trimws(as.character(haven::zap_labels(x)))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	dms <- function(h, mi, se, hmin, hmax) {
		h <- num(h)
		mi <- num(mi)
		se <- num(se)
		ok <- !is.na(h) & h >= hmin & h <= hmax & !is.na(mi) & mi >= 0 & mi < 60 & !is.na(se) & se >= 0 & se < 60
		out <- rep(NA_real_, length(h))
		out[ok] <- h[ok] + mi[ok] / 60 + se[ok] / 3600
		out
	}

	fa <- ff[basename(ff) == "MODULE A HOUSEHOLD IDENTIFICATION-1.dta"]
	fp <- ff[basename(ff) == "MODULE F POTATO MANAGEMENT-1.dta"]
	a <- haven::read_dta(fa)
	p <- haven::read_dta(fp)

	year <- num(a$A24C)
	month <- num(a$A24B)
	day <- num(a$A24A)
	okd <- !is.na(year) & !is.na(month) & !is.na(day) & month >= 1 & month <= 12 & day >= 1 & day <= 31
	date <- rep(NA_character_, nrow(a))
	date[okd] <- sprintf("%04d-%02d-%02d", year[okd], month[okd], day[okd])
	date[okd] <- as.character(as.Date(date[okd]))

	d <- data.frame(
		hhid = chr(a$HHID),
		region_code_ = chr(a$A01B),
		zone_code_ = chr(a$A02B),
		woreda_code_ = chr(a$A03B),
		kebele_code_ = chr(a$A04B),
		location = tools::toTitleCase(tolower(chr(a$A05B))),
		crop_type_ = chr(a$CROPTYPE),
		farm_type_ = chr(a$FARMTYPE),
		date = date,
		latitude = dms(a$BPLG, a$BPLS, a$BPLM, 3, 15),
		longitude = dms(a$BPLOG, a$BPLOS, a$BPLOM, 30, 48),
		elevation = num(a$BPAL),
		potato_area_m2_ = num(a$BPP),
		sweetpotato_area_m2_ = num(a$BSPP),
		ofsp_area_m2_ = num(a$BOFSP)
	)
	pf <- data.frame(
		hhid = chr(p$HH_ID),
		potato_area_belg_m2_ = num(p$F4A),
		potato_area_meher_m2_ = num(p$F4B),
		variety = chr(p$VU_1),
		variety_2_ = chr(p$VU_2),
		variety_3_ = chr(p$VU_3),
		fertilizer_inorganic_ = chr(p$F12)
	)
	pf <- pf[!duplicated(pf$hhid), ]
	d <- merge(d, pf, by="hhid", all.x=TRUE)
	d$crop <- NA_character_
	ct <- tolower(d$crop_type_)
	d$crop[grepl("sweet", ct)] <- "sweetpotato"
	d$crop[grepl("potato", ct) & !grepl("sweet", ct)] <- "potato"
	d$country <- "Ethiopia"
	d$trial_id <- paste0("BPBL_", d$hhid)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$elevation[d$elevation < -200] <- NA_real_

	carobiner::write_files(path, meta, d)
}
