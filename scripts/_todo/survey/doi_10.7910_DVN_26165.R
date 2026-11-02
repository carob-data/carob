# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline household survey (2008) of the "Dissemination of submergence-tolerant varieties" project (STRASA), exported from Access as one
# set of tables per country. Files without suffix, with "_1", "_2" and with prefix "1" are different countries (not duplicates):
#   Laos: "Tbl_Part 04 Rice Varieties Grow.xlsx" + "Tbl_Part 0 Intro.xlsx"
#   Central Vietnam: "..._1.xlsx"; South Vietnam: "..._2.xlsx"
#   Cambodia: "Tbl_Part 04 Rice Varieties Grow.xls" + "Tbl_Part 0 Intro.xls"
#   Thailand: "Tbl_Part 04 Rice Varieties Grown_1.xls" + "Tbl_Part 0 Intro_2.xls"
#   Indonesia: "1Tbl_Part 04 Rice Varieties Grow.xls" (numerically coded)
# Mapped: Part 04 "Rice varieties grown" = household x flood/normal year x season x variety, with land type, crop establishment method,
# area, production, planting and harvest month. One record per row.
# yield = Prod / Area (the files' own Yield column equals Prod / Area).
# Reference year (planting_date) is the household's "NormalYear" or "FloodYear" from the Intro table.
# Provinces (adm1) from "Project details.xlsx" and the district names; Cambodia from the Province column.
# Vietnamese seasons: Winter-Spring -> dry, Summer-Autumn -> early wet, Autumn-Winter -> late wet.
# season_constraint is "flood" for the flood-year records.
# planting_method: TPR = transplanting; DSR and WSR = direct seeding (crop_establishment_ keeps dry/wet seeding).

## ISSUES
# Units of Area and Prod are not given in the variable list ("BaselineVarList.xls"); values imply ha and kg (yields 0-7.9 t/ha). Verify.
# Indonesia (676 households) is numerically coded (Landtype, Season, MVTV, CEM, PlMonth) and there is no code list; only district,
#   variety, flood/normal, area and yield are mapped; its codes are kept as "code n" in land_type_ and crop_establishment_ and in
#   season_code_, so a human can decode them. Indonesia Intro has duplicated FarmerIDs and no Normal/Flood years.
# Thailand province is unknown (districts "Sahatart" and "Sampanta"; the project lists Prachinburi and Ubon Ratchathani).
# Many Cambodian households have implausible or missing NormalYear/FloodYear (1996 - 2008); used as given.
# planting_date is a year only. Dry season / Winter-Spring crops may have been planted in the previous calendar year (planting_month_ Dec).
# The Philippines (Nueva Ecija and North Cotabato) data are in a different, questionnaire-page layout
#   ("... raw data_Sub1.xls" with "Documentation_Sub1 ...xls") and are not processed.
# Not processed: Part 20 (largest parcel: soil type, tenure, price), 20A (material inputs incl. fertilizer quantities for the largest
#   parcel; product names in free text), 20B/20C (power, labor), Part 03 (other crops), 05 (crop disposal), 06 (coping practices),
#   income, assets, consumption, relief, information modules. Fertilizer N/P/K are therefore not mapped.
# Several varieties are recorded together ("RD10 and TDK1"); kept as given.
# No coordinates (GeogDegree/GeogMins are empty).

## NEW VARIABLES
# land_type_: land type as recorded (lowland, upland, medium, intermediate, highland)
# crop_establishment_: TPR (transplanted), DSR (dry direct-seeded), WSR (wet direct-seeded)
# planting_month_, harvest_month_: month of planting and harvest (with the week in planting_week_, harvest_week_)
# variety_choice_reason_: reason for growing the variety
# season_code_: undocumented numeric season code (Indonesia only)


carob_script <- function(path) {

"
Dissemination of Submergence-Tolerant Varieties and Associated New Production Practices to Southeast Asia

This is a household baseline survey that will measure ex-post and ex-ante economic impact of submergence to the rice farming communities in Southeast Asia. The survey has 1,173 samples that collected rice, non-rice and non-farm economic activities and production output during flood and non-flood years. Demographic characteristics, land holding, rice and non-rice production, sources of income and production practices are some of the basic variables used in the survey. In addition, mitigating measures during flood year and gender roles were also collected.
"

	uri <- "doi:10.7910/DVN/26165"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "STRASA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 25,
		carob_effort = 0.7
	)

	getf <- function(name) ff[basename(ff) == name]
	b <- data.frame(
		country = c("Laos", "Vietnam", "Vietnam", "Cambodia", "Thailand", "Indonesia"),
		cc = c("LAO", "CVN", "SVN", "KHM", "THA", "IDN"),
		f4 = c("Tbl_Part 04 Rice Varieties Grow.xlsx", "Tbl_Part 04 Rice Varieties Grow_1.xlsx", "Tbl_Part 04 Rice Varieties Grow_2.xlsx",
			"Tbl_Part 04 Rice Varieties Grow.xls", "Tbl_Part 04 Rice Varieties Grown_1.xls", "1Tbl_Part 04 Rice Varieties Grow.xls"),
		f0 = c("Tbl_Part 0 Intro.xlsx", "Tbl_Part 0 Intro_1.xlsx", "Tbl_Part 0 Intro_2.xlsx",
			"Tbl_Part 0 Intro.xls", "Tbl_Part 0 Intro_2.xls", NA)
	)

	proc <- function(i) {
		r <- carobiner::read.excel(getf(b$f4[i]))
		r$FarmerID <- as.character(r$FarmerID)
		if (is.null(r$Village)) r$Village <- NA
		if (is.null(r$Province)) r$Province <- NA
		if (is.null(r$ReasonVar)) r$ReasonVar <- NA
		coded <- b$cc[i] == "IDN"
		d <- data.frame(
			country = b$country[i],
			hhid = paste0(b$cc[i], "-", r$FarmerID),
			adm1 = r$Province,
			adm2 = r$District,
			location = r$Village,
			season_constraint = ifelse(r$FloodNormal == "Flood", "flood", NA),
			flood_year = r$FloodNormal == "Flood",
			land_type_ = if (coded) paste("code", r$Landtype) else tolower(r$Landtype),
			season = if (coded) NA else r$Season,
			season_code_ = if (coded) r$Season else NA,
			variety = r$RiceVar,
			variety_type = if (coded) NA else as.vector(c(MV="modern", TV="traditional")[r$MVTV]),
			crop_establishment_ = if (coded) paste("code", r$CEM) else r$CEM,
			field_size = r$Area,
			yield = ifelse(r$Area > 0, r$Prod / r$Area, NA),
			planting_month_ = if (coded) NA else r$PlMonth,
			planting_week_ = if (coded) NA else r$PlWeek,
			harvest_month_ = if (coded) NA else r$HarMonth,
			harvest_week_ = if (coded) NA else r$HarWeek,
			variety_choice_reason_ = r$ReasonVar
		)
		if (!is.na(b$f0[i])) {
			h <- carobiner::read.excel(getf(b$f0[i]))
			h$FarmerID <- as.character(h$FarmerID)
			j <- match(r$FarmerID, h$FarmerID)
			if (all(is.na(d$location))) d$location <- h$Village[j]
			d$planting_date <- as.character(ifelse(d$flood_year, h$FloodYear[j], h$NormalYear[j]))
		} else {
			d$planting_date <- NA
		}
		d
	}
	d <- do.call(rbind, lapply(seq_len(nrow(b)), proc))
	d$flood_year <- NULL

	d$adm1[d$adm2 %in% c("Pakse", "Sanasomboun")] <- "Champasak"
	d$adm1[d$adm2 == "Sebangfai"] <- "Khammouan"
	d$adm1[d$adm2 %in% c("An Nhon", "Tuy Phuoc")] <- "Binh Dinh"
	d$adm1[d$adm2 == "Phung Hiep"] <- "Hau Giang"
	d$adm1[d$adm2 == "Vinh Hung"] <- "Long An"
	d$adm1[d$adm1 == "Kampong Thum"] <- "Kampong Thom"
	d$adm1[d$adm1 == "Pouthisat"] <- "Pursat"
	d$adm1[d$adm2 == "INDRAMAYU"] <- "Jawa Barat"
	d$adm1[d$adm2 == "KAYU AGUNG / OKI"] <- "Sumatera Selatan"
	d$adm2[d$adm2 == "INDRAMAYU"] <- "Indramayu"
	d$adm2[d$adm2 == "KAYU AGUNG / OKI"] <- "Ogan Komering Ilir"

	d$season <- carobiner::replace_values(d$season,
		c("DS", "WS", "Dry season", "Wet season", "Wet Season", "Winter-Spring", "Summer-Autumn", "Autumn-Winter"),
		c("dry", "wet", "dry", "wet", "wet", "dry", "early wet", "late wet"))
	d$variety_choice_reason_ <- trimws(d$variety_choice_reason_)
	d$crop_establishment_[d$crop_establishment_ == "code NA"] <- NA
	d$planting_method <- c(TPR="transplanting", DSR="direct seeding", WSR="direct seeding")[d$crop_establishment_] |> as.vector()

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
