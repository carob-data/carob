# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household rows from 01_Sweetpotato_Vine_Silage_Dataset.csv. Uganda.
# gps_lat and gps_long are the interview coordinates.
# Sweetpotato area is e2_sp_land_qty. Square meters are divided by 10000. Acres are multiplied by 0.404685642. Square yards are multiplied by 0.0000836127 (0.836127 m2 per square yard). Hectares are already hectares.
# resp_sex Female/Male is stored as female/male.
# Crop is sweetpotato where the household reported growing it.

## ISSUES
# No yield and no fertilizer. This is a feeding-practices interview.
# The abstract spells the district Wakaiso. The district names in the file are kept as recorded.
# Some households did not report a sweetpotato area.
# write_files: missing variables: carob_contributor.

## NOT PROCESSED
# Livestock, milk, silage-perception, price, and residue-use columns. They are not a crop measurement.
# Land quantities other than the sweetpotato area (owned, pasture, fodder, food, cash, fallow).
# resp_educ, marital status, and relationship codes.

carob_script <- function(path) {

"
Dataset for: Dataset for Determination of smallholder dairy farmers feeding practices and their perceptions on the use of sweetpotato vines silage (SPVS) as a feed resource in their production cycle.

Smallholder dairy farmers constitute up to 85% of dairy farmers in Uganda (Atuhaire, 2014). However, this category of farmers is challenged and faced by unstable feed quality and quantity supply year-round (Kiggundu et al., 2014). This therefore has led to unstable milk production and productivity which has dare effects on the milk prices and incomes of this category of dairy farmers. Studies have indicated that crop residues have a high potential to bridge the feed gap and provide a cheaper option to maintain a stable feed supply when well preserved (Dione et al., 2015). A study funded by International Potato Center (CIP) was conducted in three districts of Wakaiso, Masaka, and Kamuli to determine prevalent smallholder dairy farmers feeding practices and their perceptions on the use of sweetpotato vines silage (SPVS) as a feed resource in their production cycle. The local government structures were used to select potential farmer respondents. A list of smallholder dairy farmers in a district was obtained from the District Veterinary Officer (DVO), five sub-counties in Wakiso and two each from Masaka and Kamuli were selected based on the population density of the smallholder dairy farmers in these districts.  Systematically using an interval of five, farmers were selected and a structured questionnaire designed in the CSEntry data tool was administered to the selected farmers. A total of 147 farmers with the help of a local language translator were interviewed, on-farm observations were made, data were screened for any misrepresentations and follow-up questions were posed and saved. Preliminary findings during the study indicated that zero-grazing (58.59%) is most commonly practiced. Farmers reported contamination of sweetpotato vines with soil that hinders the use of the feed resource for feeding livestock. Significant amounts of vines are lost by some farmers after being brought to the farm for the luck of chopping and other forms of feed manipulations to facilitate intake. The majority of women (76.2%) and men (80.9%) agree to the statement 'I have a say over how household income from dairy products is spent' Data analysis is underway and a complete report will be produced documenting findings from the study.  Financial support from (CIP) is highly appreciated.
"

	uri <- "doi:10.21223/1J1YDL"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "field_size",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_Sweetpotato_Vine_Silage_Dataset.csv"]
	r <- read.csv(f, check.names=FALSE)
	to_ha <- function(qty, unit) {
		q <- as.numeric(qty)
		u <- trimws(as.character(unit))
		ha <- rep(NA_real_, length(q))
		ha[u == "Hectares"] <- q[u == "Hectares"]
		ha[u == "Square Meters"] <- q[u == "Square Meters"] / 10000
		ha[u == "Acres"] <- q[u == "Acres"] * 0.404685642
		ha[u == "Square Yards"] <- q[u == "Square Yards"] * 0.0000836127
		ha
	}
	vcols <- c("e3_sp_var01", "e3_sp_var02", "e3_sp_var03", "e3_sp_var04", "e3_sp_varo")
	variety <- apply(r[, vcols, drop=FALSE], 1, function(z) {
		z <- trimws(as.character(z))
		z <- z[!is.na(z) & nzchar(z)]
		if (length(z) == 0) NA_character_ else paste(unique(z), collapse="; ")
	})
	grow <- trimws(r$e2_grow_sp_yesno) == "Yes"

	d <- data.frame(
		hhid = as.character(r$qnid),
		country = trimws(r$country_uganda),
		adm1 = trimws(r$region),
		adm2 = trimws(r$district),
		location = trimws(r$village),
		latitude = as.numeric(r$gps_lat),
		longitude = as.numeric(r$gps_long),
		elevation = as.numeric(r$gps_alt),
		geo_from_source = TRUE,
		sex = tolower(trimws(r$resp_sex)),
		age = as.numeric(r$resp_age),
		hh_size = as.numeric(r$a3_household_size),
		field_size = to_ha(r$e2_sp_land_qty, r$sp_land_unit),
		variety = variety,
		crop = ifelse(grow, "sweetpotato", NA_character_),
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA
	)

	carobiner::write_files(path, meta, d)
}
