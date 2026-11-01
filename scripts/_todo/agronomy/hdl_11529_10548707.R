# R script for "carob"
# license: GPL (>=3)

## NOTES
# Meta-analysis of paired conventional and conservation-agriculture yields. Each pair becomes two rows.
# OF_Data_yld is on-farm. OS_Data_YLD is on-station. Grain yield columns are t/ha.
# CT_N and CA_N are small integers in the on-farm sheet and are not used as fertilizer rates.

## ISSUES
# No coordinates.
# Nitrogen columns are not rates.
# Cotton yield is stored as seed. The sheet does not say lint or seed cotton.
# write_files: missing variables: yield_moisture, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. duplicates: duplicate records detected.

## NOT PROCESSED
# Partial-factor productivity, water, cost, net return, and their standard deviations.
# Treatment codes and soil-texture codes.
# On-station publication year is not used as the trial year.


carob_script <- function(path) {

"
Conservation agriculture for sustainable intensification in South Asia

Agriculture’s contribution to the Sustainable Development Goals requires climate-smart and profitable farm innovations. In the past decade, attention has been given to conservation agriculture as a ‘sustainable intensification’ strategy, although a lack of evidence-based consensus on the merits of conservation agriculture prevails in the context of intensive smallholder farming in South Asia. A meta-analysis using 9,686 paired site–year comparisons representing different indicators of cropping-system performance suggest significant (P < 0.05) benefits when conservation agriculture was implemented.
"

	uri <- "hdl:11529/10548707"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.4
	)

	texture_name <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x[!nzchar(x)] <- NA_character_
		x[x == "silt loam"] <- "silty loam"
		x[x == "silt clay loam"] <- "silty clay loam"
		x[x == "silty clya loam"] <- "silty clay loam"
		x[x == "clayey"] <- "clay"
		x
	}

	crop_name <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x[x %in% c("mb", "mungbean", "mung bean")] <- "mung bean"
		x[x == "pigeonpea"] <- "pigeon pea"
		x
	}

	pair <- function(country, adm1, location, crop, texture, year, on_farm, y_ct, y_ca, id) {
		n <- length(country)
		ct <- data.frame(
			country = country,
			adm1 = adm1,
			location = location,
			crop = crop,
			soil_texture = texture,
			planting_date = year,
			on_farm = on_farm,
			treatment = "conventional",
			yield = as.numeric(y_ct) * 1000,
			trial_id = id
		)
		ca <- ct
		ca$treatment <- "conservation agriculture"
		ca$yield <- as.numeric(y_ca) * 1000
		rbind(ct, ca)
	}

	f <- ff[grepl("data.xlsx$", basename(ff))]
	a <- carobiner::read.excel(f, sheet="OF_Data_yld", fix_names=TRUE)
	a <- a[!is.na(a$Crop), ]
	texture <- texture_name(a$Soil.texture.17)
	d1 <- pair(
		carobiner::fix_name(a$country, "title"),
		carobiner::fix_name(a$State, "title"),
		carobiner::fix_name(a$Location, "title"),
		crop_name(a$Crop),
		texture,
		as.character(a$Yr),
		TRUE,
		a$CT_GY,
		a$CA_GY,
		paste(a$country, a$Location, a$Yr, a$Sty.No, a$Crop)
	)

	b <- carobiner::read.excel(f, sheet="OS_Data_YLD", fix_names=TRUE)
	b <- b[!is.na(b$Crop), ]
	d2 <- pair(
		carobiner::fix_name(b$Country, "title"),
		NA_character_,
		carobiner::fix_name(b$Location, "title"),
		crop_name(b$Crop),
		texture_name(b$Soil_texture),
		NA_character_,
		FALSE,
		b$GY_C,
		b$GY_CA,
		paste("OS", b$Country, b$Location, b$Stud_No, b$Crop)
	)

	d <- rbind(d1, d2)
	d <- d[!is.na(d$yield), ]
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- FALSE
	d$yield_part <- ifelse(d$crop %in% c("rice", "wheat", "maize"), "grain", "seed")
	d$trial_id <- as.character(as.integer(as.factor(d$trial_id)))

	carobiner::write_files(path, meta, d)
}
