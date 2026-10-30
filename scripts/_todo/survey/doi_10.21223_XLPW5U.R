# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer agroecological-knowledge survey. The dictionary codes province as 1 Phu Tho, 2 Phu Yen, and 3 Tay Ninh, and sex as 1 male and 2 female.
# Q26FERTILIZERN is kg of N per hectare. Q26FERTILIZERP is kg of P per hectare. NPK and urea questions are kg of product per hectare, not elemental K.
# Q27TOTALHARVEST is tonnes of cassava in the previous season, not tonnes per hectare. The dictionary missing code for that question is 99.
# The crop named in those questions is cassava.

## ISSUES
# The dataset geographic coverage does not name a country, and the tables do not either.
# No latitude or longitude.
# Cassava area questions do not state a unit.
# missing variables: carob_contributor
# missing variables: country, latitude, longitude
# unknown variables: npk_basal_kg_ha_, npk_second_kg_ha_, urea_second_kg_ha_, cassava_harvest_t_
# out of bounds: P_fertilizer (0, 750)

## NOT PROCESSED
# Area, pest, gender-role, and training questions.
# Q26 product rates other than the N, P, NPK, and urea items named above are not in the file.
# 02_CA3561 dictionary: the definitions used above.


carob_script <- function(path) {

"
Dataset for: Farmer agroecological knowledge - CA3561

This dataset included information on 83 farmers' agroecological knowledge in  Phu Tho Phu Yen and Tay Ninh (CA3561).
"

	uri <- "doi:10.21223/XLPW5U"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[grepl("^01_CA3561", basename(ff))]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	num <- function(x) {
		x <- trimws(as.character(x))
		out <- rep(NA_real_, length(x))
		ok <- !is.na(x) & grepl("^-?[0-9]+(\\.[0-9]+)?$", x)
		out[ok] <- as.numeric(x[ok])
		out
	}
	prov <- c("1" = "Phu Tho", "2" = "Phu Yen", "3" = "Tay Ninh")
	sex <- c("1" = "male", "2" = "female")
	fecha <- r$DATE
	if (inherits(fecha, "Date") || inherits(fecha, "POSIXt")) {
		fecha <- format(fecha, "%Y-%m-%d")
	} else {
		fecha <- trimws(as.character(fecha))
	}
	harvest <- num(r$Q27TOTALHARVEST)
	harvest[harvest == 99] <- NA_real_
	p_fert <- num(r$Q26FERTILIZERP)

	d <- data.frame(
		trial_id = "CA3561_agroecology",
		hhid = trimws(as.character(r$NUM)),
		adm1 = unname(prov[as.character(r$PROVINCE)]),
		adm2 = trimws(as.character(r$DISTRICT)),
		adm3 = trimws(as.character(r$COMMUNE)),
		location = trimws(as.character(r$VILLAGE)),
		date = fecha,
		crop = "cassava",
		sex = unname(sex[as.character(r$SEX)]),
		age = num(r$AGE),
		hh_size = num(r$HHSIZE),
		N_fertilizer = num(r$Q26FERTILIZERN),
		P_fertilizer = p_fert,
		npk_basal_kg_ha_ = num(r$Q26FERTILIZERNPK1),
		npk_second_kg_ha_ = num(r$Q26FERTILIZERNPK2),
		urea_second_kg_ha_ = num(r$Q26FERTILIZERUREA2),
		cassava_harvest_t_ = harvest,
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
