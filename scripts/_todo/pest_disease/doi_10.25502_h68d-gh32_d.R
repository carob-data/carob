# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. ID 69 (FieldNo 14, plant 4) records Rotten_root = 5 out of Total_root = 4, giving rotting_incidence = 125% (above 100); left as recorded.
# 2. Surf_area has no unit stated (values 600-40000, likely m2 = 0.06-4 ha), and FieldNo 3 has 2500, 2501, 2502, 2503, 2504 across plants 1-5 (likely an Excel drag-fill error).
# 3. FieldNo 5 plant 5 (ID 25, Mbam essoombam) has longitude 11.16602 vs 11.39218 on plants 1-4 of the same field; left as recorded.
# 4. ARTS_presence (African root and tuber scale, Stictococcus vayssierei) is 0/1 presence/absence (2 NAs), not a count; pest_species is set when present and arts_present_ preserves absence/NA.

## NEW
# field_area_: field surface area as recorded (Surf_area; unit not stated, likely m2)
# crop_age_months_: cassava field age in months after planting (Field_age)
# plant_number_: sampled plant number within field (1-5)
# root_number_: total storage roots counted per plant (Total_root)
# root_rotten_: number of rotten storage roots per plant (Rotten_root)
# bud_necrosis_: logical; bud necrosis present (bud_necrosis)
# necrosis_aerial_: logical; necrosis on aerial plant parts (Necrosis_aerial_part)
# necrosis_type_: root necrosis type ("Wet", "Dry", "No necrosis")
# necrosis_color_: root necrosis color ("Black", "Dark brown", "Brown")
# arts_present_: logical; presence of African root and tuber scale (ARTS_presence)
# soil_moisture_class_: soil moisture class from codebook ("humid", code 2)
# disease values not in terminag: cassava root rot
# pest_species values not in terminag: Stictococcus vayssierei

## MISSING
# planting_date

## NOTES
# Diagnostic survey of cassava root rot across 45 fields (5 plants per field = 225 plant observations)
#   in South (Sud), Centre, and Littoral regions of Cameroon, October 2022.
# All 225 plant-level records are kept without aggregation, grouped by field_id/hhid.
# Dates mix d/m/yyyy (4-9 Oct in South/Centre) and m/d/yyyy (12-16 Oct in Littoral); parsed accordingly.
# Weediness (code 2 = 25% weedy) and MC (code 2 = humid) are constant across all rows.

## NOT PROCESSED
# Rotting_presence: 0/1 flag completely redundant with Rotten_root > 0.


carob_script <- function(path) {

"
Understanding cassava rotting and underlying pathogens across major production regions of Cameroon

Diagnostic and surveillance network established for characterization, monitoring and surveillance of Cassava root rot. The present initiative aims at assessing the situation of cassava rotting and underlying pathogens  in major cassava production regions in Cameroon to inform priority actions.
"

	uri <- "doi:10.25502/h68d-gh32/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "PHI-WP1: Bridging Knowledge Gaps and Networks: Plant Health Threat Identification and Characterization",
		design = "Diagnostic field survey across 3 regions of Cameroon; 45 cassava fields, 5 plants sampled per field",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "root_infection;disease_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "cassava-root-rot-data.csv"]
	r <- read.csv(f, check.names = FALSE, fileEncoding = "latin1")

	# October 2022 survey: South/Centre rows use d/m/yyyy (4/10/2022); Littoral rows use m/d/yyyy (10/12/2022)
	parse_date <- function(x) {
		parts <- strsplit(trimws(x), "/", fixed = TRUE)
		vapply(parts, function(z) {
			a <- as.integer(z[1])
			b <- as.integer(z[2])
			y <- z[3]
			if (a == 10L) {
				sprintf("%s-%02d-%02d", y, a, b)
			} else {
				sprintf("%s-%02d-%02d", y, b, a)
			}
		}, character(1))
	}

	adm <- c(South = "Sud", Center = "Centre", Littoral = "Littoral")
	loc <- carobiner::fix_name(trimws(r$Loc), "title")
	variety <- trimws(r$Variety)
	variety[tolower(variety) == "local"] <- "local"
	neccol <- trimws(r$Necrosis_color)
	neccol[!nzchar(neccol)] <- NA_character_
	arts <- r$ARTS_presence == 1L

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = paste(loc, r$FieldNo, sep = "_"),
		hhid = paste(loc, r$FieldNo, sep = "_"),
		field_id = as.character(r$FieldNo),
		plot_id = as.character(r$FieldNo),
		country = "Cameroon",
		adm1 = unname(adm[trimws(r$Region)]),
		location = loc,
		latitude = as.numeric(r$Lat),
		longitude = as.numeric(r$Long),
		elevation = as.numeric(r$Altitude),
		geo_from_source = TRUE,
		date = parse_date(r$Date),
		crop = "cassava",
		variety = variety,
		yield_part = "roots",
		field_area_ = as.numeric(r$Surf_area),
		crop_age_months_ = as.integer(r$Field_age),
		plant_number_ = as.integer(r$Plant_no),
		root_number_ = as.integer(r$Total_root),
		root_rotten_ = as.integer(r$Rotten_root),
		root_infection = as.numeric(r$rotting_incidence),
		disease = "cassava root rot",
		disease_severity = as.character(r$Necrosis_severity),
		severity_scale = "1-5",
		bud_necrosis_ = r$bud_necrosis == 1L,
		necrosis_aerial_ = r$Necrosis_aerial_part == 1L,
		necrosis_type_ = trimws(r$Necrosis_type),
		necrosis_color_ = neccol,
		pest_species = ifelse(!is.na(arts) & arts, "Stictococcus vayssierei", NA_character_),
		arts_present_ = arts,
		# Dictionary codes constant across all rows: Weediness 2 = 25% weedy, MC 2 = humid
		weed_cover = 25,
		soil_moisture_class_ = "humid",
		stress = ifelse(!is.na(arts) & arts, "disease;pests", "disease"),
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
