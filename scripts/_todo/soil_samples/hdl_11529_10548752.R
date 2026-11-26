# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil enzyme activities after 7 years of climate-smart agriculture (CSA) practices in cereal-based systems at ICAR-CSSRI, Karnal, Haryana, India (Datta et al., 2021, J. Indian Soc. Soil Sci. 69(1): 86-95, doi:10.5958/0974-0228.2021.00024.4; PDF bundled in deposit).
# Site details from Materials and Methods: ICAR-CSSRI experimental farm, Karnal, Haryana, India (29.7058 N, 76.9555 E; paper prints 29°70' N, 76°95' E as decimal-degree typo). Soil samples taken at 0-15 cm depth across 4 wheat growth stages during the 2016-17 wheat season.
# Treatments (Sc1-Sc6, Table 1 of paper):
#   Sc1: Farmer's practice (CT rice-wheat-fallow, residues removed)
#   Sc2: Partial CA rice-wheat-mungbean (CT rice, ZT wheat/mungbean, residues retained)
#   Sc3: Partial CSA rice-wheat-mungbean (ZT DSR-ZT wheat-ZT mungbean, residues retained)
#   Sc4: Partial CSA maize-wheat-mungbean (ZT maize-ZT wheat-ZT mungbean, residues retained)
#   Sc5: Full CSA rice-wheat-mungbean (Sc3 + subsurface drip irrigation)
#   Sc6: Full CSA maize-wheat-mungbean (Sc4 + subsurface drip irrigation)
# Stage codes a-d correspond to wheat growth stages S1-S4 (before sowing, maximum tillering, panicle initiation, after harvest), verified against Table 2 DHA means.
# Rhizosphere codes r/nr = rhizosphere / non-rhizosphere soil.
# Sheet " PHOSPHATASE" is alkaline phosphatase (Table 3 of paper).
# Enzyme units (Tables 2-3, Figs 1-4): dehydrogenase = ug TPF/g soil/h; acid phosphatase, alkaline phosphatase, beta-glucosidase, arylsulphatase = ug PNP/g soil/h; fluorescein diacetate hydrolase = ug fluorescein/g soil/h; urease = ug urea/g soil/h; cellulase = ug glucose/g soil/h.

## ISSUES
# write_files: unknown variables rhizosphere_, soil_microbial_activity_unit_.

## NEW VARIABLES
# rhizosphere_: sampling zone relative to roots ("rhizosphere" or "non-rhizosphere")
# soil_microbial_activity_unit_: unit of soil_microbial_activity_value for each enzyme assay

## NOT PROCESSED
# Absorbance readings, standard curves, and two-year mean summary blocks on each sheet.
# 11-Ashim-Datta.pdf: published paper (used for site metadata, coordinates, treatment decoding, depth, and units).


carob_script <- function(path) {

"
Climate Smart Agriculture Influences Soil Enzymes Activity under Cereal-based Systems of North-West India

Soil enzyme activities measured after seven years of the same management practices in cereal-based systems of north-west India.
"

	uri <- "hdl:11529/10548752"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CSSRI; CIMMYT",
		publication = "doi:10.5958/0974-0228.2021.00024.4",
		project = "CSISA",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment; growth_stage; rhizosphere_",
		response_vars = "soil_microbial_activity_value",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 80,
		carob_effort = 0
	)

	label <- c(
		"Acid Phosphatase" = "acid phosphatase",
		"Arylsulfatase" = "arylsulphatase",
		"Beta glucosidase" = "beta-glucosidase",
		"Urease" = "urease",
		" PHOSPHATASE" = "alkaline phosphatase",
		"Flourescien Diacetate Hydrolase" = "fluorescein diacetate hydrolase",
		"DEHYDROGENASE " = "dehydrogenase",
		"cellulase" = "cellulase"
	)
	units <- c(
		"Acid Phosphatase" = "ug PNP/g soil/h",
		"Arylsulfatase" = "ug PNP/g soil/h",
		"Beta glucosidase" = "ug PNP/g soil/h",
		"Urease" = "ug urea/g soil/h",
		" PHOSPHATASE" = "ug PNP/g soil/h",
		"Flourescien Diacetate Hydrolase" = "ug fluorescein/g soil/h",
		"DEHYDROGENASE " = "ug TPF/g soil/h",
		"cellulase" = "ug glucose/g soil/h"
	)
	sc_map <- c(
		"1" = "Sc1: CT rice-wheat-fallow",
		"2" = "Sc2: partial CA rice-wheat-mungbean",
		"3" = "Sc3: partial CSA ZT rice-wheat-mungbean",
		"4" = "Sc4: partial CSA ZT maize-wheat-mungbean",
		"5" = "Sc5: full CSA ZT rice-wheat-mungbean + SDI",
		"6" = "Sc6: full CSA ZT maize-wheat-mungbean + SDI"
	)
	st_map <- c(
		"a" = "before sowing",
		"b" = "maximum tillering",
		"c" = "panicle initiation",
		"d" = "harvest"
	)
	rh_map <- c("r" = "rhizosphere", "nr" = "non-rhizosphere")

	f <- ff[basename(ff) == "11-Ashim Enzyme_skk.xlsx"]
	one <- function(sheet) {
		r <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE, col_types="text")
		hdr <- NULL
		for (i in seq_len(min(8, nrow(r)))) {
			cells <- as.character(unlist(r[i, ]))
			if ("rep" %in% cells && "sc" %in% cells) {
				hdr <- i
				break
			}
		}
		cells <- as.character(unlist(r[hdr, ]))
		crep <- which(cells == "rep")[1]
		body <- r[(hdr + 1):nrow(r), ]
		rep <- trimws(as.character(body[[crep]]))
		ok <- !is.na(rep) & nzchar(rep)
		body <- body[ok, ]
		sc <- trimws(as.character(body[[crep + 1]]))
		st <- tolower(trimws(as.character(body[[crep + 2]])))
		rh <- tolower(trimws(as.character(body[[crep + 3]])))
		data.frame(
			country = "India",
			adm1 = "Haryana",
			adm2 = "Karnal",
			location = "CSSRI, Karnal",
			# Exact CSSRI Karnal experimental farm coordinates
			latitude = 29.7058,
			longitude = 76.9555,
			crop = "wheat",
			date = "2016",
			soil_sample_date = "2016",
			depth_top = 0,
			depth_bottom = 15,
			trial_id = "CSSRI_Karnal_Enzymes_2016",
			rep = as.integer(trimws(as.character(body[[crep]]))),
			treatment = unname(sc_map[sc]),
			growth_stage = unname(st_map[st]),
			rhizosphere_ = unname(rh_map[rh]),
			soil_microbial_activity_category = unname(label[sheet]),
			soil_microbial_activity_value = as.numeric(trimws(as.character(body[[crep + 4]]))),
			soil_microbial_activity_unit_ = unname(units[sheet]),
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = TRUE
		)
	}

	d <- do.call(rbind, lapply(names(label), one))
	d <- d[!is.na(d$treatment) & !is.na(d$soil_microbial_activity_value), ]
	d$sample_id <- paste(d$soil_microbial_activity_category, d$rep, d$treatment, d$growth_stage, d$rhizosphere_, sep="_")

	carobiner::write_files(path, meta, d)
}
