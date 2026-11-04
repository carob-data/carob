# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey of the Papas Puño de Hierro seed pilot in La Libertad, Peru. Country is stated in the dataset description. Region, province, district, and town are the survey columns.
# P21a is the quantity of seed the household plans to plant next season, with its own unit column. It is not a harvest yield.
# P03 is coded M or F.

## ISSUES
# No coordinates. Towns need georeferencing.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude. unknown variables are the p07_ to p40_ answer columns, seed_quantity_, and seed_unit_.

## NOT PROCESSED
# 03_Cuestionario_Encuesta_EvaluacionPPH_20 Jul_2021.docx: the questionnaire, not the answers.

## NEW VARIABLES
# The p07_ to p40_ columns are the questionnaire answers as recorded. seed_quantity_ and seed_unit_ are the planned seed amount and its unit.


carob_script <- function(path) {

"
Dataset for: Monitoreo del piloto de intervención Papas “Puño de Hierro” Distribución y uso de semilla

Through a gender-responsive pilot intervention, the project Anemia Zero is contributing to improve food security, nutrition, gender equality, and empowerment in potato-based production systems in high Andean regions of Peru (Curgos and Julcan Districts, Province Sanchez Carrion, Department of La Libertad) and to scale agricultural and nutrition innovations in coordination with the social protection programs (SPP) Juntos and Cuna Mas (both overseen by the Ministry of Development and Social Inclusion (MIDIS), as well as with other stakeholders. This survey aims to measure the perception and preliminary adoption of biofortified potato clones and varieties with iron. The survey corresponds to the following sections: (1) General information, (2) Informed consent, (3) potato planting information, (4) potato harvesting information, (5) biofortified seed distribution, (6) biofortified potato consumption, (7) general comments.
"

	uri <- "doi:10.21223/PRUOEG"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
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
		carob_completion = 85,
		carob_effort = 0.4
	)

	f <- ff[grepl("01_DataSet", basename(ff))]
	r <- carobiner::read.excel(f, sheet="DataBase")
	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	d <- data.frame(
		hhid = chr(r$P05),
		farmer_gender = chr(r$P03),
		adm1 = chr(r$P06a),
		adm2 = chr(r$P06b),
		adm3 = chr(r$P06c),
		location = chr(r$P06d),
		p07_ = chr(r$P07),
		p08_ = chr(r$P08),
		variety = chr(r$P09),
		p10_ = chr(r$P10),
		p10a_ = chr(r$P10a),
		p10b_ = chr(r$P10b),
		p10c_ = chr(r$P10c),
		p11_ = chr(r$P11),
		p12_ = chr(r$P12),
		p13_ = chr(r$P13),
		p14_ = chr(r$P14),
		p15_ = chr(r$P15),
		p16_ = chr(r$P16),
		p16a_ = chr(r$P16a),
		p17_ = chr(r$P17),
		p17a_ = chr(r$P17a),
		p17b_ = chr(r$P17b),
		p18_ = chr(r$P18),
		p19_ = chr(r$P19),
		p19a_ = chr(r$P19a),
		p20_ = chr(r$P20),
		p20a_ = chr(r$P20a),
		p20b_ = chr(r$P20b),
		p21_ = chr(r$P21),
		seed_quantity_ = num(r[["P21a.Cantidad"]]),
		seed_unit_ = chr(r[["P21a.Unidad"]]),
		p22_ = chr(r$P22),
		p23_ = chr(r$P23),
		p23a_ = chr(r$P23a),
		p23b_ = chr(r$P23b),
		p24_ = chr(r$P24),
		p25_ = chr(r$P25),
		p26_ = chr(r$P26),
		p26a_ = chr(r$P26a),
		p26b_ = chr(r$P26b),
		p27_ = chr(r$P27),
		p28_ = chr(r$P28),
		p28a_ = chr(r$P28a),
		p29_ = chr(r$P29),
		p29a_ = chr(r$P29a),
		p30_ = chr(r$P30),
		p31_ = chr(r$P31),
		p32_ = chr(r$P32),
		p33_ = chr(r$P33),
		p34_ = chr(r$P34),
		p35_ = chr(r$P35),
		p36_ = chr(r$P36),
		p36a_ = chr(r$P36a),
		p37_ = chr(r$P37),
		p38_ = chr(r$P38),
		p38a_ = chr(r$P38a),
		p38b_ = chr(r$P38b),
		p39_ = chr(r$P39),
		p39a_ = chr(r$P39a),
		p40_ = chr(r$P40),
		p40a_ = chr(r$P40a),
		p40b_ = chr(r$P40b),
		p40c_ = chr(r$P40c)
	)
	d$variety[!nzchar(d$variety) | is.na(d$variety)] <- NA_character_
	d$crop <- "potato"
	d$country <- "Peru"
	d$trial_id <- paste0("PPH_", d$hhid, "_", seq_len(nrow(d)))
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
