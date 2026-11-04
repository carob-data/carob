# R script for "carob"
# license: GPL (>=3)

## NOTES
# CARBAP station, Njombé, Littoral, Cameroon. One on-station trial, 2009-2011. Country, adm1, and location are geographicCoverage.
# Variety codes are the nine names in the description. CR, DC, DD, and FH are the hybrid group in the file; the others are natural.
# Bunch weight is kg per plant. Plant density is not in the files, so yield is not in kg/ha.
# Mother-plant growth every 15 days is the long table. Height is cm and girth is cm at 50 cm (captions). Leaf area columns are m2.

## ISSUES
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variables: exp_plot_, plant_position_, plantain_subgroup_, flowering_height_, flowering_girth_, harvest_leaves_, harvest_green_area_, bunch_weight_, hands_, fingers_
# unknown variables: pseudostem_girth_, emitted_leaves_, living_leaves_, emitted_leaf_area_, green_leaf_area_
# out of bounds: flowering_days (214, 448), harvest_days (296, 525)
# out of bounds: plant_height (8, 552)

## NOT PROCESSED
# optim_mp-leaves-length.txt and optim_mp-leaves-width.txt. One column per leaf number.
# optim_sucker-growth.txt. Sucker height and girth on a 15-day step.
# Fingers on each hand (NFH1 to NFH13).
# Thermal time (FHP2, PHP2, F1FP, F1F2P, F2FP).


## NEW VARIABLES
# bunch_weight_: bunch weight at harvest, kg per plant.
# flowering_height_: pseudostem height at flowering, cm.
# flowering_girth_: pseudostem girth at 50 cm at flowering, cm.
# plantain_subgroup_: subgroup code in the file (Frh, Fre, Fah, Hyb).
# plant_position_: position of the plant in the elementary plot.
# pseudostem_girth_: girth at 50 cm on the 15-day series, cm.
# emitted_leaves_: emitted leaves on that date.
# living_leaves_: living leaves on that date.
# emitted_leaf_area_: emitted leaf area, m2.
# green_leaf_area_: green leaf area, m2.
# harvest_green_area_: green area at harvest, m2.
# harvest_leaves_: living leaves at harvest.
# hands_: hands on the bunch.
# fingers_: fingers on the bunch.
# exp_plot_: elementary plot number in the harvest file.


carob_script <- function(path) {

"
The 'Plantain-Optim' dataset: growth, development and yield of 405 plantains

The 'Plantain-Optim' dataset contains growth, development and yield data of 405 plantain every 15 days from planting to harvest. These data represent 9 plantain varieties, included 5 natural plantains (Essong, Batard, French clair, Mbouroukou n°3 and Big Ebanga) and 4 plantain-like hybrids (CRBP39, FHIA21, DD248, DC535). These data come from the 'Optim' experiment conducted at CARBAP station (Cameroon) from 2009 to 2011.
"

	uri <- "doi:10.18167/DVN1/CBUVWU"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	# English and French descriptions make yuri build two metadata rows and stop. Both texts stay in one description.
	jf <- list.files(file.path(path, "data", "raw", group, "doi_10.18167_DVN1_CBUVWU"), pattern="\\.json$", full.names=TRUE)
	js <- jsonlite::read_json(jf)
	fields <- js$data$latestVersion$metadataBlocks$citation$fields
	for (i in seq_along(fields)) {
		if (identical(fields[[i]]$typeName, "dsDescription") && length(fields[[i]]$value) > 1) {
			texts <- vapply(fields[[i]]$value, function(z) z$dsDescriptionValue$value, "")
			fields[[i]]$value[[1]]$dsDescriptionValue$value <- paste(texts, collapse="\n\n")
			fields[[i]]$value <- fields[[i]]$value[1]
			js$data$latestVersion$metadataBlocks$citation$fields <- fields
			jsonlite::write_json(js, jf, auto_unbox=TRUE)
		}
	}

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.dib.2018.01.065",
		project = "Optim",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "bunch_weight_;flowering_days;plant_height",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.7
	)

	fh <- ff[basename(ff) == "optim_mp-harvest.txt"]
	ffl <- ff[basename(ff) == "optim_mp-flowering.txt"]
	fg <- ff[basename(ff) == "optim_mp-growth.txt"]

	h <- read.delim(fh, na.strings="NA", check.names=FALSE)
	# Flowering file is UTF-16. The harvest and growth files are not.
	fl <- read.delim(ffl, fileEncoding="UTF-16LE", na.strings="NA", check.names=FALSE)
	g <- read.delim(fg, na.strings="NA", check.names=FALSE)
	names(h) <- trimws(names(h))
	names(fl) <- trimws(names(fl))
	names(g) <- trimws(names(g))
	fl <- fl[, names(fl) != ""]

	# Codes follow the description: hybrids are CRBP39, DC535, DD248, FHIA21.
	vname <- c(BA="Batard", BE="Big Ebanga", ES="Essong", FC="French clair", MB="Mbouroukou n°3", CR="CRBP39", DC="DC535", DD="DD248", FH="FHIA21")

	d <- data.frame(
		trial_id = "CARBAP_2009",
		plot_id = as.character(h$Plant),
		exp_plot_ = as.integer(h$Plot),
		plant_position_ = as.integer(h$Pos),
		variety = unname(vname[h$Var]),
		variety_type = ifelse(h$Type == "Nat", "natural", "hybrid"),
		plantain_subgroup_ = h$Group,
		treatment = unname(vname[h$Var]),
		flowering_days = as.numeric(fl$PFP[match(h$Plant, fl$Plant)]),
		flowering_height_ = as.numeric(fl$Htf[match(h$Plant, fl$Plant)]),
		flowering_girth_ = as.numeric(fl$G50f[match(h$Plant, fl$Plant)]),
		harvest_days = as.numeric(h$PHP1),
		harvest_leaves_ = as.numeric(h$NLLh),
		harvest_green_area_ = as.numeric(h$GAh),
		bunch_weight_ = as.numeric(h$BW),
		hands_ = as.numeric(h$NHB),
		fingers_ = as.numeric(h$NFB),
		country = "Cameroon",
		adm1 = "Littoral",
		location = "Njombé",
		crop = "plantain",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	# Growth file uses the Excel token #N/A.
	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "#N/A")] <- NA_character_
		as.numeric(x)
	}
	dlong <- data.frame(
		trial_id = "CARBAP_2009",
		plot_id = as.character(g$Plant),
		DAP = as.integer(num(g$DAP)),
		plant_height = num(g$Ht),
		pseudostem_girth_ = num(g$G50),
		emitted_leaves_ = num(g$NEL),
		living_leaves_ = num(g$NLL),
		emitted_leaf_area_ = num(g$ELA),
		green_leaf_area_ = num(g$GFA)
	)
	dlong <- dlong[!is.na(dlong$DAP), ]

	carobiner::write_files(path, meta, d, long=dlong)
}
