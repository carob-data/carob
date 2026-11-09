# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field trial of faba bean strips as a trap crop for Liriomyza huidobrensis on potato. Randomized block design, six treatments, three replications.
# papa(+) is potato sprayed with insecticide. papa(-) is potato without insecticide. haba(+) and haba(-) are faba bean strips with and without insecticide. papa(+) and papa(-) alone have no trap crop.
# Larvae and feeding punctures are counts per potato leaflet, weeks 1 to 12, on nine leaflets. Adult trap values are yellow-sticky-trap catches in the potato plots, weeks 1 to 10. Several adult values are not integers, so they are stored as recorded rather than as a raw total.
# The variables sheet rows named Tratamiento, Repeticion, and the single rows coded 1 to 6 are labels, not plot records.

## ISSUES
# Geographic coverage lists Lima and La Molina and does not include a country. The description says the central coast of Peru. Country is not filled from that sentence.
# The metadata access string "-76.4, -12.083333" is not in the data table and is not used as coordinates.
# No fertilizer rate, no yield, no planting date, and no insecticide product name.
# No time unit links week_ to a date.
# missing variables: yield, yield_moisture, yield_part, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variable: trap_crop_ (crop?)
# unknown variables: trap_insecticide_
# invalid terms: pest_species: Liriomyza huidobrensis
# time/depth: no time/depth variables in long records?
# unknown variable: leaflet_ (leaf_Fe?)
# unknown variables: week_, larvae_per_leaflet_, feeding_punctures_, adult_trap_count_

## NOT PROCESSED
# None beyond the label rows on the larval sheet.


## NEW VARIABLES
# trap_crop_: faba bean, or none when the treatment has no trap crop.
# trap_insecticide_: insecticide, none, or no trap. none means the faba strip was not sprayed.
# week_: evaluation week.
# leaflet_: leaflet number within the plot.
# larvae_per_leaflet_: larvae recovered per potato leaflet.
# feeding_punctures_: feeding punctures per potato leaflet.
# adult_trap_count_: leafminer adults on yellow sticky traps in the potato plot.


carob_script <- function(path) {

"
Dataset for: Use of faba bean as trap crop on potato crop protection

Habitat manipulation by trap crops establishes a management component which allows to decrease insect pressure in main crop reducing the requirement of chemical pesticides. In the central coast of Peru, leafminer is one of the major biotic constraints in the production of potato (Solanum tuberosum L.), representing the harmfulest pest. Because of there is no information on the use of trap crop on potato crop protection in our conditions, the following research is to assess the effectiveness of faba bean (Vicia faba L.) around potato crop to control leafminer Liriomyza huidobrensis under field conditions. The research was conducted using an experimental randomized block design with six treatments and three replications. The activity of the leafminer (adults/trap, feeding and oviposition stings) was higher in trap crop than potato plots, displaying bean crop is attractive to adults of that specie.
"

	uri <- "doi:10.21223/P3/OX1RXM"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIP",
		publication = NA,
		project = "Expanding the rational and biological control of invasive Liriomyza leafminer flies to major horticultural production systems of East Africa",
		design = "randomized block design",
		data_type = "experiment",
		treatment_vars = "insecticide_used;trap_crop_;trap_insecticide_",
		response_vars = "larvae_per_leaflet_;feeding_punctures_;adult_trap_count_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Student thesis data 4 data.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Larval Infestation data")
	r2 <- carobiner::read.excel(f, sheet="Feeding data")
	r3 <- carobiner::read.excel(f, sheet="Adult traps")

	leg <- data.frame(
		treatment = c("papa(+)_haba(+)", "papa(+)_haba(-)", "papa(+)", "papa(-)_haba(+)", "papa(-)_haba(-)", "papa(-)"),
		insecticide_used = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
		trap_crop_ = c("faba bean", "faba bean", "none", "faba bean", "faba bean", "none"),
		trap_insecticide_ = c("insecticide", "none", "no trap", "insecticide", "none", "no trap"),
		stringsAsFactors = FALSE
	)
	keep <- function(x) trimws(as.character(x[["Treatment Name"]])) %in% leg$treatment
	r1 <- r1[keep(r1), ]
	r2 <- r2[keep(r2), ]
	r3 <- r3[keep(r3), ]

	trap <- rep(leg$trap_crop_, times=3)
	has_trap <- trap == "faba bean"
	d <- data.frame(
		trial_id = "LaMolina",
		crop = "potato",
		treatment = rep(leg$treatment, times=3),
		insecticide_used = rep(leg$insecticide_used, times=3),
		trap_crop_ = trap,
		trap_insecticide_ = rep(leg$trap_insecticide_, times=3),
		intercropped = has_trap,
		intercrops = ifelse(has_trap, "faba bean", NA_character_),
		intercrop_type = ifelse(has_trap, "strip", NA_character_),
		rep = rep(1:3, each=nrow(leg)),
		adm1 = "Lima",
		location = "La Molina",
		pest_species = "Liriomyza huidobrensis",
		stress = "pests",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE,
		record_id = seq_len(nrow(leg) * 3)
	)

	rid <- function(treatment, rep) {
		i <- match(paste(trimws(as.character(treatment)), as.integer(rep)), paste(d$treatment, d$rep))
		d$record_id[i]
	}
	week_block <- function(treatment, rep, leaflet, mat, prefix) {
		out <- vector("list", 12)
		for (w in 1:12) {
			out[[w]] <- data.frame(
				record_id = rid(treatment, rep),
				week_ = as.integer(w),
				leaflet_ = as.integer(leaflet),
				value = as.numeric(mat[[paste0(prefix, w)]])
			)
		}
		do.call(rbind, out)
	}

	lv <- week_block(r1[["Treatment Name"]], r1$Repetition, r1$Leaflet, r1, "Infest_")
	fv <- week_block(r2[["Treatment Name"]], r2$Repetition, r2$Leaflet, r2, "FeedPunct_")
	leaf <- merge(
		data.frame(record_id=lv$record_id, week_=lv$week_, leaflet_=lv$leaflet_, larvae_per_leaflet_=lv$value),
		data.frame(record_id=fv$record_id, week_=fv$week_, leaflet_=fv$leaflet_, feeding_punctures_=fv$value),
		by = c("record_id", "week_", "leaflet_"),
		all = TRUE
	)
	leaf$adult_trap_count_ <- NA_real_

	trap <- vector("list", 10)
	for (w in 1:10) {
		trap[[w]] <- data.frame(
			record_id = rid(r3[["Treatment Name"]], r3$Repetition),
			week_ = as.integer(w),
			leaflet_ = NA_integer_,
			larvae_per_leaflet_ = NA_real_,
			feeding_punctures_ = NA_real_,
			adult_trap_count_ = as.numeric(r3[[paste0("tramp", w)]])
		)
	}
	tr <- do.call(rbind, trap)
	x <- rbind(leaf, tr)
	rownames(x) <- NULL

	carobiner::write_files(path, meta, d, long=x)
}
