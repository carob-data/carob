# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Dataverse metadata lists accessToSources as "-76.4, -12.083333" (an apparent longitude typo for -76.94 that places the point in the mountains east of Lima); standard CIP/UNALM La Molina coordinates (-12.076289, -76.948417) are used with geo_from_source = FALSE.
# Adult yellow sticky trap counts (tramp1-tramp10) are all exact multiples of 16/3 (likely scaled from subsampled trap counts/area); values are kept as recorded in adult_trap_count_.
# Full methods (potato variety, insecticide active ingredient, fertilizer rates, irrigation, and yield) are in the UNALM thesis (Caytuiro 2015, hdl:20.500.12996/1414), which was not accessible from the raw files.

## NEW
# trap_insecticide_: insecticide regime applied to the faba bean trap crop strip ("insecticide", "none", or NA when no trap crop)
# leaflet_: sampled potato leaflet index within each plot (1-9)
# larvae_per_leaflet_: number of Liriomyza huidobrensis larvae recovered per potato leaflet (count/leaflet)
# feeding_punctures_: number of leafminer feeding punctures per potato leaflet (count/leaflet)
# adult_trap_count_: leafminer adults caught on yellow sticky traps in potato plots (count/trap, as recorded)

## MISSING
# yield, yield_moisture, yield_part, harvest_date, irrigated, N_fertilizer, P_fertilizer, K_fertilizer

## NOTES
# Field experiment at La Molina, Lima, Peru testing faba bean (Vicia faba) trap-crop strips and insecticide applications against Liriomyza huidobrensis on potato (RCBD, 6 treatments x 3 reps; Caytuiro 2015, hdl:20.500.12996/1414).
# Evaluation dates (2009-07-27 to 2009-10-13) and DAP (19 to 83 dds for weeks 1-10, implying planting_date = 2009-07-08 and 90/97 DAP for weeks 11-12) were recovered from the summary block below row 162 of the Larval Infestation sheet.
# Larvae and feeding punctures were recorded weekly (weeks 1-12) on 9 potato leaflets per plot, and yellow-sticky-trap adult catches weekly (weeks 1-10) per plot; all are stored in long with date, DAP, and order.

## NOT PROCESSED
# Variables sheet: variable and treatment definitions.
# Summary blocks below row 162 on Larval Infestation data (per-plot sums across 9 leaflets and treatment means; dates and DAP were extracted from rows 165 and 194).

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
		publication = "hdl:20.500.12996/1414",
		project = "Expanding the rational and biological control of invasive Liriomyza leafminer flies to major horticultural production systems of East Africa",
		design = "randomized complete block design",
		data_type = "experiment",
		treatment_vars = "insecticide_used;intercropped;trap_insecticide_",
		response_vars = "larvae_per_leaflet_;feeding_punctures_;adult_trap_count_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Student thesis data 4 data.xlsx"]
	r1_raw <- carobiner::read.excel(f, sheet="Larval Infestation data", col_types="text")
	r2 <- carobiner::read.excel(f, sheet="Feeding data")
	r3 <- carobiner::read.excel(f, sheet="Adult traps")

	# Extract weekly evaluation dates from row 165 (Excel serial numbers) and derive planting_date (19 DAP on 2009-07-27)
	eval_dates <- as.Date(as.numeric(unlist(r1_raw[165, paste0("Infest_", 1:12)])), origin="1899-12-30")
	planting_date <- eval_dates[1] - 19L
	eval_dap <- as.integer(eval_dates - planting_date)
	eval_dates_chr <- as.character(eval_dates)

	leg <- data.frame(
		treatment = c("papa(+)_haba(+)", "papa(+)_haba(-)", "papa(+)", "papa(-)_haba(+)", "papa(-)_haba(-)", "papa(-)"),
		insecticide_used = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
		intercropped = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
		intercrops = c("faba bean", "faba bean", NA_character_, "faba bean", "faba bean", NA_character_),
		intercrop_type = c("strip", "strip", NA_character_, "strip", "strip", NA_character_),
		trap_insecticide_ = c("insecticide", "none", "none", "insecticide", "none", "none"),
		stringsAsFactors = FALSE
	)

	r1 <- r1_raw[trimws(r1_raw[["Treatment Name"]]) %in% leg$treatment, ]
	r2 <- r2[trimws(as.character(r2[["Treatment Name"]])) %in% leg$treatment, ]
	r3 <- r3[trimws(as.character(r3[["Treatment Name"]])) %in% leg$treatment, ]

	idx <- rep(seq_len(nrow(leg)), times=3)
	d <- data.frame(
		record_id = seq_len(nrow(leg) * 3),
		trial_id = "LaMolina_2009",
		country = "Peru",
		adm1 = "Lima",
		adm2 = "Lima",
		adm3 = "La Molina",
		location = "La Molina",
		latitude = -12.076289,
		longitude = -76.948417,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		geo_source = "CIP/UNALM La Molina",
		planting_date = as.character(planting_date),
		crop = "potato",
		treatment = leg$treatment[idx],
		insecticide_used = leg$insecticide_used[idx],
		intercropped = leg$intercropped[idx],
		intercrops = leg$intercrops[idx],
		intercrop_type = leg$intercrop_type[idx],
		trap_insecticide_ = leg$trap_insecticide_[idx],
		rep = rep(1:3, each=nrow(leg)),
		pest_species = "Liriomyza huidobrensis",
		stress = "pests",
		on_farm = FALSE,
		is_survey = FALSE
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
				date = eval_dates_chr[w],
				DAP = eval_dap[w],
				order = as.integer(w),
				leaflet_ = as.integer(leaflet),
				value = as.integer(as.numeric(mat[[paste0(prefix, w)]]))
			)
		}
		do.call(rbind, out)
	}

	lv <- week_block(r1[["Treatment Name"]], r1$Repetition, r1$Leaflet, r1, "Infest_")
	fv <- week_block(r2[["Treatment Name"]], r2$Repetition, r2$Leaflet, r2, "FeedPunct_")
	leaf <- merge(
		data.frame(record_id=lv$record_id, date=lv$date, DAP=lv$DAP, order=lv$order, leaflet_=lv$leaflet_, larvae_per_leaflet_=lv$value),
		data.frame(record_id=fv$record_id, date=fv$date, DAP=fv$DAP, order=fv$order, leaflet_=fv$leaflet_, feeding_punctures_=fv$value),
		by = c("record_id", "date", "DAP", "order", "leaflet_"),
		all = TRUE
	)
	leaf$adult_trap_count_ <- NA_real_

	trap <- vector("list", 10)
	for (w in 1:10) {
		trap[[w]] <- data.frame(
			record_id = rid(r3[["Treatment Name"]], r3$Repetition),
			date = eval_dates_chr[w],
			DAP = eval_dap[w],
			order = as.integer(w),
			leaflet_ = NA_integer_,
			larvae_per_leaflet_ = NA_integer_,
			feeding_punctures_ = NA_integer_,
			adult_trap_count_ = as.numeric(r3[[paste0("tramp", w)]])
		)
	}
	dlong <- rbind(leaf, do.call(rbind, trap))
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
