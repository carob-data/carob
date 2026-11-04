# R script for "carob"
# license: GPL (>=3)

## NOTES
# Standard evaluation of advanced clones for leaf miner fly. The abstract says the readings are the accumulative percentage of foliage damage, three readings on two exposures, under intermediate and high pressure.
# The sheet has two header rows under the column names. Those rows are dropped. Reaction type is the summary class on each clone.

## ISSUES
# The dataset coverage gives degree-and-minute positions for Cañete and Ica, not one trial coordinate. Sites need georeferencing.
# No fertilizer, irrigation statement, planting date, harvest date, or yield.
# Country is Peru from the dataset geographic coverage.
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variables: reaction_
# time/depth: no time/depth variables in long records?
# unknown variables: pressure_, exposure_, foliar_damage_percent_

## NOT PROCESSED
# Primary Trait.
# Data_Dictionary.xls: the trait method, not plot rows.


carob_script <- function(path) {

"
Dataset for: Standard Evaluation Trial for Leaf Miner Fly

Adult females of L. huidobrensis use their ovipositor to make holes on the top and bottom sides of the leaves, causing the production of exudates that feed adult males and females. The larval stage is the most destructive because the mining reduces the photosynthetic ability of the plant. Larval damage in a growing plant follows a well-established pattern: the larvae attack the plant from the bottom, and later the rest of the plant becomes infested as well. The rate and level of infestation of plants are affected by varietal differences, age, and the physiological state of the plant. The procedure for evaluation of advanced clones to Leaf Miner Fly (LMF) resistance involves trials over 2 seasons under moderate and high adult infestation. All evaluation trials include a susceptible local variety as a control. However, in the case of evaluation under high infestation, a resistant variety should be included as well. The LMF population level was monitored to study the population dynamic throughout the experimental exposure. Three readings of the accumulative percentage of foliage damage were made randomly on 5 individuals plants within each experimental plot to determine the level of resistance. Evaluation for Leaf Miner Fly resistance was conducted for 205 advance clones. The results showing different reaction types (susceptible, highly susceptible, resistant, moderately resistant and highly resistant).
"

	uri <- "doi:10.21223/P3_E6A7NG"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "foliar_damage_percent_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Leaf Miner Fly.xls"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	names(r) <- trimws(names(r))
	clone <- trimws(as.character(r$Clone))
	ok <- !is.na(clone) & nzchar(clone) & clone != "Clone"
	r <- r[ok, ]
	clone <- clone[ok]
	reaction <- trimws(as.character(r[["Reaction Type"]]))

	d <- data.frame(
		trial_id = "LMF_standard_evaluation",
		country = "Peru",
		crop = "potato",
		variety = clone,
		variety_code = clone,
		treatment = clone,
		reaction_ = reaction,
		stress = "pests",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$plot_id <- paste(d$variety, seq_len(nrow(d)), sep = "_")

	# Column order after the two header rows: intermediate exposure 1 and 2, then high exposure 1 and 2, three evaluations each.
	blocks <- data.frame(
		start = c(4, 7, 10, 13),
		pressure = c("intermediate", "intermediate", "high", "high"),
		exposure = c(1, 2, 1, 2)
	)
	parts <- list()
	for (b in seq_len(nrow(blocks))) {
		for (ev in 1:3) {
			raw <- trimws(as.character(r[[blocks$start[b] + ev - 1]]))
			val <- rep(NA_real_, length(raw))
			numok <- !is.na(raw) & grepl("^-?[0-9]+(\\.[0-9]+)?$", raw)
			val[numok] <- as.numeric(raw[numok])
			parts[[length(parts) + 1]] <- data.frame(
				plot_id = d$plot_id,
				pressure_ = blocks$pressure[b],
				exposure_ = blocks$exposure[b],
				order = ev,
				foliar_damage_percent_ = val
			)
		}
	}
	dlong <- do.call(rbind, parts)
	dlong <- dlong[!is.na(dlong$foliar_damage_percent_), ]
	dlong$trial_id <- d$trial_id[1]
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long = dlong)
}
