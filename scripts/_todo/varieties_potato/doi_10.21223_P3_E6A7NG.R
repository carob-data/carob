# R script for "carob"
# license: GPL (>=3)

## NOTES
# Standard evaluation of advanced clones for leaf miner fly (Liriomyza huidobrensis). Accumulative percentage of foliage damage (three readings on two exposures under intermediate and high pressure) is stored in the long table under pest_species = "Liriomyza huidobrensis", pest_severity (% foliage damage).
# The sheet has two header rows under the column names. Those rows are dropped. Reaction type is stored in reaction_.

## ISSUES
# Liriomyza huidobrensis is not yet in terminag values_pest; kept in pest_species per Carob guidelines so maintainers can add it.
# Dataverse coverage gives Cañete and Ica without linking sets to sites; no per-row coordinates assigned.
# No fertilizer, irrigation statement, planting date, harvest date, or yield.

## NEW VARIABLES
# reaction_: leaf miner fly resistance reaction class as recorded.
# pressure_: pest infestation pressure ("intermediate" or "high").
# exposure_: exposure season number (1 or 2).

## NOT PROCESSED
# Primary Trait.
# Data_Dictionary.xls: the trait method, not plot rows.


carob_script <- function(path) {

"
Dataset for: Standard Evaluation Trial for Leaf Miner Fly

Adult females of L. huidobrensis use their ovipositor to make holes on the top and bottom sides of the leaves, causing the production of exudates that feed adult males and females. The larval stage is the most destructive because the mining reduces the photosynthetic ability of the plant. Larval damage in a growing plant follows a well-established pattern: the larvae attack the plant from the bottom, and later the rest of the plant becomes infested as well. The rate and level of infestation of plants are affected by varietal differences, age, and the physiological state of the plant. The procedure for evaluation of advanced clones to Leaf Miner Fly (LMF) resistance involves trials over 2 seasons under moderate and high adult infestation. All evaluation trials include a susceptible local variety as a control. However, in the case of evaluation under high infestation, a resistant variety should be included as well. The LMF population level was monitored to study the population dynamic throughout the experimental exposure. Three readings of the accumulative percentage of foliage damage were made randomly on 5 individuals plants within each experimental plot to determine the level of resistance. Evaluation for Leaf Miner Fly resistance was conducted for 205 advance clones. The results showing different reaction types (susceptible, highly susceptible, resistant, moderately resistant and highly resistant).
"

	uri <- "doi:10.21223/P3/E6A7NG"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "pest_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 60,
		carob_effort = 0.4
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
		record_id = seq_along(clone),
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
		exposure = c(1L, 2L, 1L, 2L)
	)
	parts <- list()
	for (b in seq_len(nrow(blocks))) {
		for (ev in 1:3) {
			raw <- trimws(as.character(r[[blocks$start[b] + ev - 1]]))
			val <- rep(NA_real_, length(raw))
			numok <- !is.na(raw) & grepl("^-?[0-9]+(\\.[0-9]+)?$", raw)
			val[numok] <- as.numeric(raw[numok])
			keep <- !is.na(val)
			if (!any(keep)) next
			parts[[length(parts) + 1]] <- data.frame(
				record_id = d$record_id[keep],
				order = (b - 1L) * 3L + ev,
				pressure_ = blocks$pressure[b],
				exposure_ = blocks$exposure[b],
				pest_species = "Liriomyza huidobrensis",
				pest_severity = as.character(val[keep])
			)
		}
	}
	dlong <- do.call(rbind, parts)
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long = dlong)
}
