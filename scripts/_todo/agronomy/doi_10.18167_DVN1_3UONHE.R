# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rice yield (t/ha) and total weed biomass (g/m2) from the biomass workbook, stored as kg/ha (biomass times 10). The file does not say whether weed biomass is fresh or dry.
# F2 is 80 kg/ha of NPK 11/22/16 (metadata sheet). N is 8.8 kg/ha. P and K use the oxide factors (P2O5/2.29, K2O/1.2051). F1 is manure only, so mineral N, P, and K are 0.
# Manure is described as 5 to 10 t DM/ha, a range, so OM_amount is not set. OM_used is TRUE.
# Country Madagascar, state Vakinankaratra, and city Ivory are the dataset geographicCoverage. The description says rainfed rice, so irrigated is FALSE. No coordinates are in the files.
# Weed cover (percent) by days after sowing is the long table. Species rows are cover greater than 0.

## ISSUES
# shoot t/ha is not defined in the metadata sheet, so it is not mapped. Tillers, panicles, and holes have no plot area. No coordinates or dates in the files.
# write_files: missing variables: project. missing variables: yield_moisture, geo_from_source, latitude, longitude, planting_date, harvest_date. unknown variables: species_cover_.

## NOT PROCESSED
# shoot t/ha, 200-grain weights (g/m2, not a seed weight), striga counts, and tiller and panicle counts.

## NEW VARIABLES
# species_cover_: percent cover of one species on a survey date. Zeros are omitted.


carob_script <- function(path) {

"
Data on weed cover, weed biomass and rice yield in conventional and no-till low-inputs based cropping systems in Mid-West of Madagascar

This dataset contains weed cover observation in rainfed rice field considering overall weed cover and cover per weed species, rice yield and weed biomass at rice harvest to study the effect of soil cover management (tillage or no tillage) and weed management (weedy, unweeded, or weed and stylosanthes (in NT cropping systems) weeded) and weed and stylosanthes harmfulness on rainfed rice yield. We conducted this experiment from November 2015 to April 2018, consequently it covers three rainfed  rice cropping seasons.
"

	uri <- "doi:10.18167/DVN1/3UONHE"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIRAD",
		publication = "doi:10.1111/wre.12578",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;land_prep_method;N_fertilizer",
		response_vars = "yield;weed_biomass;weed_cover",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.7
	)

	fy <- ff[grepl("WeedBiomass", basename(ff))][1]
	fc <- ff[grepl("WeedCover", basename(ff))][1]
	y <- carobiner::read.excel(fy)
	wc <- carobiner::read.excel(fc)

	y$weed_management_ <- gsub("-", ".", y$weeds)
	y$N_fertilizer <- ifelse(y$ferti == "F2", 80 * 0.11, 0)
	y$P_fertilizer <- ifelse(y$ferti == "F2", 80 * 0.22 / 2.29, 0)
	y$K_fertilizer <- ifelse(y$ferti == "F2", 80 * 0.16 / 1.2051, 0)
	y$fertilizer_type <- ifelse(y$ferti == "F2", "NPK", "none")

	d <- data.frame(
		trial_id = paste("Ivory", y$year, sep = "_"),
		plot_id = paste(y$year, y$block, y$soil, y$ferti, y$weed_management_, y$replicates, sep = "_"),
		crop = "rice",
		country = "Madagascar",
		location = "Ivory",
		adm1 = "Vakinankaratra",
		block_id = as.character(y$block),
		rep = as.integer(y$replicates),
		land_prep_method = ifelse(y$soil == "CT", "conventional", "none"),
		N_fertilizer = y$N_fertilizer,
		P_fertilizer = y$P_fertilizer,
		K_fertilizer = y$K_fertilizer,
		fertilizer_type = y$fertilizer_type,
		OM_used = TRUE,
		treatment = y$weed_management_,
		weeding_done = y$weed_management_ != "weedy",
		intercrops = ifelse(y$soil == "NT", "stylosanthes", "none"),
		intercropped = y$soil == "NT",
		yield = as.numeric(y$yield) * 1000,
		yield_part = "grain",
		weed_biomass = as.numeric(y$tot.weeds) * 10,
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	wc$weeds <- gsub("-", ".", wc$weeds)
	wc$plot_id <- paste(wc$year, wc$block, wc[["soil management"]], wc$fertilization, wc$weeds, wc$replicates, sep = "_")
	wc <- wc[wc$plot_id %in% d$plot_id, ]
	tot <- data.frame(
		plot_id = wc$plot_id,
		DAP = as.integer(wc$DAS),
		weed_cover = as.numeric(wc$WeedCover)
	)
	idcols <- c("year", "DAS", "block", "fertilization", "soil management", "weeds", "replicates", "WeedStyloCover", "WeedCover", "plot_id")
	spp <- setdiff(names(wc), idcols)
	sp <- vector("list", length(spp))
	for (i in seq_along(spp)) {
		v <- as.numeric(wc[[spp[i]]])
		k <- !is.na(v) & v > 0
		if (!any(k)) next
		sp[[i]] <- data.frame(
			plot_id = wc$plot_id[k],
			DAP = as.integer(wc$DAS[k]),
			weed_species = spp[i],
			species_cover_ = v[k]
		)
	}
	sp <- sp[!vapply(sp, is.null, logical(1))]
	sp <- do.call(rbind, sp)
	tot$weed_species <- NA_character_
	tot$species_cover_ <- NA_real_
	sp$weed_cover <- NA_real_
	cols <- c("plot_id", "DAP", "weed_cover", "weed_species", "species_cover_")
	long <- rbind(tot[, cols], sp[, cols])

	carobiner::write_files(path, meta, d, long = long)
}
