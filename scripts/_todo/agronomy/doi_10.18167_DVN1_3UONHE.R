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
# DataWeedRiceYield_AntsaPhDthesis_Metadata.xlsx: documentation or a codebook, not an observation table.

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

	f1 <- ff[basename(ff) == "DataWeedRiceYield_AntsaPhDthesis_WeedBiomassRiceYield.xlsx"]
	f2 <- ff[basename(ff) == "DataWeedRiceYield_AntsaPhDthesis_WeedCover.xlsx"]
	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

	r1$weed_management_ <- gsub("-", ".", r1$weeds)
	r1$N_fertilizer <- ifelse(r1$ferti == "F2", 80 * 0.11, 0)
	r1$P_fertilizer <- ifelse(r1$ferti == "F2", 80 * 0.22 / 2.29, 0)
	r1$K_fertilizer <- ifelse(r1$ferti == "F2", 80 * 0.16 / 1.2051, 0)
	r1$fertilizer_type <- ifelse(r1$ferti == "F2", "NPK", "none")

	d <- data.frame(
		trial_id = paste("Ivory", r1$year, sep = "_"),
		plot_id = paste(r1$year, r1$block, r1$soil, r1$ferti, r1$weed_management_, r1$replicates, sep = "_"),
		crop = "rice",
		country = "Madagascar",
		location = "Ivory",
		adm1 = "Vakinankaratra",
		block_id = as.character(r1$block),
		rep = as.integer(r1$replicates),
		land_prep_method = ifelse(r1$soil == "CT", "conventional", "none"),
		N_fertilizer = r1$N_fertilizer,
		P_fertilizer = r1$P_fertilizer,
		K_fertilizer = r1$K_fertilizer,
		fertilizer_type = r1$fertilizer_type,
		OM_used = TRUE,
		treatment = r1$weed_management_,
		weeding_done = r1$weed_management_ != "weedy",
		intercrops = ifelse(r1$soil == "NT", "stylosanthes", "none"),
		intercropped = r1$soil == "NT",
		yield = as.numeric(r1$yield) * 1000,
		yield_part = "grain",
		weed_biomass = as.numeric(r1$tot.weeds) * 10,
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	r2$weeds <- gsub("-", ".", r2$weeds)
	r2$plot_id <- paste(r2$year, r2$block, r2[["soil management"]], r2$fertilization, r2$weeds, r2$replicates, sep = "_")
	r2 <- r2[r2$plot_id %in% d$plot_id, ]
	tot <- data.frame(
		plot_id = r2$plot_id,
		DAP = as.integer(r2$DAS),
		weed_cover = as.numeric(r2$WeedCover)
	)
	idcols <- c("year", "DAS", "block", "fertilization", "soil management", "weeds", "replicates", "WeedStyloCover", "WeedCover", "plot_id")
	spp <- setdiff(names(r2), idcols)
	sp <- vector("list", length(spp))
	for (i in seq_along(spp)) {
		v <- as.numeric(r2[[spp[i]]])
		k <- !is.na(v) & v > 0
		if (!any(k)) next
		sp[[i]] <- data.frame(
			plot_id = r2$plot_id[k],
			DAP = as.integer(r2$DAS[k]),
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
