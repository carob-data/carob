# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rainfed upland rice trial at Ivory, Vakinankaratra, Mid-West Madagascar over three seasons (Nov 2015 - Apr 2018; harvest years 2016-2018).
# Coordinates (19°33'S, 46°25'E; ~930 m a.s.l.) from Rafenomanjato et al. (2023), doi:10.1111/wre.12578.
# F1 is farmyard manure (5-10 t DM/ha; range only, so OM_amount is omitted). F2 is F1 plus 80 kg/ha NPK 11-22-16 (N = 8.8 kg/ha; P = 80*0.22/2.29; K = 80*0.16/1.2051).
# Soil cover treatments: CT (conventional tillage) vs NT (no-tillage with Stylosanthes guianensis living cover crop).
# Weed management treatments: weedy, weed-free (weeds removed regularly), and weed-stylo-free (NT only: both weeds and stylosanthes sprouts removed).
# Rice grain yield and shoot biomass (t/ha) are converted to kg/ha (* 1000). Weed and stylosanthes biomass (g/m2) are converted to kg/ha (* 10).
# 200-grain weight (full grains) is 4.2-5.9 g (metadata says "g.m-2", a typo for g); multiplied by 5 for 1000-seed weight (seed_weight).
# Weed cover (%) and individual weed species cover (%) by days after sowing (DAS) are stored in the long table (species with cover > 0). Stylosanthes guianensis cover is kept separately in stylo_cover_.

## ISSUES
# Exact sowing/harvest dates, rice cultivar, and plot area (for 105 planting holes) are not in the data files; planting_date and harvest_date are recorded at year-month precision (Nov-Apr per dataset description).
# Yield moisture basis (fresh vs dry) and whether weed/shoot biomass are fresh or dry are not stated in the metadata sheet.
# Individual grass, broadleaved, and sedge biomass breakdowns and per-plot hole/tiller/panicle/striga counts (lacking plot area) are not mapped.

## NEW VARIABLES
# weed_management_: weed management treatment level (weedy, weed-free, weed-stylo-free)
# stylo_biomass_: biomass of Stylosanthes guianensis cover crop at rice harvest (kg/ha)
# stylo_cover_: percent ground cover of Stylosanthes guianensis on a survey date (%)

## NOT PROCESSED
# WeedBiomass&RiceYield: nb.holes, missing.plant, zero.panic, white.panic, nb.till, nb.panic (counts per 105-hole plot without plot area); grass, broadleaved, sedge (biomass breakdown of tot.weeds); 200 grains empty.
# WeedCover: WeedStyloCover (combined weed + stylosanthes cover).
# DataWeedRiceYield_AntsaPhDthesis_Metadata.xlsx: codebook, not an observation table.


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
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;N_fertilizer;P_fertilizer;K_fertilizer;weeding_done;weed_management_",
		response_vars = "yield;y_residue;seed_weight;sterility_index;weed_biomass;weed_cover",
		carob_contributor = NA,
		carob_LLM = "Gemini 4 Neon",
		carob_date = "202?-??-??",
		carob_completion = 80,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "DataWeedRiceYield_AntsaPhDthesis_WeedBiomassRiceYield.xlsx"]
	f2 <- ff[basename(ff) == "DataWeedRiceYield_AntsaPhDthesis_WeedCover.xlsx"]
	r1 <- carobiner::read.excel(f1, na=c("", "NA", "n.a"))
	r2 <- carobiner::read.excel(f2, na=c("", "NA", "n.a"))

	wm <- gsub("\\.", "-", trimws(as.character(r1$weeds)))
	yr <- as.integer(r1$year)
	pct_empty <- as.numeric(r1$pourcent.empty)

	d <- data.frame(
		trial_id = paste("Ivory", yr, sep="_"),
		plot_id = paste(yr, r1$block, r1$soil, r1$ferti, wm, r1$replicates, sep="_"),
		crop = "rice",
		country = "Madagascar",
		adm1 = "Vakinankaratra",
		location = "Ivory",
		# Coordinates and elevation from Rafenomanjato et al. (2023), doi:10.1111/wre.12578 (19°33'S, 46°25'E)
		longitude = 46.4167,
		latitude = -19.55,
		elevation = 930,
		geo_from_source = TRUE,
		# Seasons ran from Nov to Apr (Nov 2015 - Apr 2018)
		planting_date = paste0(yr - 1L, "-11"),
		harvest_date = paste0(yr, "-04"),
		block_id = as.character(r1$block),
		rep = as.integer(r1$replicates),
		treatment = paste(r1$soil, r1$ferti, wm, sep="_"),
		land_prep_method = ifelse(r1$soil == "CT", "conventional", "none"),
		fertilizer_used = r1$ferti == "F2",
		fertilizer_type = ifelse(r1$ferti == "F2", "NPK", "none"),
		# F2 = 80 kg/ha NPK 11-22-16 (oxide P2O5 and K2O converted to elemental P and K)
		N_fertilizer = ifelse(r1$ferti == "F2", 80 * 0.11, 0),
		P_fertilizer = ifelse(r1$ferti == "F2", 80 * 0.22 / 2.29, 0),
		K_fertilizer = ifelse(r1$ferti == "F2", 80 * 0.16 / 1.2051, 0),
		OM_used = TRUE,
		OM_type = "farmyard manure",
		weed_management_ = wm,
		weeding_done = wm != "weedy",
		cover_crop = ifelse(r1$soil == "NT", "stylosanthes", "none"),
		cover_crop_used = r1$soil == "NT",
		intercrops = ifelse(r1$soil == "NT" & wm != "weed-stylo-free", "stylosanthes", "none"),
		intercropped = r1$soil == "NT" & wm != "weed-stylo-free",
		yield = as.numeric(r1$yield) * 1000,
		yield_part = "grain",
		y_residue = as.numeric(r1[["shoot t/ha"]]) * 1000,
		# 200 full grains weight (g) * 5 -> 1000-grain weight (g)
		seed_weight = as.numeric(r1[["200 grains full"]]) * 5,
		sterility_index = pct_empty,
		grain_fill = 100 - pct_empty,
		weed_biomass = as.numeric(r1$tot.weeds) * 10,
		stylo_biomass_ = as.numeric(r1$stylo) * 10,
		striga_infected = as.numeric(r1$striga) > 0,
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	wm2 <- gsub("\\.", "-", trimws(as.character(r2$weeds)))
	pid2 <- paste(as.integer(r2$year), r2$block, r2[["soil management"]], r2$fertilization, wm2, r2$replicates, sep="_")
	keep2 <- pid2 %in% d$plot_id
	r2k <- r2[keep2, ]
	pid2 <- pid2[keep2]

	tot <- data.frame(
		plot_id = pid2,
		DAP = as.integer(r2k$DAS),
		weed_cover = as.numeric(r2k$WeedCover),
		stylo_cover_ = as.numeric(r2k[["Stylosanthes guianensis"]]),
		weed_species = NA_character_,
		weed_species_cover = NA_real_
	)

	idcols <- c("year", "DAS", "block", "fertilization", "soil management", "weeds", "replicates", "WeedStyloCover", "WeedCover", "Stylosanthes guianensis")
	spp <- setdiff(names(r2k), idcols)
	sp <- vector("list", length(spp))
	for (i in seq_along(spp)) {
		v <- as.numeric(r2k[[spp[i]]])
		k <- !is.na(v) & v > 0
		if (!any(k)) next
		sp[[i]] <- data.frame(
			plot_id = pid2[k],
			DAP = as.integer(r2k$DAS[k]),
			weed_cover = NA_real_,
			stylo_cover_ = NA_real_,
			weed_species = spp[i],
			weed_species_cover = v[k]
		)
	}
	sp <- sp[!vapply(sp, is.null, logical(1))]
	sp <- do.call(rbind, sp)
	long <- rbind(tot, sp)

	carobiner::write_files(path, meta, d, long=long)
}
