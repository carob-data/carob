# R script for "carob"
# license: GPL (>=3)

## NOTES
# Paper: Rafenomanjato et al. 2023 Weed Research (doi:10.1111/wre.12578); its means match the data.
# Split-split-plot, 6 blocks: tillage/cover crop (CT, NT) x fertilisation (F1, F2) x weeding regime, 3 sub-subplots each.
# One field per season: 2016 and 2018 on the same plots, 2017 on the adjacent field (rice-maize rotation).
# 540 plots in d; long table: 1188 cover records (plot x 30-120 DAS) and 5939 species records. Metadata file is the codebook.

## ISSUES
# NPK 11-22-16 taken as N-P2O5-K2O; paper gives 17.6 kg P and 12.8 kg K (as if elements). Check.
# Manure: paper says 10 t DM/ha, codebook 5-10 t DM/ha; 10000 kg/ha used; OM_type assumed farmyard manure.
# "shoot t/ha" taken as rice straw dry matter (dmy_residue); fresh/dry basis is not stated.
# missing.plant often disagrees with tiller and panicle counts (e.g. 102 of 105 hills empty, 2.5 t/ha); kept.
# Likely data errors, kept: tot.weeds is not the sum of weed groups in 3 plots.
# 2 CT weedy plots (2017) with yield 0; one has no tillers, so stem/spike_density 0 is flagged (min 500).
# harvest_date not reported (4-month cycle); left NA.
# Weed-free NT plots have per-species cover but no total weed cover (WeedCover empty); kept as is.

## NEW VARIABLES
# weed_management: weeding regime ("weedy", "weed-free", "weed-stylo-free" = weeds and stylosanthes regrowth removed)
# weed_dmy: total weed dry biomass at harvest, kg/ha
# weed_dmy_grass, weed_dmy_broadleaf, weed_dmy_sedge: weed dry biomass at harvest by group, kg/ha
# cover_crop_dmy: stylosanthes regrowth dry biomass at harvest, kg/ha
# cover_crop_cover: ground cover by stylosanthes, %
# weed_cover_crop_cover: ground cover by weeds and stylosanthes together, %
# missing_hills: sown hills without rice plants, % of hills
# hills_no_panicle: hills without panicles, % of hills
# striga_hills: hills with Striga asiatica, % of hills
# white_panicle_density: white (empty) panicles, panicles/ha
# empty_grain_weight: weight of 1000 empty grains, g

## NOT PROCESSED
# Paper: rainfall 1220, 1231, 1241 mm in 2016-2018 (period not clear: season or year).
# Paper: rolled stylosanthes mulch 16 t/ha at sowing (mean over NT plots, not per plot), 7-10 seeds per hill.
# Paper: CT ploughed in May and tilled 5 cm deep in October; NT stylosanthes rolled 3 times (June, September).


carob_script <- function(path) {

"
Data on weed cover, weed biomass and rice yield in conventional and no-till low-inputs based cropping systems in Mid-West of Madagascar

This dataset contains weed cover observation in rainfed rice field considering overall weed cover and cover per weed species, rice yield and weed biomass at rice harvest to study the effect of soil cover management (tillage or no tillage) and weed management (weedy, unweeded, or weed and stylosanthes (in NT cropping systems) weeded) and weed and stylosanthes harmfulness on rainfed rice yield. We conducted this experiment from November 2015 to April 2018, consequently it covers three rainfed  rice cropping seasons.
"

	uri <- "doi:10.18167/DVN1/3UONHE"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "SSSA; CIRAD; FOFIFA",
		publication = "doi:10.1111/wre.12578",
		project = NA,
		design = "split-split-plot, 6 blocks",
		data_type = "experiment",
		treatment_vars = "land_prep_method;cover_crop;N_fertilizer;P_fertilizer;K_fertilizer;weed_management",
		response_vars = "yield;weed_dmy;weed_cover;weed_species_cover;cover_crop_dmy",
		notes = NA,
		carob_contributor = NA,
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-24",
		carob_completion = 90,
		carob_effort = 0.3
	)

	f1 <- ff[basename(ff) == "DataWeedRiceYield_AntsaPhDthesis_WeedBiomassRiceYield.xlsx"]
	f2 <- ff[basename(ff) == "DataWeedRiceYield_AntsaPhDthesis_WeedCover.xlsx"]

	r1 <- carobiner::read.excel(f1, na="n.a")         # rice yield, yield components and weed biomass at harvest, per sub-subplot
	r2 <- carobiner::read.excel(f2, guess_max=5000)   # total and per-species cover (%) per sub-subplot at 30-120 DAS

## d1 from r1: one row per sub-subplot (3 x 2.1 m)
	weeds1 <- c(weed.free="weed-free", weed.stylo.free="weed-stylo-free", weedy="weedy")[r1$weeds]
	d1 <- data.frame(
		trial_id = as.character(r1$year),
		plot_id = paste(r1$year, r1$block, r1$soil, r1$ferti, weeds1, r1$replicates, sep="_"),
		year = r1$year,
		rep = match(r1$block, LETTERS),                         # block A-F
		soil = r1$soil,
		ferti = r1$ferti,
		weed_management = weeds1,
		treatment = paste(r1$soil, r1$ferti, weeds1, sep="_"),
		missing_hills = 100 * r1$missing.plant / r1$nb.holes,   # 105 hills per sub-subplot
		hills_no_panicle = 100 * r1$zero.panic / r1$nb.holes,
		striga_hills = 100 * r1$striga / r1$nb.holes,
		striga_infected = r1$striga > 0,
		stem_density = r1$nb.till * 10000 / 6.3,                # tillers per 6.3 m2 -> per ha
		spike_density = r1$nb.panic * 10000 / 6.3,              # panicles per 6.3 m2 -> per ha
		white_panicle_density = r1$white.panic * 10000 / 6.3,
		weed_dmy_grass = r1$grass * 10,                         # g/m2 -> kg/ha; DM (paper 2.5)
		weed_dmy_broadleaf = r1$broadleaved * 10,
		weed_dmy_sedge = r1$sedge * 10,
		weed_dmy = r1$tot.weeds * 10,
		cover_crop_dmy = r1$stylo * 10,                         # DM: paper 53 g/m2 = mean over NT weed-free and weed-stylo-free
		dmy_residue = as.numeric(r1$`shoot t/ha`) * 1000,       # t/ha -> kg/ha; assumed straw DM (always < grain yield)
		seed_weight = as.numeric(r1$`200 grains full`) * 5,     # g per 200 grains (codebook "g.m-2" is wrong) -> 1000 grains
		empty_grain_weight = as.numeric(r1$`200 grains empty`) * 5,
		sterility_index = as.numeric(r1$pourcent.empty),        # % empty grains
		yield = as.numeric(r1$yield) * 1000                     # t/ha -> kg/ha, at 14% moisture (paper 2.5)
	)

## d2 from r2: total cover per sub-subplot and date (long)
	weeds2 <- c("weed-free"="weed-free", weedy="weedy")[r2$weeds]
	plot2 <- paste(r2$year, r2$block, r2$`soil management`, r2$fertilization, weeds2, r2$replicates, sep="_")
	d2 <- data.frame(
		plot_id = plot2,
		DAP = as.integer(r2$DAS),                               # days after sowing
		weed_cover_crop_cover = r2$WeedStyloCover,
		weed_cover = r2$WeedCover,
		cover_crop_cover = r2$`Stylosanthes guianensis`
	)

## d2species from r2: cover per weed species (long); empty cells = species not recorded
	spcols <- setdiff(names(r2), c("year", "DAS", "block", "fertilization", "soil management", "weeds", "replicates", "WeedStyloCover", "WeedCover", "Stylosanthes guianensis"))
	# spelling corrections and translations (French) of species names
	spfix <- c("Ageratum conyzoïde"="Ageratum conyzoides", "Alysicarpus ovalifolus"="Alysicarpus ovalifolius",
		"Aristida"="Aristida sp.", "Aristolochia"="Aristolochia sp.", "Brachairia sp."="Brachiaria sp.",
		"Cajanus scarabeoides"="Cajanus scarabaeoides", "Cassia mimosoïdes"="Cassia mimosoides",
		"Celosia trygina"="Celosia trigyna", "Chrysantellum americanum"="Chrysanthellum americanum",
		"Corchorus olitorus"="Corchorus olitorius", "Crassocephalum crepidiodes"="Crassocephalum crepidioides",
		"Cyperus (gris vert)"="Cyperus sp. (grey-green)", "Digitaria longifloria"="Digitaria longiflora",
		"Digitaria umbertii"="Digitaria humbertii", "Echinocloa colona"="Echinochloa colona",
		"espèce X"="unidentified species X", "Fimbristilis sp."="Fimbristylis sp.", "Ipomea bolusiana"="Ipomoea bolusiana",
		"Killinga erecta"="Kyllinga erecta", "Melenis repens"="Melinis repens", "Melinis munitiflora"="Melinis minutiflora",
		"Oldenlandia"="Oldenlandia sp.", "Paspalum scorbiculatum"="Paspalum scrobiculatum",
		"Tefrosia linearis"="Tephrosia linearis", "Termanus labialis"="Teramnus labialis", "Walteria indica"="Waltheria indica")
	spnames <- ifelse(spcols %in% names(spfix), spfix[spcols], spcols)
	d2species <- data.frame(
		plot_id = rep(plot2, length(spcols)),
		DAP = rep(as.integer(r2$DAS), length(spcols)),
		weed_species = rep(spnames, each=nrow(r2)),
		weed_species_cover = unlist(r2[, spcols], use.names=FALSE)
	)
	d2species <- d2species[!is.na(d2species$weed_species_cover), ]

## treatments and management (paper 2.2-2.4)
	dsoil <- data.frame(
		soil = c("CT", "NT"),
		land_prep_method = c("conventional", "none"),       # CT: ploughing in May, 5 cm tillage in October
		cover_crop = c("none", "stylosanthes"),             # S. guianensis cv. CIAT 184, living mulch
		cover_crop_used = c(FALSE, TRUE)
	)
	dferti <- data.frame(
		ferti = c("F1", "F2"),
		OM_used = TRUE,
		OM_type = "farmyard manure",                        # paper: "organic manure", in the planting holes
		OM_amount = 10000,                                  # 10 t DM/ha (paper); codebook says 5-10 t
		fertilizer_type = c("none", "NPK;urea"),
		fertilizer_amount = c(0, 80 + 50),                  # 80 kg/ha NPK 11-22-16 at sowing, 50 kg/ha urea 6 weeks later
		fertilizer_dap = c(NA, "0;42"),
		N_fertilizer = c(0, 80 * 0.11 + 50 * 0.46),
		P_fertilizer = c(0, 80 * 0.22 / 2.29),              # grade taken as P2O5
		K_fertilizer = c(0, 80 * 0.16 / 1.2051)             # grade taken as K2O
	)
	dweed <- data.frame(
		weed_management = c("weedy", "weed-free", "weed-stylo-free"),
		weeding_done = c(FALSE, TRUE, TRUE),
		weeding_times = c(0L, 12L, 12L),                    # weekly, 12 times per cycle
		weeding_method = c(NA, "manual", "manual")          # by hand and/or angady (spade)
	)

## build d and the long table
	d <- merge(d1, dsoil, by="soil", all.x=TRUE)
	d <- merge(d, dferti, by="ferti", all.x=TRUE)
	d <- merge(d, dweed, by="weed_management", all.x=TRUE)
	d$soil <- d$ferti <- NULL
	d$planting_date <- paste0(d$year - 1, "-11")          # sown in late November
	d$harvest_date <- as.character(NA)                     # not reported
	d$year <- NULL

	d$country <- "Madagascar"
	d$adm1 <- "Antananarivo"                               # GADM 4.1 units containing the paper's coordinates
	d$adm2 <- "Vakinankaratra"
	d$adm3 <- "Betafo"
	d$adm4 <- "Vinany"
	d$latitude <- -(19 + 33/60 + 26/3600)                  # paper: 19 33'26"S, 46 24'55"E, 930 m
	d$longitude <- 46 + 24/60 + 55/3600
	d$elevation <- 930
	d$geo_from_source <- TRUE
	d$geo_uncertainty <- 1000                              # experimental site

	d$on_farm <- FALSE                                     # experimental site of the SPAD research platform
	d$is_survey <- FALSE
	d$irrigated <- FALSE                                   # upland rainfed rice
	d$crop <- "rice"
	d$variety <- "Nerica 4"
	d$previous_crop <- "maize"                             # rice-maize biannual rotation
	d$crop_rotation <- "rice;maize"
	d$plot_area <- 6.3                                     # 3 x 2.1 m
	d$row_spacing <- 30                                    # 20 x 30 cm hills
	d$plant_spacing <- 20
	d$soil_type <- "Oxisol"
	d$soil_texture <- "clay loam"
	d$soil_clay <- 34                                      # 0-10 cm
	d$soil_silt <- 39
	d$soil_sand <- 27
	d$depth_top <- 0
	d$depth_bottom <- 10
	d$yield_part <- "grain"
	d$yield_moisture <- 14

	dlong <- carobiner::bindr(d2, d2species)

	carobiner::write_files(path, meta, d, long=dlong)
}
