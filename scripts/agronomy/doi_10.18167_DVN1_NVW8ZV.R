# R script for "carob"
# license: GPL (>=3)

## NOTES
# Paper: Mansuy et al. 2026 bioRxiv (doi:10.64898/2026.01.26.701649) and its supplement (Tables S1-S4).
# 5 trials (3 sites x 1-2 crop cycles), RCBD, 6 inter-row treatments; yield, borer and TFI only for the 4 non-Desmodium ones.
# "ground coverage dynamics data" repeats "global ground cover"; only the Desmodium plots and 1 extra date are taken from it.
# Desmodium plots have no plot number; plot_id is built from trial, treatment and rep (rep = row order, checked on shared rows).

## ISSUES
# 40 Desmodium plots have only ground cover data (no yield reported); kept with yield NA.
# Ratoon crops: planting_date is the ratooning date (cycle start), derived from date minus age in the data.
# Saint-Benoit 14_15 cycle start (2014-07-03) is before the reported 13_14 harvest (8-15 July 2014).
# NPK grades assumed N-P2O5-K2O (French labelling); converted to elemental P and K. S unknown, left out.
# Basis of weed_species_cover (inter-row or whole plot) is not stated; species sums match neither total.
# Per-stalk borer records (30 stalks/plot) aggregated to plot means; keep per-stalk records instead?
# Readme says dates are DD/MM/YYYY, but species file uses MM/DD/YYYY (read as such).
# Likely data errors, kept: 19 rows inter-row weed+intercrop cover < weed cover; Canavalia cover in 3 CcRo rows.
# yield (91-184 t/ha) flagged out of bounds by the generic 150 t/ha limit; normal for sugarcane.
# fertilizer_amount 1152 kg/ha at Saint-Paul is above the terminag limit (1000); kept as reported.
# geo_uncertainty left NA for the two farmer fields (field size not given).

## NEW VARIABLES
# bored_stalks: stalks with at least one internode bored by Chilo sacchariphagus, % (30 stalks per plot)
# internode_number: mean number of internodes per stalk (30 stalks per plot)
# bored_internodes: mean number of bored internodes per stalk (30 stalks per plot)
# herbicide_tfi: herbicide treatment frequency index per treatment and trial (paper section 2.6), unitless
# herbicide_pre_row, herbicide_pre_interrow: number of pre-emergence herbicide applications on the row / inter-row (paper Table 2)
# herbicide_post_row, herbicide_post_interrow: number of post-emergence herbicide applications on the row / inter-row (paper Table 2)
# interrow_weed_control: inter-row management ("intercrop", "herbicide" or "none")
# intercrop_sowing_time: intercrop sowing time, months after sugarcane emergence (paper Table 2)
# intercrop_seed_rate: intercrop seed rate, kg/ha
# ratoon_number: crop cycle (0 = planted cane, 1 = first ratoon, ...)
# weed_intercrop_cover, weed_intercrop_cover_interrow: ground cover by weeds and intercrop, whole plot / inter-row, %
# weed_cover_interrow: ground cover by weeds in the inter-row, %
# intercrop_cover_interrow: ground cover by the intercrop in the inter-row, %

## NOT PROCESSED
# borer damage: per-stalk ta, ENT, ENA (aggregated to plot means, see ISSUES).
# ground cover by species: pres (equals cover > 0) and growth_type (species attribute).
# ground coverage dynamics: 629 rows identical to global ground cover (all values match on trial, treatment, rep, date).
# Paper Table 1: 10-year mean rainfall and temperature per site.
# Supp. Table S2: herbicide compounds and doses (not linked to trials or treatments).
# Paper: intercrop sowing lines (30 cm apart, Canavalia poquets 20 cm), harvested area 30 m2.


carob_script <- function(path) {

"
Jack bean (Canavalia ensiformis) intercrop to reduce weeds and stalk borer damage in sugarcane

The aim of this study was to assess the efficiency of two intercrop species, Canavalia ensiformis and Desmodium intortum, to reduce weed growth, herbicide use and damage by the stalk borer Chilo sacchariphagus in sugarcane cropping system in Reunion.  We compared six inter-row management techniques: four treatments combined the two intercrops Canavalia ensiformis or Desmodium intortum sown either early (between 0 and 2.1 months after sugarcane emergence) or late (between 1.3 and 3.7 months after sugarcane emergence), a treatment where no weeding was carried out on the inter-row, and a treatment with chemical weeding of the inter-row (CcWp). In all treatments, the sugarcane row was weeded chemically and manually. The six treatments were compared in a multilocal randomized block design with tree localities, during one or two crop cycles depending on the locality.
"

	uri <- "doi:10.18167/DVN1/NVW8ZV"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD; eRcane",
		publication = "doi:10.64898/2026.01.26.701649",
		project = NA,
		design = "RCBD; 3 (Saint-Paul) or 4 reps",
		data_type = "experiment",
		treatment_vars = "intercrops;intercrop_sowing_time;interrow_weed_control",
		response_vars = "yield;bored_stalks;herbicide_tfi;weeding_times;weed_cover;weed_cover_interrow;intercrop_cover_interrow;weed_species_cover",
		notes = NA,
		carob_contributor = NA,
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-23",
		carob_completion = 90,
		carob_effort = 0.4
	)

	f1 <- ff[basename(ff) == "borer damage.csv"]
	f2 <- ff[basename(ff) == "global ground cover.csv"]
	f3 <- ff[basename(ff) == "ground cover by growth type and species.csv"]
	f4 <- ff[basename(ff) == "ground coverage dynamics data.csv"]
	f5 <- ff[basename(ff) == "TFI and manual weeding.csv"]
	f6 <- ff[basename(ff) == "yield.csv"]

	r1 <- read.csv(f1, sep=";")   # borer damage per stalk, 30 stalks per plot
	r2 <- read.csv(f2)            # ground cover by weeds and Canavalia per rep and date (4 treatments)
	r3 <- read.csv(f3)            # ground cover per weed species, plot and date
	r4 <- read.csv(f4, sep=";")   # ground cover per rep (row order) and age, all 6 treatments
	r5 <- read.csv(f5, sep=";")   # TFI and number of manual weedings per treatment and trial
	r6 <- read.csv(f6, sep=";")   # cane yield per plot

## d1 from r1: borer damage, plot means of 30 stalks
	bor <- aggregate(cbind(ta, ENT, ENA) ~ locality + crop_cycle + plot, data=r1, FUN=mean)
	d1 <- data.frame(
		plot_id = paste(bor$locality, bor$crop_cycle, paste0("P", bor$plot), sep="_"),
		bored_stalks = bor$ta * 100,         # % of stalks with at least one bored internode
		internode_number = bor$ENT,          # internodes per stalk
		bored_internodes = bor$ENA,          # bored internodes per stalk
		pest_species = "Chilo sacchariphagus"
	)

## d2 from r2: ground cover by rep and date (CeE, CeL, CcRo, CcWp)
	# start of the crop cycle (planting or ratooning): notation_date minus age in r3,
	# and r2 dates minus r4 ages for Saint-Benoit 13_14; the same at every date
	cstart <- data.frame(
		trial_id = c("Sainte-Marie_14_15", "Sainte-Marie_15_16", "Saint-Paul_14_15", "Saint-Benoit_13_14", "Saint-Benoit_14_15"),
		planting_date = as.Date(c("2014-04-08", "2015-08-03", "2014-10-23", "2013-07-31", "2014-07-03"))
	)
	d2 <- data.frame(
		trial_id = paste(r2$locality, r2$crop_cycle, sep="_"),
		treatment = r2$treatment,
		rep = r2$bloc,
		date = as.Date(r2$notation_date, "%d/%m/%Y"),
		weed_intercrop_cover = r2$global_score_WP,          # weeds and Canavalia, whole plot
		weed_intercrop_cover_interrow = r2$global_score_IR,
		weed_cover = r2$weed_score_WP,                      # weeds, whole plot
		weed_cover_interrow = r2$weed_score_IR,
		intercrop_cover_interrow = r2$canavalia_score_IR
	)
	d2$DAP <- as.integer(d2$date - cstart$planting_date[match(d2$trial_id, cstart$trial_id)])

## d3 from r3: ground cover per weed species (not recorded at Saint-Benoit 13_14)
	# codes are not EPPO codes (readme); names from Supp. Table S3
	species <- data.frame(
		esp = c("Agcon", "Amdub", "Armex", "Asgan", "Bipil", "Cahal", "Casca", "Cepub", "Chhir", "Clhir", "Coben", "Codif", "Cosum", "Crbon", "Crcre", "Crret", "Cydac", "Cylep", "Cyrot", "Cysp1", "Devir", "Digsp", "Dihor", "Emson", "Euhet", "Inhir", "Iphed", "Ipobs", "Lacam", "Liglu", "Memax", "Mijal", "Mipud", "Mocha", "Oxcor", "Pafoe", "Pascr", "Pasfoe", "Passp", "Phnir", "Rococ", "Sebar", "Seocc", "Siacu", "Sigla", "Siori", "Siret", "Solyc", "Somau", "Sonig", "Telab", "Tepur"),
		weed_species = c("Ageratum conyzoides", "Amaranthus dubius", "Argemone mexicana", "Asystasia gangetica", "Bidens pilosa", "Cardiospermum halicacabum", "Cajanus scarabaeoides", "Centrosema pubescens", "Chamaesyce hirta", "Clidemia hirta", "Commelina benghalensis", "Commelina diffusa", "Conyza sumatrensis", "Croton bonplandianus", "Crassocephalum crepidioides", "Crotalaria retusa", "Cynodon dactylon", "Cyclospermum leptophyllum", "Cyperus rotundus", "Cyperus sp.", "Desmanthus virgatus", "Digitaria sp.", "Digitaria horizontalis", "Emilia sonchifolia", "Euphorbia heterophylla", "Indigofera hirsuta", "Ipomoea hederifolia", "Ipomoea obscura", "Lantana camara", "Litsea glutinosa", "Megathyrsus maximus", "Mirabilis jalapa", "Mimosa pudica", "Momordica charantia", "Oxalis corniculata", "Paederia foetida", "Paspalum scrobiculatum", "Passiflora foetida", "Paspalum sp.", "Phyllanthus niruroides", "Rottboellia cochinchinensis", "Setaria barbata", "Senna occidentalis", "Sida acuta", "Sida glabra", "Sigesbeckia orientalis", "Sida retusa", "Solanum lycopersicum", "Solanum mauritianum", "Solanum nigrum", "Teramnus labialis", "Tephrosia purpurea")
	)
	d3 <- data.frame(
		plot_id = paste(r3$locality, r3$crop_cycle, r3$plot, sep="_"),
		date = as.Date(r3$notation_date, "%m/%d/%Y"),   # MM/DD/YYYY, not DD/MM/YYYY as in the readme
		DAP = as.integer(round(r3$age * 30.5)),          # age is days / 30.5 in this file
		weed_species = species$weed_species[match(r3$esp, species$esp)],
		weed_species_cover = r3$cover
	)

## d4 from r4: ground cover by age, all 6 treatments (no rep and no date in the file)
	# rep = row order within trial x treatment x age; matches r2 bloc on all shared rows
	trial4 <- sub("^(.*) 20(..)-(..)$", "\\1_\\2_\\3", r4$trial)
	d4 <- data.frame(
		trial_id = trial4,
		treatment = r4$treatment,
		rep = ave(seq_len(nrow(r4)), trial4, r4$treatment, r4$age, FUN=seq_along),
		DAP = as.integer(round(r4$age * 365 / 12)),      # age is days / (365/12) in this file
		weed_intercrop_cover = r4$cover,                 # equals global_score_WP in r2
		weed_intercrop_cover_interrow = r4$global_score_IR,
		weed_cover = r4$weed_score_WP,
		weed_cover_interrow = r4$weed_score_IR,
		intercrop_cover_interrow = r4$intercrop_score_IR # Canavalia or Desmodium
	)
	d4$date <- cstart$planting_date[match(d4$trial_id, cstart$trial_id)] + d4$DAP

## d5 from r5: TFI and manual weeding per treatment and trial
	d5 <- data.frame(
		trial_id = paste(r5$locality, r5$crop_cycle, sep="_"),
		treatment = r5$treatment,
		herbicide_tfi = r5$TFI,
		weeding_times = as.integer(r5$manual_weeding),
		weeding_method = "manual"
	)

## d6 from r6: cane yield per plot
	d6 <- data.frame(
		trial_id = paste(r6$locality, r6$crop_cycle, sep="_"),
		plot_id = paste(r6$locality, r6$crop_cycle, r6$plot, sep="_"),
		treatment = r6$treatment,
		rep = r6$bloc,
		yield = r6$yield * 1000                          # t/ha -> kg/ha, fresh stalks from the 2 central rows
	)

## dtrt: treatments per trial, paper Table 2 (order DiE, DiL, CeE, CeL, CcRo, CcWp)
	trt <- c("DiE", "DiL", "CeE", "CeL", "CcRo", "CcWp")
	dtrt <- data.frame(
		trial_id = rep(c("Saint-Paul_14_15", "Saint-Benoit_13_14", "Saint-Benoit_14_15", "Sainte-Marie_14_15", "Sainte-Marie_15_16"), each=6),
		treatment = rep(trt, 5),
		intercrop_sowing_time = c(0, 1.3, 0, 1.3, NA, NA,  1.2, 2.2, 1.2, 2.2, NA, NA,  0.8, 1.8, 0.8, 1.8, NA, NA,  1.4, 2.4, 1.4, 2.4, NA, NA,  2.1, 3.7, 2.1, 3.7, NA, NA),
		herbicide_pre_row = as.integer(c(1,1,1,1,1,1,  1,1,1,1,1,1,  0,1,0,1,0,1,  1,1,1,1,1,1,  0,0,0,0,0,0)),
		herbicide_pre_interrow = as.integer(c(0,1,0,1,1,1,  1,1,1,1,0,1,  0,0,0,0,0,1,  0,0,0,0,0,1,  0,0,0,0,0,0)),
		herbicide_post_row = as.integer(c(0,1,0,1,1,1,  2,3,2,3,3,3,  1,1,0,0,1,1,  1,1,1,1,1,1,  1,1,1,1,1,1)),
		herbicide_post_interrow = as.integer(c(0,0,0,0,0,1,  0,1,0,1,0,3,  1,2,0,1,0,1,  0,0,0,0,0,1,  0,1,0,1,0,1))
	)
	dtrt$herbicide_used <- rowSums(dtrt[, grep("^herbicide_p", names(dtrt))]) > 0
	dtrt$intercrops <- c(DiE="desmodium", DiL="desmodium", CeE="jack bean", CeL="jack bean", CcRo="none", CcWp="none")[dtrt$treatment]
	dtrt$interrow_weed_control <- c(DiE="intercrop", DiL="intercrop", CeE="intercrop", CeL="intercrop", CcRo="none", CcWp="herbicide")[dtrt$treatment]
	dtrt$intercrop_type <- ifelse(dtrt$intercrops == "none", NA, "row")   # sown on 2 lines in each inter-row
	dtrt$intercrop_seed_rate <- c(DiE=5.3, DiL=5.3, CeE=116.7, CeL=116.7, CcRo=NA, CcWp=NA)[dtrt$treatment]
	# early intercrops at Saint-Paul were natural regrowth from a previous trial, not sown
	sp_regrowth <- dtrt$trial_id == "Saint-Paul_14_15" & dtrt$treatment %in% c("DiE", "CeE")
	dtrt$intercrop_seed_rate[sp_regrowth] <- NA

## dtrial: site and management per trial, paper Table 1 and Supp. Table S1
	dtrial <- data.frame(
		trial_id = c("Sainte-Marie_14_15", "Sainte-Marie_15_16", "Saint-Paul_14_15", "Saint-Benoit_13_14", "Saint-Benoit_14_15"),
		location = c("Sainte-Marie", "Sainte-Marie", "Saint-Paul", "Saint-Benoit", "Saint-Benoit"),
		ratoon_number = c(0L, 1L, 1L, 2L, 3L),
		on_farm = c(FALSE, FALSE, TRUE, TRUE, TRUE),       # Sainte-Marie is a research station, others farmer fields
		irrigated = c(TRUE, TRUE, TRUE, FALSE, FALSE),
		irrigation_method = c("sprinkler", "sprinkler", "sprinkler", NA, NA),
		latitude = c(-20.90, -20.90, -21.07, -21.05, -21.05),
		longitude = c(55.53, 55.53, 55.28, 55.69, 55.69),
		geo_uncertainty = c(1000, 1000, NA, NA, NA),       # research station
		elevation = c(68, 68, 545, 150, 150),
		soil_type = c("Nitisol", "Nitisol", "Andic Umbrisol", "Nitisol", "Nitisol"),
		harvest_date = c("2015-08", "2016-08", "2015-10", "2014-07", "2015-07"),   # 3-5 Aug, 1-4 Aug, 16-19 Oct, 8-15 Jul, 9-13 Jul
		# 2 splits of NPK (+ urea 46% N); grades taken as N-P2O5-K2O
		fertilizer_type = c("NPK", "NPK;urea", "NPK;urea", "NPK", "NPK"),
		fertilizer_amount = c(410+200, 600+300+70, 650+285+217, 600+200, 600+300),
		N_fertilizer = c(610*0.18, 900*0.15 + 70*0.46, 935*0.20 + 217*0.46, 600*0.09 + 200*0.18, 900*0.18),
		P_fertilizer = c(610*0.07, 900*0.12, 935*0.09, 600*0.15 + 200*0.07, 900*0.07) / 2.29,
		K_fertilizer = c(610*0.30, 900*0.24, 935*0.27, 600*0.22 + 200*0.30, 900*0.30) / 1.2051
	)

## build d: yield plots plus the Desmodium plots (cover data only)
	d <- merge(d6, d1, by="plot_id", all.x=TRUE)
	d <- merge(d, d5, by=c("trial_id", "treatment"), all.x=TRUE)
	des <- unique(d4[d4$treatment %in% c("DiE", "DiL"), c("trial_id", "treatment", "rep")])
	des$plot_id <- paste(des$trial_id, des$treatment, paste0("r", des$rep), sep="_")
	d <- carobiner::bindr(d, des)
	d <- merge(d, dtrt, by=c("trial_id", "treatment"), all.x=TRUE)
	d <- merge(d, dtrial, by="trial_id", all.x=TRUE)
	d <- merge(d, cstart, by="trial_id", all.x=TRUE)
	d$planting_date <- as.character(d$planting_date)   # ratooning date for ratoon crops

	d$country <- "Réunion"
	d$crop <- "sugarcane"
	d$variety <- "R579"
	d$intercropped <- d$intercrops != "none"
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
	d$row_spacing <- 150      # 4 rows of 10 m, 1.5 m apart
	d$plot_area <- 60
	d$yield_part <- "stems"
	d$yield_moisture <- as.numeric(NA)   # not reported; fresh cane weight
	d$yield_isfresh <- TRUE

## long table: ground cover (r2, plus r4 rows not in r2) and species cover (r3)
	keys <- rbind(d6[, c("trial_id", "treatment", "rep", "plot_id")], des)
	d2 <- merge(d2, keys, by=c("trial_id", "treatment", "rep"))
	d4 <- merge(d4, keys, by=c("trial_id", "treatment", "rep"))
	d4new <- d4[!paste(d4$plot_id, d4$date) %in% paste(d2$plot_id, d2$date), ]
	dlong <- carobiner::bindr(d2, d4new, d3)
	dlong$trial_id <- dlong$treatment <- dlong$rep <- NULL
	dlong$date <- as.character(dlong$date)

	carobiner::write_files(path, meta, d, long=dlong)
}
