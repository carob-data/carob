# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station chickpea trial of five actinomycetes against Fusarium wilt on the ICRISAT wilt-sick field BIL 2C (Patancheru), plus a glasshouse pot series. Field design is RCBD; cultivar JG-62. Four inoculation methods are coded M1-M4.
# Wilt counts are percent of plants with symptoms (abstract). Field counts at 17, 21, 24 and 28 DAS and glasshouse counts at 15, 20 and 29 DAS are stored in the long table linked via record_id; final counts (28 DAS field, 29 DAS glasshouse) are in wide d.
# Field rhizosphere actinomycete counts (log10, 28 DAS, two replicates) are merged onto field wide rows (including the uninoculated Control); glasshouse end-of-experiment counts (replicates 1-6, FOC code 1/2) are in the long table linked via trial_id.
# Estimated ICRISAT Patancheru coordinates (17.511, 78.275) added with geo_from_source = FALSE and geo_uncertainty = 2000 m.
# Kept under group = "agronomy" because the raw deposit is stored under data/raw/agronomy.

## ISSUES
# No grain/stover yield, fertilizer, irrigation, or planting/harvest dates in the files.
# M1-M4 are not defined in the files or Dataverse abstract (paper doi:10.1016/j.cropro.2011.03.006 behind paywall).
# The positive control described in the abstract is absent from the field wilt-count sheet (present only in the field rhizosphere population sheet).
# Glasshouse "Treatment" 1-5 is used as rep; glasshouse cultivar is not stated.
# Glasshouse population file column FOC (1 or 2) is not decoded in the docx (stored as foc_code_).
# "Fusarium oxysporum f. sp. ciceri" is not in terminag values_pathogen.csv.
# write_files: missing carob_contributor, yield, yield_moisture, irrigated, K/N/P_fertilizer, planting_date, harvest_date; invalid pathogen; unknown inoculation_method_, actinomycete_log10_, foc_code_.

## NOT PROCESSED
# Enzymatic activities and secondary metabolite.xlsx: in vitro ratings (HCN, IAA, siderophore, cellulase, protease, dual-culture antagonism).
# Metadata docx files: documentation used above.

## NEW VARIABLES
# inoculation_method_: field inoculation-method code (M1, M2, M3, M4).
# actinomycete_log10_: rhizosphere actinomycete population (log10 CFU/g soil) at 28 DAS (field) or end of glasshouse trial.
# foc_code_: glasshouse population file FOC treatment code (1 or 2).


carob_script <- function(path) {

"
Data on evaluation of actinomycete isolates obtained from herbal vermi compost for the biological control of fusarium wilt of Chickpea

Five most promising FOC antagonistic isolates (CAI-24, CAI-121, CAI-127, KAI-32 and KAI-90) were characterized for the production of siderophore, cellulase, protease, hydrocyanic acid (HCN), indole acetic acid (IAA) and antagonistic potential against Rhizoctonia bataticola, which causes dry root rot in chickpea (three strains viz. RB-6, RB-24 and RB-115) and sorghum (one strain). All of the five FOC antagonistic isolates produced siderophore and HCN, four of them (except KAI-90) produced IAA, KAI-32 and KAI-90 produced cellulase and CAI-24 and CAI-127 produced protease. In the dual-culture assay, three of the isolates, CAI-24, KAI-32 and KAI-90, also inhibited all three strains of R. bataticola in chickpea, while two of them (KAI-32 and KAI-90) inhibited the tested strain in sorghum. When the FOC antagonistic isolates were evaluated further for their antagonistic potential in the greenhouse and wilt sick field conditions on chickpea. The five most potential antagonistic actinomycetes from the in vitro and greenhouse studies were further evaluated individually for their antagonistic potential in Fusarium-infested field at ICRISAT. The field was maintained as wilt sick plot since 1980. Actinomycete isolate was inoculated by four different methods. Thus, the combination of actinomycete isolates x four methods of inoculation constituted 20 independent treatments in addition to one positive control, where no actinomycete was inoculated. Each treatment was replicated three times in randomized complete block design (RCBD). Chickpea seeds of a Fusarium wilt highly susceptible cultivar JG-62 were surface-sterilized with sodium hypochlorite (2.5% for 5 min) and rinsed with sterilized water (eight times) before being sown into the field. Incidence of Fusarium wilt disease (number of plants showing wilt symptoms to total number of plants in a plot) was recorded on 17, 21, 24 and 28 DAS till the susceptible check showed 100% mortality. Actinomycete population was also enumerated, as explained earlier, from the rhizosphere soils at 28 DAS for all the treatments.        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/5FRCLM"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = "doi:10.1016/j.cropro.2011.03.006",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "disease_incidence",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 70,
		carob_effort = 0.25
	)

	f <- ff[basename(ff) == "Copy of Evaluation of five potential antagonistic actinomycetes.xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	iso <- trimws(r$Isolate)
	method <- trimws(r$Treatment)
	w <- data.frame(
		trial_id = "BIL2C",
		seed_treatment = iso,
		inoculation_method_ = method,
		treatment = paste(method, iso),
		rep = as.integer(r[["Replication number"]]),
		d17 = as.numeric(r[["DAS 17"]]),
		d21 = as.numeric(r[["DAS 21"]]),
		d24 = as.numeric(r[["DAS 24"]]),
		d28 = as.numeric(r[["DAS 28"]])
	)
	df <- data.frame(
		trial_id = w$trial_id,
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		site = "ICRISAT field BIL 2C",
		latitude = 17.511,
		longitude = 78.275,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		crop = "chickpea",
		variety = "JG-62",
		seed_treatment = w$seed_treatment,
		inoculation_method_ = w$inoculation_method_,
		treatment = w$treatment,
		inoculant = w$seed_treatment,
		inoculated = TRUE,
		rep = w$rep,
		stress = "disease",
		pathogen = "Fusarium oxysporum f. sp. ciceri",
		yield_part = "seed",
		on_farm = FALSE,
		is_survey = FALSE
	)

	f1 <- ff[basename(ff) == "Population of the actinomycetes at the end of the field.xlsx"]
	r1 <- carobiner::read.excel(f1, na="NA")
	pop_name <- gsub(" ", "-", trimws(r1$Treatment))
	dp <- data.frame(
		trial_id = "BIL2C",
		seed_treatment = pop_name,
		inoculation_method_ = trimws(r1$Method),
		rep = as.integer(r1[["Replication number"]]),
		actinomycete_log10_ = as.numeric(r1$Log)
	)
	dfield <- merge(df, dp, by=c("trial_id", "seed_treatment", "inoculation_method_", "rep"), all=TRUE)
	dfield$country[is.na(dfield$country)] <- "India"
	dfield$adm1[is.na(dfield$adm1)] <- "Telangana"
	dfield$adm2[is.na(dfield$adm2)] <- "Sangareddy"
	dfield$location[is.na(dfield$location)] <- "Patancheru"
	dfield$site[is.na(dfield$site)] <- "ICRISAT field BIL 2C"
	dfield$latitude[is.na(dfield$latitude)] <- 17.511
	dfield$longitude[is.na(dfield$longitude)] <- 78.275
	dfield$geo_from_source[is.na(dfield$geo_from_source)] <- FALSE
	dfield$geo_uncertainty[is.na(dfield$geo_uncertainty)] <- 2000
	dfield$crop[is.na(dfield$crop)] <- "chickpea"
	dfield$variety[is.na(dfield$variety)] <- "JG-62"
	dfield$stress[is.na(dfield$stress)] <- "disease"
	dfield$pathogen[is.na(dfield$pathogen)] <- "Fusarium oxysporum f. sp. ciceri"
	dfield$yield_part[is.na(dfield$yield_part)] <- "seed"
	dfield$on_farm[is.na(dfield$on_farm)] <- FALSE
	dfield$is_survey[is.na(dfield$is_survey)] <- FALSE
	dfield$inoculated <- dfield$seed_treatment != "Control"
	dfield$inoculant <- ifelse(dfield$seed_treatment == "Control", NA_character_, dfield$seed_treatment)
	dfield$treatment[is.na(dfield$treatment)] <- dfield$seed_treatment[is.na(dfield$treatment)]

	f2 <- ff[basename(ff) == "Evaluation of five most potential antagonistic.xlsx"]
	r2 <- carobiner::read.excel(f2, na="NA")
	gg <- data.frame(
		trial_id = "glasshouse",
		seed_treatment = trimws(r2$Isolate),
		rep = as.integer(r2$Treatment),
		DAP = as.integer(r2$Days),
		wilt_percent = as.numeric(r2$DAS)
	)
	dg <- unique(gg[, c("trial_id", "seed_treatment", "rep")])
	dg$country <- "India"
	dg$adm1 <- "Telangana"
	dg$adm2 <- "Sangareddy"
	dg$location <- "Patancheru"
	dg$site <- "ICRISAT glasshouse"
	dg$latitude <- 17.511
	dg$longitude <- 78.275
	dg$geo_from_source <- FALSE
	dg$geo_uncertainty <- 2000
	dg$crop <- "chickpea"
	dg$treatment <- dg$seed_treatment
	dg$inoculant <- dg$seed_treatment
	dg$inoculated <- TRUE
	dg$stress <- "disease"
	dg$pathogen <- "Fusarium oxysporum f. sp. ciceri"
	dg$yield_part <- "seed"
	dg$on_farm <- FALSE
	dg$is_survey <- FALSE

	d <- carobiner::bindr(dfield, dg)
	d$record_id <- seq_len(nrow(d))

	# Link field and glasshouse repeated wilt counts via record_id
	fkey <- paste(d$trial_id, d$seed_treatment, ifelse(is.na(d$inoculation_method_), "", d$inoculation_method_), d$rep, sep="|")
	wkey <- paste(w$trial_id, w$seed_treatment, w$inoculation_method_, w$rep, sep="|")
	w$record_id <- d$record_id[match(wkey, fkey)]
	longf <- reshape(w[, c("record_id", "d17", "d21", "d24", "d28")],
		varying=c("d17", "d21", "d24", "d28"), v.names="wilt_percent",
		timevar="DAP", times=c(17L, 21L, 24L, 28L), direction="long")
	longf <- data.frame(
		record_id = longf$record_id,
		DAP = as.integer(longf$DAP),
		disease_incidence = as.character(longf$wilt_percent)
	)

	gkey <- paste(gg$trial_id, gg$seed_treatment, "", gg$rep, sep="|")
	longg <- data.frame(
		record_id = d$record_id[match(gkey, fkey)],
		DAP = gg$DAP,
		disease_incidence = as.character(gg$wilt_percent)
	)

	# Glasshouse population replicates (1-6) are separate from wilt pots; store with order and foc_code_
	f3 <- ff[basename(ff) == "Population of the actinomycetes at the end of greenhouse.xlsx"]
	r3 <- carobiner::read.excel(f3, na="NA")
	gp_trt <- gsub(" ", "-", trimws(r3$Treatment))
	# Add glasshouse Control plots to d if absent so record_id is never NA
	miss_g <- setdiff(unique(gp_trt), d$seed_treatment[d$trial_id == "glasshouse"])
	if (length(miss_g) > 0) {
		extra <- data.frame(
			record_id = seq(nrow(d) + 1L, length.out=length(miss_g)),
			trial_id = "glasshouse",
			seed_treatment = miss_g,
			treatment = miss_g,
			inoculant = ifelse(miss_g == "Control", NA_character_, miss_g),
			inoculated = miss_g != "Control",
			country = "India", adm1 = "Telangana", adm2 = "Sangareddy",
			location = "Patancheru", site = "ICRISAT glasshouse",
			latitude = 17.511, longitude = 78.275,
			geo_from_source = FALSE, geo_uncertainty = 2000,
			crop = "chickpea", stress = "disease",
			pathogen = "Fusarium oxysporum f. sp. ciceri",
			yield_part = "seed", on_farm = FALSE, is_survey = FALSE
		)
		d <- carobiner::bindr(d, extra)
	}
	# Map each glasshouse population row to the first glasshouse record of that treatment
	gh_idx <- which(d$trial_id == "glasshouse")
	gh_first <- tapply(d$record_id[gh_idx], d$seed_treatment[gh_idx], `[`, 1)
	longp <- data.frame(
		record_id = as.integer(unname(gh_first[gp_trt])),
		order = as.integer(r3$Replication),
		foc_code_ = as.integer(r3$FOC),
		actinomycete_log10_gh_ = as.numeric(r3$Log)
	)

	long <- carobiner::bindr(longf, longg, longp)
	rownames(d) <- NULL
	rownames(long) <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
