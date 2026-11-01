# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station chickpea trial of five actinomycetes against Fusarium wilt on the ICRISAT wilt-sick field BIL 2C, plus a glasshouse pot series. Field design is RCBD. Variety JG-62 is named for the field sowing. Four inoculation methods are coded M1-M4. The abstract says methods were not the same as a positive control; that control is not in the wilt-count file.
# Wilt counts are percent of plants with symptoms (abstract). Field counts at 17, 21, 24 and 28 DAS are the long table. The wide row keeps the 28 DAS count. Glasshouse counts (column DAS) at 15, 20 and 29 days are a second trial; the column named Treatment is 1-5 with one row per isolate and day, and is used as the replicate.
# Field rhizosphere actinomycete counts (log10, 28 DAS, two replicates) are merged onto the field rows, including the uninoculated control, which has no wilt counts.
# This is a disease-incidence trial. It is left in agronomy because the raw files are already under data/raw/agronomy.

## ISSUES
# No grain or stover yield, fertilizer, irrigation, season, or coordinates. Georeference ICRISAT BIL 2C, Patancheru, Telangana, India.
# M1-M4 are not defined in the files or the Dataverse abstract. Do not expand them without the paper (doi:10.1016/j.cropro.2011.03.006).
# The positive control described in the abstract is missing from the wilt-count sheet.
# Glasshouse "Treatment" 1-5 may not be a replicate. Greenhouse cultivar is not stated (JG-62 is stated for the field).
# Glasshouse population file: FOC is coded 1 or 2. The docx mentions FOC, FOC- and FOC+ and does not say which number is which. Stored as foc_code_. Population replicates are 1-6, so they are not the wilt pots.
# Pathogen name is not in the terminag pathogen list.
# A reviewer may want this script under pest_disease. Changing group would make get_data() look in data/raw/pest_disease, which does not hold this download.
# inoculation_method_ is not the terminag variable method (that term is for how a value was measured).
# write_files: missing yield and the usual location, fertilizer, date and moisture fields; unknown inoculation_method_, actinomycete_log10_, foc_code_; pathogen not in the vocabulary; those names are repeated on the long table because it holds every wilt date and the population counts.

## NOT PROCESSED
# Enzymatic activities and secondary metabolite.xlsx: HCN, IAA, FOC, MP, Rhizoctonia bataticola 6, Rhizoctonia bataticola 24, Rhizoctonia bataticola 115, Sid, cellulose, protaeses. In vitro ratings. Isolate names there use spaces (CAI 24) rather than hyphens.
# Metadata docx files: the notes used above. There is no docx for the field wilt-count file.

## NEW VARIABLES
# inoculation_method_: field inoculation-method code (M1, M2, M3, M4)
# actinomycete_log10_: rhizosphere actinomycete population (log10), field at 28 DAS and glasshouse at the end of the experiment
# foc_code_: glasshouse population file column FOC (1 or 2)


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
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.8
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
		rep = as.integer(r$`Replication number`),
		d17 = as.numeric(r$`DAS 17`),
		d21 = as.numeric(r$`DAS 21`),
		d24 = as.numeric(r$`DAS 24`),
		d28 = as.numeric(r$`DAS 28`)
	)
	df <- data.frame(
		trial_id = w$trial_id,
		country = "India",
		adm1 = "Telangana",
		location = "Patancheru",
		site = "ICRISAT field BIL 2C",
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
		disease_incidence = as.character(w$d28),
		yield_part = "seed",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	longf <- reshape(w, varying=c("d17", "d21", "d24", "d28"), v.names="wilt_percent",
		timevar="DAP", times=c(17L, 21L, 24L, 28L), direction="long")
	longf$disease_incidence <- as.character(longf$wilt_percent)
	longf$wilt_percent <- NULL
	longf$id <- NULL
	longf$d17 <- longf$d21 <- longf$d24 <- longf$d28 <- NULL

	fp <- ff[basename(ff) == "Population of the actinomycetes at the end of the field.xlsx"]
	rp <- carobiner::read.excel(fp, na="NA")
	pop_name <- gsub(" ", "-", trimws(rp$Treatment))
	dp <- data.frame(
		trial_id = "BIL2C",
		seed_treatment = pop_name,
		inoculation_method_ = trimws(rp$Method),
		rep = as.integer(rp$`Replication number`),
		actinomycete_log10_ = as.numeric(rp$Log)
	)
	dfield <- merge(df, dp, by=c("trial_id", "seed_treatment", "inoculation_method_", "rep"), all=TRUE)
	dfield$country[is.na(dfield$country)] <- "India"
	dfield$adm1[is.na(dfield$adm1)] <- "Telangana"
	dfield$location[is.na(dfield$location)] <- "Patancheru"
	dfield$site[is.na(dfield$site)] <- "ICRISAT field BIL 2C"
	dfield$crop[is.na(dfield$crop)] <- "chickpea"
	dfield$variety[is.na(dfield$variety)] <- "JG-62"
	dfield$stress[is.na(dfield$stress)] <- "disease"
	dfield$pathogen[is.na(dfield$pathogen)] <- "Fusarium oxysporum f. sp. ciceri"
	dfield$yield_part[is.na(dfield$yield_part)] <- "seed"
	dfield$on_farm[is.na(dfield$on_farm)] <- FALSE
	dfield$is_survey[is.na(dfield$is_survey)] <- FALSE
	dfield$geo_from_source[is.na(dfield$geo_from_source)] <- FALSE
	dfield$inoculated <- dfield$seed_treatment != "Control"
	dfield$inoculant <- ifelse(dfield$seed_treatment == "Control", NA_character_, dfield$seed_treatment)
	dfield$treatment[is.na(dfield$treatment)] <- dfield$seed_treatment[is.na(dfield$treatment)]

	fg <- ff[basename(ff) == "Evaluation of five most potential antagonistic.xlsx"]
	rg <- carobiner::read.excel(fg, na="NA")
	gg <- data.frame(
		trial_id = "glasshouse",
		seed_treatment = trimws(rg$Isolate),
		rep = as.integer(rg$Treatment),
		DAP = as.integer(rg$Days),
		wilt_percent = as.numeric(rg$DAS)
	)
	dg <- gg[gg$DAP == 29, ]
	dg$disease_incidence <- as.character(dg$wilt_percent)
	dg$wilt_percent <- NULL
	dg$DAP <- NULL
	dg$country <- "India"
	dg$adm1 <- "Telangana"
	dg$location <- "Patancheru"
	dg$site <- "ICRISAT glasshouse"
	dg$crop <- "chickpea"
	dg$treatment <- dg$seed_treatment
	dg$inoculant <- dg$seed_treatment
	dg$inoculated <- TRUE
	dg$stress <- "disease"
	dg$pathogen <- "Fusarium oxysporum f. sp. ciceri"
	dg$yield_part <- "seed"
	dg$on_farm <- FALSE
	dg$is_survey <- FALSE
	dg$geo_from_source <- FALSE

	longg <- data.frame(
		trial_id = gg$trial_id,
		seed_treatment = gg$seed_treatment,
		rep = gg$rep,
		DAP = gg$DAP,
		disease_incidence = as.character(gg$wilt_percent)
	)

	fgp <- ff[basename(ff) == "Population of the actinomycetes at the end of greenhouse.xlsx"]
	rgp <- carobiner::read.excel(fgp, na="NA")
	longp <- data.frame(
		trial_id = "glasshouse",
		seed_treatment = gsub(" ", "-", trimws(rgp$Treatment)),
		rep = as.integer(rgp$Replication),
		foc_code_ = as.integer(rgp$FOC),
		actinomycete_log10_ = as.numeric(rgp$Log)
	)

	d <- carobiner::bindr(dfield, dg)
	long <- carobiner::bindr(longf, longg, longp)

	carobiner::write_files(path, meta, d, long=long)
}
