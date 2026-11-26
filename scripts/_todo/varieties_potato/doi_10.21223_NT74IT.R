# R script for "carob"
# license: GPL (>=3)

## NOTES
# Association-panel trial in Ica, Peru, 2017. The description states an augmented block design and two water regimes, NI (normal irrigation) and REC (water deficit with recovery).
# TTYA (0..45.02, median 10.18) is total tuber yield adjusted in t/ha (matching TTWPL median 0.275 kg/plant * ~37,037 plants/ha / 1000). Stored as kg/ha (TTYA * 1000).
# TDMC (11.39..29.27 %) is tuber dry matter content, stored as yield_moisture = 100 - TDMC.
# Metabolite columns are the NMR signals in Data S2 (unit not stated). They are one row per genotype and water regime.
# Ica, Peru coordinates added from gazetteer (geo_from_source = FALSE).

## ISSUES
# No exact planting/harvest calendar dates (only year 2017) and no fertilizer rates.
# Data S2 metabolite rows are stacked separately from Data S1 morphological rows (so yield is NA on metabolite rows).

## NOT PROCESSED
# Data S3 SNP marker set and Data S4 fumarate-associated genes: marker tables, not plot phenotypes.
# Data S6 is the trait glossary. Repeated canopy and NDVI evaluations stay in the plot table because the glossary gives a DAP range rather than one day.
# Data S3. Metabolite single nucleotide polymorphism marker set_tpj14892-sup-0009-datas3.xlsx: not read. sheet Sheet1: Trait, Threshold, Marker, Chromosome, Position, Ref, Alt, Score. Observation records are missing from d until this file is standardized.
# Data S4. Fumarate single nucleotide polymorphism‐associated genes_tpj14892-sup-0010-datas4.xlsx: not read. sheet fumarate_protein_genes.csv: Chromosome, Start position, End position, Protein Id, Gene Id. Observation records are missing from d until this file is standardized.
# Data S6. Morphological trait definitions_tpj14892-sup-0012-datas6.xlsx: documentation or a codebook, not an observation table.

## NEW VARIABLES
# water_regime_: NI or REC.
# ttwp_: total tuber weight per plot, unit not stated in the glossary.
# Metabolite columns (names ending in underscore, from Data S2): NMR signal, unit not stated.
# female_parent, male_parent.


carob_script <- function(path) {

"
Replication Data for: Morphological and metabolic profiling of a tropical‐adapted potato association panel subjected to water recovery treatment reveals new insights into plant vigor

All 258 genotypes of the potato association panel were grown in a field trial in 2017 in Ica, Peru. The field trial was performed with triplicates in an augmented block design, with (i) control conditions (NI) and (ii) water-deficit conditions with recovery (REC), as  described in the handbook for evaluation and data management of potato clones.  All potato genotypes were profiled for 45 morphological traits and 42 central metabolites via nuclear magnetic resonance.
"

	uri <- "doi:10.21223/NT74IT"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "augmented block",
		data_type = "experiment",
		treatment_vars = "variety;water_regime_",
		response_vars = "yield;yield_moisture",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0.5
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+([eE][+-]?[0-9]+)?$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	slug <- function(x) {
		x <- gsub("\u03b1", "alpha_", x)
		x <- gsub("\u03b2", "beta_", x)
		x <- gsub("[^A-Za-z0-9]+", "_", x)
		x <- gsub("_+", "_", x)
		x <- gsub("^_+|_+$", "", x)
		x <- tolower(x)
		x[!nzchar(x)] <- "metabolite"
		make.unique(paste0(x, "_"), sep="")
	}

	f1 <- ff[basename(ff) == "Data S1. Morphological trait profiles_tpj14892-sup-0007-datas1.xlsx"]
	f2 <- ff[basename(ff) == "Data S2. Metabolite profiles_tpj14892-sup-0008-datas2.xlsx"]
	f5 <- ff[basename(ff) == "Data S5. Potato association panel._tpj14892-sup-0011-datas5.xlsx"]
	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f5)

	d1 <- data.frame(
		variety = chr(r1$INSTN),
		water_regime_ = chr(r1$Treatment),
		yield = num(r1$TTYA) * 1000,
		yield_moisture = 100 - num(r1$TDMC),
		ttwp_ = num(r1$TTWP)
	)
	ped <- data.frame(
		variety = chr(r3[["CIP.code"]]),
		female_parent = chr(r3[["female.parent"]]),
		male_parent = chr(r3[["male.parent"]])
	)
	ped <- ped[!is.na(ped$variety) & !duplicated(ped$variety), ]
	d1 <- merge(d1, ped, by="variety", all.x=TRUE)

	mets <- setdiff(names(r2), c("Genotype", "Treatment"))
	d2 <- data.frame(
		variety = chr(r2$Genotype),
		water_regime_ = chr(r2$Treatment)
	)
	for (k in mets) d2[[slug(k)]] <- num(r2[[k]])

	d <- carobiner::bindr(d1, d2)
	d <- d[!is.na(d$variety) & nzchar(d$variety), ]
	d$water_regime_[is.na(d$water_regime_) | !nzchar(d$water_regime_)] <- "not recorded"
	d$treatment <- paste(d$variety, d$water_regime_)
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$yield_isfresh <- TRUE
	d$irrigated <- TRUE
	d$planting_date <- "2017"
	d$country <- "Peru"
	d$adm1 <- "Ica"
	d$location <- "Ica"
	d$latitude <- -14.0678
	d$longitude <- -75.7286
	d$geo_from_source <- FALSE
	d$trial_id <- "Ica_2017"
	d$plot_id <- as.character(seq_len(nrow(d)))
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
