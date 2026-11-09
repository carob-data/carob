# R script for "carob"
# license: GPL (>=3)

## NOTES
# Association-panel trial in Ica, Peru, 2017. The description states an augmented block design and two water regimes, NI and REC.
# Data S6 defines TTYA as TTWPL * plant density * 1000 and does not say whether that product is t/ha or kg/ha. The number is stored as yield without multiplying by 1000.
# Metabolite columns are the NMR signals in Data S2. The sheet does not state the unit. They are one row per genotype and water regime, not per plot.

## ISSUES
# No coordinates for the Ica field.
# No fertilizer rates.
# write_files: missing carob_contributor, yield_moisture, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: water_regime_, ttwp_, and the metabolite columns. low yield (tons not kg?): potato. NA detected: yield.

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
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
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
		yield = num(r1$TTYA),
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
	d$country <- "Peru"
	d$adm1 <- "Ica"
	d$location <- "Ica"
	d$trial_id <- paste("Ica_2017", d$variety, d$water_regime_, seq_len(nrow(d)), sep="_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
