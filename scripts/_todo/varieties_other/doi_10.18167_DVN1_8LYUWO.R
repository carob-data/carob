# R script for "carob"
# license: GPL (>=3)

## NOTES
# Genotype means from Data_Genotype_Phenotype_Gomis_et_al_2024.csv (r/qtl: row 2 is the chromosome, row 3 is cM). One row per genotype and year.
# Crop is groundnut: the readme describes an Arachis population (Fleur11 x (A. ipaensis x A. correntina)4x) and the title says peanut.
# Country Senegal and location Nioro are the title and the dataset geographicCoverage. The readme gives no coordinates.
# Pod, haulm, and biomass weights are g/plant means. They are not converted to kg/ha. 100-seed weight (g) is stored as seed_weight (g/1000 seeds).

## ISSUES
# No plot area or plant density, so there is no yield in kg/ha. No fertilizer, planting date, or coordinates. Plant height was measured in 2021 only.
# write_files: missing variables: project. missing variables: yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: audpc_ (AUDPC?). unknown variable: pod_length_ (plot_length?). unknown variable: pod_width_ (plot_width?). unknown variables: biomass_plant_, pod_weight_, haulm_weight_, pod100_weight_, pod_constriction_, pod_beak_. invalid terms: disease: early leaf spot.

## NOT PROCESSED
# SNP columns. D1F (days to first flower). The chromosome and cM rows.

## NEW VARIABLES
# audpc_: area under the disease progress curve (readme), for early leaf spot.
# biomass_plant_, pod_weight_, haulm_weight_: g/plant means (BT, PGo, PF).
# pod100_weight_: 100-pod weight, g (P100Go).
# pod_length_ and pod_width_: mm, 2020 only (LGo, lGo). pod_constriction_ and pod_beak_: 2020 scores (Cons, Bec).


carob_script <- function(path) {

"
Genotyping and phenotyping data of 250 individuals of the Fleur11 x IpaCor AB-QTL population phenotyped during three consecutive years (2020, 2021 and 2022) at Nioro research Station, Senegal

This dataset relates to the paper \"Mapping QTLs for early leafspot resistance and yield component traits using an interspecific AB-QTL population in peanut\" published in 2024 in Frontiers in Plant Science. It gathers three years of phenotyping data collected during the rainy season (July-October) 2020, 2021, and 2022, including traits such as leafspot and yield components-related traits, as well as genotyping data for 2329 SNPs. The data is in a .csv file, with a format that fits the r/qtl package
"

	uri <- "doi:10.18167/DVN1/8LYUWO"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "flowering_days;plant_height;seed_weight",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Data_Genotype_Phenotype_Gomis_et_al_2024.csv"]
	hdr <- names(read.csv(f, sep = ";", nrows = 0, check.names = FALSE))
	hdr <- trimws(hdr)
	use <- c("Genotype",
		"D50F_2022", "AUDPC_2022", "BT_2022", "PGo_2022", "PF_2022", "P100Go_2022", "P100Gr_2022",
		"D50F_2021", "AUDPC_2021", "HP_2021", "BT_2021", "PGo_2021", "PF_2021", "P100Go_2021", "P100Gr_2021",
		"D50F_2020", "AUDPC_2020", "BT_2020", "PF_2020", "PGo_2020", "P100Go_2020", "P100Gr_2020",
		"LGo_2020", "lGo_2020", "Cons_2020", "Bec_2020")
	cc <- rep("NULL", length(hdr))
	cc[hdr %in% use] <- "character"
	r <- read.csv(f, sep = ";", na.strings = c("", "-", "NA"), check.names = FALSE, colClasses = cc)
	names(r) <- trimws(names(r))
	# Row 1 is the chromosome, row 2 is the position in cM.
	r <- r[-(1:2), ]
	r <- r[!is.na(r$Genotype) & r$Genotype != "", ]

	num <- function(x) as.numeric(gsub(",", ".", as.character(x), fixed = TRUE))

	one_year <- function(yr) {
		suf <- paste0("_", yr)
		out <- data.frame(
			variety = r$Genotype,
			trial_id = paste("Nioro", yr),
			flowering_days = num(r[[paste0("D50F", suf)]]),
			audpc_ = num(r[[paste0("AUDPC", suf)]]),
			biomass_plant_ = num(r[[paste0("BT", suf)]]),
			pod_weight_ = num(r[[paste0("PGo", suf)]]),
			haulm_weight_ = num(r[[paste0("PF", suf)]]),
			pod100_weight_ = num(r[[paste0("P100Go", suf)]]),
			seed_weight = num(r[[paste0("P100Gr", suf)]]) * 10
		)
		if (yr == 2021) out$plant_height <- num(r$HP_2021)
		if (yr == 2020) {
			out$pod_length_ <- num(r$LGo_2020)
			out$pod_width_ <- num(r$lGo_2020)
			out$pod_constriction_ <- num(r$Cons_2020)
			out$pod_beak_ <- num(r$Bec_2020)
		}
		out
	}

	fill <- function(x) {
		need <- c("plant_height", "pod_length_", "pod_width_", "pod_constriction_", "pod_beak_")
		for (nm in need) if (is.null(x[[nm]])) x[[nm]] <- NA
		x[, c("variety", "trial_id", "flowering_days", "audpc_", "biomass_plant_", "pod_weight_",
			"haulm_weight_", "pod100_weight_", "seed_weight", need)]
	}
	d <- rbind(fill(one_year(2020)), fill(one_year(2021)), fill(one_year(2022)))
	d$crop <- "groundnut"
	d$treatment <- d$variety
	# Readme: lines from Fleur11 x (A. ipaensis x A. correntina)4x.
	d$variety_pedigree <- "Fleur11 x (A. ipaensis x A. correntina)4x"
	d$country <- "Senegal"
	d$location <- "Nioro"
	d$disease <- "early leaf spot"
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
