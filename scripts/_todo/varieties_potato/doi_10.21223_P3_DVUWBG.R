# R script for "carob"
# license: GPL (>=3)

## NOTES
# Arthropod counts from malaise and pit-fall traps in experimental potato plots of variety Canchan, July to November 2006.
# The description states a completely randomized design with two landscape treatments. It places the plots in San Juan de Ihuanco and Cañete, Cañete province, 130 km south of Lima, Peru. The specimen sheet does not label locality, landscape, plot, or date, so every count is stored on one survey record for that season.
# The description says the plots were 25 m by 25 m, with 0.30 m between plants and 0.90 m between rows. Those sizes are not on the specimen sheet, and the rows are not plots, so plot area is not stored.
# Genus and species are pasted as written. Some rows repeat the genus in the species column, for example Epitrix Epitrix.
# Trophic group is stored in lower case.

## ISSUES
# No coordinates in the file.
# The landscape treatment (simple Cañete versus complex Ihuanco) is not on the specimen rows. The trophic workbook has those totals, and using it as well as the specimen sheet would double count.
# No fertilizer rate, irrigation statement, or yield.
# The long table has no sampling date. order is the row order in the specimen sheet.
# write_files: missing yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. time/depth: no time/depth variables in long records. unknown variables: trap_type_, arthropod_order_ (suggested name order), arthropod_family_, arthropod_taxon_, arthropod_guild_, arthropod_count_.

## NOT PROCESSED
# subchapter 2.1_Trophic groups dataset.xlsx. Landscape sums of the specimen counts.
# 2.1_dictionary.xls.


## NEW VARIABLES
# arthropod_order_: taxonomic order, as written
# arthropod_family_: taxonomic family, as written
# arthropod_taxon_: genus and species, as written
# arthropod_guild_: trophic group
# trap_type_: malaise or pit-fall
# arthropod_count_: number of individuals


carob_script <- function(path) {

"
Replication data for: Effect of Landscape Structure on the Arthropod Community in Potato Agroecosystems of the Cañete Region

The study was conducted during the winter cropping season (from July to November 2006) in the localities of San Juan de Ihuanco and Cañete in the Cañete province. The valley is situated 130 km south of Lima at the central coast of Peru. During the study period, the average temperature was 15.6 ºC (13.2-18.0 ºC) with a mean monthly rainfall of 0.6 mm and a relative humidity of >80%. Three experimental potato fields were set-up in each landscape (complex and simple), with two plots by field. The experiment was laid out in a completely randomized design with two treatments (complex and simple landscapes) and six replicates. Each replicated plot had a size of 25 x 25 m (625 m2). All plots were separated with a 4-m distance between each of the plots and a 4-m vegetation-free area around the plots. The commercial potato variety Canchan was planted at a density of 0.3 m between plants and 0.9 m between rows. No insecticides were used in all plots. The trial was performed from July to November corresponding to the planting cycle of the winter season. Two stationary insect sampling methods (Malaise and pitfall traps) that provide information on different arthropod groups in the same habitat were employed. Pitfall traps used consisted of a transparent plastic container (500 ml), half-filled with a solution of water, formalin (4 %), and detergent. A set of 3 pitfall traps were installed at the center of each experimental plot, forming a line with a minimum distance between traps of 5 m. Pitfall trapping comprised periods of seven consecutive days at two-weekly intervals for 7 times during the potato growing season. Counts from pitfall traps within each plot were pooled and treated as one observation for analyses. Malaise traps were used to sample aerial arthropods. They consisted of open-sided nylon-mesh tents with a collecting jar on top to intercept flying arthropods. The slanted roof of the trap directs arthropods upwards towards the highest point of the trap, into the collecting jar. One Malaise trap was installed per plot and used in two-week intervals for a period of 5 days of sampling for a total of 5 times from vegetation (GS30) to crop maturity (GS80) growth stages. Arthropods collected from potato plots using the two methods were identified and classified into the smallest possible taxa using available keys and guides for the different taxa.
"

	uri <- "doi:10.21223/P3/DVUWBG"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "completely randomized design",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "arthropod_count_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "subchapter 2.1_Arthropod community dataset.xls"]
	r <- carobiner::read.excel(f)
	genus <- trimws(as.character(r[[4]]))
	species <- trimws(as.character(r[[5]]))
	taxon <- trimws(paste(genus, species))

	d <- data.frame(
		trial_id = "Canete_2006",
		record_id = 1L,
		crop = "potato",
		variety = "Canchan",
		treatment = "Canchan",
		country = "Peru",
		adm1 = "Lima",
		adm2 = "Cañete",
		location = "Cañete",
		# Description: July to November 2006.
		planting_date = "2006-07",
		harvest_date = "2006-11",
		on_farm = FALSE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	long <- data.frame(
		record_id = 1L,
		order = seq_len(nrow(r)),
		trap_type_ = trimws(as.character(r$Trap)),
		arthropod_order_ = trimws(as.character(r[[2]])),
		arthropod_family_ = trimws(as.character(r[[3]])),
		arthropod_taxon_ = taxon,
		arthropod_guild_ = tolower(trimws(as.character(r[["trophic group"]]))),
		arthropod_count_ = as.integer(r$number)
	)

	carobiner::write_files(path, meta, d, long = long)
}
