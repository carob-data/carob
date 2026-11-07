# R script for "carob"
# license: GPL (>=3)

## NOTES
# Potato plots in San Juan de Ihuanco (complex landscape) and San Vicente de Cañete (simple landscape), winter season June to November 2007.
# Conservation strips are Galinsoga parviflora or maize. The measured rows increase with the potato row-number treatment (1, 1, 3, and 7 rows for 2, 4, 8, and 16 potato rows), so they are read as potato-plot samples. The file has no host column.
# Plot code 1, 2, 3, 4 is 2, 4, 8, and 16 potato rows. Those extra rows within a plot and repetition are not named in the dictionary. They are kept as observation_.
# INF is larval infestation per week (larvae per leaflet). FD is accumulated foliar damage per week. The study text describes foliar damage as percent mined leaf area, and the FD values fall between 0 and 100.
# Variety Canchan, no insecticide, and watering as a standard practice are stated in the dataset description.
# mean flies, mean parasitoids, and mean pupae are seven-week means. They are not the weekly larval counts. total mean is their sum and is not stored.

## ISSUES
# The repeated rows inside one landscape, cropping system, plot, and repetition have no distinguishing column.
# No coordinates. Each geographic bounding box has only a west longitude and a south latitude.
# No fertilizer rate and no yield.
# No exact planting day. The season is June to November 2007.
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date
# unknown variable: landscape_ (landscape_position?)
# unknown variables: conservation_strip_, potato_rows_, observation_, flies_mean_, parasitoids_mean_, pupae_mean_, parasitism_percent_
# invalid terms: pest_species: Liriomyza huidobrensis
# time/depth: no time/depth variables in long records?
# unknown variables: week_

## NOT PROCESSED
# total mean. It equals mean flies + mean parasitoids + mean pupae.
# 2.2_dictionary.xls: documentation or a codebook, not an observation table.


## NEW VARIABLES
# landscape_: complex or simple vegetation structure.
# conservation_strip_: Galinsoga parviflora or maize.
# potato_rows_: number of potato rows in the subplot.
# observation_: unnamed repeated row within landscape, system, plot, and repetition.
# week_: evaluation week, 1 to 7.
# flies_mean_: mean number of adult flies over seven weeks.
# parasitoids_mean_: mean number of parasitoids over seven weeks.
# pupae_mean_: mean number of pupae over seven weeks.
# parasitism_percent_: percent parasitism.


carob_script <- function(path) {

"
Replication data for: Effect of Landscape Structure and Intercropping System on the Leafminer Fly Liriomyza huidobrensis Population, Foliar Damage and Parasitism in Potato Agroecosystems of the Cañete Region

Study area. The study was conducted through the winter cropping season (from June to November 2007), in the localities of San Juan de Ihuanco (referred to complex landscape) and Cañete valley (referred to simple landscape) in the Cañete region. These localities are situated 130 km south of Lima, at the central coast of Peru. During the potato vegetation period, the average temperature was 15.6 ºC (13.2-18.0 ºC), with a mean monthly rainfall of 0.6 mm and a relative humidity of >80%. Experimental design. Two experimental fields were installed in each type of landscape with two repetitions for each intercropping system. The intercropping systems consisted of a larger potato plot divided into four sub-plots of 2, 4, 8 and 16 rows of potato (variety Canchan) (0.90 m wide and 10 m long) as host plant of L. huidobrensis separated by 2 rows of conservation strips. The conservation strips consisted either of maize as host of Liriomyza graminivora or the weed Galinsoga parviflora as host of Liriomyza sabaziae. Conservation strips were established one week after potato planting. Standard cultural practices for fertilization, watering and disease control were applied but without insecticide applications.
"

	uri <- "doi:10.21223/P3/1BVYDY"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = "Tackling Liriomyza leafmining flies: invasive pests of global proportions",
		design = NA,
		data_type = "experiment",
		treatment_vars = "landscape_;conservation_strip_;potato_rows_",
		response_vars = "pest_incidence;pest_severity",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "subchapter 2.2 _cropping system dataset.xls"]
	r <- carobiner::read.excel(f, sheet="Cropping system dataserver")

	rows <- c("1"=2, "2"=4, "3"=8, "4"=16)
	where <- c("C"="San Juan de Ihuanco", "S"="San Vicente de Cañete")
	land <- c("C"="complex", "S"="simple")
	strip <- c("P-G"="Galinsoga parviflora", "P-M"="maize")
	sys <- as.character(r[["Cropping system"]])
	loc <- unname(where[as.character(r$Landscape)])
	key <- paste(r$Landscape, sys, r$Plot, r$Repetition, sep="|")
	observation_ <- ave(seq_len(nrow(r)), key, FUN=seq_along)

	d <- data.frame(
		country = "Peru",
		adm1 = "Lima",
		adm2 = "Cañete",
		location = loc,
		trial_id = paste(loc, "2007", sep="_"),
		crop = "potato",
		variety = "Canchan",
		treatment = paste(unname(land[as.character(r$Landscape)]), sys, as.character(r$Plot), sep="_"),
		landscape_ = unname(land[as.character(r$Landscape)]),
		conservation_strip_ = unname(strip[sys]),
		potato_rows_ = as.integer(unname(rows[as.character(r$Plot)])),
		intercropped = sys == "P-M",
		intercrops = ifelse(sys == "P-M", "maize", NA_character_),
		intercrop_type = ifelse(sys == "P-M", "strip", NA_character_),
		rep = as.integer(r$Repetition),
		observation_ = as.integer(observation_),
		plot_id = paste(r$Landscape, sys, r$Plot, r$Repetition, observation_, sep="_"),
		planting_date = "2007",
		insecticide_used = FALSE,
		irrigated = TRUE,
		pest_species = "Liriomyza huidobrensis",
		stress = "pests",
		flies_mean_ = as.numeric(r[["mean flies"]]),
		parasitoids_mean_ = as.numeric(r[["mean parasitoids"]]),
		pupae_mean_ = as.numeric(r[["mean pupae"]]),
		parasitism_percent_ = as.numeric(r[["%Parasitism"]]),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE,
		record_id = seq_len(nrow(r))
	)

	one <- function(w) {
		data.frame(
			record_id = d$record_id,
			week_ = as.integer(w),
			pest_incidence = as.integer(r[[paste0("INF", w)]]),
			pest_severity = as.character(r[[paste0("FD", w)]])
		)
	}
	x <- rbind(one(1), one(2), one(3), one(4), one(5), one(6), one(7))
	rownames(x) <- NULL

	carobiner::write_files(path, meta, d, long=x)
}
