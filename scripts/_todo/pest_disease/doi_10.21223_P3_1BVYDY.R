# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Dictionary mislabeling in 2.2_dictionary.xls: columns FD1-FD7 are labeled "Accumulated foliar damage", but mathematically they are weekly percent parasitism (FD_k * INF_k / 100 is an exact integer across all 714 cells, and for all 48 complex-landscape rows sum(FD * INF / 100) / 7 == mean parasitoids and 100 * sum(FD * INF / 100) / sum(INF) == %Parasitism). They are stored as parasitism_percent_ rather than pest_severity.
# Row misalignment in raw file for simple landscape (Landscape == "S", rows 50-102): summary columns (mean flies, mean parasitoids, mean pupae, total mean, %Parasitism) are shifted relative to INF1-FD7 on 37 of 54 rows (e.g. row 56 total mean = 116.71 matches row 63 mean(INF1..7)), and several S/P-G rows divide means by 6 instead of 7. Values are kept as recorded; verify with the authors/book chapter.
# Sub-plot sampling rows within each Landscape x Cropping system x Plot x Repetition have no explicit row identifier (1, 1, 3, 7 rows for 2, 4, 8, 16 potato rows, except S/P-G which has 1, 2, 4, 8 rows); numbered sequentially in observation_.

## NEW
# landscape_: vegetation structure of the surrounding landscape (complex or simple).
# conservation_strip_: plant species in the 2-row conservation strip (Galinsoga parviflora or maize).
# potato_rows_: number of potato rows in the subplot (2, 4, 8, or 16).
# observation_: sequential index of unnamed repeated sampling rows within landscape, cropping system, plot, and repetition.
# flies_mean_: mean number of emerged adult leafminer flies over seven sampling weeks (count/week).
# parasitoids_mean_: mean number of emerged adult parasitoids over seven sampling weeks (count/week).
# pupae_mean_: mean number of unemerged pupae over seven sampling weeks (count/week).
# parasitism_overall_: overall percent parasitism across the seven sampling weeks (%).
# parasitism_percent_: weekly percent parasitism in the long table (%, from raw columns FD1-FD7).

## MISSING
# yield, yield_moisture, yield_part, N_fertilizer, P_fertilizer, K_fertilizer.

## NOTES
# Field experiment in San Juan de Ihuanco (complex landscape) and San Vicente de Cañete (simple landscape), Cañete, Peru during winter 2007 (June-November; exact planting/harvest days not reported).
# Potato (cv. Canchan, 0.90 m row spacing, 10 m subplot length) in 2-, 4-, 8-, or 16-row subplots separated by 2-row conservation strips of maize or Galinsoga parviflora; standard fertilization/irrigation and no insecticides.
# Co-produced by CIP and ICIPE (book chapter Mujica 2016, ISBN 978-3-8236-1727-3). Weekly counts (INF1-INF7) and weekly parasitism (FD1-FD7) are stored in the long table with order = 1..7 and timing = "week 1".."week 7".

## NOT PROCESSED
# subchapter 2.2 _cropping system dataset.xls: total mean (equals mean flies + mean parasitoids + mean pupae).


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
		publication = "isbn:978-3-8236-1727-3",
		project = "Tackling Liriomyza leafmining flies: invasive pests of global proportions",
		design = NA,
		data_type = "experiment",
		treatment_vars = "landscape_;conservation_strip_;potato_rows_",
		response_vars = "pest_incidence;parasitism_percent_;flies_mean_;parasitoids_mean_;pupae_mean_;parasitism_overall_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "subchapter 2.2 _cropping system dataset.xls"]
	r <- carobiner::read.excel(f, sheet="Cropping system dataserver")

	rows_map <- c("1"=2L, "2"=4L, "3"=8L, "4"=16L)
	land_map <- c("C"="complex", "S"="simple")
	strip_map <- c("P-G"="Galinsoga parviflora", "P-M"="maize")

	# Locality coordinates in Cañete Province, Lima, Peru (Dataverse metadata gives single-corner boxes: -76.45/-13.0833 and -76.40/-13.0833)
	geo <- data.frame(
		Landscape = c("C", "S"),
		location = c("San Juan de Ihuanco", "San Vicente de Cañete"),
		longitude = c(-76.4792, -76.3853),
		latitude = c(-13.0250, -13.0756),
		geo_uncertainty = c(5000, 5000),
		geo_source = "locality lookup (Cañete Province, Peru)"
	)
	gidx <- match(as.character(r$Landscape), geo$Landscape)
	if (anyNA(gidx)) stop("unmapped landscape code")

	sys <- as.character(r[["Cropping system"]])
	potato_rows <- unname(rows_map[as.character(r$Plot)])
	key <- paste(r$Landscape, sys, r$Plot, r$Repetition, sep="|")
	obs <- as.integer(ave(seq_len(nrow(r)), key, FUN=seq_along))

	d <- data.frame(
		country = "Peru",
		adm1 = "Lima",
		adm2 = "Cañete",
		location = geo$location[gidx],
		longitude = geo$longitude[gidx],
		latitude = geo$latitude[gidx],
		geo_uncertainty = geo$geo_uncertainty[gidx],
		geo_source = geo$geo_source[gidx],
		geo_from_source = FALSE,
		trial_id = paste(geo$location[gidx], "2007", sep="_"),
		crop = "potato",
		variety = "Canchan",
		treatment = paste(unname(land_map[as.character(r$Landscape)]), sys, potato_rows, sep="_"),
		landscape_ = unname(land_map[as.character(r$Landscape)]),
		conservation_strip_ = unname(strip_map[sys]),
		potato_rows_ = potato_rows,
		intercropped = sys == "P-M",
		intercrops = ifelse(sys == "P-M", "maize", NA_character_),
		intercrop_type = ifelse(sys == "P-M", "strip", NA_character_),
		row_spacing = 90,
		plot_length = 10,
		plot_width = potato_rows * 0.9,
		plot_area = potato_rows * 0.9 * 10,
		rep = as.integer(r$Repetition),
		observation_ = obs,
		plot_id = paste(r$Landscape, sys, r$Plot, r$Repetition, obs, sep="_"),
		# Winter cropping season June to November 2007 (exact days not reported)
		planting_date = "2007-06",
		harvest_date = "2007-11",
		insecticide_used = FALSE,
		irrigated = TRUE,
		pest_species = "Liriomyza huidobrensis",
		stress = "pests",
		flies_mean_ = as.numeric(r[["mean flies"]]),
		parasitoids_mean_ = as.numeric(r[["mean parasitoids"]]),
		pupae_mean_ = as.numeric(r[["mean pupae"]]),
		parasitism_overall_ = as.numeric(r[["%Parasitism"]]),
		on_farm = FALSE,
		is_survey = FALSE,
		record_id = seq_len(nrow(r))
	)

	# INF1-INF7: weekly leafminer larval infestation counts; FD1-FD7: weekly % parasitism (mislabeled as foliar damage in dictionary)
	one <- function(w) {
		data.frame(
			record_id = d$record_id,
			order = as.integer(w),
			timing = paste("week", w),
			pest_incidence = as.integer(r[[paste0("INF", w)]]),
			parasitism_percent_ = as.numeric(r[[paste0("FD", w)]])
		)
	}
	x <- do.call(rbind, lapply(1:7, one))
	rownames(x) <- NULL

	carobiner::write_files(path, meta, d, long=x)
}
