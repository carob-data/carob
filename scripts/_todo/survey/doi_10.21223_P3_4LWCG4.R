# R script for "carob"
# license: GPL (>=3)

## NOTES
# Participatory mapping of cultivated potato parcels in three communities of Challabamba, Paucartambo, Cusco, Peru.
# Metada Plots.txt: Codigo is community-family-parcel. Latitud and longitud are UTM zone 18 south. They are converted here to WGS84 decimal degrees (EPSG:32718).
# Area_m2 is the parcel area. Altitud is meters above sea level.

## ISSUES
# Parcel areas above the vocabulary maximum of 350 m2 are kept. These are farmer parcels.
# out of bounds: plot_area (7.506, 20575.589)

## NOT PROCESSED
# Household_Dataset_Cusco.xlsx. The dictionary has household composition and livestock, and no coordinates.
# GeographicDistributionVarieties_Cusco.xlsx, one row per variety on the same parcels.
# Variety frequency and rotation workbooks, and the community-boundary shapefiles.
# SubParcela on the plot table is empty.
# CommunityBoundaries_Cusco_Chacclabamba.CPG: not read. .cpg file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Chacclabamba.dbf: not read. .dbf file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Chacclabamba.prj: not read. .prj file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Chacclabamba.rar: not read. .rar file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Chacclabamba.sbn: not read. .sbn file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Chacclabamba.sbx: not read. .sbx file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Chacclabamba.shp: not read. .shp file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Chacclabamba.shp.xml: not read. .xml file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Chacclabamba.shx: not read. .shx file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Cochacochayoc.CPG: not read. .cpg file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Cochacochayoc.dbf: not read. .dbf file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Cochacochayoc.prj: not read. .prj file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Cochacochayoc.rar: not read. .rar file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Cochacochayoc.sbn: not read. .sbn file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Cochacochayoc.sbx: not read. .sbx file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Cochacochayoc.shp: not read. .shp file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Cochacochayoc.shp.xml: not read. .xml file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Cochacochayoc.shx: not read. .shx file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Turuyoc.CPG: not read. .cpg file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Turuyoc.dbf: not read. .dbf file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Turuyoc.prj: not read. .prj file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Turuyoc.rar: not read. .rar file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Turuyoc.sbn: not read. .sbn file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Turuyoc.sbx: not read. .sbx file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Turuyoc.shp: not read. .shp file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Turuyoc.shp.xml: not read. .xml file. Observation records are missing from d until this file is standardized.
# CommunityBoundaries_Cusco_Turuyoc.shx: not read. .shx file. Observation records are missing from d until this file is standardized.
# FrecuencyOfVarietiesByArea_A_Cusco.xlsx: not read. sheet Lista_Area (of 4): categoria, Variedad, Area m2. Observation records are missing from d until this file is standardized.
# Plots_Cusco.CPG: same table as the file of this name that is read.
# Plots_Cusco.prj: same table as the file of this name that is read.
# Plots_Cusco.rar: same table as the file of this name that is read.
# Plots_Cusco.sbn: same table as the file of this name that is read.
# Plots_Cusco.sbx: same table as the file of this name that is read.
# Plots_Cusco.shp: same table as the file of this name that is read.
# Plots_Cusco.shp.xml: not read. .xml file. Observation records are missing from d until this file is standardized.
# Plots_Cusco.shx: same table as the file of this name that is read.
# RotationRatio_Cusco.xlsx: not read. sheet data (of 2): R_Alt_100m, R_Alt_200, CategoriaFinal, Area, Country, Region, Community, Code...8, Fam, Par, Code...11, latitud, .... Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Dataset for: Participatory mapping for long-term monitoring and red listing of cultivated potato populations in Cusco, Peru.
"

	uri <- "doi:10.21223/P3/4LWCG4"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Plots_Cusco.dbf"]
	r <- foreign::read.dbf(f, as.is = TRUE)

	# WGS84 UTM zone 18 south. Metada Plots.txt.
	a <- 6378137
	finv <- 1 / 298.257223563
	k0 <- 0.9996
	e2 <- finv * (2 - finv)
	ep2 <- e2 / (1 - e2)
	x <- r$longitud - 500000
	y <- r$latitud - 10000000
	lon0 <- -75 * pi / 180
	M <- y / k0
	mu <- M / (a * (1 - e2 / 4 - 3 * e2^2 / 64 - 5 * e2^3 / 256))
	e1 <- (1 - sqrt(1 - e2)) / (1 + sqrt(1 - e2))
	phi1 <- mu +
		(3 * e1 / 2 - 27 * e1^3 / 32) * sin(2 * mu) +
		(21 * e1^2 / 16 - 55 * e1^4 / 32) * sin(4 * mu) +
		(151 * e1^3 / 96) * sin(6 * mu) +
		(1097 * e1^4 / 512) * sin(8 * mu)
	sin1 <- sin(phi1)
	cos1 <- cos(phi1)
	tan1 <- tan(phi1)
	n1 <- a / sqrt(1 - e2 * sin1^2)
	t1 <- tan1^2
	c1 <- ep2 * cos1^2
	r1 <- a * (1 - e2) / (1 - e2 * sin1^2)^1.5
	dd <- x / (n1 * k0)
	lat <- phi1 - (n1 * tan1 / r1) * (
		dd^2 / 2 -
		(5 + 3 * t1 + 10 * c1 - 4 * c1^2 - 9 * ep2) * dd^4 / 24 +
		(61 + 90 * t1 + 298 * c1 + 45 * t1^2 - 252 * ep2 - 3 * c1^2) * dd^6 / 720
	)
	lon <- lon0 + (
		dd -
		(1 + 2 * t1 + c1) * dd^3 / 6 +
		(5 - 2 * c1 + 28 * t1 - 3 * c1^2 + 8 * ep2 + 24 * t1^2) * dd^5 / 120
	) / cos1

	d <- data.frame(
		trial_id = "Cusco_potato_parcels",
		hhid = trimws(as.character(r$Codigo)),
		country = "Peru",
		adm1 = carobiner::fix_name(trimws(r$Adm1), "title"),
		adm2 = carobiner::fix_name(trimws(r$Admm2), "title"),
		adm3 = carobiner::fix_name(trimws(r$Adm3), "title"),
		location = carobiner::fix_name(trimws(r$comunidad), "title"),
		crop = "potato",
		plot_area = as.numeric(r$Area_m2),
		longitude = lon * 180 / pi,
		latitude = lat * 180 / pi,
		elevation = as.numeric(r$Altitud),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
