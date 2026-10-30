# R script for "carob"
# license: GPL (>=3)

## NOTES
# Niassa baseline household survey, July-August 2013. The deposit is SPSS .sav pages from one CSPro questionnaire, read with haven::read_sav.
# Country is the dataset (Niassa, Mozambique). District names are the CSPro dictionary labels for the codes that occur. All of those labels say Niassa, so adm1 is Niassa.
# The dictionary calls A14 longitude and A15 latitude. The numbers are about 13 S and 35 E, which is Niassa, so A14 is latitude and A15 is longitude. The "(S)" and "(E)" marks are used. Decimal-in-minutes is stored as thousandths (970 means 0.970).
# A16 is the interview date, not a planting date.

## ISSUES
# The crop, sweetpotato, sales, and diet pages have 402 rows but no household-number column that matches A05. Page 9's HHID is 1 or 2, not the household number. Those pages were not attached by row order.
# Ward and village are empty on the identification page. Some household numbers are missing, and some are repeated.
# A few coordinates fall in Malawi, Tanzania, or Angola, or not on land, and elevation runs from 25 m to 6061 m. Those values are left as recorded.
# write_files: missing carob_contributor. unknown variable: interview_date_. out of bounds: elevation (25, 6061). coordinates not on land: Mozambique. coordinates in wrong country include Malawi, Angola, and Tanzania. duplicates. NA detected: hhid.

## NOT PROCESSED
# Pages 2-22 of the baseline .sav files (membership, crop flags, sweetpotato, labor, diet, assets, livestock). They are not joined.
# Names of household heads, interviewees, enumerators, and digitizers.
# BaseLineSurvey Data Dictionary.txt and the narrative PDF.

## NEW VARIABLES
# interview_date_: date of interview (A16).


carob_script <- function(path) {

"
Dataset for: Nutritious Orange-Fleshed Sweetpotat (OFSP) For Niassa - Baseline Household Survey

Relevant information on the challenges and opportunities of the project were considered for the benefit of the project results. In fact, most of the findings from the study were used as the baseline reference for evaluating the impact of the project's intervention in the project target areas three years later, including activities on seed system, crop sales, marketing, nutrition and food security in general.  This baseline survey was conducted between from July 8 to August 23, 2013, and overall, 396 households were visited in 24 villages distributed across the 8 districts surveyed. Overall, about 90% of the farmers interviewed in the present study had agriculture as the most important activity. Among the farmers who mentioned sweetpotato, only about 12% of the households produced OFSP due to earlier dissemination efforts by IIAM and partners.  However, only 2.3% of households' total landholding was under sweetpotato, while 1.3% was plated with OFSP. Although the high number of farmers producing sweetpotatoes, the area under production is relatively small, with an average of 300 m2 per household. In general, sweetpotato was mostly (79%) produced for consumption, while 21% of the interviewees mentioned the sale.  Overall, farmer-to-farmer exchange of planting material was the most important source of sweetpotato planting material. Most farmers obtain sweetpotato planting material from their own plots (69%). However, some considerable farmers depend on vines from neighbors (23%), and about 7% from relatives. There was not any reference related to the selling of planting material. In seed system, vine conservation is one of the critical activities during the sweetpotato production cycle. Vines must be read for planting even after a long dry season. According to the results from this study, most (65%) of sweetpotato is planted 2-3 after the beginning of the raining season. At this moment, vines must be ready for planting. In general, the majority (94%) of sweetpotato farmers usually conserve their vines. The typical methods of conservation include leave some portions of the plot without harvesting (60%) for later sprouting, establish small fenced plots in lowlands (28%), and conservation in small plots near their houses (15%).   Important to note was the fact that most of the selling of the fresh roots was conducted in local marketplaces (64%) as opposed to the farm gate (6%).  This means that most of the farmers had to transport their products to local markets and other places in the urban (30%) areas to sell their produce.  Vitamin A knowledge, farmers' practices, attitude and perception on sweetpotato was further assessed.  One of the most important objectives of this project is to address vitamin A deficiency through the consumption of OFSP. Overall, both male (70%) and female (66%) have heard about vitamin A. Although not important, in general, the results indicate that there were slightly more men than women informed about vitamin A among the respondents of this study. The most important source vitamin A information for the women was the health unit (50%), while for the men was the radio (44%) program aired in local language. In general, the use of radio with programs in local languages is more effective for men than women, while, most of the women can be effectively reached by using the heath unit.
"

	uri <- "doi:10.21223/P3/SL0ALD"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.6
	)

	num <- function(x) as.numeric(haven::zap_labels(x))
	f <- ff[basename(ff) == "Niassa baseline pag_1.sav"]
	r <- haven::read_sav(f)

	# Dictionary names for the district codes present in this file. Source: BaseLineSurvey Data Dictionary.txt.
	dist <- c(
		"19" = "Lago", "20" = "Sanga", "21" = "Muembe", "22" = "Chimbunila",
		"23" = "Lichinga-cidade", "27" = "Mandimba", "28" = "Cuamba", "29" = "Mecanhelas"
	)
	code <- as.character(as.integer(num(r$DISTRICT)))
	adm2 <- dist[code]
	adm2[is.na(adm2)] <- code[is.na(adm2)]

	dmy <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", as.character(r$A16_DATE))
	interview <- rep(NA_character_, nrow(r))
	interview[dmy] <- as.character(as.Date(as.character(r$A16_DATE)[dmy], "%d/%m/%Y"))

	d <- data.frame(
		country = "Mozambique",
		adm1 = "Niassa",
		adm2 = adm2,
		hhid = as.character(num(r$A05HHID)),
		latitude = -(num(r$DEGREES) + (num(r$MINUTES) + num(r$D_MINTS) / 1000) / 60),
		longitude = num(r$DEG_E) + (num(r$MIN_E) + num(r$D_M_E) / 1000) / 60,
		elevation = num(r$ELEVATION),
		interview_date_ = interview,
		stringsAsFactors = FALSE
	)
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
