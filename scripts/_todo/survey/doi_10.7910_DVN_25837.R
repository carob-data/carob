# R script for "carob"
# license: GPL (>=3)

## NOTES
# Adoption and impact survey (Yunnan, China; 130 households: 50 old adopters, 50 new adopters, 30 non-adopters) of rice variety
# mixtures (inter-planting of glutinous (GR) and hybrid (HR) rice) for blast management. Not a field experiment.
# Codes and units from "YUNNAN_CodingManual.xlsx": area in mu (1 mu = 1/15 ha), yield and seed rate in kg/mu.
# Sheet "Before" (all households): one record per household x rice type (GR, HR) with area, planting and harvest dates, variety,
#   yield, seed rate, number of insecticide and fungicide applications and herbicide use.
# Sheet "After" (adopters only, mixture): one record per household x rice type with variety, yield and seed rate. It has no area,
#   dates or pesticide counts per type; the pesticide counts in "After" refer to the mixed field and are used for both types.
# Records without variety and yield are dropped (a household without GR or HR).
# Operator sex, age, education (years) and household size from sheet "demo char & income".

## ISSUES
# Village, county and prefecture are numeric codes without names (14 villages, 5 counties, 2 prefectures); kept as codes in
#   village_code_ and county_code_. Human should find names and georeference.
# "After" records have no planting/harvest date and no area. The "Before" planting dates are in 1998 and 2000; the year of the
#   "After" season is not given (the dataset metadata says 2000).
# In the mixture (After), the GR yield is low (median 50 kg/mu) and probably per mu of the whole mixed field; verify.
# Yield moisture/form (paddy) is not stated.
# Fertilizer: only cost per mu and number of applications are given; no N/P/K.
# Codes not decoded (code lists missing): cropping pattern (CPATTER), seed source, reasons for variety choice, who decided, results,
#   problems, power source; prices received (units not stated) and the "Others" sheet (perceptions, income use) are not mapped.
# Labor (person-days, wages) per operation not mapped.
# One insecticide count (73) is likely an error; kept.
# Some Before dates are inconsistent: 3 records harvested within 45 days and 10 more than 366 days after planting (date entry errors).
# Yield is missing for some records that have a variety.

## NEW VARIABLES
# adopter_: old adopter, new adopter or non-adopter of rice mixtures
# period_: before or after adoption of the rice mixture
# rice_type_: glutinous or hybrid
# mixture_: TRUE if the record is from a GR/HR mixture (After)
# village_code_, county_code_: location codes
# education_years_: years of education of the operator
# fertilizer_applications_: number of fertilizer applications


carob_script <- function(path) {

"
Genetic Diversity for Sustainable Rice Blast Management in China: Adoption and Impact

To analyze the effects of genetic diversity on rice blast management and to determine the socio-economics of using genetic diversity, rice inter-planting, in this case, in explaining the adoption and impact on farmers' pest management, productivity and income.
"

	uri <- "doi:10.7910/DVN/25837"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 40,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Dataset.xlsx"]
	rb <- carobiner::read.excel(f, sheet="Before", skip=2)
	ra <- carobiner::read.excel(f, sheet="After", skip=2)
	rh <- carobiner::read.excel(f, sheet="demo char & income", skip=1)
	rb <- rb[rb$RESCODE %in% c("1", "2", "3"), ]

	mu <- 15

	rice <- function(r, tp, period) {
		s <- c(glutinous="GR", hybrid="HR")[tp]
		v <- function(x) if (is.null(r[[x]])) NA else r[[x]]
		dt <- function(x) if (is.null(r[[x]])) NA else as.character(as.Date(r[[x]]))
		data.frame(
			hhid = as.character(r$RESPNO),
			period_ = period,
			rice_type_ = tp,
			mixture_ = period == "after",
			variety = v(paste0("VAR", s)),
			field_size = v(paste0("AR", s)) / mu,
			planting_date = dt(paste0("PLT", s)),
			harvest_date = dt(paste0("HAR", s)),
			yield = v(paste0("YLD", s)) * mu,
			seed_rate = v(paste0("RATE", s)) * mu,
			insecticide_times = as.integer(r$NINSAPP),
			fungicide_times = as.integer(if (is.null(r$NFUNAP)) r$NFUNAPP else r$NFUNAP),
			herbicide_used = r$NHBAPP > 0,
			fertilizer_applications_ = as.integer(r$NFEAPPL),
			fertilizer_cost = r$FERTCPM * mu
		)
	}
	d <- rbind(
		rice(rb, "glutinous", "before"), rice(rb, "hybrid", "before"),
		rice(ra, "glutinous", "after"), rice(ra, "hybrid", "after")
	)
	d <- d[!(is.na(d$variety) & is.na(d$yield)), ]
	d$variety <- trimws(d$variety)
	d$insecticide_used <- d$insecticide_times > 0
	d$fungicide_used <- d$fungicide_times > 0

	j <- match(d$hhid, rh$RESPNO)
	d$adopter_ <- c("old adopter", "new adopter", "non-adopter")[rh$RESCODE[j]]
	d$village_code_ <- rh$VILLAGE[j]
	d$county_code_ <- rh$COUNTY[j]
	d$sex <- c("male", "female")[rh$OPSEX[j]]
	d$age <- rh$OPAGE[j]
	d$education_years_ <- rh$OPEDU[j]
	d$hh_size <- rh$HHSIZE[j]
	d$currency <- "CNY"
	d$farmland <- rb$FSIZE[match(d$hhid, rb$RESPNO)] / mu

	d$country <- "China"
	d$adm1 <- "Yunnan"
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$planting_method <- "transplanting"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- paste(d$hhid, d$period_, sep="_")

	carobiner::write_files(path, meta, d)
}
