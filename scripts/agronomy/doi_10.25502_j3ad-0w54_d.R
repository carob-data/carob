# R script for "carob"
# license: GPL (>=3)

## ISSUES
## Harvest date is ambiguous and needs to be investigated (it is showing that some crops were harvested 1 month after planting, which is not credible).

carob_script <- function(path) {

"
Maize Modelling Data, Agronomy, Kano

A 5-year development project titled; Kano State Agro-Pastoral Development Project (KSADP). The project aims to reach 450,000 smallholder farmers and enhance crop production and livestock productivity in Kano State
"

	uri <- "doi:10.25502/j3ad-0w54/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1016/j.fcr.2025.110079",
		project = "KSADP",
		carob_date = "2025-09-22",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield; dmy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	
	f <- ff[basename(ff) == "raw_data_maize.csv"]
  
	r <- read.csv(f)
	
	### Process
	
   d <- data.frame(
      location = r$Location,
      year = r$Year,
      adm1= "Kano",
      country = "Nigeria",
      crop = "maize",
      rep = r$Rep,
      N_fertilizer = r$N_APPLIED,
      P_fertilizer = 0 , 
      K_fertilizer = 0,
      planting_date = as.character(as.Date(r$Planting_Date, "%m/%d/%Y")),
      flowering_date = as.character(as.Date(r$Flw_Date, "%m/%d/%Y")),
      maturity_date = as.character(as.Date(r$Maturity_Date, "%m/%d/%Y")),
      #harvest_date = as.character(as.Date(r$Harvest_Date, "%m/%d/%Y")),
      flowering_days = r$Days_Flw,
      maturity_days = r$Days_Mat,
      yield = r$Grain_Yield,
      variety = r$Variety,
      dmy_total = r$Above_G_Biomass,
      #grain_N = r$N_UP_Total,
      LAI = r$LAI,
      tmax = r$TMAX,
      tmin = r$TMIN,
      rain = r$In_Crop_Rain,
      #ET= r$Seasonal.ET_mm,
      trial_id = paste0(r$Location, "-", r$AEZ), 
      on_farm = TRUE, 
      is_survey = FALSE,
      yield_part = "grain",
      yield_moisture = 0,
      irrigated = NA, 
      geo_from_source = FALSE
  )

  location <- do.call(rbind, strsplit(d$location, "\\("))
  d$location <- trimws(location[,1])

  d$planting_date <- ifelse(is.na(d$planting_date), d$year, d$planting_date)
  d$year <- NULL
  
### Adding longitude and latitude 
# how determined?
	geo <- data.frame(
		location = c("Alajawa", "Albasu Central", "Badafi", "Bagwarai", "Bumai", "Bunkure", "Butunutu", "Chula", "DAbar Gwari", "Dal", "Dalawa", "Dansoshiya", "Dawakiji", "Dugabau", "Falgore", "Faragai", "G/Malam", "Gafasa", "Gani", "Gediya", "Gude", "Gundutse", "Jita", "K/Giwa", "K/Kuka", "Kachako", "Kadan-dani", "Kara", "KauKau", "Kibiya", "Kuka", "Kuki", "Kwa", "Kwamarawa", "Kwankwaso", "Maaraku", "Maimakawa", "Maraku", "Masu", "Mesar Tudu", "Nataala", "Raba", "Rantan", "Rimin Dako", "Ruwan Bako", "Sansan", "Sarbi", "Shagogo", "Sumaila", "Tanagar", "Tangaji", "Tsaudawa", "Tsaure", "Tudun Kaya", "U/Rimi", "UNgoggo", "Wuro Bagga", "Yalwa", "Yammedi", "Yandala", "Yangizo", "yankamaye", "Yautar Kudu", "Yunbu", "Zainabi", "Zinyau", "Zoza"), 
		longitude = c(7.966, NA, 8.1692, NA, 8.1915, 8.5189, NA, 8.9707, NA, 8.825, 8.6705, 8.0723, 8.7032, 8.2096, 7.6947, 8.9888, NA, 9.133, 8.8438, 8.9122, 8.0666, 8.5137, NA, NA, NA, 9.2492, NA, 9.1551, NA, 8.6925, 8.3525, 8.3639, 8.377, 8.3898, 8.3934, NA, 9.1345, 8.6869, 8.8059, NA, NA, NA, 8.3791, 8.2536, NA, 8.63, 8.673, 8.9312, 8.9622, 8.6894, 8.4876, 8.2027, 7.88, 7.931, NA, NA, NA, 8.1853, 8.0925, NA, NA, 7.9286, NA, NA, 8.7711, NA, 7.8856), 
		latitude = c(12.1147, NA, 11.3735, NA, 12.5134, 11.6789, NA, 11.9897, NA, 11.4763, 11.3702, 11.5379, 11.8636, 11.8012, 11.5117, 11.6673, NA, 11.9846, 11.3736, 11.3505, 11.9539, 11.8245, NA, NA, NA, 11.538, NA, 11.93, NA, 11.5166, 12.4299, 11.4263, 12.158, 12.3302, 11.8515, NA, 11.795, 10.649, 11.2021, NA, NA, NA, 11.5185, 12.0848, NA, 12.4524, 12.2919, 11.8426, 11.5066, 11.9527, 12.3053, 11.6809, 12.0946, 11.7332, NA, NA, NA, 11.4855, 11.8238, NA, NA, 12.3284, NA, NA, 10.7889, NA, 11.6291)
	)


	d <- merge(d, geo, by= "location", all.x = TRUE) 	 
 
	d$flowering_days[d$flowering_days < 15] <- NA
 
#### drop duplicate rows
	d <- unique(d)
 
	carobiner::write_files(path, meta, d)
}

