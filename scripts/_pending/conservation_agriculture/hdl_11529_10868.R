# R script for "carob"

# there are four subtreatments but only three are described
# what is subtreatment 0? Or is that 1, but then what is subtreatment 3?

# also need to process the soil profile temperature, moisture and NDVI data.

# It is not clear how you can get to the amount of N from the amount of D-compound and AN
# if D-comp is 10% N and AN is 34% you get 
#200 * .1 + .34 * 46 =  35.64
#200 * .1 + .34 * 220 =  94.8
 

carob_script <- function(path) {

"Conservation agriculture involves reduced tillage, diversification of plant associations, and retention of crop residues to maintain soil cover. However, there is knowledge gap on the appropriate rate of application and interactive effect of residues and nitrogen as in some situations cases of nitrogen lock-up have been reported. This present data set addresses the effects of different nitrogen and residue levels on maize productivity, soil temperature, soil moisture and soil structure in contrasting soil types over 6 seasons. The trials were set across southern Africa i.e. Malawi, Mozambique, Zambia and Zimbabwe. The treatments were as follows: 

Main treatments: 
1. Conventional tillage 
2. No-tillage, 0 t/ha residues 
3. No-tillage, 2 t/ha residues
4. No-tillage, 4 t/ha residues 
5. No-tillage, 6 t/ha residues 
6. No-tillage, 8 t/ha residues, 
 
Subtreatments: 
1. 0 N 
2. 30N (200 kg/ha Compound D – 46 kg/ha AN 
3. 90N (200 kg/ha Compound D –220 kg/ha AN) 

The measured attributes are as follows: 
1. Maize and grain yields 
2. Soil profile temperature 
3. Soil profile mositure 
4. Normalized difference vegetation index (NDVI)"


	uri <- "hdl:11529/10868"
	group <- "conservation_agriculture"

	ff	<- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication=NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Blessing Dzuda",
		carob_effort = NA,
		carob_date="2024-03-26"
	)
	
	f <- ff[basename(ff) == "Residue Level Trial.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
			crop="maize", 
			country=r$Country,
	        location=r$Site,
	        planting_date=as.character(r$Season),
	        treatment=r$Treatment,
	        rep=as.integer(r$Replicate),
	        dmy_total=r$`Biomass (kg/ha)`,
	        yield_part="grain",
	        yield=r$`Grain (kg/ha)`
		)
	
	d$on_farm <-FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$land_prep_method = "none"
	d$land_prep_method[d$treatment == 1] <- "conventional"
	d$residue_prevcrop <- 0
	d$residue_prevcrop <- pmax(0, (d$treatment - 2)) * 2000

	i <- r$subtreatment
	i[i ==0] <- NA
	d$N_fertilizer = c(0, 30, 90)[i]
	i[i>1] <- 2
	d$P_fertilizer = c(0, 40)[i]
	d$K_fertilizer = c(0, 20)[i]
	d$S_fertilizer = c(0, 18)[i]

	d$fertilizer_type = ifelse(is.na(i), "none", "D-compound; AN")

	d <- d[d$yield != "-", ]
	d$yield <- as.numeric(d$yield)

	tments <- c("conventional tillage", "no-tillage, 0 t/ha residue", "no-tillage, 2 t/ha residue", "no-tillage, 4 t/ha residue", "no-tillage, 6 t/ha residue", "no-tillage, 8 t/ha residue")
	d$treatment <- tments[d$treatment]

	d$site <- carobiner::replace_values(d$site, 
			c("DTC", "MFTC", "SRS", "UZ"),
			c("Domboshawa Training Centre",  "Monze Farmer Training Centre", 
					"Sussundenga Research Station", "University of Zimbabwe"))
		
	geo <- data.frame(
		site = c("Makoholi", "Domboshawa Training Centre", "University of Zimbabwe", "Sussundenga Research Station", "Monze", "Monze Farmer Training Centre", "Chitedze"), 
		longitude = c(30.7715, 31.1253, 31.0546, 33.2953, 27.4733, 27.4733, 33.8525),
		latitude = c(-19.833, -17.6738, -17.7824, -19.413, -16.2803, -16.2803, -13.8362)
	)
	
	d <- merge(d, geo, by="site", all.x = TRUE)
	d$trial_id <- as.character(as.integer(as.factor(d$site)))
	
	carobiner::write_files(path, meta, d)
}

