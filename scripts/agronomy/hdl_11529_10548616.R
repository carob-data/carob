# R script for "carob"

## ISSUES
# the fruit trees in the MIAF system are not capture yet

carob_script <- function(path) {

"Maize (Zea mays L.), the staple crop of Mexico, is often produced by smallholder farmers on sloping terrains. Historically, little agronomic research has been performed under the conditions of these farmers to support them in the sustainable intensification of their production systems. We set up trials at two locations in the state of Oaxaca to evaluate conservation agriculture and agroforestry in collaboration with local farmers. This dataset includes the data taken between 2015 and 2019 in the Santa Maria Teopoxco research platform and between 2014 and 2019 in the Tamazulapam del Espiritu Santo research platform, both located in the Mexican state of Oaxaca in the Pacífico Sur innovation hub. In both platforms the local conventional system was compared with 4 improved treatments. Improved treatments included zero tillage, residue management, liming, crop rotation and fertilization in different combinations. In both platforms these treatments were evaluated within a MIAF (Milpa Intercalada con Arboles Frutales) agroforestry arrangement. The MIAF system was implemented at both sites by planting rows of trees along the contour lines with a distance between rows of 10.6 m and between trees of 1 m in Tamazulapam (900 trees ha-1) and 2 m in Teopoxco (450 trees ha-1). Maize was sown on 60% of the area and 40% was under fruit trees. The database contains agronomic data (fertilization, density, tillage,residue management, plant spacing), yield data (grain yield of maize, squash and bean, fresh yield of peas, avocado and peach), plant development data (sowing, emergence, flowering, maturity and harvest, plant height, lodging) and economic data (production costs, gross margin and net profit). (2021-09-27)"

	uri <- "hdl:11529/10548616"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;AMDSL",
		# pub does not have DOI yet
		#Fonteyne, s., Silva Avendaño, C., Ramos Sanchez, A., Torres Zambrano, J.P., García Dávila, F., Pérez Martínez, Z.,  García Dávila, A., Castillo Villaseñor, L., Verhulst, N., 2021. Innovating traditional production systems through participatory conservation agriculture and agroforestry research. In preparation
		publication=NA, 
		project=NA,
		data_type= "experiment",
		carob_contributor= "Blessing Dzuda",
		carob_date="2024-04-09",
		treatment_vars = NA,
		response_vars = "yield"
	)
	
# read data 
	maize_mexico <- function(sheet, id)  {	

		f <- ff[basename(ff) == "DAT-Oaxaca-MIAF.xlsx"]
		ctypes <- rep("guess", 52)
		ctypes[41] <- "text"
		r <- carobiner::read.excel(f, sheet=sheet, col_types=ctypes, na=c("", "-")) 

		r$Harvest_date[r$Harvest_date==0]<-NA
		r$Row_dist[r$Row_dist == "v"] <- NA

		if (is.character(r$Harvest_date)) { 
			r$Harvest_date <- as.Date(as.numeric(r$Harvest_date), origin="1900-01-01") - 2
		}

		d <- data.frame(
			crop=tolower(r$Crop), 
			country="Mexico",
			location=r$Name,
			adm1=r$State,
			adm2=r$Municipality,
			latitude=r$Latitude,
			longitude=r$Longitude,
			geo_from_source = TRUE,
			elevation=r$Altitude,
			planting_date=as.character(r$Sowing_date),
			rep=as.integer(r$Num_Rep),
			yield=r$Yield_moist,
			dmy_storage=r$Yield_dry,	
			plant_height=as.numeric(r$Height),
			harvest_date=as.character(r$Harvest_date),
			flowering_days=as.integer(r$Flower_days),
			maturity_days=as.integer(r$Mat_days),
			seed_weight=r$Thousand,
			crop_price=as.numeric(r$Price)/1000,
			currency="??",
			N_fertilizer=r$Fert_N,
			P_fertilizer=r$Fert_P / 2.29,
			K_fertilizer=r$Fert_K,
			row_spacing=as.numeric(r$Row_dist),
			rain=as.numeric(r$Precip),
			variety=r$Variety,
			previous_crop_residue_perc=r$Res_perc,
			land_prep_method=r$Till,
			crop_rotation=tolower(r$Crop_rotation),
			treatment=r$Name_tr,
			season=r$Cycle,
			plant_density=as.numeric(r$Plants_m2)*10000,
			yield_part="grain",
			trial_id=id
			#slper=r$Lodging,
		)

		d$plant_height[d$plant_height==0] <- NA	
		return(d)
	}
 
	d1 <- maize_mexico("Teopoxco", "1")
	d2 <- maize_mexico("Tamazulapam", "2")

	d <- rbind(d1, d2)

	d$land_prep_method <- carobiner::replace_values(d$land_prep_method,
			c("Zero tillage", "Conventional tillage", "MIAF"), 
			c("none", "conventional", NA), must_have=FALSE)
		#no standard carob term for zero tillage
		#it is called "none" (i.e., "none" == "no tillage" == "zero tillage"
	d$crop <- carobiner::replace_values(d$crop,
			c("bean", "descanso", "fallow", "fig leaf gourd", "haba"),
			c("common bean", "none", "none", "fig-leaf gourd", "faba bean"))
	d <- d[d$crop!="none", ]

	d$crop_rotation <- gsub("/|-|, ", ";", d$crop_rotation)
	d$crop_rotation <- gsub("descanso|fallow", "none", d$crop_rotation)
	d$crop_rotation <- gsub("bean", "common bean", d$crop_rotation)
	d$crop_rotation <- gsub("frjiol", "common bean", d$crop_rotation)
	d$crop_rotation <- gsub("chicaro", "pea", d$crop_rotation)

	# milpa is a maize/squash/bean intercrop?

	d$lime_used <- grepl("Ca", d$treatment)

	d$on_farm <- FALSE ## I am not so sure.
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}


