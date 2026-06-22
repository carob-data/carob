# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Farmer managed evaluation of alternative tillage and crop establishment options for Mungbean in coastal Bangladesh

An on-farm experiment was carried out to evaluate mungbean machine sowing compared to conventional farmers' tillage and crop establishment practices in Kharif-1 season with sowing from 28 January to 2 February 2015.  Eleven mungbean farmers, each of whom had at least five years of experience growing mungbean, were selected randomly at roughly 3 km intervals along a north to south transect in Patuakhali. This resulted in an approximate 33 km long stretch that was utilized in order to capture variation in microclimate that may alter yield, and to assess if power-tiller operated seeder (PTOS) performance is consistent across a landscape and variety of environments. In each farmers' field, we imposed two trea  tments randomly in plots including conventional farmers' practice (FP), which entailed tillage with a power tiller followed by broadcasting of seed and then incorporation of mungbean. The number of power tiller passes were decided upon by participating farmers. This control treatment was contrasted with single-pass PTOS seeding and fertilizing, with a target seed and fertilizer placement depth of 5 cm. BARI mungbean-6 was selected as an advanced variety for the experiment.    We collected possible agronomic data and constructed partial budgets for each treatment for farmer. Labor, tillage, fertilizer, seed, costs, resulting grain prices were determined by asking the farmers and the service providers. All data information aligned with metadata for further study.
"

	uri <- "hdl:11529/11083"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
	  data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method;planting_method",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-06-22",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "MetaSheet_Mung.csv"]
	f2 <- ff[basename(ff) == "MungData.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2, na= ".")

	d1 <- data.frame(
		season = "kharif",
		crop = gsub("MUNG", "mung bean", r2$CROP),
		location = carobiner::fix_name(r2$DISTRICT.NAME, "title"),
		rep = r2$FARMERS.S.REPLICATION.NUMBER,
		treatment = r2$Main.treatment,
		land_prep_method = ifelse(grepl("T1", r2$Main.treatment), "conventional", "none"),
		planting_method = ifelse(grepl("T1", r2$Main.treatment), "broadcasting", "mechanized"),
		variety = r2$Variety,
		latitude = r2$LATITUDE,
		longitude = r2$LONGITIDE,
		landscape_position = r2$LANDSCAPE.POSITION,
		planting_date = as.character(as.Date(r2$SOWING.DATE..MONTH..DATE..YEAR., "%m/%d/%y")),
		plot_area = r2$Plot.Size..m2.,
		#blade = r2$BLADE.TYPE,
		emergence_date = as.character(as.Date(r2$EMERGENCE.DATE, "%d/%m/%y")),
		plant_density = r2$Total.No..of.Plants.1.2.m2*10000,
		flowering_date = as.character(as.Date(r2$DATE.OF.FIRST.FLOWERING..MONTH.DAY.YEAR., "%m/%d/%y")),
		harvest_date_first = as.character(as.Date(r2$DATE.OF.FIRST.HARVEST..MONTH.DAY.YEAR., "%m/%d/%y")),
		plant_height_first = rowMeans(r2[, c("Plant.hieght.1st.harvest..Q1.", "Plant.hieght.1st.harvest..Q2.", "Plant.hieght.1st.harvest..Q3.", "Plant.hieght.1st.harvest..Q4.")], na.rm = TRUE),
		#r2$HARVESTED.AREA..m2..1,
		yield_first = (r2$Total.grain.weight....Kg..harvest.area..1.8mx1.5mx4./1.8*1.5*4)*10000,
		seed_weight_first = r2$X100.seed.weight..g.*10,
		harvest_date_2nd = as.character(as.Date(r2$DATE.OF.SECOND.HARVEST..MONTH.DAY.YEAR., "%m/%d/%y")),
		plant_height_2nd = rowMeans( r2[, c("Plant.hieght.2nd.harvest..Q4.", "Plant.hieght.2nd.harvest..Q3.", "Plant.hieght.2nd.harvest..Q2.", "Plant.hieght.2nd.harvest..Q1.")], na.rm = TRUE),
		seed_weight_2nd = r2$X100.seed.weight..g..1*10,
		yield_2nd = (r2$Total.grain.weight....Kg..harvest.area..1.8mx1.5m./1.8*1.5)*10000,
		crop_price = r2$Grain.price..tk.t.,
		seed_rate = r2$Seed.Qty..kg.ha.,
		seed_price = r2$Seed.price..tk.kg.,
		N_fertilizer = r2$Urea..kg.ha.*0.46,
		P_fertilizer = r2$TSP..kg.ha.* 0.1923 + (r2$MP..kg.ha.* 0.52)/2.29 ,
		K_fertilizer = r2$MP..kg.ha.* 0.34/1.2051,
		gypsum = r2$Gypsum..kg.ha.,
		fertilizer_price = rowSums(r2[, c("Price..tk.kg..3", "Price..tk.kg..1", "Price..tk.kg.", "Price..tk.kg..2")], na.rm = TRUE),
		country = "Bangladesh",
		trial_id = paste(r2$HUB.NAME, r2$Main.treatment, sep = "-"), 
		on_farm = TRUE, 
		is_survey = FALSE, 
		yield_part = "grain", 
		yield_moisture = NA_real_, 
		geo_from_source = TRUE, 
		irrigated = NA,
		yield_isfresh = NA,
		currency = "BDT"
		
	)

	
	d <- reshape(d1, varying = list(c("harvest_date_first", "harvest_date_2nd"), c("plant_height_first","plant_height_2nd"), c("yield_first", "yield_2nd"), c("seed_weight_first", "seed_weight_2nd")),
	             v.names = c("harvest_date", "plant_height", "yield", "seed_weight"),
	             direction = "long")
	
	d$id <- d$time <- NULL
	
	
	carobiner::write_files(path, meta, d)
}

