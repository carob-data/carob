
do_LCAS <- function(r) {

	n <- nrow(r)
	plot_ha <- 0.404686 * r$C.q305_cropLargestArea / r$C.q302_acreConv

	fix_date <- function(x) {
		if (is.null(x)) return(rep(as.character(NA), n))
		x <- trimws(as.character(x))
		x[x == ""] <- NA
		x <- gsub(", ", "-", x)
		x <- gsub(" ", "-", x)
		x <- gsub("/", "-", x)
		x <- sub("-([0-9]{2})$", "-20\\1", x)
		month.num <- paste0("-", formatC(1:12, width=2, flag = "0"), "-")
		for (i in 1:12) {
			x <- gsub(paste0("-", month.abb[i], "-"), month.num[i], x)
		}
		dat <- rep(as.Date(NA), length(x))
		i <- !is.na(x) & grepl("-", x)
		j <- !is.na(x) & !grepl("-", x)
		if (any(j)) dat[j] <- as.Date("1899-12-31") + as.numeric(x[j])
		if (any(i)) dat[i] <- as.Date(x[i], "%d-%m-%Y")
		as.character(dat)
	}

	d <- data.frame(
		hhid = as.character(seq_len(n)),
		trial_id = as.character(seq_len(n)),
		interview_date = fix_date(r$collectionDate),
		country = if (is.null(r$A.q101_country)) "India" else as.character(r$A.q101_country),
		adm1 = carobiner::replace_values(as.character(r$A.q102_state),
			c("UttarPradesh", "WestBengal", "Chattisgarh"),
			c("Uttar Pradesh", "West Bengal", "Chhattisgarh"), FALSE),
		adm2 = as.character(r$A.q103_district),
		adm3 = gsub("_", " ", as.character(r$A.q104_subDistrict)),
		location = as.character(r$A.q105_village),
		latitude = as.numeric(r$O.largestPlotGPS.Latitude),
		longitude = as.numeric(r$O.largestPlotGPS.Longitude),
		geo_from_source = TRUE,
		crop = tolower(as.character(r$A.q116_crop)),
		previous_crop = trimws(tolower(as.character(r$D.prevCrop))),
		season = carobiner::replace_values(tolower(trimws(as.character(r$A.q117_season))),
			"aus", "early wet", FALSE),
		variety = as.character(r$D.q410_varName),
		variety_type = tolower(as.character(r$D.q409_varType)),
		soil_texture = carobiner::replace_values(as.character(r$D.q401_soilTexture),
			c("Heavy", "Medium", "Light"),
			c("fine", "medium", "coarse"), FALSE),
		soil_quality = tolower(as.character(r$D.q403_soilPerception)),
		landscape_position = as.character(r$D.q402_drainClass),
		previous_crop_residue_perc = as.numeric(r$D.q407_cropResiduePcnt),
		previous_crop_burnt = tolower(as.character(r$D.q408_residueBurnt)) == "yes",
		field_size = plot_ha,
		yield_part = "grain",
		on_farm = TRUE,
		is_survey = TRUE
	)

	d$country[is.na(d$country) | d$country == "8" | d$country == ""] <- "India"
	if (!is.null(r$O.largestPlotGPS.Accuracy)) {
		d$geo_uncertainty <- as.numeric(r$O.largestPlotGPS.Accuracy)
	}
	if (!is.null(r$A.q111_fGender)) d$sex <- tolower(as.character(r$A.q111_fGender))
	if (!is.null(r$A.q112_fEdu)) {
		d$education <- carobiner::replace_values(as.character(r$A.q112_fEdu),
			c("noSchooling", "seniorSecondary", "phD"),
			c("none", "senior secondary", "PhD"), FALSE)
	}
	if (!is.null(r$M.q701_hhMember)) d$hh_size <- as.numeric(r$M.q701_hhMember)
	if (!is.null(r$C.q303_cultLand)) {
		d$cropland <- 0.404686 * as.numeric(r$C.q303_cultLand) / as.numeric(r$C.q302_acreConv)
	}
	if (!is.null(r$M.q708_marketDistance)) d$market_distance <- as.numeric(r$M.q708_marketDistance)

	# Standardize previous_crop
	d$previous_crop <- carobiner::replace_values(d$previous_crop,
		c("fallow", "other", "bajra", "jowar", "greenmanure", "greengram", "pulses", "mungbean",
		  "blackgram", "caster", "chilli", "indianmustard", "pigeonpea", "redgram", "vegetables"),
		c("none", NA, "pearl millet", "sorghum", "green manure", "mung bean", "pulse", "mung bean",
		  "black gram", "castor bean", "chili pepper", "mustard", "pigeon pea", "pigeon pea", "vegetable"),
		FALSE)

	# Standardize land preparation (method, traction, implement) and crop establishment
	lp_raw <- as.character(r$D.q411_LandPrep)
	ce_raw <- as.character(r$D.q413_CropEst)
	lp_clean <- gsub("\\bNo\\s+tillage\\b", "NoTillage", lp_raw, ignore.case=TRUE)
	lp_clean <- gsub("\\bDisc\\s+Harrow\\b", "DiscHarrow", lp_clean, ignore.case=TRUE)

	map_tokens <- function(vec, from, to) {
		vapply(strsplit(ifelse(is.na(vec), "", vec), "\\s+"), function(toks) {
			toks <- toks[toks != ""]
			if (length(toks) == 0) return(as.character(NA))
			m <- to[match(tolower(toks), tolower(from))]
			m <- unique(m[!is.na(m)])
			if (length(m) == 0) return(as.character(NA))
			paste(m, collapse="; ")
		}, character(1))
	}

	d$land_prep_method <- map_tokens(lp_clean,
		c("BullockPlough", "DiscHarrow", "Disc", "Harrow", "FourWhTractCageWheels",
		  "FourWhTractRotavator", "FourWhTractorRotavator", "TwoWhTractRotavator",
		  "TyneCultivator", "WetTillagePuddling", "StripTillage", "NoTillage"),
		c("ploughing", "harrowing", "harrowing", "harrowing", "puddled",
		  "rotovating", "rotovating", "rotovating",
		  "tillage", "puddled", "strip tillage", "none"))

	d$land_prep_traction <- map_tokens(lp_clean,
		c("BullockPlough", "FourWhTractCageWheels", "FourWhTractRotavator",
		  "FourWhTractorRotavator", "TwoWhTractRotavator"),
		c("animal", "4 wheel tractor", "4 wheel tractor", "4 wheel tractor", "2 wheel tractor"))

	d$land_prep_implement <- map_tokens(lp_clean,
		c("DiscHarrow", "Disc", "Harrow", "TyneCultivator"),
		c("disc harrow", "disc harrow", "disc harrow", "cultivator"))

	# Crop establishment -> planting_method, transplanting_method, planting_implement
	pm <- rep(as.character(NA), n)
	tm <- rep(as.character(NA), n)
	pi <- rep(as.character(NA), n)

	pm[ce_raw %in% c("ManualPuddled_Random", "ManualPuddled_Line", "MachineTransplanted_Puddled",
		"MachineTransplanted_NonPuddled", "OtherSRI") |
		grepl("transplanted|transplanting", ce_raw, ignore.case=TRUE)] <- "transplanting"
	tm[ce_raw == "ManualPuddled_Random"] <- "manual"
	tm[ce_raw == "ManualPuddled_Line"] <- "line sowing"
	tm[ce_raw %in% c("MachineTransplanted_Puddled", "MachineTransplanted_NonPuddled")] <- "mechanized"

	pm[ce_raw == "DirectSown_SeedDrill"] <- "drilling"
	pi[ce_raw == "DirectSown_SeedDrill"] <- "seed drill"
	pm[ce_raw %in% c("DirectBroadcast", "Broadcasting")] <- "broadcasting"
	pm[ce_raw %in% c("DirectSown_Beushening", "ZeroTillage")] <- "direct seeding"
	pm[ce_raw %in% c("LineSowingAfterTillage", "ManualLineSowing") |
		grepl("behind country plough", ce_raw, ignore.case=TRUE)] <- "line sowing"

	d$planting_method <- pm
	if (any(!is.na(tm))) d$transplanting_method <- tm
	if (any(!is.na(pi))) d$planting_implement <- pi

	# Add Beushening (post-emergence tillage) and puddling from crop establishment if not already recorded
	i_beush <- !is.na(ce_raw) & ce_raw == "DirectSown_Beushening"
	if (any(i_beush)) {
		d$land_prep_method[i_beush] <- ifelse(is.na(d$land_prep_method[i_beush]),
			"post-emergence tillage",
			paste(d$land_prep_method[i_beush], "post-emergence tillage", sep="; "))
	}
	i_pud <- !is.na(ce_raw) & grepl("_Puddled", ce_raw) &
		(is.na(d$land_prep_method) | !grepl("puddled", d$land_prep_method))
	if (any(i_pud)) {
		d$land_prep_method[i_pud] <- ifelse(is.na(d$land_prep_method[i_pud]),
			"puddled",
			paste(d$land_prep_method[i_pud], "puddled", sep="; "))
	}

	# Planting, transplanting, and harvest dates
	nurs_date <- fix_date(r$D.q414_rcNursEstDate)
	sow_trans <- if (!is.null(r$D.seedingSowingTransplanting)) {
		fix_date(r$D.seedingSowingTransplanting)
	} else {
		fix_date(r$D.q415_seedingSowingTransDate)
	}
	harv_date <- fix_date(r$L.q601_harvestDate)

	has_nurs <- !is.na(nurs_date)
	pdate <- ifelse(has_nurs, nurs_date, sow_trans)
	tdate <- ifelse(has_nurs, sow_trans, as.character(NA))

	# Clean impossible nursery-to-transplanting intervals (< 1 or > 90 days)
	if (any(has_nurs)) {
		dn <- as.numeric(as.Date(tdate) - as.Date(pdate))
		bad_nt <- has_nurs & !is.na(dn) & (dn < 1 | dn > 90)
		if (any(bad_nt)) {
			dh_n <- as.numeric(as.Date(harv_date[bad_nt]) - as.Date(nurs_date[bad_nt]))
			dh_t <- as.numeric(as.Date(harv_date[bad_nt]) - as.Date(sow_trans[bad_nt]))
			use_n <- !is.na(dh_n) & dh_n >= 60 & dh_n <= 300
			use_t <- !use_n & !is.na(dh_t) & dh_t >= 45 & dh_t <= 300
			idx <- which(bad_nt)
			tdate[idx] <- NA
			pdate[idx[use_t]] <- sow_trans[idx[use_t]]
			pdate[idx[!use_n & !use_t]] <- NA
		}
	}

	# Drop clearly corrupt harvest dates (< 45 or > 366 days after planting)
	dh <- as.numeric(as.Date(harv_date) - as.Date(pdate))
	harv_date[!is.na(dh) & (dh < 45 | dh > 366)] <- NA

	d$planting_date <- pdate
	if (any(!is.na(tdate))) d$transplanting_date <- tdate
	d$harvest_date <- harv_date

	# Seed rate (kg/ha) and source
	d$seed_rate <- as.numeric(r$D.q420_cropSeedAmt) / plot_ha
	d$seed_rate[!is.na(d$seed_rate) & d$seed_rate < 0] <- NA
	d$seed_source <- ifelse(tolower(as.character(r$D.q421_seedSource)) == "other",
		as.character(r$D.q422_otherSeedSource), as.character(r$D.q421_seedSource))

	# Irrigation
	if (!is.null(r$G.q5305_irrigTimes)) d$irrigation_number <- as.integer(r$G.q5305_irrigTimes)
	if (!is.null(r$G.q5301_irrigAvail)) {
		avail <- tolower(as.character(r$G.q5301_irrigAvail)) == "yes"
		d$irrigated <- if (!is.null(d$irrigation_number)) {
			ifelse(!is.na(d$irrigation_number), d$irrigation_number > 0L, avail)
		} else {
			avail
		}
	}
	if (!is.null(r$G.q5302_irrigSource)) {
		isrc <- as.character(r$G.q5302_irrigSource)
		isrc <- gsub("Dug\\s+Well", "Dugwell", isrc, ignore.case=TRUE)
		isrc <- gsub("(Shallow|Deep)\\s+Tube\\s*well", "\\1TubeWell", isrc, ignore.case=TRUE)
		isrc <- gsub("CanalDeep", "Canal Deep", isrc, ignore.case=TRUE)
		d$irrigation_source <- map_tokens(isrc,
			c("River", "Canal", "Pond", "Tank", "DeepTubeWell", "DeepTubewel",
			  "ShallowTubeWell", "Dugwell", "Lift", "Other"),
			c("river", "canal", "pond", "small reservoir", "groundwater", "groundwater",
			  "groundwater", "groundwater", "surface water", "unknown"))
	}

	# Stresses, pests, weeds, diseases
	d$flood_stress <- tolower(as.character(r$I.q5504_floodSeverity))
	d$drought_stress <- tolower(as.character(r$I.q5502_droughtSeverity))
	d$pest_severity <- tolower(as.character(r$I.q5506_insectSeverity))
	d$weed_severity <- tolower(as.character(r$I.q5505_weedSeverity))
	d$disease_severity <- tolower(as.character(r$I.q5509_diseaseSeverity))

	if (!is.null(r$I.q5512_lodgingPercent)) {
		lodg <- as.numeric(r$I.q5512_lodgingPercent)
		d$stalk_lodging <- ifelse(is.na(lodg), as.character(NA), paste0(lodg, "%"))
	}

	weed_cols <- intersect(c("K.q5702_top1stWeed", "K.q5703_top2ndWeed", "K.q5704_top3rdWeed",
		"K.q5705_top4thWeed", "K.q5706_top5thWeed"), names(r))
	if (length(weed_cols) > 0) {
		wm <- apply(r[, weed_cols, drop=FALSE], 1, function(v) {
			v <- trimws(as.character(v))
			v <- v[!is.na(v) & v != "" & tolower(v) != "none" & tolower(v) != "other"]
			if (length(v) == 0) return(as.character(NA))
			v <- gsub("^(Rc|Wh)_", "", v)
			v <- gsub("_", " ", v)
			paste(unique(v), collapse="; ")
		})
		d$weed_species <- wm
	}

	# Insecticides
	if (!is.null(r$I.q5507_insecticides)) {
		d$insecticide_used <- tolower(as.character(r$I.q5507_insecticides)) == "yes"
	}
	clean_insecticide <- function(x) {
		x <- trimws(tolower(as.character(x)))
		x[x == "" | x == "na"] <- NA

		rules <- list(
			c("\\b(hamla|humla|action.?505|nurel.?505|toofan.?505|bilbo.?505|bilbo.?550|bilbo550|chalora\\s*505|chaloa\\s*505|chaloro\\s*505|chloro\\s*505|chlorpyriphos\\s*505|fighter\\s*505|strike\\s*550|uld\\s*505|adhunik\\s*505|505|chlorocyph?er|bullet|bulet|ann?aconda|cannon|canon|canan|kanan|kanon|kanun|fighter)\\b", "chlorpyrifos; cypermethrin"),
			c("\\b(profex\\s*super|profex\\s*supper|propex\\s*supar|profexbuildo|rocket|roket|racket|raket)\\b|profenofos.*cypermethrin", "profenofos; cypermethrin"),
			c("\\b(lancer\\s*gold)\\b", "acephate; imidacloprid"),
			c("\\b(vitako|virtako)\\b", "chlorantraniliprole; thiamethoxam"),
			c("\\b(ampligo)\\b", "chlorantraniliprole; lambda-cyhalothrin"),
			c("\\b(glamou?r)\\b", "ethiprole; imidacloprid"),
			c("\\b(neuro[ck]omb?i?|niurocombi|nurocombi|niro\\s*kombe|neurocron)\\b", "chlorpyrifos; cypermethrin"),
			c("\\b(th[aei]+m[aei]+t[a-z0-9]*|dhiomate|himet|ph?or[ae]t[a-z0-9]*|porate)\\b", "phorate"),
			c("\\b(ac+[ei]+p[hf]*[ae]t[ae]?|as[ai]?p[hf]*[ae]t[ae]?|es[ph]*at[ae]|is[ph]*at[ae]|asataf|ac[ei]phat)\\b", "acephate"),
			c("\\b(super\\s*killer|u?staa?d|ostaa?d|usdad|a+rth?r?in[ae]?|artin|cyper?m[aeio][a-z]*|cvpermethrin|cyperthrin|cyermeth[a-z]*|cypromith[a-z]*|cyperhit|cypher\\s*mithilin|simbuk|simca|simka|symca|cimka|cmika|colt|ripcord|repcord)\\b", "cypermethrin"),
			c("\\b(ch?l?o?r[aeio]?[a-z]*(py|pi|pa|pe|pu|po|pr)[a-z]*(ph|f)[a-z]*|cbloriapairiphos|clopariphous|chloropyipos|ch?l?o?r?a?l?o?ra|ch?al[ao]r[ao]?|ch?l?oro|clora|calora|chalpra|chalro|chalroa|chslora|chalamin[ae]?|chalmine|chlamin|cor[ao]b[ao]n|ranger|l[ei]thal|d[auo]r?s[bw]an|tricel)\\b", "chlorpyrifos"),
			c("\\b(padan|cart[aeor]?p[a-z]*|car\\s+tap[a-z]*|carap\\s+hydro|varta\\s+hydro|cartof|kart[ao]p|kritap|critap|kantap|k[ae]ld[ao]n|caldan|cartab)\\b", "cartab"),
			c("\\b(confid[ao]r[ae]?|canfidol|admire|tatamida|victor|amida|hilmida|rajmida|gaucho|imid[ae]?c?h?l[a-z]*|emidachlor|prem|premier)\\b", "imidacloprid"),
			c("\\b(mon[ao]?ch?r[ao]?t[a-z]*|mobochrotophos|momocrotophos|monchrophos|monocootophas|monocotphos|monocrophos|monortopho|mono|mon[ao]s?cil|nuvacron|nukran|nocron|monacrine)\\b", "monocrotophos"),
			c("\\b(fame|frame|tak[uo]mi[n]?|tacomi|flub?e?nd[a-z]*)\\b", "flubendiamide"),
			c("\\b(os[hs]*ee?n[ae]?|ohseen|oshine|asheen|o[cs]in[ae]?|ocean|tok[ae]n|tocan|dinot[ei]f[a-z]*|dinitrofen)\\b", "dinotefuran"),
			c("\\b(r[ae]+g[ae]nt|rajent|reejent|jump|fip[ao]?ronil)\\b", "fipronil"),
			c("\\b(f[ui]+r+[ao]+d[ao]+n[a]?|f[il]ur?adon|fhuradal|firdon|ph[auo]radon|furatox|foratox|phoratox|foradon|danadar|carbo?fur[a-z]*)\\b", "carbofuran"),
			c("\\b(a?p+l[ao]+u?d|upl[ao]+u?d|aplond|aploid|bup?r[aeio]f[a-z]*|viprophozine)\\b", "buprofezin"),
			c("\\b(f[aeo]r?t+e?r+a|ph?[aeo]t[ae]ra|fatra|fotra|bhaterra|cor[aoe]g[aei]n|cor[aoe]z[aei][nm]|kor[ao][jz][aei]n|chorazen|corizon|corgin|kurjin|rynaxypyr|chlorab?ntran[a-z]*)\\b", "chlorantraniliprole"),
			c("\\b(ch[ae]ss|chaas|bilbo[a]?|bibo|py[mn][aei]tr[a-z]*|prmetrozyne|pimetrog[a-z]*)\\b", "pymetrozine"),
			c("\\b(rog[aeo]u?r|tafgor|dimethoate)\\b", "dimethoate"),
			c("\\b(karat[ae]y?|carate|karantey|m[ae]tador|lamb?da[\\s-]*cyh[a-z]*|lamd[ai]|lemd[ae][a-z]*|lemenda)\\b", "lambda-cyhalothrin"),
			c("\\b(nu[vw]an|nov[ae]?n|nuan|nivan|dd[vb]p|dich?lor[io]?v[a-z]*|dichlorophos)\\b", "dichlorvos"),
			c("\\b(m[ae]l[aei]?th[aiy]+[ao]?n[a-z]*)\\b", "malathion"),
			c("\\b(actara|ektara|attara|thiamet[a-z0-9]*|thiometh[a-z]*)\\b", "thiamethoxam"),
			c("\\b(ekalux|akal[ae]x|akalas|qu[ie]?n[ao]l[ao]?ph[a-z]*)\\b", "quinalphos"),
			c("\\b(thiodan|thaidon|thai\\s*don|hildon|sildon|endosulp?h?[a-z]*|andosalfa|indosulp?h?[a-z]*)\\b", "endosulfan"),
			c("\\b(neem[a-z]*|azadirachtin)\\b", "azadirachtin"),
			c("\\b([ae]mam[ae]ctin[a-z\\s]*|procleyem)\\b", "emamectin benzoate"),
			c("\\b(fenval[ae]?rat[ae]?|fenel|fenwal|tata\\s*fen|tatafen)\\b", "fenvalerate"),
			c("\\b(folid[aoe]l|folider|folidr|filidol|metacid|metasid|metacyl|methyl\\s*parathion)\\b", "methyl parathion"),
			c("\\b(decis|desis|deltamethrin)\\b", "deltamethrin"),
			c("\\b(talstar|bifenthrin)\\b", "bifenthrin"),
			c("\\b(sevin|carbaryl)\\b", "carbaryl"),
			c("\\b(flonicamid[ae]?|fluconiamide)\\b", "flonicamid"),
			c("\\b(fenpropathrin)\\b", "fenpropathrin"),
			c("\\b(prof[aei]n[ao]f[a-z]*|prof[aei]n[ao]ph[a-z]*|progenophos|propenophos|proph[aei]n[ao]ph[a-z]*|praphanopos|profex|carina|karina)\\b", "profenofos"),
			c("\\b(tr[aiy]+z[ao]?ph?[a-z]*|trisofos|tarz[ao]n|tarjan|traz[ae]n|turzan|trzan)\\b", "triazophos"),
			c("\\b(p[eiw]xalon|fixlan|flexlan|fexlin|pex)\\b", "triflumezopyrim"),
			c("\\b(larvin|thiodicarb)\\b", "thiodicarb"),
			c("\\b(d[ae]m[aeio]cr[aio][nm][ae]?|demicron|damecron|phosphamidon)\\b", "phosphamidon"),
			c("\\b(ethiprol[ae]?)\\b", "ethiprole")
		)

		out <- rep(as.character(NA), length(x))
		nn <- which(!is.na(x))
		if (length(nn) == 0) return(out)

		s <- gsub("[\r\n,+/;.&()]+", " ", x[nn])
		s <- gsub("\\s+", " ", trimws(s))

		res <- vector("list", length(s))
		for (rl in rules) {
			hits <- which(grepl(rl[1], s, perl=TRUE))
			if (length(hits)) {
				parts <- trimws(unlist(strsplit(rl[2], ";")))
				for (h in hits) res[[h]] <- c(res[[h]], parts)
			}
		}
		out[nn] <- vapply(seq_along(s), function(k) {
			if (s[k] %in% c("no", "none")) return("none")
			v <- unique(res[[k]])
			if (length(v) == 0) "unknown" else paste(v, collapse="; ")
		}, character(1))
		out
	}
	d$insecticide_product <- clean_insecticide(r$I.q5508_insecticidesName)

	# Biocides (fungicides / general pesticides in Q5510-5511)
	if (!is.null(r$I.q5510_pesticides)) {
		d$biocide_used <- tolower(as.character(r$I.q5510_pesticides)) == "yes"
	}
	d$biocide_product <- trimws(tolower(as.character(r$I.q5511_pesticidesName)))
	d$biocide_product[d$biocide_product == ""] <- NA
	d$biocide_product[grepl("remember|don'?t\\s*know|do\\s+not\\s+know|not\\s+known|^don'?t$|^dontknow$", d$biocide_product)] <- "unknown"

	# Herbicides
	herb_cols <- c("J.q5601_1herbName", "J.q5603_2herbName", "J.q5605_3herbName")
	clean_herbicide <- function(mat) {
		rules <- list(
			c("2\\s*[,._-]?\\s*4\\s*[,._-]?\\s*d|\\b4-?d\\b|\\bweedmar\\s*super\\b", "2,4-D"),
			c("\\b(bis\\s*p[iy]+r[iy]*b?[ae]?c?k?\\s*sodium|biapyribac|bispyribac|n[ao]m[ie]+n[ie]*\\s*gold|n[ao]mn?igold|namini|lumini\\s*gold|adora|edora|odera)\\b", "bispyribac-sodium"),
			c("\\b(pr[ae]?t[ai]+la[a-z]*|pritilaclor|peitilachjore|r[ie]fit|repid)\\b", "pretilachlor"),
			c("\\b(buta\\s*chlor|machete)\\b", "butachlor"),
			c("\\b(oxad[iy]+a[rg]+yl|top\\s*star)\\b", "oxadiargyl"),
			c("\\b(p[aey]razosulfuron|saa?thi)\\b", "pyrazosulfuron"),
			c("\\b(al+mix)\\b", "metsulfuron; chlorimuron-ethyl"),
			c("\\b(metsulfuron)\\b", "metsulfuron"),
			c("\\b(chlorimur[ao]n[a-z\\s-]*)\\b", "chlorimuron-ethyl"),
			c("\\b(fenx?o?x?aprop|rice\\s*star|restar|wh?ip\\s*super|weed\\s*super|vip\\s*super)\\b", "fenoxaprop-p-ethyl"),
			c("\\b(clodin[ao]fop|propargyl|topic)\\b", "clodinafop"),
			c("\\b(sulfosulfuron|lead[ae]r|traget)\\b", "sulfosulfuron"),
			c("\\b(io?dosulfuron)\\b", "iodosulfuron"),
			c("\\b(carfentrazone|affinity)\\b", "carfentrazone"),
			c("\\b(pendim[ey]th[ai]l[iy]n|stomp)\\b", "pendimethalin"),
			c("\\b(alachlor|lasso)\\b", "alachlor"),
			c("\\b(londex\\s*power)\\b", "bensulfuron; pretilachlor"),
			c("\\b(bensulfuron)\\b", "bensulfuron"),
			c("\\b(paraquat|gamax[ao]n)\\b", "paraquat dichloride"),
			c("\\b(penoxs?ulam)\\b", "penoxsulam"),
			c("\\b(cyhalofop)\\b", "cyhalofop-butyl"),
			c("\\b(atrazin[ae]?)\\b", "atrazine"),
			c("\\b(glyphosate|mira\\s*71)\\b", "glyphosate"),
			c("\\b(pinoxaden)\\b", "pinoxaden"),
			c("\\b(isoproturon)\\b", "isoproturon"),
			c("\\b(ethoxysulfuron)\\b", "ethoxysulfuron"),
			c("\\b(metribuzin)\\b", "metribuzin"),
			c("\\b(fluchl?oralin)\\b", "fluchloralin"),
			c("\\b(quizalofop)\\b", "quizalofop-ethyl")
		)
		s <- apply(mat, 1, function(v) {
			v <- trimws(tolower(as.character(v)))
			v <- v[!is.na(v) & v != ""]
			if (length(v) == 0) NA_character_ else paste(v, collapse=" ; ")
		})
		out <- rep(as.character(NA), length(s))
		nn <- which(!is.na(s))
		if (length(nn) == 0) return(out)
		sn <- s[nn]
		res <- vector("list", length(sn))
		for (rl in rules) {
			hits <- which(grepl(rl[1], sn, perl=TRUE))
			if (length(hits)) {
				parts <- trimws(unlist(strsplit(rl[2], ";")))
				for (h in hits) res[[h]] <- c(res[[h]], parts)
			}
		}
		out[nn] <- vapply(res, function(v) {
			v <- unique(v)
			if (length(v) == 0) "unknown" else paste(v, collapse="; ")
		}, character(1))
		out
	}
	d$herbicide_product <- clean_herbicide(r[, herb_cols, drop=FALSE])

	h_non_na <- rowSums(!is.na(r[, herb_cols, drop=FALSE]) & r[, herb_cols, drop=FALSE] != "")
	d$herbicide_times <- if (!is.null(r$J.herbAppTimes)) {
		ht <- as.integer(r$J.herbAppTimes)
		ifelse(is.na(ht), as.integer(h_non_na), ht)
	} else {
		as.integer(h_non_na)
	}
	d$herbicide_used <- d$herbicide_times > 0L
	d$herbicide_timing <- apply(r[, c("J.q5602_1herbAppDays", "J.q5604_2herbAppDays", "J.q5606_3herbAppDays")],
		1, function(i) {
			i <- i[!is.na(i) & i != ""]
			if (length(i) == 0) as.character(NA) else paste(i, collapse=";")
		})

	d$weeding_times <- as.integer(r$J.manualWeedTimes)
	d$weeding_done <- !is.na(d$weeding_times) & d$weeding_times > 0L

	# Inorganic fertilizers
	if (!is.null(r$E.q5104_applyMineralFert)) {
		d$fertilizer_used <- tolower(as.character(r$E.q5104_applyMineralFert)) == "yes"
	}

	get_fert <- function(x, product) {
		p <- paste0(c("_basal", "_1td", "_2td", "_3td"), product, "$")
		cn <- colnames(x)
		i <- sapply(p, \(v) grep(v, cn))
		stopifnot(length(i) == 4)
		rowSums(x[, i], na.rm=TRUE)
	}

	fert <- data.frame(
		DAP = get_fert(r, "DAP"),
		NPK = get_fert(r, "NPK"),
		urea = get_fert(r, "Urea"),
		NPKS = get_fert(r, "NPKS"),
		KCl = get_fert(r, "MoP"),
		SSP = get_fert(r, "SSP"),
		TSP = get_fert(r, "TSP"),
		ZnSO4 = get_fert(r, "ZnSO4"),
		gypsum = get_fert(r, "Gypsum"),
		H3BO3 = get_fert(r, "Boron")
	) / plot_ha
	fert[is.na(fert)] <- 0

	# Parse per-row NPK / NPKS grades (N-P2O5-K2O-S) into elemental N, P, K, S (%)
	parse_grade <- function(g, default_el) {
		res <- matrix(rep(default_el, each=n), nrow=n, ncol=4)
		if (is.null(g)) return(res)
		g <- trimws(as.character(g))
		g[g == ""] <- NA
		u <- unique(na.omit(g))
		for (val in u) {
			s <- sub("^Other\\s*", "", val, ignore.case=TRUE)
			s <- gsub("013$", "0-13", s)
			# Extract first grade token if multiple grades are listed (e.g. "10_26_26 14_35_14")
			s1 <- strsplit(trimws(s), "\\s+")[[1]][1]
			nums <- as.numeric(unlist(regmatches(s1, gregexpr("[0-9]+", s1))))
			if (length(nums) >= 3 && sum(nums[1:3]) <= 100 && all(nums[1:3] <= 60)) {
				el <- c(
					nums[1],
					nums[2] / 2.29,
					nums[3] / 1.2051,
					if (length(nums) >= 4 && nums[4] <= 50) nums[4] else 0
				)
				idx <- !is.na(g) & g == val
				res[idx, ] <- rep(el, each=sum(idx))
			}
		}
		res
	}

	# Defaults: 12-32-16 for NPK, 20-20-0-13 for NPKS (dominant grades & published script)
	npk_el <- parse_grade(r$F.q51071_gradeNPK, c(12, 32 / 2.29, 16 / 1.2051, 0))
	npks_el <- parse_grade(r$F.q51211_gradeNPKS, c(20, 20 / 2.29, 0, 13))

	# Elemental nutrient percentages matching terminag values_fertilizer_type.csv
	d$N_fertilizer <- (fert$DAP * 18 + fert$urea * 46 + fert$NPK * npk_el[, 1] + fert$NPKS * npks_el[, 1]) / 100
	d$P_fertilizer <- (fert$DAP * 20.1 + fert$SSP * 8.74 + fert$TSP * 19.23 + fert$NPK * npk_el[, 2] + fert$NPKS * npks_el[, 2]) / 100
	d$K_fertilizer <- (fert$KCl * 49.8 + fert$NPK * npk_el[, 3] + fert$NPKS * npks_el[, 3]) / 100
	d$S_fertilizer <- (fert$SSP * 12 + fert$TSP * 2 + fert$ZnSO4 * 19.9 + fert$gypsum * 19 + fert$NPK * npk_el[, 4] + fert$NPKS * npks_el[, 4]) / 100
	d$B_fertilizer <- fert$H3BO3 * 17.48 / 100
	d$Zn_fertilizer <- fert$ZnSO4 * 40.5 / 100
	d$Ca_fertilizer <- fert$gypsum * 22 / 100
	d$gypsum <- fert$gypsum

	fert_names <- colnames(fert)
	d$fertilizer_type <- apply(fert > 0, 1, function(v) {
		if (!any(v)) "none" else paste(fert_names[v], collapse="; ")
	})

	# Organic matter (FYM)
	d$OM_used <- tolower(as.character(r$E.q5101_FYM)) == "yes"
	fym_type <- tolower(trimws(as.character(r$E.q5102_typeFYM)))
	d$OM_type <- ifelse(!d$OM_used, as.character(NA),
		ifelse(is.na(fym_type) | fym_type == "", "farmyard manure",
			paste0("farmyard manure (", fym_type, ")")))
	d$OM_amount <- as.numeric(r$E.q5103_amtFYM) / plot_ha

	# Harvest method
	if (!is.null(r$L.q602_harvestMethod)) {
		d$harvest_method <- carobiner::replace_values(tolower(as.character(r$L.q602_harvestMethod)),
			c("manual", "reaper", "combine"),
			c("manual", "reaper", "combine harvester"), FALSE)
	}

	# Crop-cut biomass & yield (2 x 2 m2 quadrants, where available e.g. wheat 2018)
	crop_cut_yield <- NULL
	if (all(c("B.q201_q1tagb", "B.q204_q2tagb", "B.q207_q3tagb") %in% names(r))) {
		crop_cut_biomass <- r[, c("B.q201_q1tagb", "B.q204_q2tagb", "B.q207_q3tagb")]
		d$dmy_total <- 10000 * rowMeans(crop_cut_biomass, na.rm=TRUE) / 4
		d$dmy_total[is.nan(d$dmy_total)] <- NA
	}
	if (all(c("B.q202_q1gWeight", "B.q205_q2gWeight", "B.q208_q3gWeight") %in% names(r))) {
		ccy <- r[, c("B.q202_q1gWeight", "B.q205_q2gWeight", "B.q208_q3gWeight")]
		ccy[ccy == 0] <- NA
		crop_cut_yield <- 10000 * rowMeans(ccy, na.rm=TRUE) / 4
		crop_cut_yield[is.nan(crop_cut_yield)] <- NA
		if (all(c("B.q203_q1gMoist", "B.q206_q2gMoist", "B.q209_q3gMoist") %in% names(r))) {
			moist <- r[, c("B.q203_q1gMoist", "B.q206_q2gMoist", "B.q209_q3gMoist")]
			moist[moist == 0] <- NA
			crop_cut_moist <- rowMeans(moist, na.rm=TRUE)
			crop_cut_moist[is.nan(crop_cut_moist) | is.na(crop_cut_moist)] <- 14
			crop_cut_yield <- crop_cut_yield * (100 - crop_cut_moist) / 86
		}
	}

	# Self-reported yield in quintals (1 quintal = 100 kg) per largest plot
	d$yield <- 100 * as.numeric(r$L.q606_largestPlotYieldQUNITAL) / plot_ha

	if (!is.null(crop_cut_yield) && any(!is.na(crop_cut_yield))) {
		d$crop_cut <- !is.na(crop_cut_yield)
		d$yield[d$crop_cut] <- crop_cut_yield[d$crop_cut]
	}

	# Crop selling price (raw is INR per quintal -> INR per kg)
	d$crop_price <- as.numeric(r$M.q706_cropSP) / 100
	d$crop_price[!is.na(d$crop_price) & d$crop_price == 0] <- NA
	d$currency <- "INR"

	# Ensure no stray whitespace or empty strings in any character column
	for (cn in names(d)) {
		if (is.character(d[[cn]])) {
			x <- trimws(d[[cn]])
			x[x == ""] <- NA
			d[[cn]] <- x
		}
	}

	d
}


N2A_monitoring_2 <- function(ff, path) {	

	fix_crop <- function(p) {
	
		p[p=="busbean"] <- "bush bean"	
		p[p=="bushbean"] <- "bush bean"	
		p[p=="oignon"] <- "onion"	
	
		p[grep("^grou", p, ignore.case=TRUE)] <- "groundnut"	
		p[p=="grundnut"] <- "groundnut"	
		p[p=="grungnut"] <- "groundnut"	
		p[grep("soja", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("soy", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("sweet p", p, ignore.case=TRUE)] <- "sweetpotato"	
		p[grep("sweetp", p, ignore.case=TRUE)] <- "sweetpotato"	
		p[grep("swetpot", p, ignore.case=TRUE)] <- "sweetpotato"	
		p <- gsub("n/a", NA, p)
		p <- gsub("tobaco", "tobacco", p)
		p <- gsub("beans", "common bean", p)
		p <- gsub("pumpkins", "pumpkin", p)
		p <- gsub("irish potatoes", "potato", p)
		p <- gsub("irish potato", "potato", p)
		p <- gsub("patatoe", "potato", p)
		p <- gsub("patato", "potato", p)
		p <- gsub(" ma$", " maize", p)
		p <- gsub(", ", "; ", p)
		p <- gsub(" ;", ";", p)
		p <- gsub(" and ", "; ", p)
		p <- gsub("\\+|/| &|&|,", "; ", p)
		p <- gsub("maize; bean", "maize; common bean", p)
		p <- gsub("farrow", "no crop", p)
		p <- gsub("fallow", "no crop", p)
		p <- gsub("pegion pea", "pigeon pea", p)
		p <- gsub("groundnuts", "groundnut", p)
		p <- gsub("local maize", "groundnut", p)
		p <- gsub("fingermillet", "finger millet", p)
		p <- gsub("amaranthas", "amaranth", p)
		p <- gsub("amaranthus", "amaranth", p)
		p <- gsub("tomatoes", "tomato", p)
		p <- gsub("green amarantha", "amaranth", p)
		p <- gsub("rice upland", "rice", p)
		p <- gsub("kales", "kale", p)
		p <- gsub(" intercrop", "", p)
		p <- gsub("cowpeas", "cowpea", p)
		p <- gsub("simsim", "sesame", p)
		p <- gsub("sugar cane", "sugarcane", p)
		p <- gsub(" ;", ";", p)
		p <- gsub("  ", " ", p)
		trimws(p)

	}
	
	# read the data
	bn <- basename(ff)
	r0 <- read.csv(ff[bn == "a_general.csv"])
	r1 <- read.csv(ff[bn == "c_use_of_package_2.csv"])
	r1$SN <- r1$instanceid <- NULL
	r2 <- read.csv(ff[bn == "e_harvest.csv"])
	r2$SN <- r2$instanceid <- NULL
	#f3 <- ff[bn == "c_use_of_package_3.csv"]
	#r3 <- read.csv(f3) 
	r4 <- read.csv(ff[bn == "d_cropping_calendar.csv"])
	r5 <- read.csv(ff[bn == "b_info_site_2.csv"])
	
	#start processing the 1st data
	d <- data.frame(
		country = r0$country, 
		adm2 = carobiner::fix_name(r0$district, "title"), 
		adm3 = carobiner::fix_name(r0$sector_ward, "title"), 
		location = carobiner::fix_name(r0$vilage, "title"), 
		latitude = r0$gps_latitude, 
		longitude = r0$gps_longitude, 
		season = r0$season, 
		farm_id = r0$farm_id
	)



#farm_id plot_no crop_1_area_harvested crop_1_plants_no crop_1_weight_stover crop_1_weight_grain crop_1_grain_unshelled

	dd <- merge(r1, r2, by = c("id", "farm_id", "plot_no"), all.x = TRUE )

	p <- carobiner::fix_name(dd$mineral_fert_type, "upper")
	p[grepl("UREA", p)] <- "urea"
	
	p[grepl("^SYMP", p)] <- "sympal"
	p[grepl("^S ", p)] <- "S-compound"
	p[grepl("^D ", p)] <- "D-compound"
	p[grepl("^S-COM", p)] <- "S-compound"
	p[grepl("^D-COM", p)] <- "D-compound"
	p[grepl("SUPER D", p)] <- "D-compound"
	p[grepl("SINGLE SUPER PHOSPHATE", p)] <- "SSP"
	p[p == "SUPER PHOSPHATE"] <- "SSP"
	p[p %in% c("NONE", "NOON", "NON", "NO")] <- "none"
	p[p == "FERTILIZER"] <- NA
	p[p == "23:21:0+4S"] <- NA
	p[p == "0.972916667"] <- NA
	
	dd$fertilizer_type <- p

## it is not clear what the quantities refer to if there are multiple products 
## that much of each?	
	    # to get the fertilizer/ha
	ftab <- data.frame(
		name = c("AN", "ATS", "basic slag", "CAN", "C-compound", "CMP", "DAP", "DAS", "D-compound", "DSP", "ERP", "GRP", "gypsum", "KCl", "KNO", "lime", "none", "MAP", "NPK", "NPKS", "NPS", "PKS", "SCU", "SOP", "SSP", "sympal", "TSP", "unknown", "urea", "ZnSO4", "S-compound", "MgSO4", "H3BO3", "CaCO3", "borax"), 
		N = c(34L, 12L, NA, 26L, 7L, 0L, 18L, 21L, 10L, 18L, 0L, 0L, 0L, 0L, 13L, 0L, 0L, 11L, NA, 8L, 23L, 0L, 39L, 0L, 0L, 0L, 0L, NA, 46L, 0L, 8L, 0L, 0L, 0L, 0L), 
		P = c(0, 0, NA, 0, 9.156, 7, 20.1, 0, 20, 20, NA, NA, 0, 0, 0, 0, 0, 52, NA, 21, 21, NA, 0, 0, 8.74, 23, 19.23, NA, 0, 0, 21, 0, 0, 0, 0), 
		K = c(0, 0, NA, 0, 5.785, 0, 0, 0, 10, 0, 0, 0, 0, 49.8, 44, 0, 0, 0, NA, 7, 0, NA, 0, 41.5, 0, 15, 0, NA, 0, 0, 7, 0, 0, 0, 0), 
		S = c(0, 26, NA, 0, 6, 0, 0, 24, 9, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 4, 4, NA, 13, 18, 0, 4, 0, NA, 0, 19.9, 4, 13.19, 0, 0, 0), 
		B = c(0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 17.48, 0, 11.3), 
		Mg = c(0, 0, NA, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 20.19, 0, 0, 0), 
		Ca = c(0, 0, NA, 0, 0, 23, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0.4, 0), 
		Zn = c(0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 40.5, 0, 0, 0, 0, 0)
	)

	
## NPK is undefined need to check of 20-20-20 is a good guess
	ftab[ftab$name=="NPK", c("N", "P", "K", "S")] <- c(20, 20, 20, 0)	
	get_elements <- carobiner::get_function("get_elements_from_product", path, group)
	elements <- get_elements(ftab, p)
	
	dd <- cbind(dd, elements)
  
	dd$OM_amount <- as.numeric(dd$organic_fert_amount)
	dd$OM_used <- dd$organic_fert_amount > 0
	dd$OM_type <- carobiner::fix_name(dd$organic_fert_type, "tolower")
	dd$OM_type[!dd$OM_used] <- "none"

	dd$yield <- 10000 * dd$crop_1_weight_grain / dd$crop_1_area_harvested
	
	dd$inoculant_used[dd$inoculant_used == ""] <- NA
	dd$inoculated <- dd$inoculant_used == "Y"
	
	#standardizing the crops and variety
	dd$variety_type <- NA
	dd$crop <- fix_crop(dd$crop_1)
	i <- dd$crop == "bush bean"
	dd$crop[i] <- "common bean"
	dd$variety_type[i] <- "bush bean"
	i <- dd$crop == "climbing bean"
	dd$crop[i] <- "common bean"
	dd$variety_type[i] <- "climbing bean"

	dd$variety <- carobiner::fix_name(dd$variety_1, "title")
	dd$rep <- dd$plot_no
 	
## to do: also deal with crop_2/variety_2 
	dd <- dd[, c("id", "farm_id", "rep", "crop", "variety", "inoculated", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer", "yield", "OM_used", "OM_type", "OM_amount")]
	
	#get the dates information
	if (!is.null(r4$date_planting_yyyy)) {
		dd4 <- r4[, "farm_id", drop=FALSE]	
		p <- apply(r4[, c("date_planting_yyyy", "date_planting_mm", "date_planting_dd")], 1, paste, collapse="-")
		dd4$planting_date <- as.character(as.Date(p))

		i <- r4$date_harvest_dd == 0
		j <- r4$date_harvest_yy == 0
		r4$date_harvest_dd[i] <- 15
		h <- apply(r4[, c("date_harvest_yyyy", "date_harvest_mm", "date_harvest_dd")], 1, paste, collapse="-")
		h[j] <- NA

		dd4$harvest_date <- as.character(as.Date(h))
	} else {
	## cannot merge without plot_id!!
#		pd <- r4[r4$activity == 'Date of planting', ]
#		pd$planting_date <- apply(pd[, c("yyyy", "mm", "dd")], 1, paste, collapse="_")
#		hd <- r4[r4$activity == 'Date of harvest', ]
#		hd$harvest_date <- apply(hd[, c("yyyy", "mm", "dd")], 1, paste, collapse="_")
#		v <- c("id", "farm_id")
#		m <- merge(pd[,c(v, "planting_date")], hd[,c(v, "harvest_date")], by=v)
		dd4 <- NULL
	}

	#standardizing the previous crop variable

	dd5 <- r5[, "farm_id", drop=FALSE]	
	dd5$previous_crop <- fix_crop(carobiner::fix_name(r5$main_crop_last_season, "lower"))
	
	#merge the data sets
	z <- merge(dd, d, by = "farm_id", all.x=TRUE)
	#z <- merge(z, d3, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd4, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd5, by = "farm_id", all.x=TRUE)
	
	z$yield_part <- "seed"
	z$trial_id <- z$farm_id
	z$farm_id <- NULL
	z$id <- NULL
	
	z
}





N2A_monitoring_1 <- function(ff) {	

	fix_crop <- function(p) {
		p[grep("^grou", p, ignore.case=TRUE)] <- "groundnut"	
		p[grep("soy", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("sweet pot", p, ignore.case=TRUE)] <- "sweetpotato"	
		p <- gsub("tobaco", "tobacco", p)
		p <- gsub("beans", "common bean", p)
		p <- gsub("irish potatoes", "potato", p)
		p <- gsub(" ma$", " maize", p)
		p <- gsub(", ", "; ", p)
		p
	}

	# read the data
	bn <- basename(ff)
	f0 <- ff[bn == "a_general.csv"]
	r0 <- read.csv(f0)
	
	f1 <- ff[bn == "c_use_of_package_1.csv"]
	d1 <- read.csv(f1)
	
	f2 <- ff[bn == "e_harvest.csv"]
	d2 <- read.csv(f2)
	
	f3 <- ff[bn == "c_use_of_package_3.csv"]
	d3 <- read.csv(f3) 

	f4 <- ff[bn == "d_cropping_calendar.csv"]
	r4 <- read.csv(f4)
	
	f5 <- ff[bn == "b_info_site_2.csv"]
	d5 <- read.csv(f5)
	
	#start processing the 1st data
	d <- data.frame(
		country = r0$country, 
		adm2 = carobiner::fix_name(r0$action_site, "Title"), 
		adm3 = carobiner::fix_name(r0$sector_ward, "Title"), 
		location = carobiner::fix_name(r0$vilage, "Title"), 
		latitude = r0$gps_latitude, 
		longitude = r0$gps_longitude, 
		season = r0$season, 
		farm_id= r0$farm_id
	)

	#subset the variables of interest in d1 and d2	
	d1 <- d1[, c("farm_id", "plot_no", "crop", "variety", "inoculant_used", "min_fertilizer_type", "min_fertiliser_amount_kg", "org_fertilizer_type", "org_fertilizer_amount_kg")]
	d2 <- d2[, c("farm_id", "plot_no", "area_harvested_m2", "weight_kg")]
	dd <- merge(d1, d2, by = c("farm_id", "plot_no"), all.x = TRUE )
	
	# working on fertilizer types
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("SSP/Urea", "SSP+Ureia", "SSP+Urea", "Urea+SSP")] <- "SSP; urea"	
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("ssp", "Phosphor(SSP)", "SSP+Inoc", "Y", "12")] <- "SSP"
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("Ureia", "Urea")] <- "urea"
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c( "None", "", "N")] <- "none"
	
	#working on fertilizer amounts
	dd$min_fertiliser_amount_kg[dd$min_fertiliser_amount_kg %in% c("2/0.4", "2 /0.4", "2/0,4", "2/ 0,4")] <- "2/0.4"
	dd$min_fertiliser_amount_kg <- carobiner::replace_values(dd$min_fertiliser_amount_kg, c("2kg/ha", "", "N"), c("2", "0", "0"))
	
	#split fertilizer amount column to separate urea amounts and SSP amounts
	dd$ssp_amt <- ifelse(dd$min_fertilizer_type != "urea", as.numeric(sub("/.*", "", dd$min_fertiliser_amount_kg)), 0)
	dd$urea_amt <- ifelse(dd$min_fertilizer_type == "urea", as.numeric(sub(".*?/", "", dd$min_fertiliser_amount_kg)), 0)
	dd$fertilizer_type <- dd$min_fertilizer_type
	#to get rates of N and P
	dd$P_rate_plot <- dd$ssp_amt*0.16
	
	# for 10.25502/hwdb-p578
	#v <- carobiner::replace_values(dd$area_harvested_m2, c(101, 102, 103, 104), 							c(100, 100, 100, 100))

	## how so ???
	#v[is.na(v)] <- 100
	#dd$area_harvested_m2 <- v
	
	dd$P_fertilizer <- (10000/dd$area_harvested_m2) * dd$P_rate_plot
	dd$N_rate_plot <- dd$urea_amt*0.467
	dd$N_fertilizer <- (10000/dd$area_harvested_m2) * dd$N_rate_plot
	dd$K_fertilizer <- 0
	#getting the yield
	dd$yield <- (10000/dd$area_harvested_m2) * dd$weight_kg
	
	#correcting mismatched rows in inoculated
	dd$inoculated <- dd$inoculant_used
	
	#standardizing the crops and variety
	dd$crop <- fix_crop(dd$crop)
	dd$variety <- carobiner::fix_name(dd$variety, "title")
	dd$rep <- dd$plot_no

## FIX NEEDED: ALSO USE organic_fert_type"  "organic_fert_amount
 	
	dd <- dd[, c("farm_id", "rep", "crop", "variety", "inoculated", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer", "yield")]
	
	#get the spacing information
	dd3 <- d3[, "farm_id", drop=FALSE]
	dd3$row_spacing <- as.numeric(d3$crop_1_spacing_row_to_row)
	dd3$plant_spacing <- as.numeric(d3$crop_1_spacing_plant_to_plant)
	dd3 <- unique(dd3)
	
	dd4 <- unique(r4[, "farm_id", drop=FALSE])
	p <- r4[grepl("planting", r4$activity), ]
	h <- r4[grepl("harvest", r4$activity), ]
	p$planting_date <- with(p, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	h$harvest_date <- with(h, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	p$planting_date[p$date_planting_yyyy == 0] <- NA
	h$harvest_date[h$date_planting_yyyy == 0] <- NA

	p <- p[, c("farm_id", "planting_date")]
	h <- h[, c("farm_id", "harvest_date")]
	ph <- unique(merge(p, h, by = "farm_id", all=TRUE))
	dd4 <- merge(dd4, ph, by = "farm_id", all.x=TRUE)
	
	dd4$planting_date <- as.character(as.Date(dd4$planting_date, format = "%Y-%m-%d"))
	dd4$harvest_date <- as.character(as.Date(dd4$harvest_date, format = "%Y-%m-%d"))
	
	#standardizing the previous crop variable
	dd5 <- d5[, "farm_id", drop=FALSE]
	dd5$previous_crop <- fix_crop(carobiner::fix_name(d5$main_crop_last_season, "lower"))
	
	#merge the data sets
	z <- merge(d, dd, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd3, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd4, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd5, by = "farm_id", all.x=TRUE)
	
	z$yield_part <- ifelse(z$crop == "groundnut", "pod", "seed")
	z$trial_id <- z$farm_id
	z$farm_id <- NULL
	z
}
