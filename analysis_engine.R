clean_names <- function(x) {
  x <- trimws(tolower(x)); x <- gsub("[^a-z0-9]+", "_", x); gsub("(^_+|_+$)", "", x)
}
yes <- function(x) tolower(trimws(as.character(x))) %in% c("yes", "1", "true", "y")
num <- function(x) suppressWarnings(as.numeric(x))
valid_w <- function(d) { w <- if ("weights" %in% names(d)) num(d$weights) else rep(1, nrow(d)); w[!is.finite(w) | w < 0] <- NA; w }

wealth_asset_variables <- c(
  "source_drinking_water","toilet_facility","share_toilet_facility","hh_use_toilet",
  "toilet_facility_location","type_cookstove","hh_room_sleeping","owns_livestock",
  "animals_hh","cows","other_cattle","horses_donkeys","goats","sheep","chickens",
  "hh_assetsbed","hh_assetschair","hh_assetscomputer","hh_assetselectricity",
  "hh_assetsfan","hh_assetsgenerator","hh_assetsnon_mobile_telephone","hh_assetsradio",
  "hh_assetsrefrigerator","hh_assetssofa","hh_assetstable","hh_assetstelevision",
  "floor","roof","walls"
)

derive_wealth_index <- function(d) {
  if (!nrow(d)) return(d)
  if ("wealth_quintile" %in% names(d) && any(!is.na(d$wealth_quintile) & trimws(as.character(d$wealth_quintile))!="")) return(d)

  missing_vars <- setdiff(wealth_asset_variables,names(d))
  if (length(missing_vars)) {
    stop("Cannot derive the wealth index. Missing household asset column(s): ",paste(missing_vars,collapse=", "),call.=FALSE)
  }

  wealth_data <- as.data.frame(lapply(d[wealth_asset_variables],function(x) as.numeric(as.factor(x))))
  for (v in names(wealth_data)) {
    x <- wealth_data[[v]]
    med <- suppressWarnings(stats::median(x,na.rm=TRUE))
    if (is.finite(med)) x[is.na(x)] <- med
    wealth_data[[v]] <- x
  }

  usable <- vapply(wealth_data,function(x) all(is.finite(x)) && stats::sd(x)>0,logical(1))
  if (sum(usable)<2) stop("Cannot derive the wealth index because fewer than two usable asset variables remain after cleaning.",call.=FALSE)

  wealth_pca <- stats::prcomp(wealth_data[,usable,drop=FALSE],center=TRUE,scale.=TRUE)
  score <- as.numeric(wealth_pca$x[,1])
  quintile_num <- rep(NA_integer_,length(score))
  ordered_rows <- order(score,na.last=NA)
  quintile_num[ordered_rows] <- floor((seq_along(ordered_rows)-1)*5/length(ordered_rows))+1L
  quintile_labels <- c("Poorest","Second","Middle","Fourth","Highest")

  d$wealth_score <- score
  d$wealth_quintile_num <- quintile_num
  d$wealth_quintile <- factor(quintile_labels[quintile_num],levels=quintile_labels)
  d
}

indicator_catalog <- data.frame(
  module=c(
    rep("household",3), "members", rep("household",13), rep("members",7), rep("children",9), rep("women",17)
  ),
  id=c(
    "hh_nets","avg_nets","avg_hh_size","itn_access","net_source","net_nonuse_reasons",
    "hh_income","hh_head_gender","residence_distribution","hh_head_occupation","water_source","sanitation_facility",
    "sanitation_ladder","floor_material","roof_material","wall_material","rooms_sleeping",
    "slept_itn","u5_slept_itn","pregnant_slept_itn","member_age_sex","women_15_49_count","u5_count","wealth_distribution",
    "child_fever","fever_tested","confirmed_malaria","sought_treatment","treatment_place","tested_by_source",
    "took_medicine","medicine_types","received_smc",
    "receive_anc","anc_place","anc_provider","anc_visits","first_anc_trimester","last_preg_iptp","iptp_uptake",
    "malaria_messages","message_sources","knows_prevention","prevention_methods",
    "ideation_domains","women_age","women_education","women_religion","had_live_birth","currently_pregnant"
  ),
  label=c(
    "Households with at least one ITN","Average number of ITNs per household","Average household size",
    "Population with access to an ITN","Sources of ITNs",
    "Reasons for not using an ITN","Average household income","Sex of household head","Households by rural/urban residence",
    "Occupation of household head","Source of drinking water","Sanitation facility","Sanitation service ladder",
    "Floor construction material","Roof construction material","Wall construction material","Rooms used for sleeping",
    "De-facto household members who slept under an ITN","Children under five who slept under an ITN",
    "Pregnant women who slept under an ITN","De-jure population by age group and sex","Women age 15–49",
    "Children under five","Wealth quintile distribution",
    "Children with fever in previous two weeks","Children with fever tested for malaria","Tested children with confirmed malaria",
    "Children with fever for whom treatment was sought","Place treatment was sought","Malaria testing by source of care",
    "Children with fever who took medicine","Medicines taken",
    "Children who received seasonal malaria chemoprevention",
    "Women who received ANC in their last or current pregnancy","Place ANC was received for the last pregnancy","Provider seen for ANC during the last pregnancy","ANC visits during the last pregnancy",
    "Trimester of first ANC visit during the last pregnancy","IPTp during the last pregnancy","IPTp1+/2+/3+/4+ uptake during the last pregnancy",
    "Women who heard malaria messages","Sources of malaria messages",
    "Women who know a way to prevent malaria","Malaria prevention methods mentioned",
    "Malaria ideation domains","Age distribution of women","Highest education of women","Religion of women",
    "Women who have had a live birth","Women currently pregnant"
  ), stringsAsFactors=FALSE
)

indicator_catalog$source_module <- indicator_catalog$module

bucket_map <- list(
  characteristics=c(
    "avg_hh_size","hh_income","hh_head_gender","residence_distribution","hh_head_occupation",
    "water_source","sanitation_facility","sanitation_ladder","floor_material","roof_material",
    "wall_material","rooms_sleeping","member_age_sex","women_15_49_count","u5_count",
    "wealth_distribution","women_age","women_education","women_religion","had_live_birth",
    "currently_pregnant"
  ),
  prevention=c(
    "hh_nets","avg_nets","itn_access","net_source","net_nonuse_reasons","slept_itn",
    "u5_slept_itn","pregnant_slept_itn","received_smc","receive_anc","anc_place",
    "anc_provider","anc_visits","first_anc_trimester","last_preg_iptp","iptp_uptake"
  ),
  treatment=c(
    "child_fever","fever_tested","confirmed_malaria","sought_treatment","treatment_place",
    "tested_by_source","took_medicine","medicine_types"
  ),
  beliefs=c(
    "malaria_messages","message_sources","knows_prevention","prevention_methods","ideation_domains"
  )
)

indicator_catalog$module <- NA_character_
for (bucket in names(bucket_map)) indicator_catalog$module[indicator_catalog$id %in% bucket_map[[bucket]]] <- bucket
if (any(is.na(indicator_catalog$module))) stop("Some indicators are not assigned to a display bucket: ",paste(indicator_catalog$id[is.na(indicator_catalog$module)],collapse=", "))

module_labels <- c(
  characteristics="Housing, household and respondent characteristics",
  prevention="Malaria prevention",
  treatment="Health seeking behavior and treatment",
  beliefs="Malaria beliefs and exposure to malaria messages"
)

source_module_for_indicator <- function(id) indicator_catalog$source_module[match(id,indicator_catalog$id)]

default_groups <- c("Overall","urban_rural_classification","wealth_quintile","zone","lga")
extra_groups <- list(
  household=c("gender","level_of_education"), members=c("hh_mem_gender","age_group"),
  children=c("gender"), women=c("women_hightest_educ","women_religion")
)

top_four_characteristic_indicators <- c(
  "hh_head_occupation","water_source","sanitation_facility","floor_material",
  "roof_material","wall_material","women_education","women_religion"
)

standardize_data <- function(d, rural=NULL, wealth=NULL) {
  names(d) <- clean_names(names(d))
  if ("lga" %in% names(d)) d$lga <- tolower(trimws(d$lga))
  rural_cols <- c("urban_rural_classification","urban_rural_classification_x","urban_rural_classification_y")
  existing <- rural_cols[rural_cols %in% names(d)]
  if (length(existing)) d$urban_rural_classification <- d[[existing[1]]]
  if (!is.null(rural) && "lga" %in% names(d)) {
    rural <- rural[!duplicated(rural$lga), c("lga","urban_rural_classification"), drop=FALSE]
    m <- match(d$lga, rural$lga)
    if (!"urban_rural_classification" %in% names(d)) d$urban_rural_classification <- rural$urban_rural_classification[m]
    else d$urban_rural_classification[is.na(d$urban_rural_classification) | d$urban_rural_classification==""] <- rural$urban_rural_classification[m][is.na(d$urban_rural_classification) | d$urban_rural_classification==""]
  }
  if (!is.null(wealth) && "hhid" %in% names(d)) {
    for (v in intersect(c("wealth_quintile","wealth_score","wealth_quintile_num"), names(wealth))) {
      if (!v %in% names(d)) d[[v]] <- wealth[[v]][match(d$hhid, wealth$hhid)]
    }
  }
  age_breaks <- c(-Inf,4,9,14,19,24,29,34,39,44,49,54,59,64,Inf)
  age_labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65+")
  if ("hh_mem_age" %in% names(d)) d$age_group <- cut(num(d$hh_mem_age),age_breaks,labels=age_labels,right=TRUE)
  if (!"age_group" %in% names(d) && "age_diff" %in% names(d)) d$age_group <- cut(num(d$age_diff),age_breaks,labels=age_labels,right=TRUE)
  if ("age_diff" %in% names(d)) {
    women_age <- num(d$age_diff)
    women_lower <- floor(women_age/5)*5
    women_label <- ifelse(is.finite(women_age) & women_age>=15 & women_age<=49,paste0(women_lower,"-",women_lower+4),NA_character_)
    expected <- paste0(seq(15,45,5),"-",seq(19,49,5))
    d$women_age_group <- factor(women_label,levels=expected)
  }
  d
}

available_groups <- function(d, module) unique(c(default_groups, extra_groups[[module]]))[unique(c(default_groups, extra_groups[[module]])) %in% c("Overall",names(d))]

apply_eligibility <- function(d, id) {
  keep <- rep(TRUE,nrow(d))
  eq <- function(v,val) if (v %in% names(d)) !is.na(d[[v]]) & d[[v]]==val else rep(FALSE,nrow(d))
  if (id=="net_source") keep <- eq("hh_have_nets","Yes")
  if (id=="net_nonuse_reasons") keep <- eq("hh_have_nets","Yes") & eq("sleep_under_net","No")
  if (id=="slept_itn") keep <- eq("sleep_here_last_night","Yes")
  if (id=="u5_slept_itn") keep <- (if("child_u5" %in% names(d)) num(d$child_u5)==1 else FALSE)
  if (id=="pregnant_slept_itn") keep <- eq("pregnancy","Yes")
  if (id %in% c("fever_tested","sought_treatment","treatment_place","took_medicine")) keep <- eq("child_ill","Yes")
  if (id=="treatment_place") keep <- keep & eq("seek_treatment","Yes")
  if (id=="confirmed_malaria") keep <- eq("child_ill","Yes") & eq("blood_taken","Yes")
  if (id=="tested_by_source") keep <- eq("child_ill","Yes") & eq("seek_treatment","Yes") & (if("treatment_place" %in% names(d)) !is.na(d$treatment_place) & d$treatment_place!="" else FALSE)
  if (id=="medicine_types") keep <- !is.na(d$type_medicine) & d$type_medicine!=""
  if (id=="receive_anc") keep <- eq("had_live_birth","Yes") | eq("pregnancy","Yes")
  if (id %in% c("anc_place","anc_provider","anc_visits","first_anc_trimester")) keep <- eq("had_live_birth","Yes") & eq("receive_anc","Yes")
  if (id %in% c("last_preg_iptp","iptp_uptake")) keep <- eq("had_live_birth","Yes")
  if (id=="message_sources") keep <- eq("malaria_msg","Yes")
  if (id=="prevention_methods") keep <- eq("way_to_avoid_malaria","Yes")
  d[!is.na(keep) & keep,,drop=FALSE]
}

indicator_spec <- function(d,id) {
  binary <- c(hh_nets="hh_have_nets",slept_itn="slept_ubder_net_mem",u5_slept_itn="slept_ubder_net_mem",pregnant_slept_itn="slept_ubder_net_mem",
    child_fever="child_ill",fever_tested="blood_taken",confirmed_malaria="confirm_malaria",sought_treatment="seek_treatment",took_medicine="take_medicine",
    received_smc="receive_chemoprevention",receive_anc="receive_anc",last_preg_iptp="last_preg",
    malaria_messages="malaria_msg",knows_prevention="way_to_avoid_malaria",had_live_birth="had_live_birth",currently_pregnant="pregnancy")
  means <- c(avg_nets="nets_num",avg_hh_size="hh_member",hh_income="average_income")
  categorical <- c(hh_head_gender="gender",residence_distribution="urban_rural_classification",
    hh_head_occupation="occupation_hh_head",water_source="source_drinking_water",sanitation_facility="toilet_facility",floor_material="floor",
    roof_material="roof",wall_material="walls",women_15_49_count="woman_age_1549",u5_count="child_u5",
    wealth_distribution="wealth_quintile",treatment_place="treatment_place",anc_place="where_receive_anc",
    anc_provider="whom_see",women_age="women_age_group",
    women_education="women_hightest_educ",women_religion="women_religion")
  multi <- list(
    net_nonuse_reasons=c("reason_net_wasnt_used_membed_bug","reason_net_wasnt_used_memdont_li","reason_net_wasnt_used_memnet_not","reason_net_wasnt_used_memnet_too","reason_net_wasnt_used_memno_mosq","reason_net_wasnt_used_memothers","reason_net_wasnt_used_memslept_o","reason_net_wasnt_used_memunable_","reason_net_wasnt_used_memusual_u","reason_net_wasnt_used_memweather"),
    message_sources=c("msg_heard_seenanywhereelse","msg_heard_seencommunityeventoutr","msg_heard_seencommunityhealthwor","msg_heard_seenposterbillboard","msg_heard_seenradio","msg_heard_seentelevision"),
    prevention_methods=c("malaria_preventionavoid_stagnant","malaria_preventiondont_know","malaria_preventionkeep_surroundi","malaria_preventionothers","malaria_preventionput_mosquito_s","malaria_preventionsleep_inside_a","malaria_preventionspray_house_wi","malaria_preventiontake_preventat","malaria_preventionuse_mosquito_r")
  )
  if (id=="tested_by_source") return(list(kind="binary_by_category",vars=c("blood_taken","treatment_place")))
  if (id=="member_age_sex") return(list(kind="age_sex",vars=c("age_group","hh_mem_gender")))
  if (id %in% names(binary)) return(list(kind="binary",vars=binary[[id]]))
  if (id %in% names(means)) return(list(kind="mean",vars=means[[id]]))
  if (id %in% names(categorical)) return(list(kind="categorical",vars=categorical[[id]]))
  if (id %in% names(multi)) return(list(kind="multi",vars=multi[[id]]))
  if (id=="medicine_types") return(list(kind="tokens",vars="type_medicine"))
  if (id=="itn_access") return(list(kind="ratio",vars=c("num_nets","de_facto_population")))
  if (id=="net_source") return(list(kind="derived_category",vars=c("get_through_campaign","get_net"),fun=function(z){ x<-as.character(z$get_through_campaign); x[x=="Yes_Campaign"]<-"Campaign";x[x=="Yes_ANC"]<-"ANC";x[x=="Yes_Immunization_Visit"]<-"Immunization Visit"; use_other<-x=="No" & !is.na(z$get_net) & z$get_net!="";x[use_other]<-z$get_net[use_other];x[is.na(x)|x=="No"|x==""]<-"Source not recorded";x }))
  if (id=="sanitation_ladder") return(list(kind="derived_category",vars=c("share_toilet_facility","toilet_facility"),fun=function(z){ imp <- grepl("flush|pit latrine|compost",tolower(z$toilet_facility)); ifelse(imp & z$share_toilet_facility=="No","Basic",ifelse(imp,"Limited","Unimproved")) }))
  if (id=="rooms_sleeping") return(list(kind="derived_category",vars="hh_room_sleeping",fun=function(z){ n<-num(z$hh_room_sleeping);factor(ifelse(n==1,"1",ifelse(n==2,"2",ifelse(n>=3,"3+",NA_character_))),levels=c("1","2","3+")) }))
  if (id=="first_anc_trimester") return(list(kind="derived_category",vars="weeks_anc",fun=function(z) cut(num(z$weeks_anc),c(-Inf,12,27,Inf),labels=c("1st Trimester","2nd Trimester","3rd Trimester"))))
  if (id=="anc_visits") return(list(kind="derived_category",vars="time_anc",fun=function(z){ n<-num(z$time_anc);ifelse(n==1,"One",ifelse(n==2,"Two",ifelse(n==3,"Three",ifelse(n>=4,"Four or more",NA)))) }))
  if (id=="iptp_uptake") return(list(kind="thresholds",vars="time_taken_fansidar"))
  if (id=="ideation_domains") return(list(kind="ideation",vars=character()))
  list(kind="unavailable",vars=character())
}

weighted_rows <- function(d,group,category,value=NULL,kind="binary") {
  w <- valid_w(d); g <- if(group=="Overall") rep("Overall",nrow(d)) else as.character(d[[group]]); g[is.na(g)|g==""] <- "Missing"
  idx <- split(seq_len(nrow(d)),g)
  do.call(rbind,lapply(names(idx),function(nm){i<-idx[[nm]]; ok<-is.finite(w[i]) & !is.na(value[i]); den<-sum(w[i][ok],na.rm=TRUE)
    est <- if(kind=="mean") sum(w[i][ok]*num(value[i][ok]),na.rm=TRUE)/den else 100*sum(w[i][ok]*num(value[i][ok]),na.rm=TRUE)/den
    data.frame(Group=nm,Result=category,Estimate=round(est,1),Unweighted_N=sum(ok),Weighted_N=round(den,1),stringsAsFactors=FALSE)
  }))
}

option_label <- function(v) {
  labels <- c(
    msg_heard_seenanywhereelse="Other source",
    msg_heard_seencommunityeventoutr="Community event or outreach",
    msg_heard_seencommunityhealthwor="Community health worker",
    msg_heard_seenposterbillboard="Poster or billboard",
    msg_heard_seenradio="Radio",
    msg_heard_seentelevision="Television",
    malaria_preventionavoid_stagnant="Avoid stagnant water",
    malaria_preventiondont_know="Don't know",
    malaria_preventionkeep_surroundi="Keep surroundings clean",
    malaria_preventionothers="Other method",
    malaria_preventionput_mosquito_s="Install mosquito screens",
    malaria_preventionsleep_inside_a="Sleep under a mosquito net",
    malaria_preventionspray_house_wi="Spray the house with insecticide",
    malaria_preventiontake_preventat="Take preventive treatment",
    malaria_preventionuse_mosquito_r="Use mosquito repellent",
    reason_net_wasnt_used_membed_bug="Bed bugs",
    reason_net_wasnt_used_memdont_li="Don't like smell",
    reason_net_wasnt_used_memnet_not="Net not available",
    reason_net_wasnt_used_memnet_too="Net too hot",
    reason_net_wasnt_used_memno_mosq="No mosquitoes",
    reason_net_wasnt_used_memothers="Other reason",
    reason_net_wasnt_used_memslept_o="Slept outside",
    reason_net_wasnt_used_memunable="Unable to hang net",
    reason_net_wasnt_used_memusual_u="Usually don't use nets",
    reason_net_wasnt_used_memweather="Weather condition"
  )
  if (v %in% names(labels)) unname(labels[[v]]) else gsub("_"," ",v)
}

collapse_top_categories <- function(x,w,top_n=4) {
  x <- trimws(as.character(x)); missing <- is.na(x) | x==""
  valid <- !missing & is.finite(w)
  totals <- tapply(w[valid],x[valid],sum,na.rm=TRUE)
  if (!length(totals)) return(x)
  keep <- names(sort(totals,decreasing=TRUE))[seq_len(min(top_n,length(totals)))]
  x[!missing & !x %in% keep] <- "Other"
  factor(x,levels=c(keep,if(any(x=="Other",na.rm=TRUE)) "Other"))
}

compute_member_age_sex <- function(d,group) {
  age <- as.character(d$age_group); sex <- trimws(as.character(d$hh_mem_gender)); w <- valid_w(d)
  cut_value <- if(group=="Overall") rep("Overall",nrow(d)) else as.character(d[[group]])
  ok <- !is.na(age) & age!="" & !is.na(sex) & sex!="" &
    !is.na(cut_value) & cut_value!="" & is.finite(w)
  d2 <- data.frame(cut=cut_value[ok],age=age[ok],sex=sex[ok],w=w[ok],stringsAsFactors=FALSE)
  if (!nrow(d2)) stop("No eligible records for this indicator.")
  age_order <- levels(d$age_group)
  if (is.null(age_order)) age_order <- unique(d2$age)
  cuts <- unique(d2$cut); sexes <- unique(d2$sex)
  rows <- list(); k <- 0L
  for (cv in cuts) for (ag in age_order[age_order %in% d2$age[d2$cut==cv]]) {
    z <- d2[d2$cut==cv & d2$age==ag,,drop=FALSE]; den <- sum(z$w,na.rm=TRUE)
    for (sx in sexes[sexes %in% z$sex]) {
      k <- k+1L
      rows[[k]] <- data.frame(
        Group=if(group=="Overall") ag else paste(cv,ag,sep=" | "), Result=sx,
        Estimate=round(100*sum(z$w[z$sex==sx],na.rm=TRUE)/den,1),
        Unweighted_N=nrow(z),Weighted_N=round(den,1),stringsAsFactors=FALSE)
    }
  }
  do.call(rbind,rows)
}

compute_indicator <- function(d,id,group) {
  d <- apply_eligibility(d,id); spec <- indicator_spec(d,id)
  spec$vars <- clean_names(spec$vars)
  missing <- setdiff(spec$vars,names(d)); if(length(missing)) stop("Required column(s) unavailable: ",paste(missing,collapse=", "))
  if (!nrow(d)) stop("No eligible records for this indicator.")
  if(spec$kind=="age_sex") return(compute_member_age_sex(d,group))
  if(spec$kind=="binary") return(weighted_rows(d,group,"Proportion Yes",yes(d[[spec$vars]])))
  if(spec$kind=="binary_by_category") {
    outcome <- spec$vars[1]; category <- spec$vars[2]
    x <- as.character(d[[category]]); x[is.na(x)|x==""] <- "Missing"
    lev <- unique(x)
    return(do.call(rbind,lapply(lev,function(v) weighted_rows(d[x==v,,drop=FALSE],group,v,yes(d[[outcome]][x==v])))))
  }
  if(spec$kind=="mean") return(weighted_rows(d,group,"Weighted mean",d[[spec$vars]],"mean"))
  if(spec$kind=="derived_binary") return(weighted_rows(d,group,"Proportion",spec$fun(d)))
  if(spec$kind=="categorical" || spec$kind=="derived_category") {
    x <- if(spec$kind=="derived_category") spec$fun(d) else d[[spec$vars]]
    if(id %in% top_four_characteristic_indicators) x <- collapse_top_categories(x,valid_w(d),4)
    ordered_levels <- if(is.factor(x)) levels(x) else NULL
    x <- as.character(x); x[is.na(x)|x==""] <- "Missing"
    lev <- if(!is.null(ordered_levels) && !id %in% top_four_characteristic_indicators) ordered_levels[ordered_levels %in% x] else unique(x)
    return(do.call(rbind,lapply(lev,function(v) weighted_rows(d,group,v,x==v))))
  }
  if(spec$kind=="multi") return(do.call(rbind,lapply(spec$vars,function(v) weighted_rows(d,group,option_label(v),num(d[[v]])==1))))
  if(spec$kind=="tokens") { toks<-sort(unique(unlist(strsplit(paste(d[[spec$vars]],collapse=" "),"\\s+")))); toks<-toks[nzchar(toks)]; return(do.call(rbind,lapply(toks,function(v) weighted_rows(d,group,gsub("_"," ",v),grepl(paste0("(^| )",v,"( |$)"),d[[spec$vars]]))))) }
  if(spec$kind=="thresholds") return(do.call(rbind,lapply(1:4,function(k) weighted_rows(d,group,paste0("IPTp",k,"+"),num(d[[spec$vars]])>=k))))
  if(spec$kind=="ratio") {
    if ("hhid" %in% names(d)) d <- d[!duplicated(d$hhid),,drop=FALSE]
    nets<-num(d$num_nets); pop<-num(d$de_facto_population); access<-pmin(ifelse(is.na(nets),0,nets)*2,pop); w<-valid_w(d); g<-if(group=="Overall") rep("Overall",nrow(d)) else as.character(d[[group]]); idx<-split(seq_len(nrow(d)),g)
    return(do.call(rbind,lapply(names(idx),function(nm){i<-idx[[nm]];ok<-is.finite(pop[i])&pop[i]>0&is.finite(w[i]);den<-sum(w[i][ok]*pop[i][ok]);data.frame(Group=nm,Result="ITN access",Estimate=round(100*sum(w[i][ok]*access[i][ok])/den,1),Unweighted_N=sum(ok),Weighted_N=round(den,1))})))
  }
  if(spec$kind=="ideation") {
    domains<-list("Susceptibility/Risk"=tolower(d$worry_malaria)=="agree"|tolower(d$pple_get_malaria_during_rainy)=="disagree","Severe malaria"=tolower(d$malaria_can_be_treated)=="disagree"|tolower(d$weak_child_can_die_frommalaria)=="disagree","Self-efficacy"=tolower(d$sleep_entire_night_lots)=="agree"|tolower(d$sleep_entire_night_few)=="agree","Malaria-related behaviour"=tolower(d$donotlike_sleep_inside)=="disagree"|tolower(d$best_start_taking_medicine_at_ho)=="disagree"|tolower(d$full_dose_medicine)=="agree","Community norms"=tolower(d$take_healthcare_provider)=="agree"|tolower(d$comm_sleep_inside_net)=="agree")
    return(do.call(rbind,lapply(names(domains),function(v) weighted_rows(d,group,v,domains[[v]]))))
  }
  stop("Indicator definition is unavailable.")
}

wide_results <- function(x) {
  if (!nrow(x) || length(unique(x$Result)) < 2) return(x)
  groups <- unique(as.character(x$Group)); options <- unique(as.character(x$Result))
  out <- data.frame(Group=groups,stringsAsFactors=FALSE,check.names=FALSE)
  for (opt in options) {
    z <- x[x$Result==opt,,drop=FALSE]
    out[[opt]] <- z$Estimate[match(groups,z$Group)]
  }
  same_denominator <- all(sapply(split(x,x$Group),function(z) length(unique(z$Unweighted_N))==1 && length(unique(z$Weighted_N))==1))
  if (same_denominator) {
    first <- x[!duplicated(x$Group),,drop=FALSE]
    out$Unweighted_N <- first$Unweighted_N[match(groups,first$Group)]
    out$Weighted_N <- first$Weighted_N[match(groups,first$Group)]
  } else {
    for (opt in options) {
      z <- x[x$Result==opt,,drop=FALSE]
      out[[paste(opt,"Unweighted_N")]] <- z$Unweighted_N[match(groups,z$Group)]
      out[[paste(opt,"Weighted_N")]] <- z$Weighted_N[match(groups,z$Group)]
    }
  }
  out
}

remove_missing_options <- function(x) {
  if (!"Result" %in% names(x)) return(x)
  x[tolower(trimws(as.character(x$Result)))!="missing",,drop=FALSE]
}
