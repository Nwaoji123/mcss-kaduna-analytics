options(shiny.maxRequestSize=1024^3)
need <- c("shiny","readxl","ggplot2","bslib","DT","leaflet","sf")
missing <- need[!vapply(need,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop("Run install_packages.R first. Missing: ",paste(missing,collapse=", "),call.=FALSE)
library(shiny); library(ggplot2); library(bslib); library(DT); library(leaflet); source("analysis_engine.R",local=TRUE)

read_csv_clean <- function(path) { d<-read.csv(path,check.names=FALSE,stringsAsFactors=FALSE,na.strings=c("","NA")); names(d)<-clean_names(names(d)); d }

pie_eligible_indicators <- c(
  "hh_head_gender","residence_distribution","hh_head_occupation","water_source","sanitation_facility",
  "sanitation_ladder","floor_material","roof_material","wall_material","rooms_sleeping",
  "wealth_distribution","treatment_place","anc_place","anc_provider","women_education","women_religion"
)

stacked_eligible_indicators <- c(
  pie_eligible_indicators,
  "member_age_sex","women_15_49_count","u5_count","women_age","anc_visits","first_anc_trimester",
  "net_source"
)

map_class_cols <- c(Low="#D73027",Medium="#F6C945",High="#1A9850","No data"="#D9DEE3")
chart_palette <- c("#178F7A","#4D82C4","#D99A2B","#D96861","#8272B7","#87949A")

map_lga_key <- function(x) {
  x <- clean_names(x)
  x[x=="jema_a"] <- "jemaa"
  x
}

classify_map_values <- function(v,is_mean=FALSE) {
  out <- rep("No data",length(v)); ok <- is.finite(v)
  if (!any(ok)) return(factor(out,levels=c("Low","Medium","High","No data")))
  if (is_mean) {
    r <- rank(v[ok],ties.method="average",na.last="keep")/sum(ok)
    out[ok] <- ifelse(r<=1/3,"Low",ifelse(r<=2/3,"Medium","High"))
  } else {
    out[ok] <- ifelse(v[ok]<40,"Low",ifelse(v[ok]<70,"Medium","High"))
  }
  factor(out,levels=c("Low","Medium","High","No data"))
}

read_lga_boundaries <- function(path) {
  shp <- sf::st_read(path,quiet=TRUE,stringsAsFactors=FALSE)
  shp <- sf::st_transform(shp,4326)
  names(shp) <- clean_names(names(shp))
  if (!all(c("statename","lganame") %in% names(shp))) stop("Boundary file must include statename and lganame fields.")
  shp <- shp[tolower(trimws(shp$statename))=="kaduna",,drop=FALSE]
  shp$join_lga <- map_lga_key(shp$lganame)
  shp
}

make_lga_map_plot <- function(shp,title,is_mean=FALSE) {
  subtitle <- if (is_mean) {
    "LGA weighted means classified into tertiles"
  } else {
    "LGA estimates: Low 0-39.9%; Medium 40-69.9%; High 70-100%"
  }
  # Place labels within polygons so they remain legible for irregular LGA shapes.
  label_points <- sf::st_transform(shp, 32632)
  label_points <- sf::st_point_on_surface(label_points)
  label_points <- sf::st_transform(label_points, 4326)
  suffix <- if (is_mean) "" else "%"
  label_points$map_label <- ifelse(
    is.na(label_points$Estimate),
    paste(label_points$lganame, "No data", sep = "\n"),
    paste0(label_points$lganame, "\n", sprintf("%.1f", label_points$Estimate), suffix)
  )
  ggplot(shp) +
    geom_sf(aes(fill=MapClass),color="#6F7D83",linewidth=.3) +
    geom_sf_label(
      data=label_points, aes(label=map_label), inherit.aes=FALSE,
      size=2.45, fontface="bold", lineheight=.88, color="#1F2D33",
      fill=scales::alpha("white",.82), linewidth=.15,
      label.padding=grid::unit(.10,"lines"), label.r=grid::unit(.08,"lines"),
      show.legend=FALSE
    ) +
    scale_fill_manual(values=map_class_cols,drop=FALSE,name="Map class") +
    coord_sf(expand=FALSE) +
    labs(title=title,subtitle=subtitle,caption="LGA boundaries: bundled Nigeria LGA GeoJSON") +
    theme_minimal(base_size=12) +
    theme(
      axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
      panel.grid=element_blank(),panel.background=element_rect(fill="#F8FAFA",color=NA),
      plot.title=element_text(face="bold",color="#2D3A3F"),
      plot.subtitle=element_text(color="#4B5B62"),
      legend.position="bottom",legend.title=element_text(face="bold"),
      plot.caption=element_text(color="#6F7D83",hjust=0)
    )
}

make_indicator_plot <- function(x,title,group,indicator) {
  if (!nrow(x)) return(ggplot()+theme_void()+labs(title="No data to chart"))
  x$Group <- as.character(x$Group); x$Result <- as.character(x$Result); x$Estimate <- as.numeric(x$Estimate)
  x <- x[is.finite(x$Estimate),,drop=FALSE]
  if (!nrow(x)) return(ggplot()+theme_void()+labs(title="No numeric estimates to chart"))
  value_suffix <- if (all(x$Result=="Weighted mean")) "" else "%"
  x$ValueLabel <- paste0(round(x$Estimate,1),value_suffix)
  is_overall <- identical(group,"Overall") && length(unique(x$Group))==1
  small_parts <- length(unique(x$Result))>=2 && length(unique(x$Result))<=5 && sum(x$Estimate,na.rm=TRUE) > 80 && sum(x$Estimate,na.rm=TRUE) < 105
  if (is_overall && indicator %in% pie_eligible_indicators && small_parts) {
    return(ggplot(x,aes(x="",y=Estimate,fill=Result))+
      geom_col(width=1,color="white")+
      geom_text(aes(label=ValueLabel),position=position_stack(vjust=.5),size=4,color="white")+
      scale_fill_manual(values=rep(chart_palette,length.out=length(unique(x$Result))))+
      coord_polar(theta="y")+
      theme_void(base_size=12)+
      labs(title=title,fill=NULL)+
      theme(plot.title=element_text(face="bold",color="#2D3A3F"),legend.position="right"))
  }
  if (!is_overall && indicator %in% stacked_eligible_indicators && length(unique(x$Result))>1) {
    visible_total <- ave(x$Estimate,x$Group,FUN=function(z) sum(z,na.rm=TRUE))
    x$PlotValue <- ifelse(visible_total>0,100*x$Estimate/visible_total,NA_real_)
    p <- ggplot(x,aes(x=Group,y=PlotValue,fill=Result))+
      geom_col(width=.75,color="white")+
      geom_text(aes(label=ValueLabel),position=position_stack(vjust=.5),size=3,color="white")+
      scale_fill_manual(values=rep(chart_palette,length.out=length(unique(x$Result))))+
      coord_flip()+xlab(NULL)+ylab("Percent of visible non-missing categories")+
      scale_y_continuous(limits=c(0,100),expand=c(0,0))
    return(p+labs(title=title,fill=NULL)+theme_minimal(base_size=12)+
      theme(plot.title=element_text(face="bold",color="#2D3A3F"),panel.grid.major.y=element_blank()))
  }
  if (length(unique(x$Result))==1) {
    p <- ggplot(x,aes(x=reorder(Group,Estimate),y=Estimate,fill=Estimate))+
      geom_col(show.legend=FALSE)+
      geom_text(aes(label=ValueLabel),hjust=-.1,size=3.5)+
      coord_flip()+xlab(NULL)+ylab("Estimate")+
      expand_limits(y=max(x$Estimate,na.rm=TRUE)*1.15)+
      scale_fill_gradient(low="#CDEDE5",high="#178F7A")
  } else if (is_overall) {
    p <- ggplot(x,aes(x=reorder(Result,Estimate),y=Estimate,fill=Estimate))+
      geom_col(show.legend=FALSE)+
      geom_text(aes(label=ValueLabel),hjust=-.1,size=3.5)+
      coord_flip()+xlab(NULL)+ylab("Estimate")+
      expand_limits(y=max(x$Estimate,na.rm=TRUE)*1.15)+
      scale_fill_gradient(low="#CDEDE5",high="#178F7A")
  } else if (group=="lga" && length(unique(x$Result))>1) {
    p <- ggplot(x,aes(x=reorder(Group,Estimate),y=Estimate,fill=Result))+
      geom_col(show.legend=FALSE)+
      geom_text(aes(label=ValueLabel),hjust=-.1,size=2.8)+
      scale_fill_manual(values=rep(chart_palette,length.out=length(unique(x$Result))))+
      coord_flip()+facet_wrap(~Result,scales="free_y")+xlab(NULL)+ylab("Estimate")+
      expand_limits(y=max(x$Estimate,na.rm=TRUE)*1.15)
  } else {
    p <- ggplot(x,aes(x=Group,y=Estimate,fill=Result))+
      geom_col(position=position_dodge(width=.8))+
      geom_text(aes(label=ValueLabel),position=position_dodge(width=.8),hjust=-.1,size=3)+
      scale_fill_manual(values=rep(chart_palette,length.out=length(unique(x$Result))))+
      coord_flip()+xlab(NULL)+ylab("Estimate")+
      expand_limits(y=max(x$Estimate,na.rm=TRUE)*1.15)
  }
  p+labs(title=title,fill=NULL)+theme_minimal(base_size=12)+
    theme(plot.title=element_text(face="bold",color="#2D3A3F"),panel.grid.major.y=element_blank())
}

make_trend_plot <- function(x,title) {
  if (!nrow(x)) return(ggplot()+theme_void()+labs(title="No comparable results to plot"))
  x$Estimate <- as.numeric(x$Estimate)
  x <- x[is.finite(x$Estimate),,drop=FALSE]
  if (!nrow(x)) return(ggplot()+theme_void()+labs(title="No numeric estimates to plot"))
  x$Round <- factor(as.character(x$Round),levels=unique(as.character(x$Round)))
  suffix <- if (all(as.character(x$Result)=="Weighted mean")) "" else "%"
  x$ValueLabel <- paste0(round(x$Estimate,1),suffix)
  ggplot(x,aes(x=Round,y=Estimate,group=1))+
    geom_line(color="#178F7A",linewidth=1.1)+
    geom_point(color="#178F7A",size=3.3)+
    geom_text(aes(label=ValueLabel),vjust=-.8,color="#2D3A3F",size=3.8,fontface="bold")+
    scale_y_continuous(expand=expansion(mult=c(.08,.18)))+
    labs(title=title,x=NULL,y=if(suffix=="%") "Weighted estimate (%)" else "Weighted mean")+
    theme_minimal(base_size=12)+
    theme(plot.title=element_text(face="bold",color="#2D3A3F"),panel.grid.major.x=element_blank())
}

normalise_unique_key <- function(x) {
  key <- trimws(as.character(x))
  key[is.na(key) | key==""] <- NA_character_
  key
}

attach_trend_household_weights <- function(datasets) {
  household <- datasets$household
  if (is.null(household) || !"key" %in% names(household) || !"weights" %in% names(household)) {
    stop("The previous-round household file must contain both key and weights.",call.=FALSE)
  }
  household$key <- normalise_unique_key(household$key)
  valid_household_key <- !is.na(household$key)
  if (!all(valid_household_key)) {
    stop("The previous-round household file has missing key values, so household weights cannot be joined safely.",call.=FALSE)
  }
  if (anyDuplicated(household$key)) {
    stop("The previous-round household file has duplicate key values, so household weights cannot be joined safely.",call.=FALSE)
  }
  household$hhid <- household$key
  datasets$household <- household
  missing_key_rows <- integer(3)
  names(missing_key_rows) <- c("members","children","women")
  for (source_name in c("members","children","women")) {
    d <- datasets[[source_name]]
    if (is.null(d) || !"key" %in% names(d)) {
      stop(paste0("The previous-round ",source_name," file must contain key to receive the household weight."),call.=FALSE)
    }
    d$key <- normalise_unique_key(d$key)
    matched_household <- match(d$key,household$key)
    unmatched <- !is.na(d$key) & is.na(matched_household)
    if (any(unmatched)) {
      stop(paste0("The previous-round ",source_name," file contains key value(s) that do not match the household file."),call.=FALSE)
    }
    missing_key_rows[[source_name]] <- sum(is.na(d$key))
    d$weights <- household$weights[matched_household]
    d$hhid <- household$hhid[matched_household]
    datasets[[source_name]] <- d
  }
  attr(datasets,"trend_weight_join") <- list(key="key",missing_key_rows=missing_key_rows)
  datasets
}

status_badge <- function(ok,label) {
  cls <- if (isTRUE(ok)) "file-ok" else "file-missing"
  mark <- if (isTRUE(ok)) "OK" else "—"
  tags$div(class=paste("file-pill",cls),tags$span(class="file-mark",mark),label)
}

trend_input_id <- function(round_id, field) paste0("trend_",field,"_",round_id)

trend_round_card <- function(round_id, position) {
  tags$div(
    id=paste0("trend_round_card_",round_id),class="trend-round-card",
    tags$h4(paste0("Previous round ",position)),
    tags$p("Provide a clear label, then upload all four source files for this round."),
    textInput(trend_input_id(round_id,"label"),"Round label",value=paste("Previous round",position)),
    tags$div(class="trend-upload-grid",
      fileInput(trend_input_id(round_id,"household"),"Household CSV",accept=".csv"),
      fileInput(trend_input_id(round_id,"members"),"Members CSV",accept=".csv"),
      fileInput(trend_input_id(round_id,"children"),"Children CSV",accept=".csv"),
      fileInput(trend_input_id(round_id,"women"),"Women CSV",accept=".csv")
    ),
    tags$div(class="trend-validation-row",
      tags$small("The household file must contain weights. The shared SurveyCTO key safely supplies those weights to the members, children and women files."),
      actionButton(trend_input_id(round_id,"validate"),"Validate this round",class="btn-primary")
    ),
    uiOutput(trend_input_id(round_id,"status"))
  )
}

thematic_card <- function(id,icon,title,description) {
  actionButton(
    id,
    tagList(
      tags$span(class="overview-card-icon",`aria-hidden`="true",icon),
      tags$strong(title),
      tags$span(class="overview-card-copy",description),
      tags$span(class="overview-card-link","Explore →")
    ),
    class="overview-theme-card"
  )
}

workflow_step <- function(number,icon,title,description) {
  tags$li(
    tags$span(class="workflow-icon",`aria-hidden`="true",icon),
    tags$div(tags$span(class="workflow-number",number),tags$strong(title),tags$small(description))
  )
}

ui <- fluidPage(
 theme=bslib::bs_theme(version=5,bootswatch="flatly",primary="#178F7A",secondary="#4B5B62"),
 tags$head(
  tags$style(HTML("
   html,body{min-height:100%;background:#F4F7F8;color:#4B5B62;font-family:Inter,'Segoe UI',Arial,sans-serif;font-size:14px}
   body{margin:0}
   .container-fluid{padding:0}
   .app-shell{width:100%;margin:0;padding:0}
   .mcss-layout{display:grid;grid-template-columns:60px minmax(0,1fr);gap:0;align-items:start;transition:grid-template-columns .22s ease}
   .mcss-layout.sidebar-open{grid-template-columns:330px minmax(0,1fr)}
   .mcss-sidebar{background:#147A69;color:white;padding:0;box-shadow:3px 0 14px rgba(45,58,63,.10);position:sticky;top:0;height:100vh;min-height:680px;overflow:hidden;z-index:100;display:block}
   .mcss-sidebar,.mcss-sidebar *{box-sizing:border-box}
   .sidebar-rail{display:grid;gap:6px;justify-items:center;align-content:start;padding-top:14px}
   .sidebar-open .sidebar-rail{display:none}
   .sidebar-panel{display:none;width:100%;height:100vh;overflow-x:hidden;overflow-y:auto;padding-bottom:64px}
   .sidebar-open .sidebar-panel{display:block}
   .rail-btn,.drawer-toggle{width:46px;height:46px;border-radius:10px;border:0;background:transparent;color:white;display:grid;place-items:center;font-weight:800;box-shadow:none;cursor:pointer}
   .rail-btn.primary,.drawer-toggle{background:rgba(255,255,255,.13);color:white}
   .rail-btn:hover,.drawer-toggle:hover{background:rgba(255,255,255,.20);color:white}
   .rail-label{writing-mode:vertical-rl;transform:rotate(180deg);font-size:11px;color:rgba(255,255,255,.72);font-weight:700;letter-spacing:.12em;margin-top:8px;text-transform:uppercase}
   .side-head{display:flex;justify-content:space-between;align-items:center;gap:10px;padding:18px 18px 16px;border-bottom:1px solid rgba(255,255,255,.17);position:sticky;top:0;background:#147A69;z-index:2}
   .side-title{display:grid;gap:2px}
   .side-title strong{font-size:18px;font-weight:600;color:white}
   .side-title span{font-size:13px;color:#D8F0EA}
   .upload-box{width:100%;max-width:100%;border:0;border-bottom:1px solid rgba(255,255,255,.14);background:transparent;border-radius:0;padding:0;overflow:hidden}
   .upload-box summary{display:flex;justify-content:space-between;gap:10px;align-items:center;padding:16px 18px;cursor:pointer;list-style:none;color:white;font-size:14px;font-weight:600}
   .upload-box summary::-webkit-details-marker{display:none}
   .upload-box summary::after{content:'+';font-size:20px;font-weight:400;color:#D8F0EA}
   .upload-box[open] summary::after{content:'−'}
   .upload-box summary:hover{background:rgba(255,255,255,.08)}
   .upload-fields{display:grid;gap:8px;padding:0 18px 12px}
   .upload-heading{display:none}
   .upload-box .shiny-input-container.form-group{width:calc(100% - 48px);max-width:calc(100% - 48px);min-width:0;margin:0 24px 16px}
   .upload-box .input-group{display:flex;width:100%;max-width:100%;min-width:0;flex-wrap:nowrap}
   .upload-box .input-group .form-control{width:1%;min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
   .upload-box .input-group .btn,.upload-box .input-group .input-group-text{flex:0 0 auto}
   .upload-heading strong,.control-heading{font-size:12px;font-weight:600;color:#D8F0EA;text-transform:uppercase;letter-spacing:.08em}
   .badge-soft{border-radius:999px;padding:4px 8px;background:rgba(255,255,255,.13);color:white;font-size:10px;font-weight:700}
   .control-block{display:grid;gap:10px;width:100%;min-width:0;padding:16px 24px;box-sizing:border-box;overflow:visible}
   .control-block>*,.control-block .shiny-html-output{width:100%;max-width:100%;min-width:0;box-sizing:border-box}
   .control-block .shiny-input-container{width:100%;max-width:100%;min-width:0;margin-left:0;margin-right:0;box-sizing:border-box}
   .control-block .selectize-control,.control-block .selectize-input{width:100%;max-width:100%;min-width:0;box-sizing:border-box}
   #group_ui .selectize-dropdown{top:auto!important;bottom:calc(100% + 4px)!important;max-height:300px;overflow-y:auto;z-index:120}
   .form-group{margin-bottom:10px}
   .control-block label,.upload-box label{font-weight:600;color:#F3FBF8;font-size:13px}
   .sidebar-panel .form-control,.sidebar-panel .form-select{background-color:#FFFFFF;border:1px solid #CBE2DC;color:#2D3A3F;border-radius:8px;font-size:14px}
   .sidebar-panel .form-select option{color:#2D3A3F;background:white}
   .sidebar-panel .form-control::file-selector-button{background:#E7F5F1;color:#147A69;border:0}
   .btn{border-radius:8px;font-weight:600;box-shadow:none;text-decoration:none}
   .btn-primary,.shiny-download-link.btn-primary{background:#178F7A;border-color:#178F7A;color:white;text-decoration:none}
   .main-canvas{display:grid;gap:14px;min-width:0;padding:0 24px 58px}
   .topbar{background:white;color:#2D3A3F;border-radius:0;padding:18px 24px;margin:0 -24px;box-shadow:0 1px 0 #DCE5E8;display:flex;align-items:center;justify-content:space-between;gap:16px;flex-wrap:wrap;position:sticky;top:0;z-index:60}
   .topbar-title{min-width:0}
   .topbar-brand{display:flex;align-items:center;gap:12px;flex:0 0 auto;min-height:48px}
   .topbar-scidar{display:block;width:142px;height:46px;object-fit:contain;object-position:right center}
   .topbar-divider{width:1px;height:36px;background:#DCE5E8}
   .topbar-state{display:block;width:48px;height:48px;object-fit:contain}
   .app-title{font-weight:600;font-size:26px;margin:0;letter-spacing:-.01em}
   .app-subtitle{color:#6F7D83;font-size:13px;margin-top:3px}
   .context-strip{display:flex;gap:9px;align-items:center;flex-wrap:wrap;background:white;border:1px solid #DCE5E8;border-radius:12px;padding:9px 12px;box-shadow:0 3px 10px rgba(45,58,63,.04)}
   .context-item{font-size:13px;color:#4B5B62;border:1px solid #DCE5E8;border-radius:999px;padding:6px 10px;background:#FFFFFF}
   .context-item strong{color:#2D3A3F;font-weight:600}
   .alert-success{border-radius:10px;border:0;background:#E7F5F1;color:#147A69;font-weight:600;padding:11px 14px;margin-bottom:0}
   .content-card{background:white;border:1px solid #DCE5E8;border-radius:12px;padding:0 16px 16px;box-shadow:0 4px 14px rgba(45,58,63,.05);min-width:0}
   .nav{list-style:none;padding-left:0}
   .nav-tabs{border-bottom:1px solid #e4ebef;margin:0 -16px 16px;padding:0 16px;background:white;border-radius:16px 16px 0 0;display:flex;gap:22px;flex-wrap:wrap;list-style:none}
   .nav-tabs>li{margin:0}
   .nav-tabs .nav-link,.nav-tabs>li>a{border:0;border-bottom:3px solid transparent;border-radius:0;color:#6F7D83;font-size:15px;font-weight:600;padding:15px 2px 12px;margin:0;text-decoration:none;display:block;background:transparent}
   .nav-tabs .nav-link:hover,.nav-tabs>li>a:hover{background:transparent;color:#178F7A;border-bottom-color:#A8D9CE}
   .nav-tabs .nav-link.active,.nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover{background:transparent;color:#178F7A;border-bottom-color:#178F7A;box-shadow:none}
   .tab-toolbar{display:flex;justify-content:space-between;align-items:flex-start;gap:12px;flex-wrap:wrap;margin-bottom:12px}
   .map-download-bar{display:flex;align-items:center;justify-content:space-between;gap:14px;flex-wrap:wrap;margin:10px 0 12px;padding:11px 14px;background:#F4F7F8;border:1px solid #DCE5E8;border-radius:10px}
   .map-download-bar strong{display:block;color:#2D3A3F;font-size:13px}.map-download-bar span{color:#6F7D83;font-size:12px}
   .map-download-actions{display:flex;gap:8px;flex-wrap:wrap}.map-download-actions .btn{white-space:nowrap}
   .trend-intro{display:flex;align-items:flex-start;justify-content:space-between;gap:16px;flex-wrap:wrap;padding:18px;background:#EAF6F3;border:1px solid #CBE6DF;border-radius:12px;margin-bottom:14px}
   .trend-intro h3{margin:0 0 4px;color:#1D4D45;font-size:21px;font-weight:600}.trend-intro p{margin:0;color:#4B5B62;max-width:720px}
   .trend-round-badge{background:white;border:1px solid #CBE2DC;color:#147A69;border-radius:999px;padding:7px 10px;font-size:12px;font-weight:700;white-space:nowrap}
   .trend-upload-card,.trend-selection-card,.trend-results-card{border:1px solid #DCE5E8;border-radius:12px;padding:18px;margin-bottom:14px;background:white}
   .trend-upload-head{display:flex;align-items:flex-start;justify-content:space-between;gap:14px;flex-wrap:wrap}.trend-round-stack{display:grid;gap:14px;margin-top:14px}.trend-round-card{border:1px solid #DCE5E8;border-left:4px solid #178F7A;border-radius:10px;padding:16px;background:#FCFDFD}.trend-round-card h4{margin:0 0 5px;color:#2D3A3F;font-size:16px;font-weight:600}.trend-round-card>p{margin:0;color:#6F7D83;font-size:13px}
   .trend-upload-card h4,.trend-selection-card h4,.trend-results-card h4{margin:0 0 5px;color:#2D3A3F;font-size:17px;font-weight:600}.trend-upload-card p,.trend-selection-card p,.trend-results-card p{margin:0;color:#6F7D83;font-size:13px}
   .trend-upload-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px;margin:15px 0}.trend-upload-grid .shiny-input-container{width:100%;margin:0}.trend-upload-grid label{font-weight:600;color:#2D3A3F;font-size:13px}.trend-upload-grid .form-control{border-radius:8px;border-color:#D1DEE1;font-size:13px}
   .trend-validation-row{display:flex;align-items:center;justify-content:space-between;gap:12px;flex-wrap:wrap;border-top:1px solid #E7EEF0;padding-top:14px}.trend-validation-row small{color:#6F7D83;max-width:700px}
   .trend-file-status{display:flex;gap:8px;flex-wrap:wrap;margin:12px 0 0}.trend-file-chip{border-radius:999px;padding:5px 9px;font-size:12px;font-weight:600;background:#F2F5F6;color:#64757C}.trend-file-chip.ready{background:#E6F4F0;color:#147A69}
   .trend-selection-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:12px;margin-top:14px}.trend-selection-grid .shiny-input-container{width:100%;margin:0}.trend-selection-grid label{font-weight:600;color:#2D3A3F;font-size:13px}.trend-selection-grid .form-control,.trend-selection-grid .form-select{border-radius:8px;border-color:#D1DEE1}
   .trend-chart-options{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px;margin:14px 0}.trend-chart-options .shiny-input-container{width:100%;margin:0}.trend-chart-options label{font-weight:600;color:#2D3A3F;font-size:13px}
   .trend-status-note{margin-top:12px}.trend-results-toolbar{display:flex;justify-content:space-between;align-items:flex-start;gap:12px;flex-wrap:wrap;margin-bottom:10px}.trend-results-actions{display:flex;gap:8px;flex-wrap:wrap}
   @media(max-width:780px){.trend-upload-grid,.trend-selection-grid,.trend-chart-options{grid-template-columns:1fr}}
   .title{color:#2D3A3F;font-size:21px;font-weight:600;margin-top:0}
   .help-note{background:#F4F7F8;border-left:4px solid #178F7A;border-radius:8px;padding:12px 14px;color:#4B5B62}
   .footer-note{text-align:right;color:#6F7D83;font-size:12px;margin:4px 0 0}
   .app-footer{position:fixed;left:0;right:0;bottom:0;z-index:90;min-height:36px;display:flex;align-items:center;justify-content:center;background:#DCEDEC;border-top:1px solid #CBE2DC;color:#667880;font-size:12px;letter-spacing:.02em;text-transform:uppercase}
   .app-footer strong{margin-left:4px;color:#087B6B;font-weight:700;letter-spacing:0;text-transform:none}
   table.dataTable{color:#4B5B62;font-size:14px}
   table.dataTable thead th{background:#E7F5F1;color:#2D3A3F;font-weight:600;border-bottom:1px solid #CBE2DC}
   table.dataTable tbody td{border-color:#E8EEF0}
   .overview-page{display:grid;gap:28px;padding:4px 2px 12px}
   .overview-hero{display:grid;grid-template-columns:minmax(0,1.35fr) minmax(240px,.65fr);align-items:center;gap:28px;background:#E7F5F1;border-radius:12px;padding:42px clamp(24px,4vw,54px);overflow:hidden}
   .overview-badge{display:inline-flex;width:max-content;background:#FFFFFF;color:#147A69;border:1px solid #CBE2DC;border-radius:999px;padding:5px 10px;font-size:12px;font-weight:600}
   .overview-hero h2{max-width:760px;margin:14px 0 9px;color:#2D3A3F;font-size:clamp(28px,3.3vw,44px);font-weight:600;line-height:1.12;letter-spacing:-.025em}
   .overview-hero p{max-width:680px;margin:0 0 22px;color:#4B5B62;font-size:16px}
   .overview-actions{display:flex;gap:10px;flex-wrap:wrap}
   .overview-actions .btn{display:inline-flex;align-items:center;gap:8px;padding:10px 16px}
   .overview-action-icon{font-size:17px;line-height:1}
   .overview-visual{min-height:210px;display:grid;grid-template-columns:1.15fr .85fr;gap:16px;align-items:end;padding:20px;color:#178F7A}
   .overview-bars{height:160px;display:flex;gap:9px;align-items:end;border-bottom:2px solid #A8D9CE}
   .overview-bars span{flex:1;background:currentColor;border-radius:6px 6px 0 0;opacity:.82}
   .overview-bars span:nth-child(1){height:40%}.overview-bars span:nth-child(2){height:68%}.overview-bars span:nth-child(3){height:53%}.overview-bars span:nth-child(4){height:90%}.overview-bars span:nth-child(5){height:74%}
   .overview-lines{align-self:center;display:grid;gap:13px}
   .overview-lines span{display:block;height:11px;border-radius:999px;background:#A8D9CE}
   .overview-lines span:nth-child(2){width:76%}.overview-lines span:nth-child(3){width:92%}.overview-lines span:nth-child(4){width:60%}
   .overview-section-head{display:flex;align-items:end;justify-content:space-between;gap:16px;margin-bottom:14px}
   .overview-section-head h2,.overview-section-head p{margin:0}.overview-section-head h2,.overview-workflow h2{color:#2D3A3F;font-size:22px;font-weight:600}.overview-section-head p{color:#6F7D83}
   .overview-theme-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:14px}
   .overview-theme-card.btn{min-height:176px;display:grid;grid-template-rows:auto auto 1fr auto;justify-items:start;align-content:start;gap:9px;padding:18px;text-align:left;white-space:normal;background:#FFFFFF;color:#2D3A3F;border:1px solid #DCE5E8;border-radius:12px}
   .overview-theme-card .action-label{width:100%;height:100%;display:grid;grid-template-rows:auto auto 1fr auto;justify-items:start;align-content:start;gap:9px;text-align:left}
   .overview-theme-card.btn:hover{border-color:#62BEAA;background:#F3FBF8;transform:translateY(-2px)}
   .overview-card-icon{display:grid;place-items:center;width:42px;height:42px;border-radius:50%;background:#E7F5F1;color:#147A69;font-size:21px}
   .overview-theme-card strong{font-size:15px;font-weight:600}.overview-card-copy{color:#6F7D83;font-size:13px}.overview-card-link{color:#178F7A;font-weight:600;font-size:13px}
   .overview-workflow h2{margin:0 0 14px}
   .overview-workflow ol{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:14px;list-style:none;padding:0;margin:0}
   .overview-workflow li{display:flex;gap:12px;align-items:flex-start;padding:16px 0;border-top:1px solid #DCE5E8}
   .workflow-icon{display:grid;place-items:center;flex:0 0 42px;height:42px;border-radius:50%;background:#E7F5F1;color:#147A69;font-size:20px}
   .overview-workflow li div{display:grid;gap:2px}.workflow-number{color:#178F7A;font-size:12px;font-weight:600}.overview-workflow strong{color:#2D3A3F;font-weight:600}.overview-workflow small{color:#6F7D83;font-size:12px}
   .overview-page.guided{gap:24px}
   .overview-page.guided .overview-hero{display:flex;justify-content:space-between;align-items:center;grid-template-columns:none;gap:24px;padding:36px clamp(22px,4vw,48px)}
   .overview-page.guided .overview-hero h2{margin-bottom:8px}
   .overview-page.guided .overview-hero p{margin-bottom:0;max-width:760px}
   .overview-page.guided .overview-hero .btn{display:inline-flex;align-items:center;gap:8px;white-space:nowrap;padding:10px 16px}
   .overview-guide-head{display:flex;align-items:end;justify-content:space-between;gap:14px;margin-bottom:14px}
   .overview-guide-head h2{margin:2px 0 0;color:#2D3A3F;font-size:22px;font-weight:600}
   .overview-eyebrow{color:#178F7A;font-size:12px;font-weight:600;letter-spacing:.07em}
   .overview-step-status{color:#6F7D83;font-size:13px}
   .overview-stepper{display:grid;grid-template-columns:auto minmax(18px,1fr) auto minmax(18px,1fr) auto;align-items:center;margin-bottom:17px}
   .overview-step{display:flex;align-items:center;gap:9px;color:#6F7D83}
   .overview-step-icon{display:grid;place-items:center;width:38px;height:38px;border-radius:50%;background:#EEF2F3;color:#6F7D83;font-size:18px}
   .overview-step div{display:grid}.overview-step small{font-size:11px}.overview-step strong{font-size:13px;font-weight:600}
   .overview-step.current,.overview-step.complete{color:#2D3A3F}.overview-step.current .overview-step-icon,.overview-step.complete .overview-step-icon{background:#178F7A;color:white}
   .overview-step-line{height:1px;background:#DCE5E8}
   .overview-stage{display:grid;grid-template-columns:auto minmax(0,1fr) auto;align-items:center;gap:16px;border:1px solid #DCE5E8;border-radius:12px;padding:20px;background:#FFFFFF}
   .overview-stage-icon{display:grid;place-items:center;width:54px;height:54px;border-radius:50%;background:#E7F5F1;color:#147A69;font-size:22px}
   .overview-stage-copy h3,.overview-stage-copy p{margin:2px 0}.overview-stage-copy h3{color:#2D3A3F;font-size:18px;font-weight:600}.overview-stage-copy p{color:#6F7D83}
   .overview-progress-label{display:block;margin-top:9px;color:#4B5B62;font-size:13px}
   .overview-progress-track{height:7px;margin-top:5px;border-radius:999px;background:#EEF2F3;overflow:hidden}.overview-progress-track span{display:block;height:100%;background:#178F7A}
   .overview-map-note{display:flex;align-items:center;gap:10px;border-top:1px solid #DCE5E8;padding:14px 0;color:#6F7D83}.overview-map-note-icon{color:#178F7A;font-size:20px}
   .overview-selected{display:block;margin-top:8px;color:#147A69;font-size:13px;font-weight:600}
   .shiny-input-container{width:100%}
   input[type=file]{font-size:12px}
   @media(max-width:850px){
    .mcss-layout{grid-template-columns:56px minmax(0,1fr)}
    .mcss-layout.sidebar-open{grid-template-columns:minmax(250px,38vw) minmax(0,1fr)}
    .main-canvas{padding:0 12px 16px}
    .topbar{margin:0 -12px;padding:15px 16px}
    .topbar{padding:15px 16px}
    .nav-tabs{position:static}
    .overview-theme-grid{grid-template-columns:repeat(2,minmax(0,1fr))}
    .overview-workflow ol{grid-template-columns:repeat(2,minmax(0,1fr))}
    .overview-step div{display:none}
   }
   @media(max-width:560px){
    .mcss-layout{grid-template-columns:52px minmax(0,1fr)}
    .mcss-layout.sidebar-open{grid-template-columns:minmax(230px,76vw) minmax(0,1fr)}
    .rail-btn,.drawer-toggle{width:38px;height:38px;border-radius:13px}
    .app-title{font-size:19px}
    .topbar-brand{gap:8px;min-height:36px}.topbar-scidar{width:104px;height:34px}.topbar-divider{height:28px}.topbar-state{width:36px;height:36px}
    .context-item{max-width:100%;overflow:hidden;text-overflow:ellipsis}
    .overview-hero{grid-template-columns:1fr;padding:28px 20px}.overview-visual{display:none}
    .overview-theme-grid,.overview-workflow ol{grid-template-columns:1fr}
    .overview-page.guided .overview-hero{align-items:flex-start;flex-direction:column}
    .overview-guide-head{align-items:flex-start;flex-direction:column}
    .overview-stage{grid-template-columns:auto 1fr}.overview-stage>.btn{grid-column:1/-1}
   }
  ")),
  tags$script(HTML("
   document.addEventListener('click', function(e) {
    var btn = e.target.closest('[data-sidebar-toggle]');
    if (!btn) return;
    var shell = document.getElementById('mcss_layout');
    if (!shell) return;
    var open = !shell.classList.contains('sidebar-open');
    shell.classList.toggle('sidebar-open', open);
    document.querySelectorAll('[data-sidebar-toggle]').forEach(function(x){x.setAttribute('aria-expanded', open ? 'true' : 'false');});
   });
  "))
 ),
 tags$div(class="app-shell",
  tags$div(id="mcss_layout",class="mcss-layout",
   tags$aside(class="mcss-sidebar",
    tags$div(class="sidebar-rail",
     tags$button(type="button",class="rail-btn primary",`data-sidebar-toggle`="",`aria-expanded`="false",`aria-label`="Open filters",HTML("&#8250;")),
     tags$span(class="rail-label","Filters")
    ),
    tags$div(class="sidebar-panel",
     tags$div(class="side-head",
      tags$div(class="side-title",tags$strong("Filters"),tags$span("Refine analysis scope")),
      tags$button(type="button",class="drawer-toggle",`data-sidebar-toggle`="",`aria-expanded`="true",`aria-label`="Collapse filters",HTML("&#8249;"))
     ),
     tags$details(id="upload_details",class="upload-box",
      tags$summary("Upload files"),
      tags$div(class="upload-heading",tags$strong("Upload files"),tags$span(class="badge-soft","CSV · XLSX")),
      fileInput("household","Household CSV",accept=".csv"),
      fileInput("members","Members CSV",accept=".csv"),
      fileInput("children","Children CSV",accept=".csv"),
      fileInput("women","Women CSV",accept=".csv"),
      fileInput("rural","Rural/urban classification CSV",accept=".csv"),
      fileInput("questionnaire","Questionnaire XLSX (optional)",accept=c(".xlsx",".xls"))
     ),
     tags$div(class="control-block",
      tags$div(class="control-heading","Selections"),
      selectInput("module","Thematic area",setNames(names(module_labels),module_labels)),
      uiOutput("indicator_ui"),
      uiOutput("group_ui")
     )
    )
   ),
   tags$main(class="main-canvas",
    tags$div(class="topbar",
     tags$div(class="topbar-title",tags$h1(class="app-title","MCSS Analytical Tool"),tags$div(class="app-subtitle","Current-round weighted indicators, tables, charts, LGA maps, and trends")),
     tags$div(class="topbar-brand",tags$img(src="scidar_logo.png",class="topbar-scidar",alt="SCIDaR"),tags$span(class="topbar-divider",`aria-hidden`="true"),tags$img(src="kaduna_logo.png",class="topbar-state",alt="Kaduna State Government"))
    ),
    conditionalPanel("input.main_tabs === 'Table' || input.main_tabs === 'Chart' || input.main_tabs === 'Map'",uiOutput("status"),uiOutput("context_strip")),
    tags$div(class="content-card",
     tabsetPanel(
      id="main_tabs",
      tabPanel("Overview",br(),
       tags$div(class="overview-page guided",
        tags$section(class="overview-hero",
         tags$div(
          tags$span(class="overview-badge","Kaduna MCSS current survey round"),
          tags$h2("Analyse Kaduna MCSS survey data"),
          tags$p("Upload your Kaduna survey files, choose an indicator and disaggregation, then review the results as a table, chart, map or multi-round trend.")
         ),
         actionButton("overview_upload",tagList(tags$span(class="overview-action-icon",`aria-hidden`="true","↥"),"Upload survey files"),class="btn-primary",onclick="document.getElementById('mcss_layout').classList.add('sidebar-open'); document.getElementById('upload_details').open=true;")
        ),
        tags$section(class="overview-guide",
         tags$div(class="overview-guide-head",
          tags$div(tags$span(class="overview-eyebrow","GETTING STARTED"),tags$h2("Complete these three steps")),
          uiOutput("overview_step_status")
         ),
         uiOutput("overview_stepper"),
         uiOutput("overview_stage")
        ),
        tags$section(
         tags$div(class="overview-section-head",tags$div(tags$h2("Explore by thematic area"),tags$p("Choose an area during step two to load its indicators."))),
         tags$div(class="overview-theme-grid",
          thematic_card("overview_characteristics","⌂","Housing, household and respondent characteristics","Household, housing and population profiles"),
          thematic_card("overview_prevention","◈","Malaria prevention","ITNs and malaria in pregnancy"),
          thematic_card("overview_treatment","♡","Health seeking behavior and treatment","Fever, testing and treatment among children"),
          thematic_card("overview_beliefs","✉","Malaria beliefs and exposure to messages","Knowledge, prevention methods and message sources")
         )
        ),
        tags$div(class="overview-map-note",tags$span(class="overview-map-note-icon",`aria-hidden`="true","⌖"),tags$span(tags$strong("Want an LGA map? "),"Select LGA under ‘Disaggregate by,’ then open the Map tab."))
       )
      ),
      tabPanel("Table",br(),tags$div(class="tab-toolbar",h3(textOutput("result_title"),class="title"),downloadButton("download","Download table",class="btn-primary")),DTOutput("results")),
      tabPanel("Chart",br(),tags$div(class="tab-toolbar",h3("Chart view",class="title"),downloadButton("download_chart","Download chart",class="btn-primary")),plotOutput("chart",height="640px")),
      tabPanel("Map",br(),uiOutput("map_controls"),leafletOutput("map",height="640px")),
      tabPanel("Trends",br(),
       tags$div(class="trend-intro",
        tags$div(tags$h3("Compare survey rounds"),tags$p("Add one or more complete previous Kaduna survey rounds, validate each one, then choose an indicator and available cut to compare across rounds.")),
        tags$span(class="trend-round-badge","Current-round rural/urban lookup reused")
       ),
       tags$div(class="trend-upload-card",
        tags$div(class="trend-upload-head",
         tags$div(tags$h4("Step 1. Add and validate previous rounds"),tags$p("Add every previous round you want to compare. Upload the four source files for each round; do not upload the rural/urban classification again.")),
         actionButton("trend_add_round","Add another previous round",class="btn-primary")
        ),
        tags$div(id="trend_rounds_container",class="trend-round-stack"),
        uiOutput("trend_rounds_summary")
       ),
       uiOutput("trend_selection_ui"),
       uiOutput("trend_results_ui")
      ),
      tabPanel("Catalogue",br(),DTOutput("catalogue")),
      tabPanel("Help",br(),
       h3("Using the tool",class="title"),
       tags$div(class="help-note",
        tags$ol(
         tags$li("Load the four survey CSV files plus the rural/urban classification file."),
         tags$li("Choose a thematic area, indicator, and disaggregation."),
         tags$li("Review the table, chart, map, or add one or more complete previous rounds in Trends to compare across rounds."),
        ),
        p("Estimates use the weights column. Indicators follow the eligibility and derived-variable logic in the supplied R script. Overall, residence, wealth quintile, zone and LGA cuts appear where the source columns are available."),
        p("Each previous round must be validated before it is included in a trend. The rural/urban classification file is shared across all selected rounds.")
       ),
       tags$hr(),
       h3("Questionnaire dictionary",class="title"),
       tags$p("Use this reference to connect dataset variables to the original questionnaire wording, translations and skip conditions."),
       uiOutput("dictionary_note"),
       DTOutput("dictionary")
      )
     )
    ),
    tags$div(class="footer-note","MCSS Analytical Tool | Current-round analysis and trends | Version 52")
   ),
   tags$footer(class="app-footer",tags$span("Powered by"),tags$strong("SCIDaR"))
  )
 )
)

server <- function(input,output,session){
 raw<-reactiveValues(household=NULL,members=NULL,children=NULL,women=NULL,rural=NULL)
 trend_sources<-c("household","members","children","women")
 trend_round_ids<-reactiveVal("round_1")
 trend_round_counter<-reactiveVal(1L)
 trend_round_data<-reactiveValues()
 trend_round_state<-reactiveValues()
 trend_round_joined<-reactiveValues()
 overview_step<-reactiveVal(1)
 overview_theme<-reactiveVal(NULL)
 observeEvent(input$overview_characteristics,{updateSelectInput(session,"module",selected="characteristics");overview_theme(module_labels[["characteristics"]]);overview_step(2)})
 observeEvent(input$overview_prevention,{updateSelectInput(session,"module",selected="prevention");overview_theme(module_labels[["prevention"]]);overview_step(2)})
 observeEvent(input$overview_treatment,{updateSelectInput(session,"module",selected="treatment");overview_theme(module_labels[["treatment"]]);overview_step(2)})
 observeEvent(input$overview_beliefs,{updateSelectInput(session,"module",selected="beliefs");overview_theme(module_labels[["beliefs"]]);overview_step(2)})
 observeEvent(input$overview_continue,overview_step(3))
 observeEvent(input$overview_open_table,updateTabsetPanel(session,"main_tabs",selected="Table"))
 lapply(c("household","members","children","women","rural"),function(nm) observeEvent(input[[nm]],{raw[[nm]]<-read_csv_clean(input[[nm]]$datapath)}))
 core_loaded<-reactive(sum(vapply(c("household","members","children","women","rural"),function(nm)!is.null(raw[[nm]]),logical(1))))
 initialise_trend_round<-function(round_id){
   trend_round_data[[round_id]]<-setNames(vector("list",length(trend_sources)),trend_sources)
   trend_round_state[[round_id]]<-list(validated=FALSE,message="Upload all four files for this round, then validate it.")
   trend_round_joined[[round_id]]<-NULL
 }
 round_state_value<-function(round_id){
   state<-trend_round_state[[round_id]]
   if(is.null(state)) list(validated=FALSE,message="Upload all four files for this round, then validate it.") else state
 }
 set_round_state<-function(round_id,validated,message){
   trend_round_state[[round_id]]<-list(validated=isTRUE(validated),message=message)
 }
 round_loaded_count<-function(round_id){
   round_data<-trend_round_data[[round_id]]
   if(is.null(round_data)) return(0L)
   sum(vapply(trend_sources,function(source_name)!is.null(round_data[[source_name]]),logical(1)))
 }
 round_is_validated<-function(round_id) isTRUE(round_state_value(round_id)$validated)
 round_label<-function(round_id){
   position<-match(round_id,trend_round_ids())
   fallback<-paste("Previous round",position)
   label<-trimws(input[[trend_input_id(round_id,"label")]])
   if(is.null(label) || !nzchar(label)) fallback else label
 }
 trend_round_labels<-reactive(vapply(trend_round_ids(),round_label,character(1)))
 trend_ready<-reactive({
   round_ids<-trend_round_ids()
   length(round_ids)>0 && all(vapply(round_ids,round_is_validated,logical(1)))
 })
 register_trend_round<-function(round_id){
   for(source_name in trend_sources) local({
     this_source<-source_name
     input_name<-trend_input_id(round_id,this_source)
     observeEvent(input[[input_name]],{
       upload<-input[[input_name]]
       if(is.null(upload)) return()
       parsed<-tryCatch(read_csv_clean(upload$datapath),error=function(e) e)
       round_data<-trend_round_data[[round_id]]
       if(inherits(parsed,"error")) {
         round_data[[this_source]]<-NULL
         trend_round_data[[round_id]]<-round_data
         trend_round_joined[[round_id]]<-NULL
         set_round_state(round_id,FALSE,paste0("Could not read the ",this_source," file: ",conditionMessage(parsed)))
       } else {
         round_data[[this_source]]<-parsed
         trend_round_data[[round_id]]<-round_data
         trend_round_joined[[round_id]]<-NULL
         set_round_state(round_id,FALSE,"All four files for this round must be uploaded and validated before analysis.")
       }
     })
   })
   output[[trend_input_id(round_id,"status")]]<-renderUI({
     round_data<-trend_round_data[[round_id]]
     state<-round_state_value(round_id)
     chips<-lapply(trend_sources,function(source_name){
       ok<-!is.null(round_data[[source_name]])
       tags$span(class=paste("trend-file-chip",if(ok) "ready" else ""),paste0(tools::toTitleCase(source_name),if(ok) " loaded" else " awaiting upload"))
     })
     msg_class<-if(isTRUE(state$validated)) "alert alert-success trend-status-note" else "help-note trend-status-note"
     tags$div(tags$div(class="trend-file-status",chips),tags$div(class=msg_class,state$message))
   })
   observeEvent(input[[trend_input_id(round_id,"validate")]],{
     set_round_state(round_id,FALSE,"Validating this round.")
     if(core_loaded()!=5) {
       set_round_state(round_id,FALSE,"First load the current-round household, members, children, women, and rural/urban files in the left sidebar.")
       return()
     }
     if(round_loaded_count(round_id)!=length(trend_sources)) {
       set_round_state(round_id,FALSE,"Upload all four survey files for this round before validation.")
       return()
     }
     round_data<-trend_round_data[[round_id]]
     required_by_file<-list(household=c("key","weights"),members="key",children="key",women="key")
     missing_by_file<-vapply(trend_sources,function(source_name){
       missing<-setdiff(required_by_file[[source_name]],names(round_data[[source_name]]))
       if(length(missing)) paste0(tools::toTitleCase(source_name),": ",paste(missing,collapse=", ")) else ""
     },character(1))
     missing_by_file<-missing_by_file[nzchar(missing_by_file)]
     if(length(missing_by_file)) {
       set_round_state(round_id,FALSE,paste0("Validation could not continue. Missing required join/weight column(s): ",paste(missing_by_file,collapse="; "),"."))
       return()
     }
     joined_round<-tryCatch(attach_trend_household_weights(round_data),error=function(e) e)
     if(inherits(joined_round,"error")) {
       set_round_state(round_id,FALSE,paste0("Validation could not continue. ",conditionMessage(joined_round)))
       return()
     }
     trend_round_joined[[round_id]]<-joined_round
     missing_rows<-attr(joined_round,"trend_weight_join")$missing_key_rows
     missing_note<-paste0(" Records without key retain missing weights and are excluded: ",paste0(tools::toTitleCase(names(missing_rows))," ",missing_rows,collapse="; "),".")
     set_round_state(round_id,TRUE,paste0("Round validated. Household weights were joined to the members, children, and women files using key.",missing_note))
   })
 }
 initialise_trend_round("round_1")
 register_trend_round("round_1")
 session$onFlushed(function(){
   insertUI(selector="#trend_rounds_container",where="beforeEnd",ui=trend_round_card("round_1",1))
 },once=TRUE)
 observeEvent(input$trend_add_round,{
   next_position<-trend_round_counter()+1L
   trend_round_counter(next_position)
   round_id<-paste0("round_",next_position)
   initialise_trend_round(round_id)
   trend_round_ids(c(trend_round_ids(),round_id))
   register_trend_round(round_id)
   insertUI(selector="#trend_rounds_container",where="beforeEnd",ui=trend_round_card(round_id,next_position),immediate=TRUE)
 })
 output$trend_rounds_summary<-renderUI({
   round_ids<-trend_round_ids()
   validated<-sum(vapply(round_ids,round_is_validated,logical(1)))
   if(validated==length(round_ids)) {
     tags$div(class="alert alert-success trend-status-note",sprintf("%s previous round%s validated. Choose the trend analysis below.",validated,if(validated==1) " is" else "s are"))
   } else {
     tags$div(class="help-note trend-status-note",sprintf("%s of %s previous rounds validated. Validate every added round before choosing an analysis.",validated,length(round_ids)))
   }
 })
 observe({if(core_loaded()==5 && overview_step()==1) overview_step(2)})
 output$overview_step_status<-renderUI({
   labels<-c("Upload files","Choose analysis","Review results")
   tags$span(class="overview-step-status",sprintf("Step %s of 3 · %s",overview_step(),labels[[overview_step()]]))
 })
 output$overview_stepper<-renderUI({
   current<-overview_step()
   node<-function(n,icon,title){
     cls<-if(n<current)"overview-step complete" else if(n==current)"overview-step current" else "overview-step"
     tags$div(class=cls,tags$span(class="overview-step-icon",`aria-hidden`="true",icon),tags$div(tags$small(paste("STEP",n)),tags$strong(title)))
   }
   tags$div(class="overview-stepper",node(1,"↥","Upload files"),tags$span(class="overview-step-line"),node(2,"☷","Choose analysis"),tags$span(class="overview-step-line"),node(3,"▦","Review results"))
 })
 output$overview_stage<-renderUI({
   step<-overview_step(); loaded<-core_loaded(); selected<-overview_theme()
   if(step==1) return(tags$div(class="overview-stage",
     tags$span(class="overview-stage-icon",`aria-hidden`="true","▤"),
     tags$div(class="overview-stage-copy",tags$span(class="overview-eyebrow","FIRST ACTION"),tags$h3("Load the five survey source files"),tags$p("The app needs the household, members, women, children and rural/urban files before all indicators and disaggregations are available."),tags$span(class="overview-progress-label",sprintf("%s of 5 required files loaded",loaded)),tags$div(class="overview-progress-track",tags$span(style=sprintf("width:%s%%",loaded*20)))),
     actionButton("overview_upload_stage","Select files",class="btn-primary",onclick="document.getElementById('mcss_layout').classList.add('sidebar-open'); document.getElementById('upload_details').open=true;")
   ))
   if(step==2) return(tags$div(class="overview-stage",
     tags$span(class="overview-stage-icon",`aria-hidden`="true","☷"),
     tags$div(class="overview-stage-copy",tags$span(class="overview-eyebrow","CHOOSE YOUR ANALYSIS"),tags$h3("Select a thematic area"),tags$p("Then use the left sidebar to choose an indicator and the required disaggregation."),tags$span(class="overview-selected",if(is.null(selected))"Select one of the thematic-area cards below." else paste("Selected:",selected))),
     if(is.null(selected)) tags$button(type="button",class="btn",disabled="disabled","Continue to results") else actionButton("overview_continue","Continue to results",class="btn-primary")
   ))
   tags$div(class="overview-stage",
     tags$span(class="overview-stage-icon",`aria-hidden`="true","▦"),
     tags$div(class="overview-stage-copy",tags$span(class="overview-eyebrow","RESULTS"),tags$h3("Review, compare and download"),tags$p("Use Table for exact estimates, Chart for comparisons, and Map after selecting LGA as the disaggregation.")),
     actionButton("overview_open_table","Open table view",class="btn-primary")
   )
 })
 rural_data<-reactive({r<-raw$rural;if(is.null(r))return(NULL); names(r)<-clean_names(names(r));r$lga<-tolower(trimws(r$lga));r})
 wealth<-reactive({
   h<-raw$household
   if(is.null(h))return(NULL)
   h<-standardize_data(h,rural_data())
   h<-derive_wealth_index(h)
   h[!duplicated(h$hhid),intersect(c("hhid","wealth_quintile","wealth_score","wealth_quintile_num"),names(h)),drop=FALSE]
 })
 lga_shapes<-reactive({
   path <- file.path("data","NGA_LGA_Boundaries.geojson")
   validate(need(file.exists(path),"The bundled LGA boundary file is missing from data/NGA_LGA_Boundaries.geojson."))
   read_lga_boundaries(path)
 })
 selected_source_module<-reactive({req(input$indicator);source_module_for_indicator(input$indicator)})
 prepared<-reactive({src<-selected_source_module();d<-raw[[src]];req(d);d<-standardize_data(d,rural_data(),wealth());
   if(src%in%c("household","members") && !is.null(raw$members) && "hhid"%in%names(d)){m<-standardize_data(raw$members,rural_data(),wealth()); if("sleep_here_last_night"%in%names(m)){p<-aggregate(yes(m$sleep_here_last_night),list(hhid=m$hhid),sum,na.rm=TRUE);names(p)<-c("hhid","de_facto_population");d$de_facto_population<-p$de_facto_population[match(d$hhid,p$hhid)]}}
   d})
 trend_previous_data<-function(round_id){
   req(round_is_validated(round_id))
   datasets<-trend_round_joined[[round_id]]
   req(datasets)
   datasets
 }
 trend_wealth<-function(round_id){
   h<-trend_previous_data(round_id)$household;req(h)
   h<-standardize_data(h,rural_data())
   h<-derive_wealth_index(h)
   h[!duplicated(h$hhid),intersect(c("hhid","wealth_quintile","wealth_score","wealth_quintile_num"),names(h)),drop=FALSE]
 }
 prepare_trend_source<-function(store,src,wealth_data){
   d<-store[[src]];req(d)
   d<-standardize_data(d,rural_data(),wealth_data)
   if(src%in%c("household","members") && !is.null(store$members) && "hhid"%in%names(d)){
     m<-standardize_data(store$members,rural_data(),wealth_data)
     if("sleep_here_last_night"%in%names(m)){
       p<-aggregate(yes(m$sleep_here_last_night),list(hhid=m$hhid),sum,na.rm=TRUE)
       names(p)<-c("hhid","de_facto_population")
       d$de_facto_population<-p$de_facto_population[match(d$hhid,p$hhid)]
     }
   }
   d
 }
 trend_selected_source<-reactive({req(input$trend_indicator);source_module_for_indicator(input$trend_indicator)})
 trend_current_prepared<-reactive({prepare_trend_source(raw,trend_selected_source(),wealth())})
 trend_previous_prepared<-function(round_id,source_module=trend_selected_source()){
   prepare_trend_source(trend_previous_data(round_id),source_module,trend_wealth(round_id))
 }
 trend_group_choices<-reactive({
   src<-trend_selected_source()
   current_groups<-available_groups(trend_current_prepared(),src)
   previous_groups<-lapply(trend_round_ids(),function(round_id) tryCatch(available_groups(trend_previous_prepared(round_id,src),src),error=function(e) character()))
   usable_previous_groups<-Filter(length,previous_groups)
   if(!length(usable_previous_groups)) return(setNames(character(),character()))
   groups<-Reduce(intersect,c(list(current_groups),usable_previous_groups))
   setNames(groups,gsub("_"," ",groups))
 })
 output$trend_selection_ui<-renderUI({
   if(!trend_ready()) return(NULL)
   tags$div(class="trend-selection-card",
    tags$h4("Step 2. Choose the trend analysis"),
    tags$p("Only cuts available in the comparable rounds are shown. A round that did not capture an indicator will be flagged as unavailable rather than used in the estimate."),
    tags$div(class="trend-selection-grid",
      selectInput("trend_module","Thematic area",setNames(names(module_labels),module_labels)),
      uiOutput("trend_indicator_ui"),
      uiOutput("trend_group_ui")
    )
   )
 })
 output$trend_indicator_ui<-renderUI({
   req(input$trend_module)
   x<-indicator_catalog[indicator_catalog$module==input$trend_module,]
   selectInput("trend_indicator","Indicator",setNames(x$id,x$label))
 })
 output$trend_group_ui<-renderUI({
   choices<-trend_group_choices()
   validate(need(length(choices),"No common comparison cuts are available for this indicator."))
   selectInput("trend_group","Compare by",choices)
 })
 trend_computation<-reactive({
   req(trend_ready(),input$trend_indicator,input$trend_group)
   round_ids<-trend_round_ids()
   round_labels<-trend_round_labels()
   if(anyDuplicated(c(round_labels,"Current round"))) return(list(data=NULL,error="Each previous round must have a unique label that is different from Current round.",skipped=character()))
   previous_results<-list()
   skipped<-character()
   for(i in seq_along(round_ids)){
     result<-tryCatch(compute_indicator(trend_previous_prepared(round_ids[[i]]),input$trend_indicator,input$trend_group),error=function(e) e)
     if(inherits(result,"error")) {
       skipped<-c(skipped,paste0(round_labels[[i]],": ",conditionMessage(result)))
     } else {
       result$Round<-round_labels[[i]]
       previous_results[[length(previous_results)+1L]]<-result
     }
   }
   if(!length(previous_results)) return(list(data=NULL,error="The selected indicator or cut is unavailable in every uploaded previous round.",skipped=skipped))
   current<-tryCatch(compute_indicator(trend_current_prepared(),input$trend_indicator,input$trend_group),error=function(e) e)
   if(inherits(current,"error")) return(list(data=NULL,error=conditionMessage(current),skipped=skipped))
   current$Round<-"Current round"
   list(data=do.call(rbind,c(previous_results,list(current))),error=NULL,skipped=skipped)
 })
 trend_result_long<-reactive({
   z<-trend_computation()
   validate(need(is.null(z$error),paste0("This indicator cannot yet be compared: ",z$error)))
   z$data
 })
 output$trend_result_choice_ui<-renderUI({
   d<-trend_result_long();opts<-unique(as.character(d$Result))
   if(length(opts)<=1) return(tags$div(class="help-note",paste0("Result: ",opts[[1]])))
   selectInput("trend_result_choice","Result or option to trend",opts)
 })
 output$trend_group_value_ui<-renderUI({
   d<-trend_result_long();groups<-unique(as.character(d$Group))
   if(identical(input$trend_group,"Overall")) return(tags$div(class="help-note","Showing the overall comparison."))
   selectInput("trend_group_value","Group to plot",groups)
 })
 trend_chart_data<-reactive({
   d<-trend_result_long()
   result_choice<-input$trend_result_choice
   if(is.null(result_choice) || !result_choice%in%as.character(d$Result)) result_choice<-as.character(d$Result)[1]
   d<-d[as.character(d$Result)==result_choice,,drop=FALSE]
   if(!identical(input$trend_group,"Overall")){
     group_choice<-input$trend_group_value
     if(is.null(group_choice) || !group_choice%in%as.character(d$Group)) group_choice<-as.character(d$Group)[1]
     d<-d[as.character(d$Group)==group_choice,,drop=FALSE]
   }
   d$Round<-factor(as.character(d$Round),levels=c(trend_round_labels(),"Current round"))
   d[order(d$Round),,drop=FALSE]
 })
 trend_comparison_table<-reactive({
   d<-trend_result_long()
   round_order<-c(trend_round_labels(),"Current round")
   available_rounds<-round_order[round_order %in%as.character(d$Round)]
   out<-NULL
   for(round_name in available_rounds){
     piece<-d[as.character(d$Round)==round_name,c("Group","Result","Estimate","Unweighted_N","Weighted_N"),drop=FALSE]
     names(piece)[3:5]<-paste(round_name,c("estimate","unweighted N","weighted N"))
     out<-if(is.null(out)) piece else merge(out,piece,by=c("Group","Result"),all=TRUE)
   }
   first_previous<-setdiff(available_rounds,"Current round")[1]
   if(length(first_previous) && "Current round"%in%available_rounds){
     change_name<-paste0("Change: Current round vs ",first_previous)
     out[[change_name]]<-round(out[["Current round estimate"]]-out[[paste(first_previous,"estimate")]],1)
   }
   out
 })
 output$trend_results_ui<-renderUI({
   if(!trend_ready()) return(NULL)
   z<-trend_computation()
   if(!is.null(z$error)) return(tags$div(class="alert alert-danger trend-status-note",paste0("Comparison unavailable: ",z$error,". Upload a compatible prior round or add its approved mapping before interpreting a trend.")))
   tags$div(class="trend-results-card",
    tags$div(class="trend-results-toolbar",tags$div(tags$h4("Step 3. Review the comparison"),tags$p("The chart shows one selected result and group for clarity; the table retains all available groups and options.")),tags$div(class="trend-results-actions",downloadButton("download_trend_chart","Download trend chart",class="btn-primary"),downloadButton("download_trend","Download trend table",class="btn-outline-secondary"))),
    if(length(z$skipped)) tags$div(class="help-note trend-status-note",paste0("Unavailable previous round(s): ",paste(z$skipped,collapse="; "))),
    tags$div(class="trend-chart-options",uiOutput("trend_result_choice_ui"),uiOutput("trend_group_value_ui")),
    plotOutput("trend_chart",height="430px"),
    DTOutput("trend_results")
   )
 })
 output$indicator_ui<-renderUI({x<-indicator_catalog[indicator_catalog$module==input$module,];selectInput("indicator","Indicator",setNames(x$id,x$label))})
 group_choices<-reactive({d<-prepared();src<-selected_source_module();groups<-available_groups(d,src);setNames(groups,gsub("_"," ",groups))})
 output$group_ui<-renderUI({selectInput("group","Disaggregate by",group_choices())})
 observeEvent(group_choices(),{
   choices<-group_choices();current<-isolate(input$group)
   updateSelectInput(session,"group",choices=choices,selected=if(!is.null(current) && current%in%unname(choices)) current else unname(choices)[1])
 },ignoreInit=TRUE)
 result_long<-reactive({req(input$indicator,input$group);compute_indicator(prepared(),input$indicator,input$group)})
 chart_data<-reactive({remove_missing_options(result_long())})
 result<-reactive({wide_results(remove_missing_options(result_long()))})
 map_choice_data<-reactive({
   d <- chart_data()
   validate(need(input$group=="lga","Select lga as the disaggregation to view the map."))
   opts <- unique(as.character(d$Result))
   choice <- if(length(opts)>1 && !is.null(input$map_result)) input$map_result else opts[1]
   d[d$Result==choice,,drop=FALSE]
 })
 map_shape_data<-reactive({
   validate(need(input$group=="lga","Select lga as the disaggregation to view the map."))
   shp <- lga_shapes(); d <- map_choice_data()
   d$join_lga <- map_lga_key(d$Group)
   m <- match(shp$join_lga,d$join_lga)
   shp$Estimate <- d$Estimate[m]
   shp$Mapped_Result <- d$Result[m]
   is_mean <- any(d$Result=="Weighted mean")
   shp$MapClass <- classify_map_values(shp$Estimate,is_mean=is_mean)
   list(
     shp=shp,
     is_mean=is_mean,
     title=indicator_catalog$label[match(input$indicator,indicator_catalog$id)]
   )
 })
 output$status<-renderUI({d<-prepared();src<-selected_source_module();div(class="alert alert-success",sprintf("Loaded %s records and %s variables from the %s file for this indicator.",format(nrow(d),big.mark=","),ncol(d),src))})
 output$context_strip<-renderUI({
   d<-prepared(); src<-selected_source_module(); label<-indicator_catalog$label[match(input$indicator,indicator_catalog$id)]
   tags$div(class="context-strip",
     tags$span(class="context-item",HTML(paste0("<strong>Source:</strong> ",tools::toTitleCase(src)))),
     tags$span(class="context-item",HTML(paste0("<strong>Cut:</strong> ",gsub("_"," ",input$group)))),
     tags$span(class="context-item",HTML(paste0("<strong>Records:</strong> ",format(nrow(d),big.mark=",")))),
     tags$span(class="context-item",HTML(paste0("<strong>Indicator:</strong> ",label)))
   )
 })
 output$result_title<-renderText(indicator_catalog$label[match(input$indicator,indicator_catalog$id)])
 output$results<-renderDT({datatable(result(),rownames=FALSE,options=list(scrollX=TRUE,pageLength=10,dom="tip"),class="compact stripe hover")})
 output$chart<-renderPlot({make_indicator_plot(chart_data(),indicator_catalog$label[match(input$indicator,indicator_catalog$id)],input$group,input$indicator)})
 trend_plot<-reactive({
   d<-trend_chart_data()
   label<-indicator_catalog$label[match(input$trend_indicator,indicator_catalog$id)]
   selected_group<-if(identical(input$trend_group,"Overall")) "Overall" else as.character(d$Group)[1]
   make_trend_plot(d,paste0(label," — ",selected_group))
 })
 output$trend_chart<-renderPlot(trend_plot())
 output$trend_results<-renderDT({
   datatable(trend_comparison_table(),rownames=FALSE,options=list(scrollX=TRUE,pageLength=12,dom="tip"),class="compact stripe hover")
 })
 output$map_controls<-renderUI({
   d <- chart_data()
   if (!identical(input$group,"lga")) return(tags$div(class="help-note","Select lga as the disaggregation to view the map."))
   opts <- unique(as.character(d$Result))
   tagList(
     if(length(opts)>1) selectInput("map_result","Map option/category",choices=opts,selected=opts[1]),
     tags$div(class="map-download-bar",
      tags$div(tags$strong("Download this map"),tags$span("Export a clean PNG image or PDF with LGA names and estimates, without a web-map background.")),
       tags$div(class="map-download-actions",
         downloadButton("download_map_png","Download PNG",class="btn-primary"),
         downloadButton("download_map_pdf","Download PDF",class="btn-outline-secondary")
       )
     ),
     tags$div(class="help-note","Map classes: Low (red) 0–39.9%, Medium (yellow) 40–69.9%, High (green) 70–100%. Mean indicators use Low/Medium/High tertiles across LGAs. Grey means no data or unmatched LGA.")
   )
 })
  output$map<-renderLeaflet({
   validate(need(input$group=="lga","Select lga as the disaggregation to view the map."))
   x <- map_shape_data(); shp <- x$shp
   # Assign named colours directly: leaflet::colorFactor() alphabetises
   # character classes, which can make it disagree with static map exports.
   shp$MapFill <- unname(map_class_cols[as.character(shp$MapClass)])
   shp$MapFill[is.na(shp$MapFill)] <- unname(map_class_cols["No data"])
   suffix <- if(x$is_mean) "" else "%"
   lbl <- paste0("<strong>",shp$lganame,"</strong><br/>",
     ifelse(is.na(shp$Estimate),"No data",paste0(round(shp$Estimate,1),suffix," — ",as.character(shp$MapClass))))
   leaflet(shp) %>%
     addTiles(attribution="&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors") %>%
     addPolygons(fillColor=~MapFill,fillOpacity=.85,color="#6F7D83",weight=1,opacity=.8,
       label=lapply(lbl,htmltools::HTML),
       popup=lapply(lbl,htmltools::HTML),
       highlightOptions=highlightOptions(weight=3,color="#2D3A3F",bringToFront=TRUE)) %>%
     addLegend(position="bottomright",colors=unname(map_class_cols),labels=names(map_class_cols),title="Map class",opacity=.9)
 })
 output$catalogue<-renderDT({x<-indicator_catalog[,c("module","source_module","id","label")];x$module<-module_labels[x$module];names(x)<-c("Thematic area","Source file","Indicator ID","Indicator");datatable(x,rownames=FALSE,options=list(pageLength=12,scrollX=TRUE),class="compact stripe hover")})
 output$dictionary_note<-renderUI({
   if(is.null(input$questionnaire)) tags$div(class="help-note","Upload the optional questionnaire XLSX file from the sidebar to display the questionnaire dictionary here.")
   else tags$div(class="alert alert-success","Questionnaire loaded. The dictionary below shows the available survey metadata.")
 })
 output$dictionary<-renderDT({req(input$questionnaire);d<-readxl::read_excel(input$questionnaire$datapath,sheet="survey");names(d)<-clean_names(names(d));keep<-intersect(c("type","name","label_english_en","label_hausa_ha","relevant","required"),names(d));datatable(head(d[,keep,drop=FALSE],500),rownames=FALSE,options=list(pageLength=12,scrollX=TRUE),class="compact stripe hover")})
 output$download<-downloadHandler(filename=function()paste0(input$indicator,"_by_",input$group,".csv"),content=function(f)write.csv(result(),f,row.names=FALSE,na=""))
 output$download_chart<-downloadHandler(filename=function()paste0(input$indicator,"_by_",input$group,".png"),content=function(f)ggsave(f,plot=make_indicator_plot(chart_data(),indicator_catalog$label[match(input$indicator,indicator_catalog$id)],input$group,input$indicator),width=11,height=7,dpi=150))
 output$download_trend_chart<-downloadHandler(
   filename=function()paste0("kaduna_",input$trend_indicator,"_multi_round_trend.png"),
   content=function(f)ggsave(f,plot=trend_plot(),width=11,height=6.5,units="in",dpi=300)
 )
 output$download_trend<-downloadHandler(
   filename=function()paste0("kaduna_",input$trend_indicator,"_multi_round_trend.csv"),
   content=function(f)write.csv(trend_comparison_table(),f,row.names=FALSE,na="")
 )
 output$download_map_png<-downloadHandler(
   filename=function()paste0("kaduna_",input$indicator,"_lga_map.png"),
   content=function(f){x<-map_shape_data();ggsave(f,plot=make_lga_map_plot(x$shp,x$title,x$is_mean),width=10,height=8,units="in",dpi=300)}
 )
 output$download_map_pdf<-downloadHandler(
   filename=function()paste0("kaduna_",input$indicator,"_lga_map.pdf"),
   content=function(f){x<-map_shape_data();ggsave(f,plot=make_lga_map_plot(x$shp,x$title,x$is_mean),width=10,height=8,units="in",device=grDevices::pdf)}
 )
}
shinyApp(ui,server)
