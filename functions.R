# ZS colors
zs_orange = "#ED8B00"
zs_green = "#4F868E"

#TimeTextToInt function
TimeTextToInt <- function(x) {
  # TimeTextToInt converts an input character for call length to an integer
  y = ifelse(x == "UP TO 15 MIN",7.5,
             ifelse(x == "16 TO 30 MIN",23,
                    ifelse(x == "31 TO 45 MIN",38,
                           ifelse(x == "46 TO 60 MIN",53,
                                  ifelse(x == "61 TO 120 MIN",90.5,
                                         ifelse(x == "OVER 120 MIN",120,NA))))))
  return(y)
}


#pakInstall function
pakInstall <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# write.excel function
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# Function to find feature names that include the input prefix in the input ds
GetFeatureNames <- function(prefix="", ds, suffix=NULL) {
  feature.names = names(ds)
  these.features = feature.names[substr(feature.names, 1, nchar(prefix)) == prefix]
  if (length(suffix) > 0) {
    these.features = these.features[substr(these.features,
                                           nchar(these.features) - nchar(suffix) + 1,
                                           nchar(these.features)) == suffix]
  }
  return(these.features)
}



######## CREATE EDA PLOTS WITH PROFILE OR CALLS DATA BY CATEGORICAL OR TIME VARIABLE  ########
#' @description CREATE EDA PLOTS WITH PROFILE OR CALLS DATA BY CATEGORICAL OR TIME VARIABLE
#' @param Data Raw Dataset for the EDA.
#' @param UID Unique ID in Data.
#' @param cat_var categorical variable for grouping the data.
#' @param time_var  time variable for the x-axis.
#' @param profdt whether a profile data (without time var) or calls data (with time var).
#' @param plot_values count - total calls , uniqueID - total no of Hospitals/HCPs.
#' @param remove_na_cat whether to remove the NA category of cat_var.
#' @param PriorityOnly whether to take Priority1 calls or the PDE.
#' @param DataName Hospital Data or HCP Data (for the plots labels).
#' @return prints the ggplot
##################################################################################

EDAplots <- function(Data,UID = NULL,cat_var = NULL,time_var = NULL,profdt = FALSE ,plot_value = c("count","uniqueID"),remove_na_cat = FALSE,PriorityOnly = TRUE,DataName = c("Hospital","HCP")){
  
  
  ## build plot data
  plot_data <- Data
  if(remove_na_cat){ plot_data <- plot_data[!is.na(get(cat_var))]}
#  if(PriorityOnly) { plot_data <- plot_data[Detail.Priority == 1]}
  
  if(is.null(time_var)) {
    if(profdt){
      plot_data <- plot_data[,.(No.of.ID = length(unique(get(UID)))),
                             by = .(cat =get(cat_var))]
    }else{
      plot_data <- plot_data[,.(No.of.ID = length(unique(get(UID))),CallCnt=sum(Effort,na.rm = T)),
                             by = .(cat =get(cat_var),ID = get(UID))]
    }
    
  } else {
    
    if (is.null(cat_var)) {
      plot_data <- plot_data[,.(No.of.ID = length(unique(get(UID))),CallCnt=sum(Effort,na.rm = T)),
                             by = .(time =get(time_var))]
    } else {
      plot_data <- plot_data[,.(No.of.ID = length(unique(get(UID))),CallCnt=sum(Effort,na.rm = T)),
                             by = .(time =get(time_var),cat =get(cat_var))]
    }
  }
  
  ### stack up ggplots
  barplot <- ggplot2::ggplot(plot_data,
                             aes_string(y=ifelse(plot_value == "count","CallCnt","No.of.ID")))
  
  if(is.null(time_var)) {
    if(profdt){
      barplot <- barplot + ggplot2::geom_bar(aes_string(x = "cat"),stat = "identity")
    }else{
      barplot <- barplot + ggplot2::geom_point(aes_string(x = "cat",color = "cat"),size = 1)
    }
  } else {
    
    if(is.null(cat_var)){
      barplot <- barplot + ggplot2::geom_line(aes_string(x = "time"),color = "blue")
    } else {
      barplot <- barplot + ggplot2::geom_bar(aes_string(x = "time",fill = "cat"),stat = "identity")
    } 
    barplot <- barplot +  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      theme(axis.text.x=element_text(angle=60, hjust=1),legend.position = "bottom")
    
  }  
  barplot <- barplot +  labs(title=ifelse(is.null(DataName),NULL,paste0("MJN ",DataName,ifelse(plot_value == "count"," Calls","s"),ifelse(is.null(time_var),paste0(" vs. ",cat_var)," by Month")," (2015-2018)")),
                             subtitle=ifelse(!PriorityOnly,"Overall Effort","Priority 1 Calls"),
                             caption=ifelse(DataName == "HCP","Source: ZS_HCP Calls2_201507_201806.csv",ifelse(DataName == "Hospital","Source: ZS_201507_201806_HospitalCalls.csv",NULL)),
                             x=ifelse(is.null(time_var),cat_var,"Month"),
                             y=paste0("Total Number of ",DataName,ifelse(plot_value == "count"," Calls","s")),
                             color = cat_var,fill = cat_var)  
  print(barplot)
  
}

###### Laged scatter plots of variables in a panel data
lag_plot_bypanel<- function(data,ID_var,timevar = "yearmon",True_var,Lag_var,Lag_value = 1,show.plot = T,...){
  
  setDT(data)
  plot_data <- data[,.("ID_var"= get(ID_var),"timevar" = get(timevar),"True_var" = get(True_var),"Lag_var" = get(Lag_var))]
  setkey(plot_data,ID_var,timevar)
  plot_data[,Lagged_var := dplyr::lag(Lag_var,n = Lag_value),ID_var]
  plot_data[,Lag_var := NULL]
  plot_data <- plot_data[!is.na(Lagged_var) & !is.na(True_var)]
  if(show.plot){
    print(paste0("Correlation between Lag",Lag_value," of ",Lag_var," and ",True_var," is ",plot_data[,scales::percent(cor(True_var,Lagged_var))]))
    print(ggplot(plot_data,aes(Lagged_var,True_var)) + geom_point() + geom_smooth(method = "lm") +labs(x =paste0("Lag",Lag_value," of ",Lag_var), y = True_var,...))
  } else {
    return(plot_data[,cor(True_var,Lagged_var)])
  }
}




# get feature names which are unique at a given level
unique.features <- function(data,id_var){
  unq_counts <- data[,lapply(.SD, function(x){uniqueN(data.table(x,get(id_var)))}),.SDcols = setdiff(colnames(data),id_var)]
  unq_feat <- names(unq_counts)[unq_counts == data[,uniqueN(get(id_var))]]
  return(unq_feat)
}

## Creating a function to create a scatter plot
plotScatter <- function(data,response,iv,norm_var = NULL,group = NULL,out_cutoff = 0){
  
  if (length(norm_var) > 0) {
    data[,`:=`(y = ifelse(get(norm_var) == 0,0,get(response)/get(norm_var)),
              x = ifelse(get(norm_var) == 0,0,get(iv)/get(norm_var)))]
  } else {
    data[,`:=`(y = get(response),
               x = get(iv))]
  }
  data <- data[x <= quantile(x,1-out_cutoff,na.rm = T)]
  if (is.null(group)) {
    pltScat <- ggplot2::ggplot(data,aes(y = y,x = x)) + 
      geom_point() + 
      geom_smooth(method = "lm") +
      labs(x = iv,y = response)
  } else{ 
    pltScat <- ggplot2::ggplot(data,aes(y = y,x = x)) + 
      geom_point(aes(color = get(group))) + 
      geom_smooth(method = "lm") +
      theme(legend.position="none") + 
      labs(x = iv,y = response)
  }
  pltHist <- ggplot2::ggplot(data,aes(x)) + 
    geom_histogram() + 
    labs(x = iv)
  
  
  plt <- grid.arrange(pltScat,pltHist)
  return(plt)
  
}


#### EDA plots ####

bar_plot <- function(data,data_column = c("Avg_presens_per_HCP","Avg_presn_dur"),cut_by ){
  
  plot_data <- data[,c("ownr_id","src_bp_id","msl_presn_dur_in_min_txt",cut_by),with = F]
  plot_data[,presen_dur_min := TimeTextToInt(msl_presn_dur_in_min_txt)]
  
  setnames(plot_data,cut_by,paste0("Var",1:length(cut_by)))
  
  if(length(cut_by) == 1){
    plot_data <- plot_data[,.(total_presentations = .N,total_MSLs_connected = uniqueN(ownr_id),total_HCPs_connected = uniqueN(src_bp_id),
                              Avg_presens_per_HCP = .N/uniqueN(src_bp_id),Avg_presens_per_MSL = .N/uniqueN(ownr_id), Avg_presn_dur = mean(presen_dur_min,na.rm = T)),.(Var1)]
    bar_plot <- ggplot(plot_data,aes(Var1,get(data_column))) + geom_bar(stat = "identity")
  }else if(length(cut_by) == 2){
    plot_data <- plot_data[,.(total_presentations = .N,total_MSLs_connected = uniqueN(ownr_id),total_HCPs_connected = uniqueN(src_bp_id),
                              Avg_presens_per_HCP = .N/uniqueN(src_bp_id), Avg_presens_per_MSL = .N/uniqueN(ownr_id),Avg_presn_dur = mean(presen_dur_min,na.rm = T)),.(Var1,Var2)]
    bar_plot <- ggplot(plot_data,aes(Var1,get(data_column),fill = Var2)) + geom_bar(stat = "identity",position = "dodge")+ 
      theme(legend.position = "bottom")+labs(fill = cut_by[2])
  }else if(length(cut_by) == 3){
    plot_data <- plot_data[,.(total_presentations = .N,total_MSLs_connected = uniqueN(ownr_id),total_HCPs_connected = uniqueN(src_bp_id),
                              Avg_presens_per_HCP = .N/uniqueN(src_bp_id), Avg_presens_per_MSL = .N/uniqueN(ownr_id),Avg_presn_dur = mean(presen_dur_min,na.rm = T)),.(Var1,Var2,Var3)]
    bar_plot <- ggplot(plot_data,aes(Var1,get(data_column),fill = Var2)) + geom_bar(stat = "identity",position = "dodge") + 
      facet_wrap(~Var3,scales = "free")+ theme(legend.position = "bottom")+labs(fill = cut_by[2])
  }
  
  bar_plot <- bar_plot + xlab(cut_by[1]) + ylab(data_column)
  return(bar_plot)
}

time_plot <- function(data,data_column = c("Avg_presens_per_HCP","Avg_presn_dur"),cut_by ,time_agg = "monthly"){
  
  plot_data <- data[,c("ownr_id","src_bp_id","msl_presn_dur_in_min_txt","presn_dt",cut_by),with = F]
  plot_data[,presen_dur_min := TimeTextToInt(msl_presn_dur_in_min_txt)]
  
  if(time_agg == "monthly"){
    plot_data[,presn_mon := as.Date(as.yearmon(presn_dt))]
  } else if(time_agg == "quarterly") {
    plot_data[,presn_mon := as.Date(as.yearqtr(presn_dt))]
  }
  
  setnames(plot_data,cut_by,"Var1")
  
  plot_data <- plot_data[,.(total_presentations = .N,total_MSLs_connected = uniqueN(ownr_id),total_HCPs_connected = uniqueN(src_bp_id),
                              Avg_presens_per_HCP = .N/uniqueN(src_bp_id),Avg_presens_per_MSL = .N/uniqueN(ownr_id), Avg_presn_dur = mean(presen_dur_min,na.rm = T)),.(Var1,presn_mon)]
  
  plot_data <- merge(as.data.table(expand.grid(Var1 = unique(plot_data$Var1),presn_mon = unique(plot_data$presn_mon))),plot_data,by = c("Var1","presn_mon"),all.x = T)
  plot_data[is.na(plot_data)] <- 0
  
  bar_plot <- ggplot(plot_data,aes(presn_mon,get(data_column),color = Var1)) + geom_line()+ facet_grid(Var1~.,scales = "free_y") + theme(legend.position = "none")
  
  bar_plot <- bar_plot + xlab(ifelse(time_agg == "monthly","Presentation Month","Presentation Quarter")) + ylab(data_column)
  return(bar_plot)
}
