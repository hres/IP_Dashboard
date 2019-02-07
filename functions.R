# timeplot2<-function(df){
#   
#   #df$Schedule.Health <- factor(df$Schedule.Health, levels=status_levels, ordered=TRUE)
#   
#   positions <- c(0.3, -0.3, 0.8, -0.8, 1.2, -1.2)
#   directions <- c(1, -1)
#   
#   line_pos <- data.frame(
#     "date"=unique(df$Actual_date),
#     "position"=rep(positions, length.out=length(unique(df$Actual_date))),
#     "direction"=rep(directions, length.out=length(unique(df$Actual_date)))
#   )
#   
#   df<-left_join(df,line_pos,by=c('Actual_date'='date'))
#   text_offset <- 0.1
#   
#   df$month_count <- ave(df$Actual_date==df$Actual_date, df$Actual_date, FUN=cumsum)
#   df$text_position <- (df$month_count * text_offset * df$direction) + df$position
#   
#   month_buffer <- 4
#   
#   month_date_range <- seq(min(df$Actual_date,na.rm=T) - months(month_buffer), max(df$Actual_date,na.rm=T) + months(month_buffer), by='month')
#   month_df <- data.frame(month_date_range)
#   month_df$month_format <- paste0(year(month_df$month_date_range),' ',quarters(month_df$month_date_range))
#   month_df$month_format<-ifelse(month_df$month_format==lead(month_df$month_format,default=''),'',month_df$month_format)
#   
#   timeline_plot<-ggplot(df,aes(x=Actual_date,y=0,label=Major.Milestone))+
#     labs(col="Milestones")+
#     theme_classic()
#   #timeline_plot<-timeline_plot+scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)
#   
#   # Plot horizontal black line for timeline
#   timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
#                                           color = "black", size=0.3)
#   
#   # Plot vertical segment lines for milestones
#   timeline_plot<-timeline_plot+geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=Actual_date), color='black', size=0.2)
#   
#   # Plot scatter points at zero and date
#   timeline_plot<-timeline_plot+geom_point(aes(y=0), size=3)
#   
#   # Don't show axes, appropriately position legend
#   timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
#                                      axis.text.y=element_blank(),
#                                      axis.title.x=element_blank(),
#                                      axis.title.y=element_blank(),
#                                      axis.ticks.y=element_blank(),
#                                      axis.text.x =element_blank(),
#                                      axis.ticks.x =element_blank(),
#                                      axis.line.x =element_blank(),
#                                      legend.position = "bottom")
#   
#   # Show text for each month
#   timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=3,vjust=0.5, color='black')
#   # Show text for each milestone
#   timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=Major.Milestone),size=3)
#   
#   ggplotly(timeline_plot,height=450)
# }


budget_plot<-function(ds){
  p<-ggplot(ds,aes(x=Year,y=value,fill=var,text=paste0('Amount: $',prettyNum(value,big.mark=','))))+
    geom_bar(stat='identity',position='dodge')+
    scale_y_continuous(labels=dollar)+
    scale_fill_discrete(name = "")+
    labs(x='',y='')+
    theme_minimal()
  
  return(p)
  #ggplotly(p,tooltip = "text")%>%layout(margin=list(b=50),xaxis=list(tickangle=-45))
}

budget_plot2<-function(ds){
  
  ds$col<-ifelse(ds$value>=0,'#1f77b4','#980008')
  p<-ggplot(ds,aes(x=cat,y=value,fill=col))+geom_bar(stat='identity')+
    scale_fill_manual(values=c('#1f77b4','#980008'))+
    scale_y_continuous(labels=dollar)+
    labs(x='',y='')+
    geom_text(aes(label=dollar(value),vjust=ifelse(value>0,-1,1.5)))+
    guides(fill=FALSE)+
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.text.x =element_text(size=11),
          axis.title.y =element_blank())
  
  p
}


timeplot<-function(df){
  
  status_levels <- c("within 3 months", "3-6 months", "6+ months","no change")
  status_colors <- c( "#00B050", "#FFC000", "#C00000","#000000")
  
  df$Schedule.Health <- factor(df$Schedule.Health, levels=status_levels, ordered=TRUE)
  
  positions <- c(0.3, -0.3, 0.5, -0.5,0.9,-0.9,1.2, -1.2)
  directions <- c(1, -1)
  
  line_pos <- data.frame(
    "date"=sort(unique(df$Actual_date),na.last=T),
    "position"=rep(positions, length.out=length(unique(df$Actual_date))),
    "direction"=rep(directions, length.out=length(unique(df$Actual_date)))
  )
  
  df<-left_join(df,line_pos,by=c('Actual_date'='date'))
  text_offset <- 0.1
  
  df$month_count <- ave(df$Actual_date==df$Actual_date, df$Actual_date, FUN=cumsum)
  df$text_position <- (df$month_count * text_offset * df$direction) + df$position
  
  month_buffer <- 4
  
  month_date_range <- seq(min(df$Actual_date,na.rm=T) - months(month_buffer), max(df$Actual_date,na.rm=T) + months(month_buffer), by='month')
  month_df <- data.frame(month_date_range)
  month_df$month_format <- paste0(year(month_df$month_date_range),' ',quarters(month_df$month_date_range))
  month_df$month_format<-ifelse(month_df$month_format==lead(month_df$month_format,default=''),'',month_df$month_format)
  
  timeline_plot<-ggplot(df,aes(x=Actual_date,y=0,label=Major.Milestone,color=Schedule.Health))+
    labs(col="Milestones")+
    theme_classic()
  timeline_plot<-timeline_plot+scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)
  
  # Plot horizontal black line for timeline
  timeline_plot<-timeline_plot+geom_hline(yintercept=0, color = "black", size=0.3)
  
  # Plot vertical segment lines for milestones
  timeline_plot<-timeline_plot+geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=Actual_date), color='black', size=0.2)
  
  # Plot scatter points at zero and date
  timeline_plot<-timeline_plot+geom_point(aes(y=0), size=3)
  
  # Don't show axes, appropriately position legend
  timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                     axis.text.y=element_blank(),
                                     axis.title.x=element_blank(),
                                     axis.title.y=element_blank(),
                                     axis.ticks.y=element_blank(),
                                     axis.text.x =element_blank(),
                                     axis.ticks.x =element_blank(),
                                     axis.line.x =element_blank())
                                     #legend.position='bottom')
  
  # Show text for each month
  timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=3,vjust=0.5, color='black')
  # Show text for each milestone
  timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=Major.Milestone),size=3)
  
  return(timeline_plot)
  #ggplotly(timeline_plot,height=450,tooltip=NULL)
}
