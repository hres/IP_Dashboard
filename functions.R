
function_plot<-function(df){
  cols<-c('YES'='#00b050','NO'='#C00000')

  ggplot(df,aes(x=status,y=n,fill=status))+geom_bar(stat='identity')+
    scale_fill_manual(values=cols)+
    ylim(0,max(df$n+10))+
    geom_text(aes(label=as.character(n)),position=position_dodge(width=0.9),vjust=-0.5)+
    #theme_minimal()+
    labs(x='Functionality Met',y='Number of IP')+
    theme(
      #axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.justification = 'top',
      panel.grid       = element_line(colour = "grey84"),
      panel.grid.major = element_line(size = rel(0.3)),
      panel.grid.minor = element_line(size = rel(0.15)),
      panel.background  = element_blank(),
      panel.border      = element_rect(linetype='solid',size=0.1, fill='NA'),
      #strip.background  = element_blank(),
      plot.background   = element_blank(),
      axis.text.x =element_text(size=10),
      legend.title=element_blank(),
      strip.text = element_text(size = 12)
    )+facet_grid(.~internal_external)
  
  # level<-c('#00b050','#97edbe','#C00000','#eda8a8')
  # df$color<-factor(df$color,levels=level)
  # 
  # ggplot(df,aes(x=status,y=n,fill=color))+geom_bar(stat='identity',position = 'dodge')+
  #   scale_fill_manual(values=level,labels=c('External','Internal','External','Internal'))+
  #   ylim(0,max(df$n+10))+
  #   geom_text(aes(label=as.character(n)),position=position_dodge(width=0.9),vjust=-0.5)+
  #   theme_minimal()+
  #   labs(x='Functionality Met',y='Number of IP')+
  #   theme(
  #     axis.text.x =element_text(size=10),
  #     legend.title=element_blank(),
  #     strip.text = element_text(size = 12)
  #   )
  
}


budget_plot<-function(ds){
  
  ds$total<-ds$capital+ds$non_capital
 
  ds<-ds%>%
    mutate(label=case_when(var=='Project Expenditures' ~ paste0('Expenditure:$',prettyNum(non_capital,big.mark=',')),
                           var=='Project Authority' & capital !=0 ~ paste0('Authority: $',prettyNum(total,big.mark=','),'\n',
                                                                           'Non-Capital: $',prettyNum(non_capital,big.mark=',')),
                           var=='Project Authority' & capital ==0 ~ paste0('Authority: $',prettyNum(total,big.mark=','))))
  
  
  plot_ly(ds%>%filter(var=='Project Authority'),x=~year-0.2,y=~capital,type='bar',name='Project Authority - Capital',
          marker=list(color='rgb(252,205,201)'),
          hoverinfo='text',
          text=~paste('Capital: $',prettyNum(capital,big.mark=',')))%>%
    add_trace(y=~non_capital,name='Project Authority - Non-Capital',marker=list(color='rgb(248,118,109'),
              hoverinfo='text',
              text=~ label)%>%
    layout(barmode='stack',yaxis=list(title='Budget'))%>%
    add_trace(data=ds%>%filter(var=='Project Expenditures'),x=~year+0.2,y=~non_capital,type='bar',name='Project Expenditure',
              marker=list(color='rgb(0,191,196'),
              hoverinfo='text',
              text=~label)%>%
    layout(xaxis=list(title='Fiscal Year',
                      ticktext=list("2016-17","2017-18","2018-19","2019-20","2020-21","2021-22","2022-23"),
                      tickvals = list(2016, 2017, 2018, 2019, 2020,2021,2022),
                      tickmode='array'))%>%
    layout(legend=list(y=1,x=0.7))
  
  
  # if(internal==TRUE){
  #   ds$internal_external<-factor(ds$internal_external,levels=c('External','Internal'))
  #   
  #   p<- ggplot(ds,aes(x=Year,y=value,fill=var,alpha=internal_external,text=paste0('Amount: $',prettyNum(value,big.mark=','))))+
  #       scale_y_continuous(labels=dollar_y,breaks=seq(1000000,12000000,2500000))+
  #       scale_alpha_manual(values=c(1,0.3))+
  #       labs(x='',y='Budget Amount',alpha='',fill='')
  # }else{
  #   
  #   p<-ggplot(ds,aes(x=Year,y=value,fill=var,text=paste0('Amount: $',prettyNum(value,big.mark=','))))+
  #      scale_y_continuous(labels=dollar_y)+
  #      labs(x='',y='Budget Amount',fill='')
  #     
  # }
  # 
  #   p+
  #   geom_bar(stat='identity',position='dodge',width=0.8)+
  #   theme_minimal()
}


dollar_y<-function(x){
  x<-paste0('$',round(x/10^6,1),'M')
  x
}


budget_plot2<-function(ds,internal){
  
  min<-ifelse(min(ds$value)<0,abs(min(ds$value))*-1.2,0)
  max<-max(ds$value)*1.2
  
  if(internal==TRUE){
    
    ds<-ds%>%
      mutate(col=case_when(value>=0 & internal_external=='Internal'~ '#add5f7',
                           value>=0 & internal_external=='External'~ '#1f77b4',
                           value<0 & internal_external=='Internal'~ '#f2c1c4',
                           value<0 & internal_external=='External'~ '#980008'))  
    
    level<-c('#1f77b4','#add5f7','#980008','#f2c1c4')
    col<-c('#1f77b4','#add5f7','#980008','#f2c1c4')
    ds$col <- factor(ds$col, levels=level, ordered=TRUE)
    
    p<-ggplot(ds,aes(x=cat,y=value,fill=col,group=internal_external))+geom_bar(stat='identity',position='dodge')+
      scale_fill_manual(values=col, labels=c('External','Internal','External','Internal'),drop=F)+
      guides(fill=guide_legend(title=""))
    
  }else{
    
    ds$col<-ifelse(ds$value>=0,'#1f77b4','#980008')
    
    p<-ggplot(ds,aes(x=cat,y=value,fill=col))+geom_bar(stat='identity',position='dodge')+
      scale_fill_manual(values=c('#1f77b4','#980008'))+
      guides(fill=FALSE)
  }
  
  p+scale_y_continuous(labels=dollar_y,limits=c(min,max))+
    labs(y='Budget Amount')+
    geom_text(aes(label=dollar(value),vjust=ifelse(value>0,-1,1.5)),position = position_dodge(width = 1))+
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.text.x =element_text(size=11,family='sans',color='#494949'),
          legend.text=element_text(size=12,family='sans',color='#494949'),
          legend.justification = 'top')
  
}


stage_plot<-function(df){
  cols<-c('On Track'='#00B050','Caution'='#FFC000','Elevated Risk'='#C00000','Not yet started'='#1f77b4')
  
  label='E- External\nI- Internal'

  ggplot(df,aes(x=stage,y=count,group=status,fill=status))+geom_bar(stat='identity',position='dodge',width=0.9,alpha=0.9)+
    scale_fill_manual(values=cols)+
    scale_y_continuous(breaks=c(0,1,2,3,4,5))+
    geom_text(aes(y=count-0.5,label=IP),position=position_dodge(width=0.9),size=2.5)+
    annotate("text",x=length(unique(df$stage)),y=3,label=label,size=3,color='#494949')+
    theme_minimal()+
    labs(y='Number of IP')+
    theme(axis.title.x=element_blank(),
          #axis.text.x =element_text(size=12),
          legend.title=element_blank())
          #legend.justification = 'top',
          #legend.text=element_text(size=12,color='#494949'),
          #legend.spacing = unit(1.0,'cm'))
    
}

status_plot<-function(df){
  
  cols<-c('On Track'='#00B050','Caution'='#FFC000','Elevated Risk'='#C00000')
  
  # df<-df%>%mutate(txt_position=case_when(status =='Elevated Risk'~3,
  #                                        status=='Caution'~2,
  #                                        status=='On Track'~1))
  # df<-df%>%arrange(status,desc(`Approved Budget`))
  # df$index<-ifelse(as.numeric(rownames(df))%%2==0,1,-1)
  # df$txt_position<-df$txt_position+0.3*df$index
  label='E- External\nI- Internal'
  
  df%>%
    arrange(status)%>%
    ggplot(aes(x=as.character(IP),y=`Approved Budget`,size=`Approved Budget`,color=status,text=dollar(`Approved Budget`)))+
    scale_color_manual(values=cols)+
    geom_point(alpha=0.5)+
    geom_text(aes(label=paste0('IP',IP2)),size=3,nudge_y=-300000,nudge_x=0.4)+
    scale_size_continuous(breaks=c(500,100000,500000,5000000,20000000),range=c(1,30))+
    scale_y_continuous(limits=c(0,17000000),breaks=seq(1500000,20000000,2000000),labels = dollar_y)+
    theme_minimal()+
    labs(x='IP Project')+
    annotate("text",x=length(unique(df$IP)),y=17*10^6,label=label,size=3)+
    theme(axis.text.x =element_blank(),legend.position='none')
  
}

timeplot<-function(df,internal){
  status_levels <- c("Forecasted completion date is within 3 months of baseline date",
                     "Forecasted completion date within 3-6 months of baseline date", 
                     "Forecasted completion date is over 6 months of baseline date")
  status_colors <- c( "#00B050", "#FFC000", "#C00000")
  
  df$Schedule.Health <- factor(df$Schedule.Health, levels=status_levels, ordered=TRUE)
  
  positions <- c(0.4, -0.4, 0.5, -0.5,0.9,-0.9,1.25, -1.25)
  directions <- c(1, -1)
  
  line_pos <- data.frame(
    "date"=sort(unique(df$Actual_date),na.last=T),
    "position"=rep(positions, length.out=length(unique(df$Actual_date))),
    "direction"=rep(directions, length.out=length(unique(df$Actual_date)))
  )
  
  df<-left_join(df,line_pos,by=c('Actual_date'='date'))
  text_offset <- 0.25
  
  df$month_count <- ave(df$Actual_date==df$Actual_date, df$Actual_date, FUN=cumsum)
  df$text_position <- (df$month_count * text_offset * df$direction) + df$position
  
  month_buffer <- 4
  
  month_date_range <- seq(min(df$Actual_date,na.rm=T) - months(month_buffer), max(df$Actual_date,na.rm=T) + months(month_buffer), by='month')
  month_df <- data.frame(month_date_range)
  month_df$month_format <- paste0(year(month_df$month_date_range),' ',quarters(month_df$month_date_range))
  month_df$month_format<-ifelse(month_df$month_format==lag(month_df$month_format,default=''),'',month_df$month_format)
  
  if(internal){
  timeline_plot<-ggplot(df,aes(x=Actual_date,y=0,label=Major.Milestone,color=Schedule.Health))+
    scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)+
    scale_shape_manual(values=c(15,16))+
    labs(col="",shape="")+
    theme_classic()+
    geom_hline(yintercept=0, color = "black", size=0.3)+
    # Plot vertical segment lines for milestones
    geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=Actual_date), color='black', size=0.2)+
    geom_point(aes(y=0,shape=internal_external), size=4)+  # Plot scatter points at zero and date
    geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=4,family='sans',vjust=0.5, color='gray23')+
    geom_text(aes(y=text_position,label=Major.Milestone),size=4.5,family='sans')+
    theme(
      legend.text=element_text(size=12,family='sans',color='#494949')
    )
   
  }else{
  timeline_plot<-ggplot(df,aes(x=Actual_date,y=0,label=Major.Milestone,color=Schedule.Health))+
      scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)+
      labs(col="")+
      theme_classic()+
      geom_hline(yintercept=0, color = "black", size=0.3)+
      # Plot vertical segment lines for milestones
      geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=Actual_date), color='black', size=0.2)+
      geom_point(aes(y=0), size=3)+  # Plot scatter points at zero and date
      geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),vjust=0.5, color='gray23',size=3)+
      geom_text(aes(y=text_position,label=Major.Milestone),size=3)+
      theme(
      legend.text=element_text(size=8,color='#494949')
    )
  
  }
  
  # Don't show axes, appropriately position legend
  timeline_plot<-timeline_plot+
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x =element_blank(),
          axis.ticks.x =element_blank(),
          axis.line.x =element_blank(),
          legend.position="bottom")
  
  return(timeline_plot)
}
