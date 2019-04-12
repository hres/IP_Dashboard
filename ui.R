
ui<-dashboardPage(
  dashboardHeader(title=paste0('HPFB IP Dashboard \n as of ',dat),
                  titleWidth=500),
  
  dashboardSidebar(width=150,
                   sidebarMenu(id='sidebar',
                     menuItem('Overview',tabName='overview'),
                     menuItem('Individual',tabName='individual'),
                     
                     conditionalPanel(
                       condition="input.sidebar == 'individual' ",
                       selectInput('selectip',label="Select an IP project",choices=ip)
                     ),
                     conditionalPanel(
                       condition="input.sidebar == 'overview' ",
                       selectInput('selectdir',label="Select a Directorate",choices=directorate),
                       #radioButtons('internal','Show internal or external status:',c("Yes","No"),selected="No",inline=TRUE),
                       actionButton('info','View IP Name',icon=icon('eye'))
                     ),
                     
                     br(),br(),
                     tags$b('Download:',style="margin-left:10px;"),
                     br(),
                     br(),
                     tags$style(type="text/css", "#downloadData {color: black;margin-left:10px;}"),
                     downloadButton('downloadData','Data'),
                     br(),
                     br(),
                     tags$style(type="text/css", "#downloadreport {color: black;margin-left:10px;}"),
                     downloadButton('downloadreport','Report'),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     actionButton('contact','Contact us',icon=icon('phone')))
  ),
  dashboardBody(
    
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: Arial, Helvetica, sans-serif;
        font-weight: bold;
        font-size: 20px;
      }

     .skin-blue .main-header .logo:hover{
        background-color: #333000;
     }

     .skin-blue .main-header .logo{
        background-color: #333000;
     }

     .skin-blue .main-header .navbar{
       background-color: #333000;
     }

     .skin-blue .main-sidebar {
       background-color: #333000;
     }

     .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
       background-color: #0278A4;

    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
       background-color: #0278A4;

    '))),
    
    tabItems(
      
      tabItem(tabName='overview',
               fluidRow(
                       div(style='display: inline-block;vertical-align:center;width:100px;', uiOutput('project_name2')),
                       div(style="display: inline-block;vertical-align:center; width: 300px;",HTML("<br>")),
                       bsModal('modal','IP Name','info',tableOutput('ip_tbl'))
                       
               ),
              uiOutput('ui_output3'),
              uiOutput('ui_output1'),
              uiOutput('ui_output2')
              
              # fluidRow(
              #   box(title='Project Risks',
              #       plotOutput('projrisk')),
              #   box(title='Project Issues',
              #       plotOutput('projissue'))
              # )
              ),
      
    
      tabItem(tabName='individual',
               
       fluidRow(width=12,
          uiOutput('project_name'),
          tags$style(".small-box.bg-red {background-color: #C00000 !important;}"),
          valueBoxOutput('overall'),
          valueBoxOutput('overall_stage'),
          valueBoxOutput('internal_external')
          
     ),
     
     fluidRow(
       column(12,
              box(title='Schedule',width=NULL,
                  withSpinner(plotOutput('schedule_plt')),
                  br(),
                  br(),
                  DT::dataTableOutput('schedule_tb')))
     ),
     
     fluidRow(
  
      box(title='Project Functionality',height='500px',
          tabsetPanel(id='tabs',
                      
              tabPanel(title='Table',
                       DT::dataTableOutput("function_tb"))
          )),
      
      box(title='Project Budget',height='500px',
          tabsetPanel(
            tabPanel(title='Breakdown by Year',
                     withSpinner(plotlyOutput('budget_plt'))),
            tabPanel(title='Table',
                     DT::dataTableOutput('budget_tbl')),
            tabPanel(title='Projections',
                     withSpinner(plotOutput('budget_all'))))
            
       )),
    
  
    fluidRow(
      column(12,
             box(title='Project Risks',width=NULL,
                 DT::dataTableOutput('proj_risk_tb')))
    ),
    
    fluidRow(
      column(12,
             box(title='Project Issues',width=NULL,
                 DT::dataTableOutput('proj_issue_tb')))
    )
    
     
      )
    )
            
  )             
)