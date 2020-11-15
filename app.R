setwd("/data/web/shiny/shiny_server/Teamwork/")

options(shiny.maxRequestSize=200*1024^2)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(dashboardthemes)
library(shinycssloaders)
library(shinybusy)
library(shinyjs)
library(shinyauthr)
library(glue)
library(DT)
library(fontawesome)
library(DBI)
library(tidyr)
library(plyr); library(dplyr) 
library(stringr)
library(data.table)
library(Hmisc)
require(DOSE)
require(clusterProfiler)
library(GOSemSim)
require(enrichplot)
library(ggplot2)
library(plotly)
library(ggsci)
library(ggpubr)
library(cowplot)
library(envalysis)
library(PFAM.db)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(GO.db)
library(KEGGREST)
library(reactome.db)
library(MeSH.db)
library(MeSH.Mmu.eg.db)
library(MeSH.Hsa.eg.db)
library(openxlsx)
library(profvis)
library(promises)
library(future)
plan(strategy = "multiprocess", workers = 10)
source("/data/web/shiny/advanced-shiny/busy-indicator/helpers.R")

user_base <- data_frame(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  password_hash = sapply(c("pass1", "pass2"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("User One (admin)", "User Two (standard)"),
  team=c("SKLRM;ShaLab;ES诱导分化","SKLRM;ShaLab;GuoLab")
)


##### theme #####
theme_shz_website <- function(){shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(40,53,59)"
  # ,infoFontColor = "rgb(40,53,59)"
  # ,successFontColor = "rgb(40,53,59)"
  # ,warningFontColor = "rgb(40,53,59)"
  # ,dangerFontColor = "rgb(40,53,59)"
  ,bodyBackColor = "rgb(255,255,254)"
  
  ### header
  ,logoBackColor = "rgb(45,59,66)"
  
  ,headerButtonBackColor = "rgb(45,59,66)"
  ,headerButtonIconColor = "rgb(255,255,255)"
  ,headerButtonBackColorHover = "rgb(45,59,66)"
  ,headerButtonIconColorHover = "rgb(47,117,153)"
  
  ,headerBackColor = "rgb(45,59,66)"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(47,117,153)"
  ,sidebarPadding = 5
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 5
  ,sidebarMenuBorderRadius = 5
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(47,117,153)"
  ,sidebarSearchBorderColor = "rgb(255,255,255)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(47,117,153)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = "rgb(45,59,66)"
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "5px"
  
  ,sidebarTabBackColorHover = "rgb(40,53,59)"
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 5
  ,sidebarTabRadiusHover = "5px"
  
  ### boxes
  ,boxBackColor = "rgb(248,248,248)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "2px 2px 2px"
  ,boxShadowColor = "rgb(238,238,238)"
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(248,248,248)"
  ,boxPrimaryColor = "rgb(15,124,191)"
  ,boxInfoColor = "rgb(225,225,225)"
  ,boxSuccessColor = "rgb(59,133,95)"
  ,boxWarningColor = "rgb(178,83,149)"
  ,boxDangerColor = "rgb(47,117,153)"
  
  ,tabBoxTabColor = "rgb(248,248,248)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(47,117,153)"
  ,tabBoxBackColor = "rgb(248,248,248)"
  ,tabBoxHighlightColor = "rgb(47,117,153)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(248,248,248)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(0,0,0)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(40,53,59)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(40,53,59)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(118,118,118)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(118,118,118)"
  
  ### tables
  ,tableBackColor = "rgb(248,248,248)"
  ,tableBorderColor = "rgb(235,235,235)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)}

##### loginUI #####
loginUI <- function (id, title = "用户登陆", user_title = "用户ID", 
                     pass_title = "用户密码", login_title = "登陆", error_message = "用户名或密码错误!") 
{
  ns <- shiny::NS(id)
  shiny::div(
    id = ns("panel"),
    style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
    shiny::wellPanel(
      shiny::tags$h2(title, class = "text-center",
                     style = "padding-top: 0;"),
      shiny::textInput(ns("user_name"),
                       shiny::tagList(shiny::icon("user"), user_title)),
      shiny::passwordInput(ns("password"),
                           shiny::tagList(shiny::icon("unlock-alt"),
                                          pass_title)),
      shiny::div(
        style = "text-align: center;",
        shiny::actionButton(ns("button"), login_title,
                            class = "btn-primary")
      ),
      shinyjs::hidden(shiny::div(
        id = ns("error"),
        shiny::tags$p(error_message,
                      style = "color: red; font-weight: bold; padding-top: 5px;",
                      class = "text-center")
      ))
    )
  )
}

##### ui and server #####
ui <- dashboardPagePlus(
  enable_preloader = T,loading_duration = 1,sidebar_fullCollapse=T,
  #skin = "black",
  #### Sidebar Header ####
  dashboardHeaderPlus(
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears",
    title=shinyDashboardLogoDIY(
      boldText = "",
      mainText = "Teamwork",
      textSize = 16,
      badgeText = "BETA",
      badgeTextColor = "rgb(45,59,66)",
      badgeTextSize = 0.5,
      badgeBackColor = "white",
      badgeBorderRadius = 2
    ),
    left_menu =  tagList(uiOutput("navbar")),
    # title = tagList(
    #   span(class = "logo-lg", "shiny"), 
    #   img(src = "https://image.flaticon.com/icons/svg/204/204074.svg")),
    
    #### Sidebar Header dropdownMenu ####
    tags$li(class = "dropdown", style = "padding: 8px;",
            shinyauthr::logoutUI("logout",label = "注销",style = "")),
    dropdownMenuOutput(outputId = "dropdown_ui")
  
   ),
  #### Sidebar content ####
  dashboardSidebar(
    sidebarMenuOutput("siderbar_ui")
   ),
  #### Body content ####
  dashboardBody(
    #### dashboard themes and header font ####
    theme_shz_website(),
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 18px;
                              }
                              '))),
    ##### ui before login #####
    shinyjs::useShinyjs(),
    tags$head(tags$style(".table{margin: 0 auto;}"),
              tags$script(src="./iframeResizer.contentWindow.min.js",
                          type="text/javascript")
              
    ),
    loginUI("login"),
    uiOutput("note_ui"),

    ##### ui after login #####
    uiOutput("tabitems")

    )
    )

server <- function(input, output,session) {
  
  ##### shinyauthr #####
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password_hash,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  user_data <- reactive({credentials()$info})
  
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  output$note_ui <- renderUI({
    # only show pre-login
    if(credentials()$user_auth) return(NULL)
    tagList(
      tags$p("登陆账号请向管理员申请", class = "text-center")
    )
  })
  
  # output$navbar <- renderUI({
  #   req(credentials()$user_auth)
  #   navbarPage("App Title",
  #              tabPanel("Plot"),
  #              tabPanel("Summary"),
  #              tabPanel("Table")
  #   )
    
    # tagList(
    #   menuSubItem(
    #     "存货清单"
    #   # tabName =  "inventory",
    #   ),
    #   menuSubItem(
    #     "购入申请"
    #     # tabName =  "requests",
    #   ))
  # })
 
  output$dropdown_ui <- renderMenu({
    req(credentials()$user_auth)
    dropdownMenu(type = "messages",headerText="Redirect to the other page",icon = icon("directions"),badgeStatus=NULL,
                 messageItem(
                   from = "Elab",
                   message = "http://202.195.183.4/elab/Login.aspx",
                   href = "http://202.195.183.4/elab/Login.aspx",
                   icon = icon("map-marker-alt")
                 ),
                 messageItem(
                   from = "Bio-workstation",
                   message = "http://172.16.146.77:3838/Bio-workstation",
                   href = "http://172.16.146.77:3838/Bio-workstation",
                   icon = icon("map-marker-alt")
                 )
                 )
  })
  
  output$siderbar_ui <- renderMenu({
    req(credentials()$user_auth)
    useShinyjs()
    sidebarMenu(
      HTML(paste0('<div class="user-panel">
    <div class="pull-left image">
        <img src="icons8-customer-64.png" class="img-circle" alt="User Image">
            </div>
            <div class="pull-left info">
                <p style="font-size:12pt;text-align:center">',user_data()$user,'</p>
                <a href="#" style="text-dark"><i class="fa fa-circle" style="color:grey"></i>testing</a>
            </div>
    </div>')),
      div(style="color:whitel;text-align:center",
          strong("",style="color:whitel;text-align:center;font-family:'arial';font-size:12pt")),
      menuItem("首页", tabName = "dashboard", icon = icon("location-arrow")),
      menuItem("库存管理", tabName = "inventory", icon = icon("dolly"),startExpanded=F,
               menuSubItem("采购申请", tabName = "requests", icon = icon("sync-alt")),
               menuSubItem("试剂耗材", tabName = "reagent", icon = icon("box-open")),
               menuSubItem("仪器", tabName = "instrument", icon = icon("gears")),
               menuSubItem("领用记录", tabName = "instrument", icon = icon("pen-square"))
      )
    )
  })
  
  
  ##### load ui function #####
  output$tabitems <- renderUI({
    req(credentials()$user_auth)
    tabItems(

      source(file.path("ui","dashboard.R"),  local = TRUE)$value,
      source(file.path("ui","requests.R"),  local = TRUE)$value,
      source(file.path("ui","reagent.R"),  local = TRUE)$value
  
    )
  })

 
  ##### load server function #####
  source(file.path("server", "dashboard.R"),  local = TRUE)$value
  source(file.path("server", "requests.R"),  local = TRUE)$value
  source(file.path("server", "reagent.R"),  local = TRUE)$value

  
}

shinyApp(ui, server)
# profvis({shinyApp(ui, server)})





