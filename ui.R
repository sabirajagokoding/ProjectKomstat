library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinymaterial)
library(leaflet)


ui <- dashboardPage(
  dashboardHeader(title = tagList(
    icon = icon("square-poll-vertical"), "Dashboard Kelompok 4"
  )),
  
  dashboardSidebar(
    width = 250,
    div(style = "padding: 10px; text-align: center; font-weight: bold; font-size: 16px;", "MENU"),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem(
        "Grafik",
        tabName = "grafik",
        icon = icon("chart-bar"),
        startExpanded = FALSE,
        menuSubItem(
          "Luas Kebakaran Hutan",
          tabName = "kebakaran",
          icon = icon("fire")
        ),
        menuSubItem("Emisi Karbon", tabName = "karbon", icon = icon("cloud")),
        menuSubItem(
          "Suhu Udara",
          tabName = "suhu",
          icon = icon("thermometer")
        )
      ),
      menuItem(
        "Ranking Nasional",
        tabName = "ranking",
        icon = icon("trophy")
      ),
      menuItem(
        "Peta Choropleth",
        tabName = "choropleth",
        icon = icon("globe")
      ),
      menuItem(
        "Analisis Statistik",
        tabName = "statistik",
        icon = icon("calculator")
      ),
      menuItem("About Us", tabName = "aboutus", icon = icon("users"))
    )
  ),
  
  dashboardBody(
    # Custom CSS untuk styling
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Allerta+Stencil&family=Montserrat:ital,wght@0,100..900;1,100..900&display=swap"),
      tags$style(
        HTML(
          "
    .main-header {
      background-color: #109f9e !important;
      border-bottom: none !important;
      box-shadow: none !important;
    }

    .main-sidebar {
      background-color: #505b6b !important;
      height: 100%;
    }

    .sidebar-menu > li.header {
      background-color: #ff0099;
      color: #ff0099;
    }

    .sidebar-menu > li > a:hover {
      background-color: rgba(76, 175, 80, 0.4); !important;
      color: white !important;
      border-left-color: #195e5e !important;
    }

    .sidebar-menu > li.active > a {
      background-color: #109f9e !important;
      border-left-color: #195e5e !important;
    }

    .box {
      box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
      background-color: white;
    }

    .content-header > h1 {
      font-size: 24px;
      font-weight: bold;
    }

    .main-header .navbar-brand {
      font-size: 16px !important;
      font-weight: bold;
    }

    .main-header .logo {
      font-size: 14px !important;
      background-color: #109f9e !important;
      font-family: 'Allerta Stencil', Montserrat;
    }

    .main-header .navbar {
      font-size: 16px !important;
      background-color: #109f9e!important;
    }

    .box .box-header {
      background-color: #3ad6d5 !important;
      color: white !important;
    }
    body {
      background-image: url('www/cover15.webp');
      background-size: cover;
      background-position: center center;
    }


    .containerform1{
        background-color: #505b6b;
        border-radius: 4vw;
        box-shadow: 0 10vw 15vw rgba(0, 0, 0, 0.35);
        position:relative;
        width: 40vw;
        max-width: 100vw;
        min-height: 50vh;
        display: flex;
        justify-content: center;
        align-items: center;
        margin-left:28vw;
        margin-top:3vw;
    }

    .containerform1 p {
        font-size: 1vw;
        line-height: 1vw;
        letter-spacing: 0.1vw;
        margin:1vw 0;
    }


    .containerform1 button {
        background-color:#fff;
        color:black;
        font-size: 1vw;
        padding: 0.8vw 2.5vw;
        border: 0.2vw solid transparent;
        border-radius: 1vw;
        font-weight: 600;
        letter-spacing: 0.1vw;
        margin-top: 0.7vw;
        cursor:pointer;
    }

    .containerform1 button.hidden {
        background-color: transparent;
        border-color: background-color:#109f9e;;
    }

    .containerform1 form1{
        background-color: #109f9e;
        display:flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        padding:0 2vw;
        height:100%;
        border-radius: 4vw;
    }

    .containerform1 input1 {
        background-color:#109f9e;
        border: none;
        margin: 0.5vw 0;
        padding: 0.8vw 0.4vw;
        border-radius: 0.6vw;
        width: 25vw;
        outline: none;
        font-size: 1vw;
    }

    .form1-container1 {
        position: absolute;
        top:0;
        height:100%;
        transition: all 0.6s ease-in-out;
        text-align:center;
    }

    .sign-in1 {
        left: 0;
        width: 50%;
        z-index: 2;
    }

    .containerform1.active1 .sign-in1 {
        transform: translateX(100%);
    }

    .sign-up1 {
        left:0;
        width:50%;
        opacity:0;
        z-index:1;
    }

    .containerform1.active1 .sign-up1 {
        transform: translateX(100%);
        opacity: 1;
        z-index: 5;
        animation: move 0.5s ease-in-out;
    }

    @keyframes move{
        0%, 44.99%{
            opacity:0;
            z-index:1;
        }
        50%, 100%{
            opacity: 1;
            z-index: 5;
        }
    }

    .toggle1-container1{
        position: absolute;
        top:0;
        left: 50%;
        width: 50%;
        height:100%;
        overflow: hidden;
        transition: all 0.6s ease-in-out;
        z-index: 1000;
        border-radius: 9vw 3.8vw 3.8vw 9vw;
    }

    .containerform1.active1 .toggle1-container1{
        transform: translateX(-100%);
        border-radius:  3.8vw 9vw 9vw 3.8vw;
    }

    .toggle1 {
        background-color: #109f9e;
        height: 100%;
        color:#fff;
        position:relative;
        left:-100%;
        height: 100%;
        width:200%;
        transform: translateX(0);
        transition: all 0.6s ease-in-out;
    }

    .containerform1.active1 .toggle1 {
        transform: translateX(50%);
    }

    .toggle1-panel1{
        background-color:#109f9e;
        position:absolute;
        width: 50%;
        height:100%;
        display:flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        padding: 0 3vw;
        text-align: center;
        top:0;
        transform: translateX(0);
        transition: all 0.6s ease-in-out;

    }

    .toggle1-left1{
        transform: translateX(-200%);
        background-color:#109f9e;
    }

    .containerform1.active1 .toggle1-left1{
        transform: translateX(0);
    }

    .toggle1-right1{
        right: 0;
        transform: translateX(0);
        background-color:#109f9e;
    }

    .containerform1.active1 .toggle1-right1{
        transform: translateX(200%);
    }

    .judullogin1 {
        font-size: 1.5vw;
    }

    .judulformlogin1{
        font-size: 2.5vw;
    }
    
    .nama1{
        font-size:5vw;
        font-family:Montserrat;
        font-weight:bold;
        font-style:italic;
    }
    
    .identitas1{
        margin-top:9vw;
        color:white;
        font-weight:bold;
    }  
    
    .cardIntro{
        display:flex;
        flex-direction:column;
    }
    
    
    
    .containerform2{
        background-color: #505b6b;
        border-radius: 4vw;
        box-shadow: 0 10vw 15vw rgba(0, 0, 0, 0.35);
        position:relative;
        width: 40vw;
        max-width: 100vw;
        min-height: 50vh;
        display: flex;
        justify-content: center;
        align-items: center;
        margin-left:28vw;
        margin-top:3vw;
    }

    .containerform2 p {
        font-size: 1vw;
        line-height: 1vw;
        letter-spacing: 0.1vw;
        margin:1vw 0;
    }


    .containerform2 button {
        background-color:#fff;
        color:black;
        font-size: 1vw;
        padding: 0.8vw 2.5vw;
        border: 0.2vw solid transparent;
        border-radius: 1vw;
        font-weight: 600;
        letter-spacing: 0.1vw;
        margin-top: 0.7vw;
        cursor:pointer;
    }

    .containerform2 button.hidden {
        background-color: transparent;
        border-color: background-color:#109f9e;;
    }

    .containerform2 form2{
        background-color: #109f9e;
        display:flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        padding:0 2vw;
        height:100%;
        border-radius: 4vw;
    }

    .containerform2 input2 {
        background-color:#109f9e;
        border: none;
        margin: 0.5vw 0;
        padding: 0.8vw 0.4vw;
        border-radius: 0.6vw;
        width: 25vw;
        outline: none;
        font-size: 1vw;
    }

    .form2-container2 {
        position: absolute;
        top:0;
        height:100%;
        transition: all 0.6s ease-in-out;
        text-align:center;
    }

    .sign-in2 {
        left: 0;
        width: 50%;
        z-index: 2;
    }

    .containerform2.active2 .sign-in2 {
        transform: translateX(100%);
    }

    .sign-up2 {
        left:0;
        width:50%;
        opacity:0;
        z-index:1;
    }

    .containerform2.active2 .sign-up2 {
        transform: translateX(100%);
        opacity: 1;
        z-index: 5;
        animation: move 0.6s ease-in-out;
    }

    @keyframes move{
        0%, 44.99%{
            opacity:0;
            z-index:1;
        }
        50%, 100%{
            opacity: 1;
            z-index: 5;
        }
    }

    .toggle2-container2{
        position: absolute;
        top:0;
        left: 50%;
        width: 50%;
        height:100%;
        overflow: hidden;
        transition: all 0.6s ease-in-out;
        z-index: 1000;
        border-radius: 9vw 3.8vw 3.8vw 9vw;
    }

    .containerform2.active2 .toggle2-container2{
        transform: translateX(-100%);
        border-radius:  3.8vw 9vw 9vw 3.8vw;
    }

    .toggle2 {
        background-color: #109f9e;
        height: 100%;
        color:#fff;
        position:relative;
        left:-100%;
        height: 100%;
        width:200%;
        transform: translateX(0);
        transition: all 0.6s ease-in-out;
    }

    .containerform2.active2 .toggle2 {
        transform: translateX(50%);
    }

    .toggle2-panel2{
        background-color:#109f9e;
        position:absolute;
        width: 50%;
        height:100%;
        display:flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        padding: 0 3vw;
        text-align: center;
        top:0;
        transform: translateX(0);
        transition: all 0.6s ease-in-out;

    }

    .toggle2-left2{
        transform: translateX(-200%);
        background-color:#109f9e;
    }

    .containerform2.active2 .toggle2-left2{
        transform: translateX(0);
    }

    .toggle2-right2{
        right: 0;
        transform: translateX(0);
        background-color:#109f9e;
    }

    .containerform2.active2 .toggle2-right2{
        transform: translateX(200%);
    }

    .judullogin2 {
        font-size: 1.5vw;
    }

    .judulformlogin2{
        font-size: 2.5vw;
    }
    
    .nama2{
        font-size:5vw;
        font-family:Montserrat;
        font-weight:bold;
        font-style:italic;
    }
    
    .identitas2{
        margin-top:9vw;
        color:white;
        font-weight:bold;
    }
    
    
    .containerform3{
        background-color: #505b6b;
        border-radius: 4vw;
        box-shadow: 0 10vw 15vw rgba(0, 0, 0, 0.35);
        position:relative;
        width: 40vw;
        max-width: 100vw;
        min-height: 50vh;
        display: flex;
        justify-content: center;
        align-items: center;
        margin-left:28vw;
        margin-top:3vw;
    }

    .containerform3 p {
        font-size: 1vw;
        line-height: 1vw;
        letter-spacing: 0.1vw;
        margin:1vw 0;
    }


    .containerform3 button {
        background-color:#fff;
        color:black;
        font-size: 1vw;
        padding: 0.8vw 2.5vw;
        border: 0.2vw solid transparent;
        border-radius: 1vw;
        font-weight: 600;
        letter-spacing: 0.1vw;
        margin-top: 0.7vw;
        cursor:pointer;
    }

    .containerform3 button.hidden {
        background-color: transparent;
        border-color: background-color:#109f9e;;
    }

    .containerform3 form3{
        background-color: #109f9e;
        display:flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        padding:0 2vw;
        height:100%;
        border-radius: 4vw;
    }

    .containerform3 input3 {
        background-color:#109f9e;
        border: none;
        margin: 0.5vw 0;
        padding: 0.8vw 0.4vw;
        border-radius: 0.6vw;
        width: 25vw;
        outline: none;
        font-size: 1vw;
    }

    .form3-container3 {
        position: absolute;
        top:0;
        height:100%;
        transition: all 0.6s ease-in-out;
        text-align:center;
    }

    .sign-in3 {
        left: 0;
        width: 50%;
        z-index: 2;
    }

    .containerform3.active3 .sign-in3 {
        transform: translateX(100%);
    }

    .sign-up3 {
        left:0;
        width:50%;
        opacity:0;
        z-index:1;
    }

    .containerform3.active3 .sign-up3 {
        transform: translateX(100%);
        opacity: 1;
        z-index: 5;
        animation: move 0.6s ease-in-out;
    }

    @keyframes move{
        0%, 44.99%{
            opacity:0;
            z-index:1;
        }
        50%, 100%{
            opacity: 1;
            z-index: 5;
        }
    }

    .toggle3-container3{
        position: absolute;
        top:0;
        left: 50%;
        width: 50%;
        height:100%;
        overflow: hidden;
        transition: all 0.6s ease-in-out;
        z-index: 1000;
        border-radius: 9vw 3.8vw 3.8vw 9vw;
    }

    .containerform3.active3 .toggle3-container3{
        transform: translateX(-100%);
        border-radius:  3.8vw 9vw 9vw 3.8vw;
    }

    .toggle3 {
        background-color: #109f9e;
        height: 100%;
        color:#fff;
        position:relative;
        left:-100%;
        height: 100%;
        width:200%;
        transform: translateX(0);
        transition: all 0.6s ease-in-out;
    }

    .containerform3.active3 .toggle3 {
        transform: translateX(50%);
    }

    .toggle3-panel3{
        background-color:#109f9e;
        position:absolute;
        width: 50%;
        height:100%;
        display:flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        padding: 0 3vw;
        text-align: center;
        top:0;
        transform: translateX(0);
        transition: all 0.6s ease-in-out;

    }

    .toggle3-left3{
        transform: translateX(-200%);
        background-color:#109f9e;
    }

    .containerform3.active3 .toggle3-left3{
        transform: translateX(0);
    }

    .toggle3-right3{
        right: 0;
        transform: translateX(0);
        background-color:#109f9e;
    }

    .containerform3.active3 .toggle3-right3{
        transform: translateX(200%);
    }

    .judullogin3 {
        font-size: 1.5vw;
    }

    .judulformlogin3{
        font-size: 2.5vw;
    }
    
    .nama3{
        font-size:5vw;
        font-family:Montserrat;
        font-weight:bold;
        font-style:italic;
    }
    
    .identitas3{
        margin-top:9vw;
        color:white;
        font-weight:bold;
    }   

  "
        )
      )
    ),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          style = "text-align: center; padding-top: 100px; color: #fff;
                           background-image: url('hutan.jpg');
                           background-size: cover;  /* Gambar memenuhi div */
                           background-position: center center;
                           background-repeat: no-repeat; /* Menghindari pengulangan gambar */
                           height: 100vh;
                           width : 100vw;
                           position: relative;
                           margin-top:-1.3vw;
                           padding-top:0",
          div(
            style = "text-align: center; padding-top: 100px; color: white;",
            h3("Area untuk menampilkan informasi Dashboard"),
            p("Informasi mengenai Dashboard akan ditampilkan di sini")
          )
        )
        
      ),
      
      tabItem(tabName = "kebakaran", fluidRow(
        tabsetPanel(id = "grafikkebakaran", tabPanel(
          "Bar Chart",
          box(
            column(
              width = 12,
              tags$div(
                style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                selectInput(
                  "dropdownKebakaranBar",
                  label = NULL,
                  choices = c(2015:2024),
                  selected = 2024,
                  width = "200px"
                )
              )
            ),
            title = "Bar Chart",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "700px",
            div(
              style = "text-align: center; padding-top: 20px;",
              plotOutput("barChartKebakaran", height = "400px")
            )
          )
        )
        , tabPanel(
          "Line Chart",
          box(
            column(
              width = 12,
              tags$div(
                style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                selectInput(
                  "dropdownKebakaranLine",
                  label = NULL,
                  choices = c(
                    "Indonesia",
                    "Aceh",
                    "Bali",
                    "Bangka Belitung",
                    "Banten",
                    "Bengkulu",
                    "DI Yogyakarta",
                    "DKI Jakarta",
                    "Gorontalo",
                    "Jambi",
                    "Jawa Barat",
                    "Jawa Tengah",
                    "Jawa Timur",
                    "Kalimantan Barat",
                    "Kalimantan Selatan",
                    "Kalimantan Tengah",
                    "Kalimantan Timur",
                    "Kalimantan Utara",
                    "Kepulauan Riau",
                    "Lampung",
                    "Maluku",
                    "Maluku Utara",
                    "Nusa Tenggara Barat",
                    "Nusa Tenggara Timur",
                    "Papua",
                    "Papua Barat",
                    "Riau",
                    "Sulawesi Barat",
                    "Sulawesi Selatan",
                    "Sulawesi Tengah",
                    "Sulawesi Tenggara",
                    "Sulawesi Utara",
                    "Sumatera Barat",
                    "Sumatera Selatan",
                    "Sumatera Utara",
                    "Banten",
                    "DKI Jakarta"
                  ),
                  selected = "Indonesia",
                  width = "200px"
                )
              )
            ),
            title = "Line Chart",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "700px",
            div(
              style = "text-align: center; padding-top: 100px; color: #666;",
              plotOutput("lineChartKebakaran", height = "400px")
            )
          )
        ))
      )),
      tabItem(tabName = "karbon", fluidRow(
        tabsetPanel(id = "grafikkarbon", tabPanel(
          "Bar Chart",
          box(
            column(
              width = 12,
              tags$div(
                style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                selectInput(
                  "dropdownKarbonBar",
                  label = NULL,
                  choices = c(2015:2024),
                  selected = 2024,
                  width = "200px"
                )
              )
            ),
            title = "Bar Chart",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "700px",
            div(
              style = "text-align: center; padding-top: 100px; color: #666;",
              plotOutput("barChartKarbon", height = "400px")
            )
          )
        ), tabPanel(
          "Line Chart",
          box(
            column(
              width = 12,
              tags$div(
                style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                selectInput(
                  "dropdownKarbonLine",
                  label = NULL,
                  choices = c(
                    "Indonesia",
                    "Aceh",
                    "Bali",
                    "Bangka Belitung",
                    "Banten",
                    "Bengkulu",
                    "DI Yogyakarta",
                    "DKI Jakarta",
                    "Gorontalo",
                    "Jambi",
                    "Jawa Barat",
                    "Jawa Tengah",
                    "Jawa Timur",
                    "Kalimantan Barat",
                    "Kalimantan Selatan",
                    "Kalimantan Tengah",
                    "Kalimantan Timur",
                    "Kalimantan Utara",
                    "Kepulauan Riau",
                    "Lampung",
                    "Maluku",
                    "Maluku Utara",
                    "Nusa Tenggara Barat",
                    "Nusa Tenggara Timur",
                    "Papua",
                    "Papua Barat",
                    "Riau",
                    "Sulawesi Barat",
                    "Sulawesi Selatan",
                    "Sulawesi Tengah",
                    "Sulawesi Tenggara",
                    "Sulawesi Utara",
                    "Sumatera Barat",
                    "Sumatera Selatan",
                    "Sumatera Utara",
                    "Banten",
                    "DKI Jakarta"
                  ),
                  selected = "Indonesia",
                  width = "200px"
                )
              )
            ),
            title = "Line Chart",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "700px",
            div(
              style = "text-align: center; padding-top: 100px; color: #666;",
              plotOutput("lineChartKarbon", height = "400px")
            )
          )
        ))
      )),
      tabItem(tabName = "suhu", fluidRow(
        tabsetPanel(id = "grafiksuhu", tabPanel(
          "Line Chart",
          box(
            column(
              width = 12,
              tags$div(
                style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
              )
            ),
            title = "Line Chart",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "700px",
            div(
              style = "text-align: center; padding-top: 100px; color: #666;",
              plotOutput("lineChartSuhu", height = "400px")
            )
          )
        ))
      )),
      tabItem(tabName = "ranking", fluidRow(
        tabsetPanel(
          id = "grafikrank",
          tabPanel(
            "Luas Kebakaran Hutan",
            box(
              column(
                width = 12,
                tags$div(
                  style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                  selectInput(
                    "dropdownKebakaranRank",
                    label = NULL,
                    choices = c(2015:2024),
                    selected = 2024,
                    width = "200px"
                  )
                )
              ),
              title = "Luas Kebakaran Hutan",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "700px",
              div(
                style = "text-align: center; padding-top: 100px; color: #666;",
                plotOutput("rankKebakaran", height = "400px")
              )
            )
          ),
          tabPanel(
            "Emisi Karbon",
            box(
              column(
                width = 12,
                tags$div(
                  style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                  selectInput(
                    "dropdownKarbonRank",
                    label = NULL,
                    choices = c(2015:2024),
                    selected = 2024,
                    width = "200px"
                  )
                )
              ),
              title = "Emisi Karbon",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "700px",
              div(
                style = "text-align: center; padding-top: 100px; color: #666;",
                plotOutput("rankKarbon", height = "400px")
              )
            )
          ),
          tabPanel(
            "Suhu",
            box(
              column(
                width = 12,
                tags$div(
                  style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                  selectInput(
                    "dropdownSuhuRank",
                    label = NULL,
                    choices = c(2015:2024),
                    selected = 2024,
                    width = "200px"
                  )
                )
              ),
              title = "Suhu",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "700px",
              div(
                style = "text-align: center; padding-top: 100px; color: #666;",
                plotOutput("rankSuhu", height = "400px")
              )
            )
          )
        )
      )),
      tabItem(tabName = "choropleth", fluidRow(
        tabsetPanel(
          id = "grafikchoropleth",
          tabPanel(
            "Luas Kebakaran Hutan",
            box(
              column(
                width = 12,
                tags$div(
                  style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                  selectInput(
                    "dropdownKebakaranMap",
                    label = NULL,
                    choices = c(2015:2024),
                    selected = 2024,
                    width = "200px"
                  )
                )
              ),
              title = "Luas Kebakaran Hutan",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "700px",
              div(
                style = "text-align: center; padding-top: 100px; color: #666;",
                leafletOutput("mapKebakaran", height = "400px")
              )
            )
          ),
          tabPanel(
            "Emisi Karbon",
            box(
              column(
                width = 12,
                tags$div(
                  style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                  selectInput(
                    "dropdownEmisiMap",
                    label = NULL,
                    choices = c(2015:2024),
                    selected = 2024,
                    width = "200px"
                  )
                )
              ),
              title = "Emisi Karbon",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "700px",
              div(
                style = "text-align: center; padding-top: 100px; color: #666;",
                leafletOutput("mapKarbon", height = "400px")
              )
            )
          )
        )
      )),
      tabItem(tabName = "statistik", fluidRow(
        tabsetPanel(
          id = "analisisstatitsik",
          tabPanel(
            "Uji Normalitas",
            box(
              title = "Uji Normalitas",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "700px",
              div(
                style = "text-align: center; padding-top: 100px; color: #666;",
                h3("Area untuk Uji Normalitas"),
                p("Konten analisis Uji Normalitas akan ditampilkan di sini")
              )
            )
          ),
          tabPanel(
            "Uji Multikolinearitas",
            box(
              title = "Uji Multikolinearitas",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "700px",
              div(
                style = "text-align: center; padding-top: 100px; color: #666;",
                h3("Area untuk Uji Multikolinearitas"),
                p("Konten Uji Multikolinearitas akan ditampilkan di sini")
              )
            )
          ),
          tabPanel(
            "Uji Homoskedastisitas",
            box(
              title = "Uji Homoskedastisitas",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "700px",
              div(
                style = "text-align: center; padding-top: 100px; color: #666;",
                h3("Area untuk Uji Homoskedastisitas"),
                p("Konten Uji Homoskedastisitas akan ditampilkan di sini")
              )
            )
          ),
          tabPanel(
            "Uji Non-Autokorelasi",
            box(
              title = "Uji Non-Autokorelasi",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "700px",
              div(
                style = "text-align: center; padding-top: 100px; color: #666;",
                h3("Area untuk Uji Non-Autokorelasi"),
                p("Konten Uji Non-Autokorelasi akan ditampilkan di sini")
              )
            )
          ),
          tabPanel(
            "Regresi",
            box(
              title = "Regresi",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "700px",
              div(
                style = "text-align: center; padding-top: 100px; color: #666;",
                h3("Area untuk analisis regresi"),
                p("Konten analisis regresi akan ditampilkan di sini")
              )
            )
          )
        )
      )),
      tabItem(tabName = "aboutus", fluidRow(

          tags$script(
            HTML(
              "
            $(document).ready(function() {
              $(document).on('click', '#register1', function() {
                $('#containerform1').addClass('active1');
                Shiny.setInputValue('toggleForm1', 'signup1');
              });

              $(document).on('click', '#login1', function() {
                $('#containerform1').removeClass('active1');
                Shiny.setInputValue('toggleForm1', 'signin1');
              });
            });
            
            $(document).ready(function() {
                $(document).on('click', '#register2', function() {
                    $('#containerform2').addClass('active2');
                    Shiny.setInputValue('toggleForm2', 'signup2');
                });
            
                $(document).on('click', '#login2', function() {
                    $('#containerform2').removeClass('active2');
                    Shiny.setInputValue('toggleForm2', 'signin2');
                });
            });
            
            $(document).ready(function() {
                $(document).on('click', '#register3', function() {
                    $('#containerform3').addClass('active3');
                    Shiny.setInputValue('toggleForm3', 'signup3');
                });
            
                $(document).on('click', '#login3', function() {
                    $('#containerform3').removeClass('active3');
                    Shiny.setInputValue('toggleForm3', 'signin3');
                });
            });
          "
            )
          ),
          div(class="cardIntro",
            div(
              class = "containerform1",
              id = "containerform1",
              
              conditionalPanel(
                condition = "input1.toggleForm1 == 'signup1'",
                div(
                  class = "form1-container1 sign-up1",
                  div(class="identitas1",
                      p("Nama : Aulia Ul Hasanah"),
                      p("Nama Panggilan : Awl"),
                      p("NIM :222313000"),
                      p("Asal : Sulawesi Tenggara"),
                      br(),
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input1.toggleForm1 == 'signin1'",
                div(
                  class = "form1-container1 sign-in1",
                )
              ),
              
              div(class = "toggle1-container1", div(
                class = "toggle1",
                div(
                  class = "toggle1-panel1 toggle1-left1",
                  img(
                    src = 'cardFotoAul.png',width="180vw",height="140vw",
                    alt = "logologin2",
                    width = "50px",
                    height = "50px"
                  ),
                  actionButton("login1", "Oke udah kenal")
                ),
                div(
                  class = "toggle1-panel1 toggle1-right1",
                  p("Aulia Ul Hasanah",class="nama1"),
                  p("Belum kenal nih?"),
                  actionButton("register1", "Mau kenalan dong")
                )
              ))
            ),
          div(
            class = "containerform2",
            id = "containerform2",
            
            conditionalPanel(
              condition = "input2.toggleForm2 == 'signup2'",
              div(
                class = "form2-container2 sign-up2",
                div(class="identitas2",
                    p("Nama : Ilham Tesa Nur Mazua"),
                    p("Nama Panggilan : Tesa"),
                    p("NIM :222313133"),
                    p("Asal : Jawa Tengah"),
                    br(),
                )
              )
            ),
            
            conditionalPanel(
              condition = "input2.toggleForm2 == 'signin2'",
              div(
                class = "form2-container2 sign-in2",
              )
            ),
            
            div(class = "toggle2-container2", div(
              class = "toggle2",
              div(
                class = "toggle2-panel2 toggle2-left2",
                img(
                  src = 'cardFotoTesa.png',width="180vw",height="140vw",
                  alt = "logologin2",
                  width = "50px",
                  height = "50px"
                ),
                actionButton("login2", "Oke udah kenal")
              ),
              div(
                class = "toggle2-panel2 toggle2-right2",
                p("Ilham Tesa Nur Mazua",class="nama2"),
                p("Belum kenal nih?"),
                actionButton("register2", "Mau kenalan dong")
              )
            ))
          ),
          div(
            class = "containerform3",
            id = "containerform3",
            
            conditionalPanel(
              condition = "input3.toggleForm3 == 'signup3'",
              div(
                class = "form3-container3 sign-up3",
                div(class="identitas3",
                    p("Nama : Rahman Al Gifary"),
                    p("Nama Panggilan : Gifar"),
                    p("NIM :222313328"),
                    p("Asal : Nusa Tenggara Barat"),
                    br(),
                )
              )
            ),
            
            conditionalPanel(
              condition = "input3.toggleForm3 == 'signin3'",
              div(
                class = "form3-container3 sign-in3",
              )
            ),
            
            div(class = "toggle3-container3", div(
              class = "toggle3",
              div(
                class = "toggle3-panel3 toggle3-left3",
                img(
                  src = 'cardFotoGifa.png',width="180vw",height="140vw",
                  alt = "logologin2",
                  width = "40px",
                  height = "50px"
                ),
                actionButton("login3", "Oke udah kenal")
              ),
              div(
                class = "toggle3-panel3 toggle3-right3",
                p("Rahman Al Gifary",class="nama3"),
                p("Belum kenal nih?"),
                actionButton("register3", "Mau kenalan dong")
              )
            ))
          ))
        
        
      ))
    )
  )
)

