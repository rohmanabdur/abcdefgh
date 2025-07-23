source("plotting_bobot_rev_shiny.R")


ui <- page(
  tags$head(
    tags$style(
      HTML("
        body, html {
        height: 100%;
        margin: 0;
        padding: 0;
      }
      .container-fluid {
        height: 100%;
        margin: 0;
        padding: 0;
      }
      
      body {
        background-color: #f5f5f5;
      }
      .depan {
        background-color: #a1d76a;
        width: 25vw;
        height: 20vh;
        margin: 10vh auto;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        border-radius: 20px;
        padding: 10px;
        position: relative;
      }
      .navbar {
       display: none;
      }
      .judul {
      color:#8c510a;
      text-align: center;
      }
      .tombol {
        color: white;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 1.5em;
        cursor: pointer;
        transition: background-image 0.3s ease;
      }
       ")
    )
  ),
  navbarPage(
    title = NULL,
    id = "beranda",
    collapsible = TRUE,
    header = NULL,
    footer = NULL,
    selected = "laman_kunci",
    tabPanel(
      title = NULL,
      value = "laman_kunci",
      div(
        class = "judul",
        tags$h1("Sibanjar"),
        tags$h3(tags$i("Simulator Beban Mengajar (versi 0.1)"))
      ),
      div(
        class = "depan",
        passwordInput("kunci", h4("Masukkan kata kunci:"))
      ),
      div(
        class = "tombol",
        actionButton("ok_kunci", "Masuk")
      )
    ),
    tabPanel(
      title = NULL, value = "laman_input",
      card(
        full_screen = TRUE, height = 750,
        page_fillable(
          theme = bs_theme(bootswatch = "united"),
          layout_sidebar(
            sidebar =
              card(
                card_header(h4("Input")),
                textAreaInput("sheets_mk_pengampu",
                  label = tooltip(
                    trigger = list(
                      "Link Google Sheets Daftar MK Prodi dan Pengampunya:",
                      icon("circle-exclamation")
                    ),
                    "Pastikan tautan yang disalin memuat https://docs.google.com/spreadsheets/.  
              Tidak menerima link yang dipendekkan (shortlink)",
                    placement = "auto"
                  )
                ),
                actionButton("ok_unggah", "OK", icon = icon("arrow-pointer"))
              ),
            layout_column_wrap(
              width = NULL, height = 300,
              navset_card_pill(
                nav_panel(
                  full_screen = TRUE, value ="rekap_banjar",
                  card_header("Rekap Beban Mengajar"),
                  card_body(
                    reactableOutput("tabel_banjar_rekap")
                  )
                ),
                nav_panel(
                  full_screen = TRUE, value = "banjar_tiap_dosen",
                  card_header("Beban Mengajar Tiap Dosen"),
                  selectInput("nama_dosen", label = "Daftar Nama Dosen",
                              choices = NULL, selected = NULL),
                  reactableOutput("tabel_banjar_tiap_dosen")
                ),
                nav_panel(
                  full_screen = TRUE, value = "data_lain",
                  card_header("Data Lain"),
                  selectInput("data_lain", label = "Daftar Data", 
                              choices = NULL, selected = NULL),
                  reactableOutput("tabel_lain")
                ),
                card_footer(
                  fillable = FALSE, height = 30,
                  "Untuk mengunduh rekap dalam bentuk file Excel, klik ",
                  downloadButton("unduh_rekap", "Unduh")
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  onSessionEnded(function() {
    stopApp()
  })
  kunci <- reactive(input$kunci)
  observeEvent(input$ok_kunci, {
    if (kunci() == "sipilunej") {
      updateNavbarPage(session, inputId = "beranda", selected = "laman_input")
    }
  })
 
  observeEvent(input$ok_unggah, {
    data_mk <<- reactive(fhitung_banjar(input$sheets_mk_pengampu))
    req(data_mk())
    updateSelectInput(session, inputId = "nama_dosen", label = "Daftar Nama Dosen", choices = data_mk()[["rekap_sks_tiap_dosen"]][["Nama Dosen"]])
    updateSelectInput(session, inputId = "data_lain", label = "Daftar Data", choices = names(data_mk())[1:5])
    output$tabel_banjar_rekap = renderReactable(reactable(data_mk()$rekap_sks_tiap_dosen, 
                                                striped = TRUE, searchable = TRUE))
    output$tabel_banjar_tiap_dosen = renderReactable(reactable(data_mk()[[input$nama_dosen]], 
                                                     striped = TRUE, searchable = TRUE))
    output$tabel_lain = renderReactable(reactable(data_mk()[[input$data_lain]], 
                                        striped = TRUE, searchable = TRUE))
  })
  
}

shinyApp(ui = ui, server = server, options = list(
  launch.browser = TRUE,
  display.mode = "normal"
))