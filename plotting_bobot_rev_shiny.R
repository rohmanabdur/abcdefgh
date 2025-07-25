fpaket <- function(paket1, ...) {
  nama_paket <- c(paket1, ...)
  for (k in seq_along(nama_paket)) {
    if (isFALSE(suppressWarnings(require(nama_paket[k],
      character.only = TRUE
    )))) {
      install.packages(nama_paket[k],
        repos = "https://cran.usk.ac.id/"
      )
      library(nama_paket[k], character.only = TRUE)
    }
  }
}

fpaket("openxlsx", "shiny", "bslib", "reactable")
# filebobot1= "https://docs.google.com/spreadsheets/d/15srhlRfcojnlfoG0qy94kqQv5GJtMJy_/edit?usp=sharing&ouid=112676657395266139926&rtpof=true&sd=true"
#
fbaca_bobot <- function(file_plot_bobot) {
    if(is.na(file_plot_bobot)){
      showModal(modalDialog("Kolom input di sini tidak boleh kosong dan harus
                              diisi link Google Sheets berisi
                              daftar mata kuliah dan pengampunya.",
                            title = "Salah Input"
      ))
    }
    else if (grepl("http", file_plot_bobot)) {
    google_id <- gsub(".*https://docs.google.com/spreadsheets/d/(.*)/edit.*",
      "\\1",
      x = file_plot_bobot, perl = TRUE
    )
    download.file(paste0("https://drive.google.com/uc?export=download&id=", google_id),
      destfile = paste0(getwd(), "/plotting_bobot.xlsx"), mode = "wb"
    )
    nama_sheets <- getSheetNames(paste0(getwd(), "/plotting_bobot.xlsx"))
    dataplot <- lapply(nama_sheets, function(x) {
      read.xlsx(paste0(getwd(), "/plotting_bobot.xlsx"), sheet = x)
    })
    names(dataplot) <- nama_sheets
    return(dataplot)
  }  else {
    showModal(modalDialog("Kolom input di sini tidak boleh kosong dan harus
                              diisi link Google Sheets berisi
                              daftar mata kuliah dan pengampunya.",
      title = "Salah Input"
    ))
    return()
  }
}

fhitung_banjar <- function(link_sheets) {
  if(is.null(link_sheets)) return()
  else{
  dosen_bobot <- fbaca_bobot(link_sheets)
  dosen_saja <- dosen_bobot$plot_tengah[, c("kode", "nama_mata_kuliah", paste0("dosen", 1:6), "sks_total")]
  bobot_saja <- dosen_saja
  isi_bobot_jabatan <- function(vektor_nama_dosen) {
    isi_bobot_jabatan_satu <- function(nama_dosen) {
      lokasi_nama <- which(dosen_bobot$daftar_dosen$nama_teks == nama_dosen)
      bobot <- ifelse(length(lokasi_nama) < 1L, 0L,
        dosen_bobot$daftar_dosen$bobot_jabatan[lokasi_nama]
      )
      return(bobot)
    }
    vapply(vektor_nama_dosen, isi_bobot_jabatan_satu, numeric(1))
  }
  bobot_saja[, paste0("dosen", 1:6)] <- bobot_saja[, paste0("dosen", 1:6)] |>
    apply(2, isi_bobot_jabatan) |>
    as.data.frame()
  bobot_proporsi <- bobot_saja
  bobot_proporsi[, paste0("dosen", 1:6)] <- bobot_saja[, paste0("dosen", 1:6)] |>
    apply(1, function(x) {
      if (any(x > 0)) {
        proportions(x)
      } else {
        x
      }
    }) |>
    t() |>
    as.data.frame()


  bobot_sks <- bobot_proporsi
  bobot_sks[, paste0("dosen", 1:6)] <- bobot_proporsi[, paste0("dosen", 1:6)] |>
    apply(2, function(x) round(x * bobot_proporsi[, "sks_total"], 4)) |>
    as.data.frame() |>
    setNames(paste0("dosen", 1:6))


  isi_bobot_sks <- function(vektor_nama_dosen) {
    vektor_nama_dosen <- na.omit(vektor_nama_dosen)
    tabel_dosen_saja <- dosen_bobot$plot_tengah[, c("kode", "nama_mata_kuliah", paste0("dosen", 1:6))]
    tabel_bobot_saja <- as.data.frame(bobot_sks[, c("kode", "nama_mata_kuliah", paste0("dosen", 1:6))])
    list_sks_dosen <- lokasi <- vector("list", length(vektor_nama_dosen))
    names(list_sks_dosen) <- vektor_nama_dosen
    for (k in vektor_nama_dosen) {
      lokasi[[k]] <- which(tabel_dosen_saja == k, arr.ind = TRUE)
      list_sks_dosen[[k]] <- tabel_bobot_saja[lokasi[[k]][, "row"], c("kode", "nama_mata_kuliah")] |>
        cbind.data.frame(as.numeric(tabel_bobot_saja[lokasi[[k]]]))
      colnames(list_sks_dosen[[k]])[3] <- "Beban SKS"
      list_sks_dosen[[k]][nrow(list_sks_dosen[[k]]) + 1, ] <- data.frame(
        kode = "", nama_mata_kuliah = "TOTAL", "Beban SKS" = sum(list_sks_dosen[[k]][, "Beban SKS"], na.rm = TRUE)
      )
    }
    return(list_sks_dosen)
  }
  tabel_nama_dosen <- dosen_bobot$plot_tengah[, c("kode", "nama_mata_kuliah", paste0("dosen", 1:6))]
  tabel_bobot_sks_dosen <- bobot_sks[, c("kode", "nama_mata_kuliah", paste0("dosen", 1:6), "sks_total")]
  bobot_sks_dosen_mk <- isi_bobot_sks(dosen_bobot$daftar_dosen$nama_teks)

  nama_tiap_dosen <- names(bobot_sks_dosen_mk)
  bobot_tiap_dosen <- bobot_sks_dosen_mk |>
    lapply(function(x) {
      x[nrow(x), ncol(x)]
    }) |>
    unlist()
  rekap_bobot_sks <- data.frame(
    "Nama Dosen" = nama_tiap_dosen,
    "Beban SKS Total" = bobot_tiap_dosen,
    check.names = FALSE, row.names = NULL
  )


  hasil_plot <- list(
    rekap_sks_tiap_dosen = rekap_bobot_sks,
    rincian_dosen_saja = tabel_nama_dosen,
    rincian_bobot_jabatan = bobot_saja[!colnames(bobot_saja) %in% "sks_total"],
    rincian_proporsi_bobot = bobot_proporsi[!colnames(bobot_proporsi) %in% "sks_total"],
    rincian_bobot_sks = tabel_bobot_sks_dosen
  ) |> c(bobot_sks_dosen_mk)
  for (k in seq_along(names(hasil_plot))) {
    names(hasil_plot)[k] <- ifelse(nchar(names(hasil_plot)[k]) < 32,
      names(hasil_plot)[k],
      substr(names(hasil_plot)[k], 1, 31)
    )
  }
  return(hasil_plot)
  }
}

# write.xlsx(hasil_plot, "hasil_plotting_bobot.xlsx")
