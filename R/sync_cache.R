#' Title
#'
#' @param local_folder The full path to local cloud cache
#' @param gdrive_folder The google drive folder id like "https://drive.google.com/drive/u/3/folders/<here is folder id>"
#' @return
#' @export
#'
#' @examples
#' syncGdCache(here::here("cloud_data"), gdrive_folder = "1pMs9-auXmGsw9OGZD-9KiCCEaPryZ52s")
syncGdCache <- function(local_folder, gdrive_folder){

  # 0. Выцепляем директорию и имя файла из пути
  # при этом важно проверить сначала существование директории, иначе is_dir даст FALSE
  if(! fs::dir_exists(local_folder)) fs::dir_create(local_folder)
  fdir <- fs::path(local_folder)

  # функция утилитарная, завязана на сложный сетевой и файловый обмен, включаем логгер
  invisible(futile.logger::flog.appender(futile.logger::appender.tee(fs::path(fdir, "sync_gdcache.log"))))
  invisible(futile.logger::flog.threshold(futile.logger::INFO))
  invisible(futile.logger::flog.info("Start cloud sync"))

  # обновляем весь кэш одним махом
  cache_fname <- "gdrive_sig.Rds"

  # 1. Делаем memoise на загрузку данных из гугла
  getGdriveFolder <- memoise::memoise(function(gdrive_folder){
    googledrive::drive_ls(googledrive::as_id(gdrive_folder), recursive = FALSE)
  })

  # 2. Проверяем наличие и загружаем облачные идентификаторы указанной папки
  cloud_gdrive_sig <- purrr::possibly(getGdriveFolder, NULL)(gdrive_folder)
  # Если в облаке ничего нет или связи с ним нет, то и делать дальше нечего
  if(is.null(cloud_gdrive_sig)) {
    message("Some Google Drive issues happened. Can't update cache")
    return()
  }

  # 3. Загружаем список локальных файлов на файловой системе
  local_files <- fs::dir_ls(fdir, recurse = FALSE) %>%
    fs::path_file()
  invisible(futile.logger::flog.info(paste("Local cache folder is", fdir)))

  # browser()

  # 4. Проверяем наличие и загружаем локальный кэш облачных идентификаторов
  local_gdrive_sig <- purrr::possibly(readRDS, NULL, quiet = TRUE)(fs::path(fdir, cache_fname))
  if(is.null(local_gdrive_sig)){
    # Если локального кэша нет, то сверять нечего, просто загружаем все из облака
    # сохраняем структуру, удаляем все данные
    local_gdrive_sig <- cloud_gdrive_sig %>%
      dplyr::filter(dplyr::row_number() == -1)
  }
  # актуализируем сигнатуры локальных файлов с учетом реально существующих файлов
  local_gdrive_sig <- local_gdrive_sig %>%
    dplyr::filter(name %in% local_files)

  # 5. Сверяем идентификаторы и времена последнего изменения, оставляем файл на обновление, если есть отличия
  # Облако первично, по нему формируем список файлов для загрузки
  reconcile_tbl <- cloud_gdrive_sig %>%
    dplyr::rename(drive_resource_cloud = drive_resource) %>%
    dplyr::left_join(local_gdrive_sig, by = c("name", "id")) %>%
    tidyr::hoist(drive_resource_cloud, cloud_modified_time = "modifiedTime") %>%
    tidyr::hoist(drive_resource, local_modified_time = "modifiedTime") %>%
    # TODO: надо сверять время, а тут времена изменения выступают как строки
    # для отсутствующих локальных файлов время модификации = NA
    dplyr::mutate(not_in_sync = is.na(local_modified_time) | cloud_modified_time != local_modified_time)

  invisible(futile.logger::flog.info(paste("We have to sync", nrow(reconcile_tbl), "file(s)")))

  # 6. Выгружаем файлы на диск
  syncFile <- function(fpath, id){
    res <- FALSE
    invisible(futile.logger::flog.info(paste("Start syncing file", fpath)))

    tryCatch({
      googledrive::drive_download(
        googledrive::as_id(id), path = fpath, overwrite = TRUE, verbose = TRUE)
      res <- TRUE
    },
    error = function(e) invisible(futile.logger::flog.info(paste("Error syncing file", fpath)))
    )

    res
  }

  # пакетная загрузка, для сброса сигнатур в кэш оставляем только те, что успешно загрузились
  sync_gdrive_sig <- reconcile_tbl %>%
    dplyr::filter(not_in_sync) %>%
    dplyr::mutate(fpath = fs::path(fdir, name)) %>%
    dplyr::mutate(sync_status = purrr::map2_lgl(fpath, id, syncFile)) %>%
    dplyr::select(name, id, sync_status)

  # 7. Сбрасываем в локальный кэш только файлы, находящиеся в синхронизации
  # Собираем все воедино
  cloud_gdrive_sig %>%
    # исключаем ошибочные файлы
    dplyr::anti_join(dplyr::filter(sync_gdrive_sig, sync_status == FALSE), by = c("name", "id")) %>%
    saveRDS(fs::path(fdir, cache_fname))

  invisible(futile.logger::flog.info("Cloud sync is finished"))
}

