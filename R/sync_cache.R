syncGdCache <- function(local_folder, cloud_folder){

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
  cloud_gdrive_sig <- purrr::possibly(getGdriveFolder, NULL)(cloud_folder)
  # Если в облаке ничего нет или связи с ним нет, то и делать дальше нечего
  if(is.null(cloud_gdrive_sig)) {
    message("Some Google Drive issues happened. Can't update cache")
    return()
  }

  # 3. Загружаем список локальных файлов на файловой системе
  local_files <- fs::dir_ls(fdir, recurse = FALSE) %>%
    fs::path_file()
  invisible(futile.logger::flog.info(paste("Local cache folder is", fdir)))

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

  # 6. Выгружаем файлы на диск
  syncFile <- function(fpath, id){
    browser()
    res <- purrr::possibly(googledrive::drive_download, otherwise = NULL)(
      googledrive::as_id(id), path = fpath, overwrite = TRUE, verbose = TRUE)

    if(is.null(res)) invisible(futile.logger::flog.info(paste("Error syncing file", fpath)))

    !is.null(res)
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
}

syncGdCache(here::here("data/"), cloud_folder = "10DzkyU5ddjCLn2FM2toiF5eodgu5V2JC")
# https://drive.google.com/drive/folders/1HZvneoJBz9OWKowh3gyKptZycYe3JIbi
