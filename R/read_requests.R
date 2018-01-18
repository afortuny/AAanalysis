#'
#'  read_requests.R
#'
#'
#'

STATUS_SUBMITTED 	<- 'submitted'
STATUS_VALIDATED 	<- 'validated'
STATUS_REJECTED 	<- 'rejected'
STATUS_PROCESSED 	<- 'processed'
STATUS_FAILED 		<- 'failed'

read_requests <- function(technical_settings, spark_context, report_progress, by_status) {

  ts <- technical_settings
  sc <- spark_context

  request_table <- dplyr::tbl(sc, ts$table$request)
  status_table <- dplyr::tbl(sc, ts$table$status)

  requests <-
    status_table %>%
      dplyr::filter(rlang::sym(ts$field$status) == by_status) %>%
      dplyr::left_join(request_table, by = ts$field$request) %>%
      dplyr::collect()

  for (r in seq_len(nrow(requests))) {
    req <- requests[r,c(ts$field$request, ts$field$status)]
    report_progress$info(paste(req[[ts$field$request]], req[[ts$field$status]]))
  }

  return(requests)
}

update_request_status <- function(technical_settings, spark_context, report_progress, request_id, new_status, description = ' ') {

  ts <- technical_settings
  sc <- spark_context

  sql <- sprintf(
    'insert overwrite table %s partition(%s = "%s") select "%s", "%s"',
    ts$table$status, ts$field$request, request_id, description, new_status
  )

  sparklyr::hive_context(sc) %>%
    sparklyr::invoke('sql', sql)

  report_progress$info(sprintf("Update request [%s] status to [%s]", request_id, new_status))
}
