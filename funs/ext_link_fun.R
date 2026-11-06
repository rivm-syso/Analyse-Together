# Helper to build links to external websites
ext_link <- function(text, href) {
  tags$a(
    text,
    tags$span(class = "ext"),
    tags$span(class = "sr-only", paste0(" ", i18n$t("txt_external_link"))),
    href = href,
    target = "_blank",
    rel = "noopener noreferrer"
  )
}
