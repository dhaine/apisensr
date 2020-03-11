# Show a shinyalert box
# Set label of the confirmation button to `confirm_btn_label`.
pop_up <- function(title, html_file, confirm_btn_label = 'OK', className = '') {
    shinyalert(
        title = title,
        text = readChar(html_file, file.info(html_file)$size),
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = confirm_btn_label,
        timer = 0,
        imageUrl = "",
        confirmButtonCol = "#FF6B17", 
        animation = FALSE,
        className = className
    )
}
