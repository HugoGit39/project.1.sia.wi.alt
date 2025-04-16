############################################################################################
#
#  Function for email
#
#############################################################################################

# Function to send email
send_email <- function(body, subject, attachment = NULL) {

  # SMTP settings (use environment variables for security)
  smtp <- server(
    host = Sys.getenv("SMTP_HOST"),
    port = Sys.getenv("SMTP_PORT"),
    username = Sys.getenv("MAIL_NAME_ID"),
    password = Sys.getenv("MAIL_KEY_ID"),
    tls = TRUE  # Enable TLS for security
  )

  # Create email message
  msg <- envelope(
    to = "disc@stress-in-action.nl",
    from = "disc@stress-in-action.nl"
  ) %>%
    subject(subject) %>%
    text(body)

  # Add attachment only if provided
  if (!is.null(attachment) && file.exists(attachment)) {
    msg <- msg %>% attachment(attachment)
  }

  # Send email
  smtp(msg, verbose = FALSE)
}

