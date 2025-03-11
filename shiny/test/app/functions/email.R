############################################################################################
#
#  Function for email
#
#############################################################################################

# Function to send email
send_email <- function(name, email, telephone, message, subject = "Wearable Shiny App message") {
  # Create email body
  body <- paste("Name: ", name,
                "\nEmail: ", email,
                "\nTelephone: ", telephone,
                "\nMessage: ", message)

  # SMTP settings (use environment variables for security)
  smtp <- emayili::server(
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

  # Send email
  smtp(msg, verbose = FALSE)
}

