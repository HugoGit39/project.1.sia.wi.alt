############################################################################################
#
#  Function for email
#
#############################################################################################

# Function to send email
send_email <- function(body, subject, receiver = "disc@stress-in-action.nl", attachment = NULL) {

  # SMTP settings (use environment variables for security)
  smtp <- server(
    host = "smtp.transip.email",
    port = 465,
    username = "disc@stress-in-action.nl",
    password = "xxxxxxxxxxx"
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

