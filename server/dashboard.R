##### show user hash and permission info #####
output$user_table <- renderTable({
  # use req to only render results when credentials()$user_auth is TRUE
  req(credentials()$user_auth)
  user_data()[,c(1,2,4,5)]
})
