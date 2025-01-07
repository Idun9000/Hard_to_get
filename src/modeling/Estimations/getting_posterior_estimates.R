# Convert the list `posterior_samples` into a dataframe
df <- do.call(rbind, lapply(seq_along(posterior_samples), function(i) {
  # Extract the 7 lists for the current subject
  subject_data <- posterior_samples[[i]]
  
  # Combine into a dataframe and add the subject ID
  subject_df <- as.data.frame(subject_data)
  subject_df$Subject <- i
  
  return(subject_df)
}))

# Reorder columns to put the Subject column first (optional)
df <- df[, c("Subject", names(posterior_samples[[1]]))]

write.csv(df, "posterior_samples_Wetzels_heroin_low.csv")