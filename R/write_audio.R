#' Generate and Save audio files for Each Call in the Data Frame
#'
#'@author Ari Cross, Grace Smith-Vidaurre
#'
#' This function loops through each row in the data frame and uses the `gen_synth_signal` function
#' to generate and save WAV files for each call.
#'
#' @param df A data frame containing the frequency values and metadata.
#' @param save_path The directory where the WAV files will be saved.
#' @param sampling_rate The sampling rate for the audio file.
#' @param sylLen The length of the syllable in milliseconds.
#' @param prefix A prefix to distinguish between individual-level and group-level audio files.
#' @return The input data frame, updated to have an additional column with the audio file name
#' @examples
#' individual_freq_df <- generate_frequency(individual_df, "Parsons", "GroupID", "IndividualID", "CallID")
#' generate_and_save_wav(individual_freq_df, "path/to/save/directory", 150000, 200, "individual")
#' @export write_audio
write_audio <- function(df, save_path, sampling_rate, sylLen, prefix = "call") {
  # Ensure the save path exists
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }

  df2 <- data.table::rbindlist(lapply(1:nrow(df), function(i){
    # Extract frequencies from the data frame
    frequencies <- as.numeric(df[i, grep("^Frequency", colnames(df))])

    # Construct the audio filename with a unique identifier
    audio_filename <- paste0(prefix, "_Group", df$Group[i], "_Ind", df$Individual[i],"_Call", df$Call[i], ".wav")

    audio_pathname <- file.path(save_path, audio_filename)

    # Generate and save the WAV file
    gen_synth_signal(frequencies, audio_pathname, sampling_rate = sampling_rate, sylLen = sylLen)

    return(
      df[i, ] %>%
        dplyr::mutate(
          audio_file_name = audio_filename
        )
    )

  }))

  return(df2)

}


# A helper function to generate a synthetic audio signal in .wav format from a vector of frequencies using the `soundgen` package

# Aurguments
# frequencies - A numeric vector of frequency values.
# audio_filename - The name of the output WAV file.
# sampling_rate - The sampling rate for the audio file.
# sylLen - The length of the syllable in milliseconds.

# Example usage:
# frequencies <- c(4000, 5000, 4000, 4000, 5000, 4000)
# audio_filename <- "output.wav"
# sampling_rate <- 150000
# sylLen <- 200
# gen_synth_signal(frequencies, audio_filename, sampling_rate, sylLen)

gen_synth_signal <- function(frequencies, audio_filename, sampling_rate, sylLen) {
  # Adjust pitch floor and ceiling based on the sampling rate
  pitch_floor <- 1
  pitch_ceiling <- sampling_rate / 2

  # Generate waveform from frequency with the fixed sampling rate
  sound_wave <- suppressMessages(soundgen(
    pitch = frequencies,
    samplingRate = sampling_rate,
    sylLen = sylLen,
    pitchFloor = pitch_floor,
    pitchCeiling = pitch_ceiling,
    saveAudio = audio_filename
  ))

}


