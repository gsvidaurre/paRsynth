#' Save synthetic audio for each string in a data frame
#'
#' @author Ari Cross, Grace Smith-Vidaurre
#'
#' @description The `write_audio` function loops through each row in the data frame and uses the `gen_synth_signal` function (a wrapper for the `soundgen::soundgen()`) to generate and save audio files in .WAV format for each string.
#'
#' @param df Data frame. A data frame object generated by `frequency_anchors()` that contains the frequency anchor values and string metadata.
#' @param sylLen Numeric value. The length of the vocalization in milliseconds. The default is 200 ms. This is an argument used directly by `soundgen::soundgen()`. The vocalization duration can influence visualizations of the resulting synthetic vocalizations in ways that depend on the length of the character string (see details below).
#' @param sampling_rate Numeric value. The sampling rate for the audio file, in Hz. The default is 44100 Hz or 44.1 kHz. This is an argument used directly by `soundgen::soundgen()`.
#' @param pitch_sampling_rate Numeric value. The sampling rate for the pitch contour of the audio file, in Hz. The default is 44100 Hz or 44.1 kHz. This is an argument used directly by `soundgen::soundgen()`, and it is recommended to set this argument to be the same as the audio sampling rate for quantitative analyses.
#' @param smoothing A list with the named elements `interpol`, `loessSpan`, `discontThres`, and `jumpThres` to control how smoothing of frequency contours is performed by `soundgen` when audio files are generated using frequency anchors. The default list provided to this argument is `list(interpol = "loess", loessSpan = 1, discontThres = 0, jumpThres = 0)` to perform strong Loess smoothing (local polynomial regression).
#' @param rolloffExact A numeric object encoding static amplitude values across the fundamental and harmonics (a vector) or encoding dynamic changes in amplitude across the fundamental and harmonics (a matrix). Numeric values representing amplitudes in this object should be scaled from 0 to 1. The default list provided to this argument is `c(0.25, 0.25, 0.25, 0.25, 0.25)`, which assigns amplitude values of 0.25 to the fundamental and each of 4 harmomics (or overtones). Since there is one numeric value assigned to the fundamental and each harmonic, the amplitude values will not change over time. To encode dynamic changes in amplitude across the fundamental and harmonics, create a matrix of amplitude values in which each row corresponds to a timepoint and each column corresponds to the fundamental or a harmonic, such as: "matrix(c(0.5, 0.2, 1, 0.02, 0.22,  0.1, 0.4, .01, 0.05, 0.2), ncol = 2)" in which the first 5 values (row 1) are the strength of F0 to H4 at time 0 and the second 5 values (row 2) are the strength of F0 to H4 at time 200 (when "sylLen" is 200).
#' @param formants A vector of formant frequencies or a list of manually specified formant times, frequencies, amplitudes, and bandwidths. If you want to automatically generate formant times, amplitudes, and bandwidths, then specify only a vector of numeric values to indicate the frequencies for a given number of formants, although this will result in `soundgen` generating formants using knowledge about formants from human vocal production. The best practice for including formants in synthetic audio files meant to simulate non-human animal vocalizations will be to manually specify a biologically relevant number of stationary or dynamic formants, and the frequency, amplitude, and bandwidth of each formant (see section 2.9.2 of the `soundgen` sound generation vignette for more info, link below). The default value is `NA`, which will not generate formants. The current version of paRsynth only tests for NULL values of formants, so please ensure that you follow 'soundgen' guidelines for specifying formants.
#' @param vocalTract A numeric value indicating the vocal tract length of the organism that "produced" the synthetic vocalization. This argument is used only if `formants` is not `NA`. The default value of this argument here is `NA`.
#' @param temperature A numeric argument controlling stochastic sound generation. The default value here is 0.
#' @param subratio An integer that indicates the ratio of the fundamental frequency (F0) to the first harmonic or overtone (G0). Here the default value is 2, or period doubling, such that G0 = F0 * 2.
#' @param shortestEpoch Integer. The minimum duration of an epoch with constant subharmonics or locking of formants. This is a soundgen argument in milliseconds that corresponds to how soundgen splits long sounds into smaller sections or epochs to generate harmonics. The default is 300 ms. See the soundgen sound generation vignette for more detailed information.
#' @param vibratoFreq An integer specifying the amount of baseline pitch modulation or vibrato (in Hz) across the synthetic audio file. Here the default is 1, or the lowest value allowed.
#' @param prefix Character string. A prefix for each audio file name that can be used to distinguish among calls for different datasets. For example, using "IndividualSignatures" versus "GroupSignatures" when creating datasets with more or less individual versus group information. The default is "IndividualSignatures".
#' @param save_path Character string. The directory where the sound files will be saved on your machine.
#' @param invalidArgAction Character string. This is an argument used directly by `soundgen::soundgen()` to determine how to continue when an argument is passed a value flagged by `soundgen`. The default is "ignore", so that the function will accept sampling rates specified by the user.
#' @importFrom soundgen soundgen
#' @importFrom pbapply pblapply
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @details This function uses the frequency anchors generated by `frequency_anchors()` to create a synthetic frequency modulated vocalization that contains specific levels of group and individual information. The information about social affiliation or individual identity is contained in the frequency modulation patterns and their convergence or divergence with respect to group members. This function was modified from and relies on \code{\link[soundgen]{soundgen}}. The current version of this function uses the some of the general default values in the `soundgen::soundgen()` package, which can be further customized as needed to add or remove different acoustic features from the synthetic files. See the sound generation vignette for the `soundgen` for more information across all of the `soundgen` arguments that we include in the current function. Check out http://cogsci.se/soundgen/sound_generation.html#intro or run `vignette('sound_generation', package = 'soundgen'))`.
#'
#' The length of the character string determined when using `generate_strings()` can influence visualizations of the synthetic vocalizations, depending on the duration of the synthetic vocalization (which is set here in `write_audio()`). Long character strings that are converted to short synthetic vocalizations may have frequency contours that appear thick and blurry in spectrograms. A general rule of thumb to yield clearer frequency contours can be to increase the duration of the synthetic vocalization (although this will increase computational time for large datasets). We have found that vocalizations of 200 ms duration have clearer frequency contours when character string length in `generate_strings()` is set to 10 characters or fewer. For strings that are 12 to about 22 characters long, a 400 ms duration vocalization will have clearer contours.
#'
#' @return This function writes one audio file per vocalization (row) in the input data frame. Each audio file name contains the group, individual, and call identifiers, as well as the prefix supplied to the function. The function also returns the input data frame with an additional column that contains the name of each audio file. This can be used in downstream bioacoustics analyses.
#'
#' @examples
#' seed <- 8
#' set.seed(seed) # For reproducibility
#' library(tidyverse)
#'
#' example_calls <- generate_strings(n_groups = 2,
#'                                    n_individuals = 5,
#'                                    n_calls = 10,
#'                                    string_length = 16,
#'                                    group_information = 8,
#'                                    individual_information = 2,
#'                                    random_variation = 2
#'                                  )
#'
#' example_calls_parsons <- parsons_code(example_calls,
#'                                       "Call",
#'                                       "Global_head",
#'                                       "Group_head",
#'                                       "Individual_middle",
#'                                       "Random_variation",
#'                                       "Group_tail",
#'                                       "Global_tail",
#'                                       list("A" = "up",
#'                                            "B" = "down",
#'                                            "C" = "constant")
#'                                      )
#'
#' anchors <- frequency_anchors(example_calls_parsons,
#'                              "Call_Parsons_Code",
#'                              "Group",
#'                              "Individual",
#'                              "Call_ID",
#'                              "Call",
#'                              starting_frequency = 4000,
#'                              frequency_shift = 1000,
#'                              section_transition = "starting_frequency"
#'                             )
#'
#' path <- "~/Desktop" # Update this path to reflect your own directory structure
#' tmp_dir <- "testing"
#' tmp_path <- file.path(path, tmp_dir)
#'
#' # Create the temporary directory if it doesn't already exist
#' if(!dir.exists(tmp_path)){
#'  dir.create(tmp_path)
#' }
#'
#' # Write out a randomly subsample of the full dataset of vocalizations
#' set.seed(seed)
#' inds <- sample(1:nrow(anchors), 10, replace = FALSE)
#' write_audio(anchors[inds, ], sylLen = 200, sampling_rate = 44100,
#'             pitch_sampling_rate = 44100,
#'             smoothing = list(interpol = "loess", loessSpan = 1, discontThres = 0, jumpThres = 0),
#'             rolloffExact = c(0.25, 0.25, 0.25, 0.25, 0.25),
#'             formants = NA, vocalTract = NA, temperature = 0, subratio = 2,
#'             shortestEpoch = 300, vibratoFreq = 1,
#'             prefix = "IndividualSignatures",
#'             save_path = tmp_path, invalidArgAction = "ignore")
#'
#' # Remove the temporary directory and all files within it
#' if(tmp_path == file.path(path, tmp_dir)){
#'  unlink(tmp_path, recursive = TRUE)
#' }
#'
#' @export write_audio

write_audio <- function(df, sylLen = 200, sampling_rate = 44100,
                        pitch_sampling_rate = 44100,
                        smoothing = list(interpol = "loess", loessSpan = 1, discontThres = 0, jumpThres = 0),
                        rolloffExact = c(0.25, 0.25, 0.25, 0.25, 0.25),
                        formants = NA, vocalTract = NA, temperature = 0,
                        subratio = 2, shortestEpoch = 300, vibratoFreq = 1,
                        prefix = "IndividualSignatures",
                        save_path, invalidArgAction = "ignore") {

  if (!is.data.frame(df)) {
    stop("The 'df' argument must be a data frame.")
  }
  if (nrow(df) == 0) {
    stop("Input data frame is empty")
  }
  if (!all(c("Group", "Individual", "Call_ID") %in% colnames(df))) {
    stop("One or more columns were not found in the data frame")
  }
  if (missing(save_path) || is.null(save_path) || save_path == "") {
    stop("The 'save_path' argument must be provided and cannot be empty.")
  }
  if (!is.numeric(sampling_rate) || sampling_rate <= 0) {
    stop("The 'sampling_rate' must be a positive numeric value.")
  }
  if (!is.numeric(pitch_sampling_rate) || pitch_sampling_rate <= 0) {
    stop("The 'pitch_sampling_rate' must be a positive numeric value.")
  }
  if (!is.numeric(sylLen) || sylLen <= 0) {
    stop("The 'sylLen' argument must be a positive numeric value.")
  }
  if (!is.numeric(temperature)) {
    stop("The 'temperature' argument must be a numeric value.")
  }
  if (!is.numeric(subratio) || subratio <= 0) {
    stop("The 'subratio' argument must be a positive numeric value.")
  }
  if (!is.numeric(shortestEpoch) || shortestEpoch <= 0) {
    stop("The 'shortestEpoch' argument must be a positive numeric value.")
  }
  if (!is.numeric(vibratoFreq) || vibratoFreq <= 0) {
    stop("The 'vibratoFreq' argument must be a positive numeric value.")
  }

  if (!is.numeric(rolloffExact) ||
        (!is.vector(rolloffExact) && !is.matrix(rolloffExact))) {
    stop("The 'rolloffExact' argument must be a numeric vector or matrix.")
  }

  # Check for valid range of values (0 to 1)
  if (is.vector(rolloffExact) && any(rolloffExact < 0 | rolloffExact > 1)) {
    stop("When 'rolloffExact' is a vector, all values must be between 0 and 1.")
  }

  # If it's a matrix, check each value
  if (is.matrix(rolloffExact)) {
    if (any(rolloffExact < 0 | rolloffExact > 1)) {
      stop("When 'rolloffExact' is a matrix, all values must be between 0 and 1.")
    }
  }

  # Check formants is null
  if (is.null(formants)) {
      stop("The formants can be NA, a numeric vector, or a list that contains the following elements: times, freqs, amps, bwds.")
  }

  # if formants is not NA, then vocalTract is allowed to not be NA. But if the vocalTract is not NA, it must be a numeric value
  if (!is.null(formants) && is.na(formants) && !is.na(vocalTract)) {
      stop("The 'vocalTract' can only be specified when formants is also specified.")
  }
  
  if (!is.null(formants) && !is.na(formants) && !is.na(vocalTract) && !is.numeric(vocalTract)) {
    stop("When 'vocalTract' is provided, it must be a numeric value.")
  }

  if (!is.character(prefix)) {
    stop("The 'prefix' argument must be a character string.")
  }

  if (!is.character(invalidArgAction) ||
        !invalidArgAction %in% c("ignore", "adjust", "abort")) {
    stop("The 'invalidArgAction' argument must be a character string and be one of 'ignore', 'adjust', or 'abort'.")
  }

  if (!is.list(smoothing)) {
    stop("The 'smoothing' argument must be a list.")
  }
  if (!all(c("interpol", "loessSpan", "discontThres", "jumpThres") %in% names(smoothing))) {
    stop("The 'smoothing' argument must contain the following elements: interpol, loessSpan, discontThres, jumpThres.")
  }

  # Ensure the save path exists
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }

  df2 <- data.table::rbindlist(pbapply::pblapply(seq_len(nrow(df)), function(i) {

    # Extract frequencies from the data frame
    frequencies <- as.numeric(df[i, grep("^Frequency", colnames(df))])

    # Adjust pitch floor and ceiling based on the sampling rate
    pitch_floor <- 1 # this is the default value in soundgen
    pitch_ceiling <- sampling_rate / 2 # Nyquist rule for sampling rate and max frequency

    # Construct the audio filename with a unique identifier
    audio_filename <- paste0(prefix, "_Group", df$Group[i], "_Indiv", df$Individual[i],"_Call", df$Call_ID[i], ".wav")

    audio_pathname <- file.path(save_path, audio_filename)

    # Generate a synthetic audio file from the frequency anchors using the soundgen package
    suppressMessages(soundgen::soundgen(
      pitch = frequencies,
      sylLen = sylLen,
      samplingRate = sampling_rate,
      pitchSamplingRate = pitch_sampling_rate,
      pitchFloor = pitch_floor,
      pitchCeiling = pitch_ceiling,
      smoothing = smoothing,
      subratio = subratio,
      rolloffExact = rolloffExact,
      formants = formants,
      vocalTract = vocalTract,
      temperature = temperature,
      shortestEpoch = shortestEpoch,
      vibratoFreq = vibratoFreq,
      saveAudio = audio_pathname,
      invalidArgAction = invalidArgAction
    ))

    return(
      df[i, ] %>%
        dplyr::mutate(
          audio_file_name = audio_filename
        )
    )

  }))

  return(df2)

}