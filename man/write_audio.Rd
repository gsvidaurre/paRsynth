% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_audio.R
\name{write_audio}
\alias{write_audio}
\title{Save synthetic audio for each string in a data frame}
\usage{
write_audio(
  df,
  sylLen = 200,
  sampling_rate = 44100,
  pitch_sampling_rate = 44100,
  smoothing = list(interpol = "loess", loessSpan = 1, discontThres = 0, jumpThres = 0),
  rolloffExact = c(0.25, 0.25, 0.25, 0.25, 0.25),
  formants = NA,
  vocalTract = NA,
  temperature = 0,
  subratio = 2,
  shortestEpoch = 300,
  vibratoFreq = 1,
  prefix = "IndividualSignatures",
  save_path,
  invalidArgAction = "ignore"
)
}
\arguments{
\item{df}{Data frame. A data frame object generated by \code{frequency_anchors()} that contains the frequency anchor values and string metadata.}

\item{sylLen}{Numeric value. The length of the vocalization in milliseconds. The default is 200 ms. This is an argument used directly by \code{soundgen::soundgen()}. The vocalization duration can influence visualizations of the resulting synthetic vocalizations in ways that depend on the length of the character string (see details below).}

\item{sampling_rate}{Numeric value. The sampling rate for the audio file, in Hz. The default is 44100 Hz or 44.1 kHz. This is an argument used directly by \code{soundgen::soundgen()}.}

\item{pitch_sampling_rate}{Numeric value. The sampling rate for the pitch contour of the audio file, in Hz. The default is 44100 Hz or 44.1 kHz. This is an argument used directly by \code{soundgen::soundgen()}, and it is recommended to set this argument to be the same as the audio sampling rate for quantitative analyses.}

\item{smoothing}{A list with the named elements \code{interpol}, \code{loessSpan}, \code{discontThres}, and \code{jumpThres} to control how smoothing of frequency contours is performed by \code{soundgen} when audio files are generated using frequency anchors. The default list provided to this argument is \code{list(interpol = "loess", loessSpan = 1, discontThres = 0, jumpThres = 0)} to perform strong Loess smoothing (local polynomial regression).}

\item{rolloffExact}{A numeric object encoding static amplitude values across the fundamental and harmonics (a vector) or encoding dynamic changes in amplitude across the fundamental and harmonics (a matrix). Numeric values representing amplitudes in this object should be scaled from 0 to 1. The default list provided to this argument is \code{c(0.25, 0.25, 0.25, 0.25, 0.25)}, which assigns amplitude values of 0.25 to the fundamental and each of 4 harmomics (or overtones). Since there is one numeric value assigned to the fundamental and each harmonic, the amplitude values will not change over time. To encode dynamic changes in amplitude across the fundamental and harmonics, create a matrix of amplitude values in which each row corresponds to a timepoint and each column corresponds to the fundamental or a harmonic, such as: "matrix(c(0.5, 0.2, 1, 0.02, 0.22,  0.1, 0.4, .01, 0.05, 0.2), ncol = 2)" in which the first 5 values (row 1) are the strength of F0 to H4 at time 0 and the second 5 values (row 2) are the strength of F0 to H4 at time 200 (when "sylLen" is 200).}

\item{formants}{A vector of formant frequencies or a list of manually specified formant times, frequencies, amplitudes, and bandwidths. If you want to automatically generate formant times, amplitudes, and bandwidths, then specify only a vector of numeric values to indicate the frequencies for a given number of formants, although this will result in \code{soundgen} generating formants using knowledge about formants from human vocal production. The best practice for including formants in synthetic audio files meant to simulate non-human animal vocalizations will be to manually specify a biologically relevant number of stationary or dynamic formants, and the frequency, amplitude, and bandwidth of each formant (see section 2.9.2 of the \code{soundgen} sound generation vignette for more info, link below). The default value is \code{NA}, which will not generate formants. The current version of paRsynth only tests for NULL values of formants, so please ensure that you follow 'soundgen' guidelines for specifying formants.}

\item{vocalTract}{A numeric value indicating the vocal tract length of the organism that "produced" the synthetic vocalization. This argument is used only if \code{formants} is not \code{NA}. The default value of this argument here is \code{NA}.}

\item{temperature}{A numeric argument controlling stochastic sound generation. The default value here is 0.}

\item{subratio}{An integer that indicates the ratio of the fundamental frequency (F0) to the first harmonic or overtone (G0). Here the default value is 2, or period doubling, such that G0 = F0 * 2.}

\item{shortestEpoch}{Integer. The minimum duration of an epoch with constant subharmonics or locking of formants. This is a soundgen argument in milliseconds that corresponds to how soundgen splits long sounds into smaller sections or epochs to generate harmonics. The default is 300 ms. See the soundgen sound generation vignette for more detailed information.}

\item{vibratoFreq}{An integer specifying the amount of baseline pitch modulation or vibrato (in Hz) across the synthetic audio file. Here the default is 1, or the lowest value allowed.}

\item{prefix}{Character string. A prefix for each audio file name that can be used to distinguish among calls for different datasets. For example, using "IndividualSignatures" versus "GroupSignatures" when creating datasets with more or less individual versus group information. The default is "IndividualSignatures".}

\item{save_path}{Character string. The directory where the sound files will be saved on your machine.}

\item{invalidArgAction}{Character string. This is an argument used directly by \code{soundgen::soundgen()} to determine how to continue when an argument is passed a value flagged by \code{soundgen}. The default is "ignore", so that the function will accept sampling rates specified by the user.}
}
\value{
This function writes one audio file per vocalization (row) in the input data frame. Each audio file name contains the group, individual, and call identifiers, as well as the prefix supplied to the function. The function also returns the input data frame with an additional column that contains the name of each audio file. This can be used in downstream bioacoustics analyses.
}
\description{
The \code{write_audio} function loops through each row in the data frame and uses the \code{gen_synth_signal} function (a wrapper for the \code{soundgen::soundgen()}) to generate and save audio files in .WAV format for each string.
}
\details{
This function uses the frequency anchors generated by \code{frequency_anchors()} to create a synthetic frequency modulated vocalization that contains specific levels of group and individual information. The information about social affiliation or individual identity is contained in the frequency modulation patterns and their convergence or divergence with respect to group members. This function was modified from and relies on \code{\link[soundgen]{soundgen}}. The current version of this function uses the some of the general default values in the \code{soundgen::soundgen()} package, which can be further customized as needed to add or remove different acoustic features from the synthetic files. See the sound generation vignette for the \code{soundgen} for more information across all of the \code{soundgen} arguments that we include in the current function. Check out http://cogsci.se/soundgen/sound_generation.html#intro or run \verb{vignette('sound_generation', package = 'soundgen'))}.

The length of the character string determined when using \code{generate_strings()} can influence visualizations of the synthetic vocalizations, depending on the duration of the synthetic vocalization (which is set here in \code{write_audio()}). Long character strings that are converted to short synthetic vocalizations may have frequency contours that appear thick and blurry in spectrograms. A general rule of thumb to yield clearer frequency contours can be to increase the duration of the synthetic vocalization (although this will increase computational time for large datasets). We have found that vocalizations of 200 ms duration have clearer frequency contours when character string length in \code{generate_strings()} is set to 10 characters or fewer. For strings that are 12 to about 22 characters long, a 400 ms duration vocalization will have clearer contours.
}
\examples{
seed <- 8
set.seed(seed) # For reproducibility
library(tidyverse)

example_calls <- generate_strings(n_groups = 2,
                                   n_individuals = 5,
                                   n_calls = 10,
                                   string_length = 16,
                                   group_information = 8,
                                   individual_information = 2,
                                   random_variation = 2
                                 )

example_calls_parsons <- parsons_code(example_calls,
                                      "Call",
                                      "Global_head",
                                      "Group_head",
                                      "Individual_middle",
                                      "Random_variation",
                                      "Group_tail",
                                      "Global_tail",
                                      list("A" = "up",
                                           "B" = "down",
                                           "C" = "constant")
                                     )

anchors <- frequency_anchors(example_calls_parsons,
                             "Call_Parsons_Code",
                             "Group",
                             "Individual",
                             "Call_ID",
                             "Call",
                             starting_frequency = 4000,
                             frequency_shift = 1000,
                             section_transition = "starting_frequency"
                            )

path <- "~/Desktop" # Update this path to reflect your own directory structure
tmp_dir <- "testing"
tmp_path <- file.path(path, tmp_dir)

# Create the temporary directory if it doesn't already exist
if(!dir.exists(tmp_path)){
 dir.create(tmp_path)
}

# Write out a randomly subsample of the full dataset of vocalizations
set.seed(seed)
inds <- sample(1:nrow(anchors), 10, replace = FALSE)
write_audio(anchors[inds, ], sylLen = 200, sampling_rate = 44100,
            pitch_sampling_rate = 44100,
            smoothing = list(interpol = "loess", loessSpan = 1, discontThres = 0, jumpThres = 0),
            rolloffExact = c(0.25, 0.25, 0.25, 0.25, 0.25),
            formants = NA, vocalTract = NA, temperature = 0, subratio = 2,
            shortestEpoch = 300, vibratoFreq = 1,
            prefix = "IndividualSignatures",
            save_path = tmp_path, invalidArgAction = "ignore")

# Remove the temporary directory and all files within it
if(tmp_path == file.path(path, tmp_dir)){
 unlink(tmp_path, recursive = TRUE)
}

}
\author{
Ari Cross, Grace Smith-Vidaurre
}
