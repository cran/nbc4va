% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbc4va_main.R
\name{nbc}
\alias{nbc}
\title{Train a NBC model}
\usage{
nbc(train, test, known = TRUE)
}
\arguments{
\item{train}{Dataframe of verbal autopsy train data (See \href{https://rrwen.github.io/nbc4va/data}{Data documentation}).
\itemize{
  \item Columns (in order): ID, Cause, Symptom-1 to Symptom-n..
  \item ID (vectorof char): unique case identifiers
  \item Cause (vectorof char): observed causes for each case
  \item Symptom-n.. (vectorsof (1 OR 0)): 1 for presence, 0 for absence, other values are treated as unknown
  \item Unknown symptoms are imputed randomly from distributions of 1s and 0s per symptom column; if no 1s or 0s exist then the column is removed
}
Example:
\tabular{ccccc}{
  ID \tab Cause \tab S1 \tab S2 \tab S3 \cr
  "a1" \tab "HIV" \tab 1 \tab 0 \tab 0\cr
  "b2" \tab "Stroke" \tab 0 \tab 0 \tab 1\cr
  "c3" \tab "HIV" \tab 1 \tab 1 \tab 0
}}

\item{test}{Dataframe of verbal autopsy test data in the same format as \emph{train} except if causes are not known:
\itemize{
  \item The 2nd column (Cause) can be omitted if \emph{known} is FALSE
}}

\item{known}{TRUE to indicate that the \emph{test} causes are available in the 2nd column and FALSE to indicate that they are not known}
}
\value{
out The result nbc list object containing:
\itemize{
  \item $prob.causes (vectorof double): the probabilities for each test case prediction by case id
  \item $pred.causes (vectorof char): the predictions for each \emph{test} case by case id
  \item Additional values:
  \itemize{
    \item * indicates that the value is only available if \emph{test} causes are known
    \item $train (dataframe): the input \emph{train} data
    \item $train.ids (vectorof char): the ids of the \emph{train} data
    \item $train.causes (vectorof char): the causes of the \emph{train} data by case id
    \item $train.samples (double): the number of input \emph{train} samples
    \item $test (dataframe): the input \emph{test} data
    \item $test.ids (vectorof char): the ids of the \emph{test} data
    \item $test.causes* (vectorof char): the causes of the \emph{test} data by case id
    \item $test.samples (double): the number of input \emph{test} samples
    \item $test.known (logical): whether the \emph{test} causes are known
    \item $symptoms (vectorof char): all unique symptoms in order
    \item $causes (vectorof char): all possible unique causes of death
    \item $causes.train (vectorof char): all unique causes of death in the \emph{train} data
    \item $causes.test* (vectorof char): all unique causes of death in the \emph{test} data
    \item $causes.pred (vectorof char): all unique causes of death in the predicted cases
    \item $causes.obs* (vectorof char): all unique causes of death in the observed cases
    \item $pred (dataframe): a table of predictions for each \emph{test} case, sorted by probability
      \itemize{
        \item Columns (in order): CaseID, TrueCause, Prediction-1 to Prediction-n..
        \item CaseID (vectorof char): case identifiers
        \item TrueCause* (vectorof char): the observed causes of death
        \item Prediction-n.. (vectorsof char): the predicted causes of death,
        where Prediction1 is the most probable cause, and Prediction-n is the least probable cause
     }
    Example:
    \tabular{cccc}{
      CaseID \tab Prediction1 \tab Prediction2 \cr
      "a1" \tab "HIV" \tab "Stroke" \cr
      "b2" \tab "Stroke" \tab "HIV" \cr
      "c3" \tab "HIV" \tab "Stroke"
    }
    \item $obs* (dataframe): a table of observed causes matching \emph{$pred} for each \emph{test} case
      \itemize{
        \item Columns (in order): CaseID, TrueCause
        \item CaseID (vectorof char): case identifiers
        \item TrueCause (vectorof char): the actual cause of death if applicable
     }
    Example:
    \tabular{cccc}{
      CaseID \tab TrueCause \cr
      "a1" \tab "HIV" \cr
      "b2" \tab "Stroke" \cr
      "c3" \tab "HIV"
    }
    \item $obs.causes* (vectorof char): all observed causes of death by case id
    \item $prob (dataframe): a table of probabilities of each cause for each test case
      \itemize{
       \item Columns (in order): CaseID, Cause-1 to Cause-n..
       \item CaseID (vectorof char): case identifiers
       \item Cause-n.. (vectorsof double): probabilies for each cause of death
      }
    Example:
    \tabular{ccc}{
      CaseID \tab HIV \tab Stroke \cr
      "a1" \tab 0.5 \tab 0.5 \cr
      "b2" \tab 0.3 \tab 0.7 \cr
      "c3" \tab 0.9 \tab 0.1
    }
  }
}
}
\description{
Performs supervised Naive Bayes Classification on verbal autopsy data.
}
\examples{
library(nbc4va)
data(nbc4vaData)

# Run naive bayes classifier on random train and test data
# Set "known" to indicate whether or not "test" causes are known
train <- nbc4vaData[1:50, ]
test <- nbc4vaData[51:100, ]
results <- nbc(train, test, known=TRUE)

# Obtain the probabilities and predictions
prob <- results$prob.causes
pred <- results$pred.causes

}
\references{
\itemize{
  \item Miasnikof P, Giannakeas V, Gomes M, Aleksandrowicz L, Shestopaloff AY, Alam D, Tollman S, Samarikhalaj, Jha P. Naive Bayes classifiers for verbal autopsies: comparison to physician-based classification for 21,000 child and adult deaths. BMC Medicine. 2015;13:286. doi:10.1186/s12916-015-0521-2.
}
}
\seealso{
Other main functions: 
\code{\link{plot.nbc}()},
\code{\link{print.nbc_summary}()},
\code{\link{summary.nbc}()}
}
\concept{main functions}
