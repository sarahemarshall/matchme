% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_score_matches.R
\name{get_rule_based_matches}
\alias{get_rule_based_matches}
\title{Get rule-based matches}
\usage{
get_rule_based_matches(
  m_ee_inn,
  m_or_inn,
  mentee_criteria_in,
  mentor_criteria_in,
  mentee_filter_in = NULL,
  mentor_filter_in = NULL,
  demo = FALSE
)
}
\arguments{
\item{m_ee_inn}{mentee dataframe. must contain `mentee_name` column as unique identifier of each mentee}

\item{m_or_inn}{mentor dataframe. must contain `mentor_name` column as unique identifier of each mentor}

\item{mentee_criteria_in}{vector of column names to match on}

\item{mentor_criteria_in}{vector of column names to match on}

\item{mentee_filter_in}{a string containing a filtering condition for mentee data, e.g. "gender_pref == 'no_pref' "}

\item{mentor_filter_in}{a string containing a filtering condition for mentor data}

\item{demo}{boolean, default = FALSE. If TRUE additional info gets returned.}
}
\value{
either a tibble of all mentees with their matches, or if demo =TRUE, a list of  matches + other things.
}
\description{
Get rule-based matches
}
\details{
matches mentees and mentors based on column names provided in mentee_criteria_in and mentor_criteria_in.
}
