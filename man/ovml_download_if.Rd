% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{ovml_download_if}
\alias{ovml_download_if}
\title{Conditional download utility}
\usage{
ovml_download_if(url, dest, expected_sha1 = NULL)
}
\arguments{
\item{url}{string: URL of file to download}

\item{dest}{string: local basename of file, if missing will be taken from the URL}

\item{expected_sha1}{string: the expected SHA1 hash of the file}
}
\value{
The path to the file in the \code{\link[=ovml_cache_dir]{ovml_cache_dir()}}
}
\description{
If the file already exists in the \code{ovml_cache_dir()}, it won't be downloaded.
}
