#' @title read covid data
#' Function to read covid data from case_time_series.csv
#'
#' @param filename \code{character}, file name.
#' @param path \code{character}, folder name.
#' @param sep \code{character}, separator for \code{read.csv}, default = ','
#'
#' @return data.frame
#'
#' @import dplyr
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' read_data("case_time_series.csv","covid_data")
#'
#' @seealso [utils::read.csv()]
read_data <- function(filename, path, sep = ",") {
  filepath <- file.path(path, filename)
  data.out <- read.csv(system.file(filepath, package = "mypkg"), sep = sep)
  # DQ check on column names
  if (!(all(
    c(
      "Date_YMD",
      "Total.Confirmed",
      "Total.Recovered",
      "Total.Deceased"
    ) %in% names(data.out)
  )))
    stop("Missing column in input ", filepath)
  # update date column
  data.out %>% select(-1) %>%
    mutate(Date = as.Date(Date_YMD)) %>%
    select(-Date_YMD) %>%
    select(Date, starts_with("Total"))
}


#' @title process covid data
#' Compute additional variables
#'
#' @param data \code{data.frame}, covid19 data.
#' @param k \code{integer}, window size for \code{roll_mean}
#'
#' @details it computes actives and daily figures
#'
#' @return data.frame
#'
#' @import dplyr
#' @export
#'
#' @examples
#' df <- read_data("case_time_series.csv","covid_data")
#' process_data(df)
#' @seealso [roll_mean()]
process_data <- function(data, k = 7) {
  if (k %% 1 != 0) {
    stop("k is not an integer: ", k)
  }
  if (any(data$Current.Active < 0))
    warning(sum(data$Current.Active < 0), " Active values are negative")

  # small utility to compute new cases
  .new_calc <- function(x)
    c(0, diff(x))

  out.data <- data %>%
    mutate(Total.Active = Total.Confirmed - Total.Recovered- Total.Deceased) %>%
    mutate(across(where(is.numeric), .new_calc, .names = "Daily.{.col}"))

  names(out.data) <-  gsub("^Daily.Total","Daily", names(out.data))

  out.data <- out.data %>%
    mutate(across(where(is.numeric), ~ roll_mean(.x, k = k), .names = "rollmean_{.col}"))

  out.data
}

#' @title theme plot
#' defined plot style
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' library(ggplot2)
#' p1 <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(title = "Fuel economy declines as weight increases") +
#'   theme_plot()
theme_plot <- function() {
  size.text <- 8
  size.line <- 0.5
  color.default <- "grey45"
  theme(
    plot.title = element_text(
      color = color.default,
      size = size.text * 1.5,
      face = "bold.italic",
      hjust = 0.5
    ),
    text = element_text(size = size.text),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "white", size = 0.1),
    line = element_line(size = 2.2),
    axis.line.x = element_line(color = color.default, size = size.line),
    axis.line.y = element_line(color = color.default, size = size.line),
    axis.text.x = element_text(
      size = size.text,
      hjust = 1,
      angle = 45
    ),
    axis.text.y = element_text(size = size.text, vjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = size.text, hjust = 0.5),
    legend.title =  element_blank(),
    legend.key = element_rect(fill = alpha("white", 0.0))
  )
}

#' Color for bar plot
.ColorBars <- "grey"
#' Color for lines in line plot
.ColorLines <- "black"

#' @title Scale plot axes
#' defined plot style for axes
#'
#' @param pp \code{ggplot} object
#' @param datebreaks \code{character} x axis date breaks, default = "1 week"
#'
#' @import ggplot2
#' @importFrom scales label_number cut_si
#'
#' @seealso [scale_x_date()] [scale_y_continuous()]
scale_axes <- function(pp, datebreaks = "1 week") {
  y.n.breaks <- 10
  date.format <- "%b %d"
  pp +
    scale_x_date(date_breaks = datebreaks, date_labels = date.format) +
    scale_y_continuous(n.breaks = y.n.breaks, labels =  scales::label_number(scale_cut = cut_si("")))
}

#' @title determine dates break
#' if the dates range is less than threshold then result is 1 week, otherwise 1 month
#'
#' @param dates \code{Date} vector
#' @param threshold \code{numeric} threshold for week month choice
#'
get_date_breaks <- function(dates, threshold = 160) {
  if (is.character(threshold))
    stop("threshold must be numeric")
  ifelse(diff(range(dates)) < threshold, "1 week", "1 month")
}


#' Lineplot function
#'
#' @param data \code{data.frame}, covid19 data.
#' @param value \code{character}, Column name variable of data.
#' @param startdate \code{character} or \code{Date}, format: "%Y-%m-%d", default start
#' @param title \code{character}, Title text, if missing no title
#'
#' @details it plots value over Date
#'
#' @return data.frame
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- read_data("case_time_series.csv","covid_data")
#' data <- process_data(df)
#' line_plot_data(data, "Daily.Confirmed")
#' line_plot_data(data, "Daily.Active", title = "Active cases")
line_plot_data <- function(data, value, startdate = data$Date[1], title) {
  if (!(value %in% names(data)))
    stop(value, " is not a valid column name of data")

  if (!missing(startdate)) {
    # conflicts with "stats" package, ::: could be required
    data <- data %>%
      filter(Date >= startdate)
  }
  date.break <- get_date_breaks(data$Date)

  p <- ggplot(data, aes(
    x = Date,
    y = !!sym(value),
    group = 1
  )) +
    geom_line(colour = .ColorLines) # add a line
  # add theme
  p <- p +
    theme_plot()
  # add scales
  p <- scale_axes(p, date.break)
  #add title if present
  if (!missing(title))
    p <- p + labs(title = title)
  p
}

#' Barplot function
#'
#' @param data \code{data.frame}, covid19 data.
#' @param value \code{character}, Column name variable of data.
#' @param startdate \code{character} or \code{Date}, format: "%Y-%m-%d", default start
#' @param rollm \code{logical}, if TRUE then a line with rolled weekly mean will be added
#' @param title \code{character}, Title text, if missing no title
#'
#' @details it plots value over Date
#'
#' @return data.frame
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- read_data("case_time_series.csv","covid_data")
#' data <- process_data(df)
#' bar_plot_data(data, "Daily.Confirmed", rollm = TRUE)
#' bar_plot_data(data, "Daily.Active")
bar_plot_data <- function(data, value, startdate = data$Date[1], rollm = FALSE, title) {
  if (!(value %in% names(data)))
    stop(value, " is not a valid column name of data")

  if (!missing(startdate)) {
    # conflicts with "stats" package, ::: could be required
    data <- data %>%
      filter(Date >= startdate)
  }
  date.break <- get_date_breaks(data$Date)

  p <- ggplot(data, aes(
    x = Date,
    y = !!sym(value),
    group = 1
  )) +
    geom_bar(stat = "identity", colour = .ColorBars)

  if (rollm) {
    # use !sym() to parse rolled.value
    message("Add rolled mean for ", value)
    rolled.value <- paste("rollmean", value, sep = "_")
    p <-
      p + geom_line(aes(x = Date, y = !!sym(rolled.value)), color = "red", size = 1.25)
  }
  # add theme
  p <- p +
    theme_plot()
  # add scales
  p <- scale_axes(p, date.break)

  #add title if present
  if (!missing(title))
    p <- p + labs(title = title)
  p
}

