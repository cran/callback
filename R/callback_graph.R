#'
#' Generic plot function
#'
#' @param data A \code{stat_glob}, \code{stat_mcr} or \code{stat_ecs} object.
#' @param method the type of confidence interval: "cp" for Clopper-Pearson,
#' "wilson" for Wilson (the default), "student" for Student.
#' @param col  A list of colors.
#' @param ... further arguments passed to or from other methods.
#'
#' @export

graph <- function(data, method, col, ...) {
  UseMethod("graph")
}

#'
#' Global callback rates plot
#'
#' @param data A \code{stat_glob} object.
#' @param method the type of confidence interval: "cp" for Clopper-Pearson,
#' "wilson" for Wilson (the default), "student" for Student.
#' @param col  A list of colors (one colour needed).
#' @param ... further arguments passed to or from other methods.
#'
#' @return A ggplot2 object
#'
#' @author Emmanuel Duguet
#'
#' @examples
#' data(origin1)
#' m <- callback(labour1,"offer","hist","callback","all")
#' s <- stat_glob(m)
#' graph(s)
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip scale_x_discrete
#' geom_errorbar ggtitle ylab xlab theme element_text
#'
#' @export

graph.stat_glob <- function(data = NULL,
                            method = "wilson",
                            col = c("#F8766D", "#00BA38", "#619CFF"),
                            ...) {
  l_method <- c("cp", "wilson", "student")
  method <- tolower(method)
  if (!(method %in% l_method)) {
    method <- "wilson"
  }
  m_name <- ifelse(
    method == "wilson",
    "Wilson",
    ifelse(method == "student", "Student", "Clopper-Pearson")
  )

  cand <- p_callback <- inf <- sup <- NULL
  props <- data[["props"]]
  m <- data[[method]]
  gr <- data.frame(
    cand = rownames(m),
    p_callback = props$p_callback,
    inf = m$inf_p_callback,
    sup = m$sup_p_callback
  )

  ggplot(data = gr, aes(x = cand, y = p_callback)) +
    geom_bar(stat = "identity", fill = col[1]) +
    coord_flip() +
    scale_x_discrete(limits = rev) +
    geom_errorbar(aes(ymin = inf, ymax = sup),
                  width = 0.2,
                  color = "black") +
    ggtitle("Callback rates") +
    ylab(paste(
      m_name,
      "confidence intervals at the",
      round(data$level * 100, 1),
      "percent level"
    )) +
    xlab("Candidates") +
    theme(
      plot.title = element_text(
        color = "gray50",
        size = 14,
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "black",
        size = 12,
        face = "plain"
      ),
      axis.title.x = element_text(
        color = "black",
        size = 10,
        face = "plain"
      )
    )
}

#'
#' Exclusive callback shares plot
#'
#' @param data a \code{stat_ecs} object.
#' @param method the type of confidence interval: "cp" for Clopper-Pearson,
#' "wilson" for Wilson (the default), "student" for Student.
#' @param col  A list of colors (three colors needed).
#' @param ... further arguments passed to or from other methods.
#'
#' @return A ggplot2 object
#'
#' @author Emmanuel Duguet
#'
#' @examples
#' data(origin1)
#' m <- callback(labour1,"offer","hist","callback","all")
#' s <- stat_ecs(m)
#' g_ecs(s,method="cp")
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip geom_errorbar ggtitle
#' ylab xlab theme element_text scale_color_manual scale_x_continuous sec_axis
#' guides scale_fill_manual geom_hline
#'
#' @importFrom stats relevel
#'
#' @export

g_ecs <- function(data = NULL,
                  method = "wilson",
                  col = c("#F564E3", "#7CAE00", "#00BFC4"),
                  ...) {
  candidate <-
    r <- inf1 <- sup1 <- c10 <- disc <- c01 <- p_cand1 <- NULL
  inf_p_cand1 <-
    inf_p_cand2 <- sup_p_cand1 <- sup_p_cand2 <- dif_p_value <-
    NULL
  l_method <- c("cp", "wilson", "student")
  method <- tolower(method)
  if (!(method %in% l_method)) {
    method <- "wilson"
  }
  m_name <- ifelse(
    method == "wilson",
    "Wilson",
    ifelse(method == "student", "Student", "Clopper-Pearson")
  )

  props <- data[["props"]]
  m <- data[[method]]
  gr <- data.frame(
    cand = rownames(m),
    r1 = props$p_cand1,
    inf1 = m$inf_p_cand1,
    sup1 = m$sup_p_cand1,
    r2 = props$p_cand2
  )
  x <- strsplit(gr$cand, ".vs.")
  l_sub <- function(x, n) {
    x[n]
  }
  gr$cand1 <- sapply(x, l_sub, 1)
  gr$cand2 <- sapply(x, l_sub, 2)

  gr1 <- gr[, c("cand1", "r1", "inf1", "sup1")]
  colnames(gr1) <- c("cand", "r", "inf1", "sup1")
  gr1$t <- 1:nrow(gr1)
  gr1$candidate <- "cand1"

  gr2 <- gr[, c("cand2", "r2")]
  colnames(gr2) <- c("cand", "r")
  gr2$t <- 1:nrow(gr2)
  gr2$candidate <- "cand2"

  ngr <- rbind(gr1[, c("t", "candidate", "cand", "r")], gr2)
  ngr$candidate <- factor(ngr$candidate)
  ngr$candidate <- relevel(ngr$candidate, ref = "cand2")

  ggplot(data = ngr, aes(x = t, group = candidate, fill = candidate)) +
    coord_flip() +
    scale_color_manual(values = col[1:2]) +
    scale_fill_manual(values = col[1:2]) +
    geom_bar(stat = "identity", aes(y = r)) +
    scale_x_continuous(
      breaks = 1:length(gr1$cand),
      labels = gr1$cand,
      trans = "reverse",
      sec.axis = sec_axis(
        ~ .,
        breaks = 1:length(gr2$cand),
        labels = gr2$cand
      )
    ) +
    geom_errorbar(
      data = gr1,
      aes(x = t, ymin = inf1, ymax = sup1),
      width = 0.3,
      color = "black"
    ) +
    geom_hline(yintercept = 0.5, color = col[3]) +
    ggtitle("share of exclusive callbacks") +
    ylab(paste(
      m_name,
      "confidence intervals at the",
      round(data$level * 100, 1),
      "percent level"
    )) +
    xlab("") +
    theme(
      plot.title = element_text(
        color = "gray50",
        size = 14,
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "black",
        size = 12,
        face = "plain"
      ),
      axis.title.x = element_text(
        color = "black",
        size = 10,
        face = "plain"
      ),
      axis.text.y = element_text(colour = col[2]),
      axis.text.y.right = element_text(color = col[1])
    ) +
    guides(fill = "none", color = "none")
}

#'
#' Total callback shares plot
#'
#' @param data a \code{stat_tcs} object.
#' @param col  A list of colors (three colors needed).
#' @param ... further arguments passed to or from other methods.
#'
#' @return A ggplot2 object
#'
#' @author Emmanuel Duguet
#'
#' @examples
#' data(origin1)
#' m <- callback(labour1,"offer","hist","callback","all")
#' s <- stat_tcs(m)
#' g_tcs(s)
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip geom_errorbar ggtitle
#' ylab xlab theme element_text scale_color_manual scale_x_continuous sec_axis
#' guides scale_fill_manual geom_hline
#'
#' @importFrom stats relevel
#'
#' @export

g_tcs <- function(data = NULL,
                  col = c("gray75", "#F564E3", "#7CAE00"),
                  ...) {
  callback <-
    r <- inf1 <- sup1 <- c10 <- disc <- c01 <- p_cand1 <- NULL
  inf_p_cand1 <-
    inf_p_cand2 <- sup_p_cand1 <- sup_p_cand2 <- dif_p_value <-
    NULL

  props <- data[["props"]]
  gr <- data.frame(
    cand = rownames(props),
    r1 = props$p_cand1,
    r2 = props$p_cand2,
    r3 = props$p_equal
  )

  x <- strsplit(gr$cand, ".vs.")
  l_sub <- function(x, n) {
    x[n]
  }
  gr$cand1 <- sapply(x, l_sub, 1)
  gr$cand2 <- sapply(x, l_sub, 2)
  gr$cand3 <- "both"

  gr1 <- gr[, c("cand1", "r1")]
  colnames(gr1) <- c("cand", "r")
  gr1$t <- 1:nrow(gr1)
  gr1$callback <- "1 only"

  gr2 <- gr[, c("cand2", "r2")]
  colnames(gr2) <- c("cand", "r")
  gr2$t <- 1:nrow(gr2)
  gr2$callback <- "2 only"

  gr3 <- gr[, c("cand3", "r3")]
  colnames(gr3) <- c("cand", "r")
  gr3$t <- 1:nrow(gr3)
  gr3$callback <- "both"

  ngr <- rbind(gr1, gr2[, c("t", "callback", "cand", "r")], gr3)
  ngr$callback <- factor(ngr$callback)
  ngr$callback <-
    ordered(ngr$callback, levels = c("both", "2 only", "1 only"))

  ggplot(data = ngr, aes(x = t, group = callback, fill = callback)) +
    coord_flip() +
    scale_color_manual(values = col[1:3]) +
    scale_fill_manual(values = col[1:3]) +
    geom_bar(stat = "identity", aes(y = r)) +
    scale_x_continuous(
      breaks = 1:length(gr1$cand),
      labels = gr1$cand,
      trans = "reverse",
      sec.axis = sec_axis(
        ~ .,
        breaks = 1:length(gr2$cand),
        labels = gr2$cand
      )
    ) +
    ggtitle("share of total callbacks") +
    xlab("") +
    theme(
      plot.title = element_text(
        color = "gray50",
        size = 14,
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "black",
        size = 12,
        face = "plain"
      ),
      axis.title.x = element_text(
        color = "black",
        size = 10,
        face = "plain"
      ),
      axis.text.y = element_text(colour = col[3]),
      axis.text.y.right = element_text(color = col[2])
    )
}


#'
#' Proportions' comparison plot
#'
#' @param data A \code{stat_mcr} object.
#' @param method the type of confidence interval: "cp" for Clopper-Pearson,
#' "wilson" for Wilson (the default), "student" for Student.
#' @param col  A list of colors (two colors needed).
#' @param ... further arguments passed to or from other methods.
#'
#' @return A ggplot2 object
#'
#' @author Emmanuel Duguet
#'
#' @examples
#' data(origin1)
#' m <- callback(labour1,"offer","hist","callback","all")
#' s <- stat_mcr(m)
#' g_prop(data=s,method="student")
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip geom_errorbar ggtitle
#' ylab xlab theme element_text scale_x_continuous sec_axis guides
#'
#' @export

g_prop <- function(data = NULL,
                   method = "wilson",
                   col = c("#F564E3", "#7CAE00", "#00BFC4"),
                   ...) {
  p_cand <-
    candidate <- inf <- sup <- inf_p_callback <- inf_p_cand1 <- NULL
  inf_p_cand2 <-
    sup_p_callback <-
    sup_p_cand1 <- sup_p_cand2 <- dif_p_value <- NULL
  l_method <- c("cp", "wilson", "student")
  method <- tolower(method)
  if (!(method %in% l_method)) {
    method <- "wilson"
  }
  m_name <- ifelse(
    method == "wilson",
    "Wilson",
    ifelse(method == "student", "Student", "Clopper-Pearson")
  )

  props <- data[["props"]]
  m <- data[[method]]
  gr <- data.frame(
    cand = rownames(m),
    p_cand1 = props$p_cand1,
    inf1 = m$inf_p_cand1,
    sup1 = m$sup_p_cand1,
    p_cand2 = props$p_cand2,
    inf2 = m$inf_p_cand2,
    sup2 = m$sup_p_cand2
  )

  x <- strsplit(gr$cand, ".vs.")
  l_sub <- function(x, n) {
    x[n]
  }
  gr$cand1 <- sapply(x, l_sub, 1)
  gr$cand2 <- sapply(x, l_sub, 2)

  gr1 <- gr[, c("cand1", "inf1", "p_cand1", "sup1")]
  colnames(gr1) <- c("cand", "inf", "p_cand", "sup")

  gr1$t <- 1:nrow(gr1)
  gr1$candidate <- "cand1"
  gr2 <- gr[, c("cand2", "inf2", "p_cand2", "sup2")]
  colnames(gr2) <- c("cand", "inf", "p_cand", "sup")

  gr2$t <- 1:nrow(gr2)
  gr2$candidate <- "cand2"
  ngr <- rbind(gr1, gr2)

  ggplot(data = ngr, aes(
    x = t,
    y = p_cand,
    group = candidate,
    colour = candidate
  )) +
    coord_flip() +
    scale_color_manual(values = col[1:2]) +
    scale_x_continuous(
      breaks = 1:length(gr1$cand),
      labels = gr1$cand,
      trans = "reverse",
      sec.axis = sec_axis(
        ~ .,
        breaks = 1:length(gr2$cand),
        labels = gr2$cand
      )
    ) +
    geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.3) +
    ggtitle("Matched callback rates") +
    ylab(paste(
      m_name,
      "confidence intervals at the",
      round(data$level * 100, 1),
      "percent level"
    )) +
    xlab("") +
    theme(
      plot.title = element_text(
        color = "gray50",
        size = 14,
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "black",
        size = 12,
        face = "plain"
      ),
      axis.title.x = element_text(
        color = "black",
        size = 10,
        face = "plain"
      ),
      axis.text.y = element_text(colour = col[1]),
      axis.text.y.right = element_text(color = col[2])
    )  +
    guides(color = "none")
}


#'
#' Difference of proportions plot
#'
#' @param data A \code{stat_mcr}, \code{stat_tcs} or \code{stat_ecs} object.
#' @param method the type of confidence interval: "cp" for Clopper-Pearson,
#' "wilson" for Wilson (the default), "student" for Student.
#' @param col  A list of colors (two colors needed).
#' @param ... further arguments passed to or from other methods.
#'
#' @return A ggplot2 object
#'
#' @author Emmanuel Duguet
#'
#' @examples
#' data(origin1)
#' m <- callback(labour1,"offer","hist","callback","all")
#' s <- stat_mcr(m)
#' g_difp(data=s,method="student")
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip geom_errorbar ggtitle
#' ylab xlab theme element_text scale_x_continuous sec_axis guides geom_hline
#'
#' @export

g_difp <- function(data = NULL,
                   method = "wilson",
                   col = c("#619CFF", "#619CFF", "#F564E3"),
                   ...) {
  cand <- candidate <- inf <- sup <- p_cand <- NULL
  l_method <- c("wilson", "student")# not available for cp
  method <- tolower(method)
  if (!(method %in% l_method)) {
    method <- "wilson"
  }
  m_name <- ifelse(method == "wilson", "Wilson", "Student")
  props <- data[["props"]]
  m <- data[[method]]
  gr <- data.frame(
    cand = rownames(m),
    inf = m$inf_cand_dif,
    sup = m$sup_cand_dif,
    p_cand = props$p_cand_dif
  )

  obc <- class(data)
  grt <- ifelse(
    obc == "stat_mcr",
    "matched callback rates differences",
    ifelse(
      obc == "stat_tcs",
      "total callback shares differences",
      "exclusive callback shares differences"
    )
  )

  x <- strsplit(gr$cand, ".vs.")
  l_sub <- function(x, n) {
    x[n]
  }
  gr$cand1 <- sapply(x, l_sub, 1)
  gr$cand2 <- sapply(x, l_sub, 2)
  gr$t <- 1:nrow(gr)

  ggplot(data = gr, aes(x = t, y = p_cand)) +
    coord_flip() +
    scale_color_manual(values = col[1:2]) +
    scale_x_continuous(
      breaks = 1:length(gr$cand1),
      labels = gr$cand1,
      trans = "reverse",
      sec.axis = sec_axis(
        ~ .,
        breaks = 1:length(gr$cand2),
        labels = gr$cand2
      )
    ) +
    geom_errorbar(aes(ymin = inf, ymax = sup),
                  width = 0.3,
                  colour = col[3]) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               colour = "gray50") +
    ggtitle(grt) +
    ylab(paste(
      m_name,
      "confidence intervals at the",
      round(data$level * 100, 1),
      "percent level"
    )) +
    xlab("") +
    theme(
      plot.title = element_text(
        color = "gray50",
        size = 14,
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "black",
        size = 12,
        face = "plain"
      ),
      axis.title.x = element_text(
        color = "black",
        size = 10,
        face = "plain"
      ),
      axis.text.y = element_text(colour = col[1]),
      axis.text.y.right = element_text(color = col[2])
    ) +
    guides(color = "none")
}

#'
#' Matched callback rates plots
#'
#' @param data a \code{stat_mcr} object.
#' @param method the type of confidence interval: "cp" for Clopper-Pearson,
#' "wilson" for Wilson (the default), "student" for Student.
#' @param col  A list of colors (three colors needed).
#' @param dif  TRUE for the difference in proportions (the default), FALSE for a
#' comparison of confidence intervals
#' @param ... further arguments passed to or from other methods.
#'
#' @return a ggplot2 object
#'
#' @author Emmanuel Duguet
#'
#' @export
#'
#' @examples
#' data(origin1)
#' m <- callback(labour1,"offer","hist","callback","all")
#' s <- stat_mcr(m)
#' graph(data=s,method="student")
#'
graph.stat_mcr <- function(data = NULL,
                           method = "wilson",
                           col = c("#F564E3", "#7CAE00", "#00BFC4"),
                           dif = TRUE,
                           ...) {
  if (dif == TRUE) {
    g_difp(data = data,
           method = method,
           col = col)
  } else {
    g_prop(data = data,
           method = method,
           col = col)
  }
}


#'
#' Total callback shares plots
#'
#' @param data a \code{stat_tcs} object.
#' @param method the type of confidence interval: "wilson" for Wilson (the
#' default), "student" for Student.
#' @param col  A list of colors (three colors needed).
#' @param dif  TRUE for the difference in proportions (the default), FALSE for a
#' comparison of confidence intervals
#' @param ... further arguments passed to or from other methods.
#'
#' @return a ggplot2 object
#'
#' @author Emmanuel Duguet
#'
#' @export
#'
#' @examples
#' data(origin1)
#' m <- callback(labour1,"offer","hist","callback","all")
#' s <- stat_tcs(m)
#' graph(data=s,method="student")
#'
graph.stat_tcs <- function(data = NULL,
                           method = "wilson",
                           col = c("#F564E3", "#7CAE00", "#00BFC4"),
                           dif = TRUE,
                           ...) {
  if (dif == TRUE) {
    g_difp(data = data,
           method = method,
           col = col)
  } else {
    g_tcs(data = data, col = col)
  }
}


#'
#' Exclusive callback shares plots
#'
#' @param data a \code{stat_ecs} object.
#' @param method the type of confidence interval: "cp" for Clopper-Pearson,
#' "wilson" for Wilson (the default), "student" for Student.
#' @param col  A list of colors (three colors needed).
#' @param dif  TRUE for the difference in proportions (the default), FALSE for a
#' comparison of confidence intervals
#' @param ... further arguments passed to or from other methods.
#'
#' @return a ggplot2 object
#'
#' @author Emmanuel Duguet
#'
#' @export
#'
#' @examples
#' data(origin1)
#' m <- callback(labour1,"offer","hist","callback","all")
#' s <- stat_tcs(m)
#' graph(data=s,method="student")
#'
graph.stat_ecs <- function(data = NULL,
                           method = "wilson",
                           col = c("#F564E3", "#7CAE00", "#00BFC4"),
                           dif = TRUE,
                           ...) {
  if (dif == TRUE) {
    g_difp(data = data,
           method = method,
           col = col)
  } else {
    g_ecs(data = data,
          method = method,
          col = col)
  }
}
