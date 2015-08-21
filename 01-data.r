r = "http://www.parliament.uk"
l = "edm-list.csv"
a = "edm-sponsors.csv"

# ==============================================================================
# PARSE EDM INDEXES
# ==============================================================================

if (!file.exists(l)) {

  d = data_frame()

  for (i in 2015:1989) {

    cat("Listing EDMs for year", i, "")
    h = substr(i, 3, 4) %>% as.integer
    h = paste0(r, "/edm/", i, "-", str_pad(h + 1, 2, pad = "0") %>% str_sub(-2), "/by-number")

    h = html(h)

    d = rbind(d, data_frame(
      num = html_nodes(h, "#number-list tbody td:first-child") %>% html_text,
      title = html_nodes(h, "#number-list tbody td:nth-child(2)") %>% html_text,
      url = html_nodes(h, "#number-list tbody td:nth-child(2) a") %>% html_attr("href"),
      n_sig = html_nodes(h, "#number-list tbody td:nth-child(4)") %>% html_text
    ))

    # paginate
    p = 1
    n = html_nodes(h, ".pagination a") %>% html_attr("href") %>% unique
    n = n[ grepl(paste0("?page=", p), n) ]

    while (length(n) > 0) {

      cat(".")

      h = html(paste0(r, n))
      d = rbind(d, data_frame(
        num = html_nodes(h, "#number-list tbody td:first-child") %>% html_text,
        title = html_nodes(h, "#number-list tbody td:nth-child(2)") %>% html_text,
        url = html_nodes(h, "#number-list tbody td:nth-child(2) a") %>% html_attr("href"),
        n_sig = html_nodes(h, "#number-list tbody td:nth-child(4)") %>% html_text
      ))

      # paginate
      p = p + 1
      n = html_nodes(h, ".pagination a") %>% html_attr("href") %>% unique
      n = n[ grepl(paste0("?page=", p), n) ]

    }

    cat("\n")

  }

  d$session = gsub("/edm/(.*)/(.*)", "\\1", d$url)
  d$num = gsub("/edm/(.*)/(.*)", "\\2", d$url)
  d$title = str_clean(d$title)
  d$n_sig = as.integer(d$n_sig)
  table(d$n_sig > 1)

  write.csv(select(d, session, everything()) %>%
              arrange(session, num), l, row.names = FALSE)

}

# ==============================================================================
# DOWNLOAD EDM PAGES
# ==============================================================================

d = read.csv(l, stringsAsFactors = FALSE)

cat("Downloading", nrow(d), "EDMs...\n")
pb = txtProgressBar(0, nrow(d), style = 3)

for (i in d$url %>% unique) {

  f = paste0(gsub("/", "-", gsub("#(.*)", "", i)), ".html")
  f = gsub("^-", "raw/", f)

  if (!file.exists(f))
    try(download.file(paste0(r, i), f, mode = "wb", quiet = TRUE), silent = TRUE)

  if (!file.info(f)$size)
    file.remove(f)

  setTxtProgressBar(pb, which(d$url == i))

}

cat("\n")

# ==============================================================================
# PARSE EDM SPONSORS
# ==============================================================================

if (!file.exists(a)) {

  s = data_frame()

  # only parse early day motions (skipping amendments)
  for (i in list.files("raw", full.names = TRUE)) {

    h = html(i)
    p = html_node(h, ".summary a") %>% html_text # primary sponsor
    t = html_node(h, ".signatories-list")
    if (length(t) > 0) {

      t = html_table(t)
      t$R = grepl("\\[R\\]", t$Name)
      t$Name = gsub("\\s\\[R\\]", "", t$Name)
      t$Status = ifelse(t$Name == p, "Primary", "Cosponsor")

      if (any(t$Status == "Primary")) {# make sure there is a primary sponsor
        s = rbind(s, cbind(EDM = i, t))
      } else {
        cat(i, ": primary sponsor not in table\n")
      }

    }

  }

  write.csv(s, a, row.names = FALSE)

}

s = read.csv(a, stringsAsFactors = FALSE)

cat("Collected", n_distinct(s$EDM), "EDMs.\n")

s$Legislature = str_extract(s$EDM, "\\d{4}") %>%
  as.integer %>%
  cut(c(1987, 1992, 1997, 2001, 2005, 2010, 2015, 2020), right = FALSE)

# remove special case: Frank Doran is listed twice, as Ab. South and Ab. Central
s = s[ -which(s$Legislature == "[1987,1992)" &
                s$Constituency == "Aberdeen Central" &
                s$Name == "Doran, Frank"), ]

# Northern Ireland
s$Party[ s$Party == "Alliance Party" ] = "NI-ALL"
s$Party[ s$Party == "Democratic Unionist Party" ] = "NI-DUP"
s$Party[ s$Party == "Social Democratic and Labour Party" ] = "NI-SDLP"
s$Party[ s$Party == "Ulster Unionist Party" ] = "NI-UUP"
s$Party[ s$Party == "United Kingdom Unionist Party" ] = "NI-UKUP"

# Wales
s$Party[ s$Party == "Plaid Cymru" ] = "W-PLAID"

# Scotland
s$Party[ s$Party == "Scottish Labour Party" ] = "S-LAB"
s$Party[ s$Party == "Scottish National Party" ] = "S-SNP"

# England
s$Party[ s$Party == "UKIP" ] = "UKIP"
s$Party[ s$Party == "Conservative Party" ] = "CON"
s$Party[ s$Party == "Liberal Democrats" ] = "LD"
s$Party[ s$Party == "Labour Party" ] = "LAB"
s$Party[ s$Party == "Green Party" ] = "GP"
s$Party[ s$Party == "Respect" ] = "R"

# unaffiliated
s$Party[ s$Party %in% c("INDEPENDENT", "No party affiliation") ] = "IND"
s$Party[ s$Party == "Independent Conservative" ] = "IND"
s$Party[ s$Party == "INDEPENDENT LABOUR" ] = "IND"
