for (j in unique(s$Legislature) %>% rev) {

  sp = filter(s, Legislature == j) %>% select(EDM:Constituency, Status)
  y = unique(sp$EDM)

  cat("\nYears", j, length(y), "EDMs\n")

  # ============================================================================
  # SOLVE DUPLICATES
  # ============================================================================

  a = sp[, 2:4 ] %>%
    group_by(Name) %>%
    arrange(Name) %>%
    mutate(n_p = n_distinct(Party), n_c = n_distinct(Constituency))

  stopifnot(a$n_c == 1) # single constituencies

  # select party with most sponsorships
  for (i in unique(a$Name[ a$n_p > 1 ])) {

    t = table(a$Party[ a$Name == i ])
    cat("Duplicate party:", i, "[", paste0(names(t), collapse = ", "))
    t = names(which.max(t))
    cat("; using", t, "]\n")
    a = a[ -which(a$Name == i & a$Party != t), ]

  }

  a = unique(a[, 1:3 ]) %>% data.frame
  rownames(a) = a$Name

  stopifnot(!duplicated(a$Name)) # duplicates solved
  stopifnot(sp$Name %in% a$Name)  # sanity check

  # ============================================================================
  # SPONSOR EDGE LIST
  # ============================================================================

  pb = txtProgressBar(0, length(y), style = 3)

  edges = list()
  for (i in y) {

    edges[[i]] = expand.grid(i = unique(sp$Name[ sp$EDM == i ]),
                             j = sp$Name[ sp$EDM == i & sp$Status == "Primary" ],
                             w = length(sp$Name[ sp$EDM == i ]) - 1,
                             stringsAsFactors = FALSE)

    setTxtProgressBar(pb, which(y == i))

  }
  edges = bind_rows(edges)

  # ============================================================================
  # EDGE WEIGHTS
  # ============================================================================

  # first author self-loops, with counts of cosponsors
  self = filter(edges, i == j)

  # count number of bills per first author
  n_au = table(self$j)

  # remove self-loops from directed edge list
  edges = filter(edges, i != j)

  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)

  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")

  # raw edge counts
  raw = table(edges$ij)

  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)

  # expand to edge list
  edges = data_frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w)

  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w

  # sanity check
  stopifnot(edges$gsw <= 1)

  # final edge set: cosponsor, first author, weights
  edges = select(edges, i, j, raw, nfw, gsw)

  cat("\n", nrow(edges), "edges, ")

  # ============================================================================
  # DIRECTED NETWORK
  # ============================================================================

  n = network(edges[ edges$gsw == 1, 1:2 ], directed = TRUE)

  # ============================================================================
  # VERTEX-LEVEL ATTRIBUTES
  # ============================================================================

  n_au = as.vector(n_au[ network.vertex.names(n) ])

  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)

  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)

  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"

  cat(network.size(n), "nodes\n")

  n %v% "constituency" = a[ network.vertex.names(n), "Constituency" ]
  n %v% "party" = a[ network.vertex.names(n), "Party" ]

  set.edge.attribute(n, "source", as.character(edges[ edges$gsw == 1, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[ edges$gsw == 1, 2])) # first author

  set.edge.attribute(n, "raw", edges$raw[ edges$gsw == 1 ]) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw[ edges$gsw == 1 ]) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw[ edges$gsw == 1 ]) # Gross-Shalizi weights

  save_plot(n, paste0("net_", substr(j, 2, 5)),
            i = colors[ a[ n %e% "source", "Party" ] ],
            j = colors[ a[ n %e% "target", "Party" ] ],
            "fruchtermanreingold", colors)

  assign(paste0("edges_", substr(j, 2, 5)), edges)
  assign(paste0("net_", substr(j, 2, 5)), n)

}

save(list = ls(pattern = "^(edges|net)_\\d{4}"), file = "edm.rda")
