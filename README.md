R code to download all early day motions from the [UK House of Commons](http://www.parliament.uk/), and plot their cosponsorship networks.

Run `make.r` to replicate after checking the dependencies at the top of the script. You will probably have the time to drink at least twenty cups of tea before the first plot shows up: over 37,000 early motions were introduced since 1989, and UK Members of Parliament cosponsor them a lot.

Since the networks are extremely dense, you will need to use edge weights to set a threshold under which to discard edges before plotting. Using 'perfect matches' (edges that represent systematic cosponsorship between a sponsor and a primary sponsor) works fine, as shown in [these plots](http://f.briatte.org/parlviz/edm):

[![](http://f.briatte.org/parlviz/edm/plots/net_1997.jpg)](http://f.briatte.org/parlviz/edm)

The code in `functions.r` was written for the [`parlnet`](https://github.com/briatte/parlnet) project.

Inspired by, but not related to, [Michael Kellermann](http://mkellermann.org/)'s research.
