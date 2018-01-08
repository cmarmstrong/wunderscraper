## Test environments

## R CMD check results

## Notes

* By default wunderscraper writes to stdout--it will write to disk only if the
user supplies a location to the 'o' paramter of 'scrape'.

* The wunderscraper examples use a hard coded 'Wunderground' API key.  The key
is valid but restricted to a daily and minute usage limit, and I cannot
guarantee that the usage limits will not be exceeded at any given point in time.
If that happens, then users may find that the examples do not work.  In such an
event, however, 'wunderscraper' will print a helpful message and encourage users
to sign up for a free API key that they may use for running the examples.
