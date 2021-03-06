## Test environments

* win-builder
* Ubuntu 14.04.5 LTS

## R CMD check results

0 errors | 0 warnings | 1 notes

* Maintainer: 'Chandler Armstrong <omni.armstrong@gmail.com>'

## Notes

* By default wunderscraper writes to stdout--it will write to disk only if the
user supplies a location to the 'o' parameter of 'scrape'.

* The wunderscraper examples require a 'Wunderground' API key.  Wunderground
terms of service do not allow API keys to be shared publically.  The
wunderscraper package will require an exception to CRAN policies that all
examples work as is; the examples will work only if users first obtain and set an
API key using the provided function, 'setApiKey'.  Attempting to run 'scrape'
without an API key will direct users to the 'Wunderground' API key sign-up page.
