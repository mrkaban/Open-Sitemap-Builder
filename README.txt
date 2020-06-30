Simple Sitemap Creator

A simple app that crawls a website and creates HTML and Google Sitemaps
compatible XML sitemap.

Simply extract the archive and run!

Any feedback is greatly welcome please visit my website at
http://www.matthewhipkin.co.uk

Or find me on twitter @hippy2094

Bug tracker can be found on sourceforge project page at
https://sourceforge.net/projects/simplesitemapcreator/

Changelog
---------
1.3.4	Fixed custom useragents being ignored
	Initial HTTP to HTTPS redirects are now followed
1.3.3	Fixed startup state spanning monitors
	Improved UI layout
1.3.2   Fixed blank useragent
        Added XiControls to about dialog
        Rewrote multithreading so it's done properly
        Fixed parsing issues with links starting ./
1.3.1   Added List tab which displays found links as a simple list
        Improved overall UI responsiveness and crawl speed
1.2.2	UI improvements and fixes
1.2.1   Improved speed related issues caused by 404 checking
1.2	Fixed update notification click
	Stopped 404s being parsed and listed
	Resolved issue with parsing of root links with filenames
1.1	Fixed saving of useragent history
	Fixed saving of URL history
	Attached return key to URL textbox
1.0     Recreated project files based on Lazarus 1.5 format
        Added option to set useragent
        Added option to disable custom theme
        Added option to change editor font
        Added option to customise filetypes to be ignored
        Changed ReadMe to DOS format
0.1.9   Fixed HTTPS bug
0.1.8   More improvements to HTML parsing
        Added CSV output
        Fixed issues with anchor links
	Resolved some issues with relative links
0.1.7   Fixed problem of parsing links that contain anchors
        Added ignore for skype: links
        Added simple sort
        Now uses <title></title> tag content for link content
0.1.6	Fixed inclusion of lone anchors in output
	Fixed last known HTML parsing bugs
	Removed alpha status
0.1.5	Improved HTML parsing again
0.1.4	Added XML output
0.1.3	General bug fixes
0.1.2	UI improvements
0.1.1	Improved HTML parsing
0.1	First release

