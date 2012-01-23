12UrenLoop
==========

People run laps. We count, give them points and monitor it all.

count-von-count
---------------

See `count-von-count/README.markdown`

pokemon
-------

Demo application that generates detection events and sends them to a
count-von-count process.

tools/data-analysis
-------------------

Checks the gathered data for inconsistencies.

tools/monitor.rb
----------------

Runs a series of checks on each monitored host.

tools/macalive.sh
-----------------

Checks if a bluetooth device is still alive using the hcitool command. Used
to assess the lifetime of our batons.
