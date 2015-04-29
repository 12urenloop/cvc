#!/usr/bin/env bash
#
# Shows the amount of votes received so far.

sqlite3 /var/spool/gammu/db.sqlite 'select count(*) from inbox;'
