#!/usr/bin/env bash
#
# This script installs and setups everything necessary for the dj rating
# system. It assumes you have root access.
#
# The sms inbox and sent folder can be found in resp. /var/spool/gammu/inbox/
# and /var/spool/gammu/sent
#
# More information can be found in the following resources.
# - http://wammu.eu/docs/manual/smsd/config.html
# - http://wammu.eu/docs/manual/smsd/dbi.html

set -o errexit
trap 'echo "Previous command did not complete successfully. Exiting." >&2' ERR


echo "Installing prerequisites..."
apt-get update
apt-get install git gammu-smsd sqlite3 libdbi1 libdb-dev libdbi-perl libdbd-sqlite3 haskell-platform

echo "Setting gammu-smsd config file..."
cat > /etc/gammu-smsdrc <<EOF
# Configuration file for Gammu SMS Daemon

# Gammu library configuration, see gammurc(5)
[gammu]
port = /dev/ttyS0
connection = fbus
model = 3310

# SMSD configuration, see gammu-smsdrc(5)
[smsd]
service = sql
driver = sqlite3
DBDir = /var/spool/gammu
Database = db.sqlite
logfile = syslog
debuglevel = 255

# Paths where messages are stored
inboxpath = /var/spool/gammu/inbox/
outboxpath = /var/spool/gammu/outbox/
sentsmspath = /var/spool/gammu/send/
errorsmspath = /var/spool/gammu/error/
EOF

echo "Setting up database"
if [[ ! -f /var/spool/gammu/db.sqlite ]]; then
	sqlite3 -init /usr/share/doc/gammu-smsd/examples/sqlite.sql /var/spool/gammu/db.sqlite
	chown -R gammu:gammu /var/spool/gammu
	chmod 777 /var/spool/gammu
fi

echo "Starting Gammu SMS Daemon"
service gammu-smsd start

echo "Installing Haskell prerequisites"
cabal update
cabal install sqlite-simple regex-pcre-builtin erf

echo "Cloning 12UL repo"
cd ~
[[ ! -d ~/12Urenloop ]] && git clone https://github.com/ZeusWPI/12Urenloop.git

echo "Running dj-ratings.hs"
cd 12Urenloop/tools/dj-ratings
ghc dj-ratings.hs
./dj-ratings
