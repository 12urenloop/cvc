12UrenLoop
==========

People run laps. We count and shit like that.

count-von-count
---------------

Backend. Receives bluetooth measurements from another component, and analyzes
the data in order to count laps.

### Installing & running

Install [cabal-install](http://www.haskell.org/cabal/), the easiest way is to
use your platform's package manager.

Compile `count-von-count`:

    cabal configure
    cabal build

Installation (optional):

    cabal install

Installation is not strictly necessary, you can also run it as

    ./dist/build/count-von-count/count-von-count

Run the tests:

    runghc -isrc -itests tests/TestSuite.hs

### Configuration

All configuration is done in the `config.yaml` file.

dr.beaker
---------

Presents the data analyzed by `count-von-count` in a nice fashion.

### Installing and running

You'll need maven to build Dr. Beaker. You can get it using your package manager of choice.

Create the mysql database "score", accessible by user "score" and password "score"

Compile `Dr. Beaker` from the project root dir

    mvn build

Download glassfish from http://glassfish.java.net/ , extract it, and start it by running 

    $GLASSFISH_HOME/bin/asadmin start-domain domain1

Install Dr. Beaker on the app-server

 * Go to http://localhost:4848
 * Navigate to "Applications" in the menu on the left
 * Choose "Deploy"
 * upload the dr.beaker.war file (located in ./target/ )

Create the authentication realm

 * Go to http://localhost:4848
 * Navigate to "Configuration > Security > Realms"
 * Create a new realm with following specifications: (fields not mentioned are empty)
    + Name = 12UrenLoopRealm
    + classname = ...JDBCRealm
    + JAAS Context = jdbcRealm
    + JNDI = jdbc/score
    + User Table = USERS
    + User Name Column = USERNAME
    + Password Column = PASSWORD
    + Group Table = USERS_GROUPS
    + Group Name Column = NAME
    + Digest Algorithm = SHA1
    + Encoding = Base64

Dr. Beaker is now available at http://localhost:8080/dr.beaker/

### API

- `PUT /api/0.1/{mac}/laps/increase`: Add a lap for a mac address
Parameters (more might be added later)
    + speed (double): average m/s
    + suspicious (boolean): true if the lap is suspicious
