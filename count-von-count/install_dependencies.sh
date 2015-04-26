#! /bin/bash
cabal sandbox init
mkdir external_sources

# get recent version of ekg
git clone https://github.com/tibbe/ekg.git external_sources/ekg
cabal sandbox add-source external_sources/ekg

# patch protocol buffers
cabal get protocol-buffers-2.0.17 -d external_sources
sed -i '33s/import Data.List$/& hiding (uncons)/' \
    external_sources/protocol-buffers-2.0.17/Text/ProtocolBuffers/Identifiers.hs
cabal sandbox add-source external_sources/protocol-buffers-2.0.17

# install
cabal install --only-dependencies --reorder-goals $@
