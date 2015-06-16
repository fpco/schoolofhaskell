FROM fpco/stack-ghcjs-build:lts-2.14
MAINTAINER Michael Sloan

RUN cabal update; cabal install ide-backend-rts
ADD soh-runner ide-backend-server /opt/soh/

EXPOSE 3000 4000
