FROM debian:stable AS build

# You can set the GHC version via a docker command line argument: --build-arg GHC_VERSION=9.4.6
ARG GHC_VERSION=9.2.8
ENV GHC_VERSION=${GHC_VERSION} \
    PATH="/root/.ghcup/bin:$PATH"

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential bash curl ca-certificates libffi-dev libgmp-dev \
    libgmp10 libncurses-dev libncurses5 libtinfo5 libnuma-dev

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh && \
    ghcup install ghc --set ${GHC_VERSION} && \
    ghcup install cabal recommended && \
    cabal update

# Maybe replace by a svn checkout
COPY src /workdir/src  
COPY mbt-server.cabal CHANGELOG.md /workdir/
WORKDIR /workdir
RUN cabal install

FROM httpd

COPY ./my-httpd.conf /usr/local/apache2/conf/httpd.conf
COPY ./html/index.html /usr/local/apache2/htdocs/
COPY ./html/index1.html /usr/local/apache2/htdocs/
COPY ./html/index2.html /usr/local/apache2/htdocs/
COPY ./html/index3.html /usr/local/apache2/htdocs/


COPY ./src/buggys /usr/local/apache2/cgi-bin/
COPY ./src/rules /usr/local/apache2/cgi-bin/
COPY ./src/par0 /usr/local/apache2/cgi-bin/
COPY ./src/par12 /usr/local/apache2/cgi-bin/
COPY ./src/par3 /usr/local/apache2/cgi-bin/
COPY --from=build "/root/.cabal/bin/mbt-server" /usr/local/apache2/cgi-bin/mbt-server.cgi
