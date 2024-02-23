FROM debian:stable AS build

# You can set the GHC version via a docker command line argument: --build-arg GHC_VERSION=9.4.6
ARG GHC_VERSION=9.4.8
ENV GHC_VERSION=${GHC_VERSION} \
    PATH="/root/.ghcup/bin:$PATH"

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential bash curl ca-certificates libffi-dev libgmp-dev \
    libgmp10 libncurses-dev libncurses5 libtinfo5

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh && \
    ghcup install ghc --set ${GHC_VERSION}

# Maybe improve with a svn checkout
COPY src /workdir/src  
COPY mbt-server.cabal CHANGELOG.md /workdir/
WORKDIR /workdir
RUN cabal install --installdir=/workdir/

FROM httpd

COPY ./my-httpd.conf /usr/local/apache2/conf/httpd.conf

COPY ./html/index.html /usr/local/apache2/htdocs/
COPY ./html/index0.html /usr/local/apache2/htdocs/
COPY ./html/index1.html /usr/local/apache2/htdocs/
COPY ./html/index2.html /usr/local/apache2/htdocs/
COPY ./html/index3.html /usr/local/apache2/htdocs/
COPY ./html/index4.html /usr/local/apache2/htdocs/
COPY ./html/index10.html /usr/local/apache2/htdocs/
COPY ./html/index11.html /usr/local/apache2/htdocs/
COPY ./html/index12.html /usr/local/apache2/htdocs/
COPY ./html/index13.html /usr/local/apache2/htdocs/
COPY ./html/index14.html /usr/local/apache2/htdocs/

COPY ./html/tutor.css /usr/local/apache2/htdocs/
COPY ./html/media /usr/local/apache2/htdocs/media

COPY ./src/buggyRules /usr/local/apache2/cgi-bin/buggyRules
COPY ./src/parameters /usr/local/apache2/cgi-bin/parameters


COPY --from=build "/workdir/mbt-server" /usr/local/apache2/cgi-bin/mbt-server.cgi
