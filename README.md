#semeion

A DynDNS Server in Haskell and Yesod

##Introduction

This is a work in Progress interface for the [bind9](http://www.isc.org/downloads/bind/) nameserver daemon, which aims to create your own dynamic dns.

*!!This is still work in progress!!*

## Development

- Get cabal, yesod-bin, happy, alex

```
cabal install yesod-bin happy alex
```

- Clone the repo

```
$ git clone https://github.com/nek0/semeion.git
```

- Initialize sandbox:

```
$ cd semeion && cabal sandbox init && cabal install --only-dependencies
```

- Run application

```
$ yesod devel
```
