# 99veto
A command-line utility that analyzes a team's map-veto behavior in the 99damage-league. 

Version: 0.1.0.0

## Installation
First, you need to install [Stack](https://docs.haskellstack.org/en/stable/README/)
in order to build the project. 99veto depends on libcurl, so you need to install
a **development-version** of it. e.g. on Ubuntu:

``` sh
sudo apt-get install libcurl4-openssl-dev
```

or on Fedora:

``` sh
sudo dnf install libcurl-devel
```

Then clone this repository into a local directory and run stack. The
build-process takes about 10-25 minutes, if you don't have any of the
dependencies installed.

``` sh
git clone https://github.com/linus4/99veto.git
cd 99veto
stack build
```

## Usage
You need to provide the URL to a team's page on 99damage.de . Execute in the
project directory.
``` sh
stack exec 99veto-exe [URL]
```

## Options & Switches
* -- help : displays a usage message

## Dependencies
* libcurl

## TODO
* write a test-suite
* break up the handleArgs function into smaller ones
* refactor the code by providing library functions in src and putting them to 
  use in app; split into multiple modules
* make it so that matches are only downloaded when there are not enough more
  recent ones with map-vetos
* find a better solution to accumulate and count the vetos
* rename Veto data type (Veto_Veto3...)?
* use a datatype Team (Name, Tag, Seasons)?
* find a solution for nested case Nothing / Just pyramid
