# 99veto
A command-line utility that analyzes a team's map-veto behavior in the 99damage-league. 

Version: 0.3.0.0

## Installation
First, you need to install [Stack](https://docs.haskellstack.org/en/stable/README/)
in order to build the project. Then clone this repository into a local
directory and run stack. The build-process takes about 10-25 minutes, if you
don't have any of the dependencies installed.

``` sh
git clone https://github.com/linus4/99veto.git
cd 99veto
stack build
```

## Usage
You need to provide the URL to a team's page on 99damage.de . Execute in the
project directory.
``` sh
stack exec 99veto [URL]
```

## Options & Switches
* -- help : displays a usage message


## TODO
* add functionality to deal with new mappool
* break up the handleArgs function into smaller ones
* refactor the code by providing library functions in src and putting them to 
  use in app; split into multiple modules
* write a test-suite
* find a better solution to accumulate and count the vetos
* add an argument parser (help, number of matches)
* use Either instead of Maybe in Lib to keep error messages
* find a solution for nested case Nothing / Just pyramid (transformer)
* maybe use Text to improve performance
* rename Veto data type (Veto_Veto3...)?
* use mapMaybe to discard matches without a veto?
* catch execption when the user supplies an invalid URL
* analyze ram usage (600 MB) / laziness -- later
* size of executable (28 MB)?
* package the app
