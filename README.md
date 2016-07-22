# Ocaloud

This is a web application in early stages of development. It uses OCaml and Ocsigen. This can be seen as an experiment to see wether it is possible or not to build some kind of platform such as Owncloud or Cozycloud with a functional language and funny technologies.
The main focus is to build a lightweight key value store that can be synced between different instances of the application, and reuse that across most sub-applications.

# Install

The recommended way to install dependencies is using [opam](http://opam.ocaml.org/).

```
opam install eliom sqlite3 sha react inotify bitstring irc-client ssl
```

(Some of this dependecies are not needed anymore or not needed yet, we will need to figure out how this list can be limited.)

At this moment it needs a custom irc-client (to support ssl for irc, connecting whithout it is not reasonable).

```
opam pin add -k git irc-client https://github.com/xapantu/ocaml-irc-client/
```
