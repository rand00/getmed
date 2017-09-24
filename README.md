getmed
======

Automate the transfer and sorting of media-files from mountable external devices, with a nice CLI.

## ! Work in progress
This is currently a work in progress to become a version 2. Mostly some minor changes are needed for `getmed` v2 to be done. New features includes:

* Realtime progress and stats during transfer.
* Colored CLI output.
* Handling of several devices and new config format.

## Why not use a bash script with rsync?

* With more program features, bash scripts get harder to reason about and you don't want to loose your images/videos. `getmed` is written in a statically typed functional language, OCaml, which gives a lot of safety in itself, but `getmed` is also designed with thorough error handling in mind.
* Rsync doesn't give you a realtime progress bar for all files being transferred - `getmed` also gives you a time estimate for when transfer is done. This is practical for when having a lot of videos to transfer.

## Howto

You supply settings in two ways:
1. A `.getmedrc` in your `$HOME` folder or current working directory. See `getmed --template-rc`.
2. Cmd-line arguments -- primarily for naming of the new folders
   to be created. See `getmed --help`.

.. A more thorough howto will come when `getmed` reaches v2. If you want to use `getmed` now - run `getmed --help`.

## Status of `getmed`:

* Getmed is currently only developed for GNU/Linux, but should support OSX.


