getmed
======

Get media from external devices, automatized and CLI.

## Motivation for writing `getmed`

#### The tool-oriented reasons
* Getting a nice to use CLI tool for transferring images/videos from
media-devices.
  * Getting useful realtime CLI output about the transfer-progress as
  an alternative to a bash-script using `cp` or `rsync` - where the
  'time remaining' or a useful progress bar is not output
  (an earlier simple bash version of getmed used `rsync`). 
* Automated handling of already defined devices.
* A safe way to transfer files - no overwriting - no deletion of
un-transferred files.
* A practical way to sort different types of media-files into seperate
destination directories.

#### The other reasons
* Testing new functional design-patterns in OCaml.
  * Testbed for bigger future programs.
* Experience with interfacing with the unix-system from OCaml.
  * Testing OCaml as a better alternative to bash-scripting.
* Getting to know the Batteries Included library for OCaml, and writing
code that makes use of some of it's interesting traits.

*More documentation on the way.*
