getmed
======

Get media from known external devices, automatized by a config-file and
cmd-line interface.

## What is `getmed` for?

* Getting a practical CLI tool for transferring images/videos from
  known media-devices - especially from camera flash-cards.
* Getting a more useful realtime CLI progress-bar for transfers, as
  an alternative to a bash-script using `cp` or `rsync`. 
* Automated handling of already defined devices.
* A safe way to transfer files, so mistakes are not made in a fast-paced
  workflow; no overwriting, or deletion of un-transferred files.
* A practical way to sort different types of media-files into seperate
  pre-defined destination directories.

## How does `getmed` function?

There are two ways you define settings for an upcoming transfer of files:
1. A `.getmedrc` in your `$HOME` folder or current working directory.
2. Cmd-line arguments -- primarily for naming of the new folders
   to be created.

Right now the config-format supports defining the following options:
* mount_path : There the device should be mounted
* device_path : The folder in the mounted device to be searched for files.
* image_to_path : The root-folder for the images to be transferred.
* video_to_path : The root-folder for the videos to be transferred.
* append_title : The title to append to the folder to be created in the
    corresponding root-media-directory.
* remove_media : *(boolean)* Remove media from device after transfer.
* unmount : *(boolean)* Unmount device after transfer.

A new extended config-format, with support for all deviced that has a
label or uuid, is bein developed. Also most other aspects will be
customizable - e.g. image/video file extensions to look for, or
user-creation of new formats, e.g. documents. 

## Status of `getmed`:

* Getmed is now functional for Canon EOS* camera-cards - a more extensive
  config-format for `getmed` is on the way, which e.g. will make it possible
  to define several device-settings in the same config.
* Getmed is currently only developed for GNU/Linux, but should easily be
  portable to OSX.


*More documentation to come.*
