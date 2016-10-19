My customized emacs
=====================

## Structure

- init.el: This file will define some function helpers and load all files
  config.
- tspyimt-pkg.el is file to define and install required packages
- load-pkg.el is file to require and config all packages installed.
- settings.el is file to set default config of Emacs.
- env.el is file responsible about theme and environment.
- keybindings.el is file to config key binding for all modes.


### How to install


### Features

- Unicode symbol mode (This feature was forked from
  [Xah Lee's repo](https://goo.gl/KIKomo) and customized)
  
  
I have a shell script to start emacs named `e` to start emacs. To start emacs
with other config file, you can type: `EMACS_INIT="path_to_your_config e"`


### Todo

- Update the installation
- Update documentations for each feature (how to use and customized)
