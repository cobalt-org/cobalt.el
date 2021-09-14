# Cobalt.el
[![Build Status](https://travis-ci.org/cobalt-org/cobalt.el.svg)](https://travis-ci.org/cobalt-org/cobalt.el)
[![MELPA](https://melpa.org/packages/cobalt-badge.svg)](https://melpa.org/#/cobalt) 
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
[![Join the chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/cobalt-org/cobalt.rs)


Cobalt.el is an Emacs interface for [Cobalt.rs](https://github.com/cobalt-org/cobalt.rs), a static site generator written in Rust.

The package provides simple-to-use Emacs commands for easy site generation and post management.

## Dependencies

The latest version of [Cobalt.rs](http://cobalt-org.github.io/getting-started/) should be installed on your machine and should be found in your Emacs' `exec-path`.

## Installation

Download the package from [Melpa](https://melpa.org/#/cobalt) or by cloning this repository. Then add the following to your init file:

```emacs-lisp
(require 'cobalt)
```

## Configuration

Create a cobalt site by calling `M-x cobalt-init`. This will ask you for a directory to create the new site in.

Add the path of the newly created site to `cobalt-site-paths` as shown below:

```emacs-lisp
(setq cobalt-site-paths '("~/path/to/cobalt/site/" "~/path/to/second/cobalt/site"))
```

Finally, call `M-x cobalt-change-current-site` to set the current site. This is the directory where all cobalt.el commands will run on.

## Usage

### Previewing Site

Call `C-u M-x cobalt-serve` to serve your site _(including drafts)_ at `127.0.0.1:3000`. Remove the `C-u` if you don't want the drafts to be served.

Call `M-x cobalt-preview-site` to automatically open the current site on a browser.

Call `M-x cobalt-serve-kill` to kill the previously created serve process.

You can change the port to serve from by setting `cobalt-serve-port` like so:

```emacs-lisp
(setq cobalt-serve-port 3003)
```

### Creating Posts

Call `cobalt-new-post` to create a new post at the current site. It will ask for the title of the post which will create the necessary `.md` file and buffer. A title like _"This is a title: Part 1"_ would create a file with the name _this-is-a-title-part-1.md_.

### Previewing Posts

Save the post buffer and call `M-x cobalt-preview-post` to open the current post in a browser.

### Publishing Posts

Publish the current post buffer by calling `M-x cobalt-publish-post`. It will update the current buffer and remove the `is_draft: true` metadata. 

### Building the Site

Build the site by calling `M-x cobalt-build`. This will generate the necessary files at the destination specified in the `_cobalt.yml` file.

### Switching Sites

You can switch to other sites you've set in `cobalt-site-paths` by calling `M-x cobalt-change-current-site`. All functions that are called will apply to the new site you switch to. Previously started `cobalt-serve-process` will also be killed so you'll have to call it again to start a new _serve process_.

## Contributing

The package is still under development but issues and pull-requests are always welcome!

### Testing

The package uses the excellent [Cask Package Toolset](https://github.com/AdrieanKhisbe/cask-package-toolset.el). Check out the page to find out how to run the tests.

