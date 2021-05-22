---
layout : default
title : Setup Instructions
permalink: setup.html
---

# Setup Instructions

_This section is deprecated since setup should now be done via [Alire](https://alire.ada.dev)._
_It is kept here as guidance for people who do not want to use Alire._

This section is for people having problems setting up OpenGLAda in their
environment. Please not that while each guide has been tested at least once,
they are not tested continuously. If you encounter any error while following one
of these guides, please open an issue on GitHub.

The guides instruct you to build the test binaries. This is a good way of
testing whether you can link properly against the C libraries used.

The following guides are available:

 * [Windows](setup/windows.html)
   * [GNAT Community 2020](setup/windows.html#gnat-community-2020)
   * [TDM-GCC 64bit](setup/windows.html#tdm-gcc-64bit)
 * [macOS](setup/macos.html)
   * [GNAT GPL 2017](setup/macos.html#gnat-gpl-2017)

For Linux, consult the documentation of your package manager. Most Linux
distributions provide packages for GNAT and the optional dependencies. On
Debian, you need to install `gprbuild` as separate package.