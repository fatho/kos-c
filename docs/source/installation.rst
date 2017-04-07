.. _Installation:

Installation
============

Binary releases
---------------

The latest binary releases of the **kOS-C** compiler for Windows and Linux can
be obtained from the `GitHub repository
<https://github.com/fatho/kos-c/releases/latest>`__. The zip files contain the
compiler binary itself as well as a "prelude" library that contains the bindings
to kOS and some common functionality that might be useful.

Be aware that the Linux binaries are compiled on Arch Linux and might not work
on other distributions. Moreover, I can only provide 64 bit builds at the
moment. If the binaries are incompatible with your system, proceed to the next
section in order to learn how to compile **kOS-C** yourself.

.. _buildinstructions:

Build Instructions
------------------

The compiler is written in Haskell, but since the project is compatible with
`Stack <https://www.haskellstack.org>`__, it should be fairly easy to compile.
Assuming that stack has been installed according to the linked documentation,
the following steps need to be done.


1. Clone the **kOS-C** source code from the `GitHub repository <https://github.com/fatho/kos-c>`__.

   * The ``master`` branch contains the source code corresponding to the latest release.

   * The ``develop`` branch contains the current development version of **kOS-C**, and therefore might be less usable and contain more bugs.

   * There are tags corresponding to every release so far.

2. Open a terminal/command prompt and change into the root directory of the
   repository cloned in the first step.

3. If you never used Haskell before, or do not have GHC [#ghc]_ 8.0.2 installed
   and configured stack to use it, you can install it by executing ``stack
   setup``. Stack will install GHC into a local directory where it will not
   interfere with any system wide installations.

4. Once GHC is installed, you can issue ``stack build`` in order to build **kOS-C**.

5. If everything went well, an executable named ``koscc`` (the **kOS-C Compiler**) can
   now be found in a sub-directory of ``.stack-work/install/`` whose exact name
   depends on the system architecture and operating system.

   * Optionally, you can use ``stack install`` in order to let stack copy the
     executable into the local user's binary directory. On Linux, that is usually
     ``~/.local/bin``, on Windows ``%APPDATA%\local\bin``.

   * Another option is to use the command ``stack exec koscc`` to run the
     generated executable from within the project directory. Any arguments that
     are meant for **kOS-C** must be separated with two dashes (``--``) from the
     rest of the command line, i.e. ``stack exec koscc -- arguments go here``.

.. [#ghc] `The Glasgow Haskell Compiler <https://www.haskell.org/ghc/>`__
