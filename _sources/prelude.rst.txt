Prelude Library
===============

.. todo:: This page is a stub. It is supposed to document the modules in the
   ``kosc-prelude`` library.

.. note:: In general, the kOS-C prelude replicates the structure hierarchy and
          variable names used by kOS in order to allow for a smooth transition.
          There are a few exceptions in cases where peculiarities of the kOS
          scripting language cannot be modeled 1:1 in kOS-C due to restrictions
          in the syntax or static checking. Examples are the ``PrintAt``
          function that wraps the ``PRINT ... AT (x, y).`` syntax used in kOS
          scripts and the name ``CurrentStage`` for referring to the current
          stage (just ``Stage`` in kOS). The latter is because kOS-C does not
          allow to use the same name for different things, whereas in kOS
          scripts, ``Stage`` either refers to a variable holding information
          about the current stage, or to the staging command (as in ``Stage.``).
