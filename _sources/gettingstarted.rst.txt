Getting Started
===============

This quick start guide shows how to use the kOS-C compiler and how to write a
basic script. You should have some knowledge about programming already, ideally
in one of the languages that kOS-C is (loosely) based on. If you have not
installed the kOS-C compiler yet, now is a good time to do that. See
:ref:`Installation` for more information on that topic.

Step 1: Creating a new program
------------------------------

Create a file called ``Main.kc`` in a directory of your choice. This module will
serve as the main entry point of your program.

Every kOS-C module begins with a module header indicating its name. In this
case, the module's name is ``Main``, therefore you should start the file you
just created with the following line.

.. code-block:: c

                module Main;

The next part after the module header are import declarations, where we declare
which other modules we may use. In kOS-C, even the most basic types and features
are provided as a library. Therefore, any module usually contains at least one
import declaration for the ``KOS::Builtin`` module defining the core of the
language. We also import the ``KOS::IO`` module for printing text to the
terminal.

.. code-block:: c

                import KOS::Builtin unqualified;
                import KOS::IO as IO;


The trailing ``unqualified`` instructs the compiler to insert the names declared
in the imported module directly into the scope of the local module without
requiring any qualifying prefixes. This corresponds to the ``using namespace``
directive in C++.

The second import declarative uses the ``as`` syntax which provides the
identifiers of ``KOS::IO`` under the shorter name ``IO``.

The last part that is mandatory in a program is the ``Main`` function, which
provides the entry point of the generated program.

.. code-block:: c

                Void Main() {
                  IO::Print("Hello, World!");
                }

We have declared a function named ``Main`` with the return type ``Void`` taking
no arguments. The ``Void`` type is defined in ``KOS::Builtin`` and due to the
import declaration, the name is unqualified. In the function body, we are
calling the function ``Print`` declared in the module ``KOS::IO`` which we have
imported under the name ``IO``. It takes a string as its only argument and
prints it to the terminal. In fact, it corresponds directly to the ``PRINT``
statement in kOS scripts.

Step 2: Compiling the program
-----------------------------

Our next step is to compile the program. The following instructions assume that
the ``koscc`` executable is in a directory that is part of the system path.
Otherwise, you are required to supply the full path to the executable in every
command.

An important thing to note is that the interface to the functionality that is
natively provided by kOS is implemented in the ``kosc-prelude`` library that is
shipped together with the kOS-C compiler. In the following commands,
``<kosc-path>`` refers to the directory where this library resides.

Open a command prompt in the directory where you created the ``Main.kc`` file in
the previous step and execute::

    koscc -L <kosc-path>/kosc-prelude Main.kc

The compiler expects the path of the main module on the command line. Additional
libraries can be made available by passing their path to the ``-L`` option. A
list of all options can be printed by passing ``--help`` as an argument.

Executing the above command should output the generated kOS script code on the
terminal. It should look along the lines of::

    FUNCTION _0 {
      Print("Hello, World!").
    }

    _0().

As we can see, the compiler renamed our ``Main`` function while generating the
code for it. This is the default behavior because storage space on the kOS CPU
parts is usually quite scarce. Additionally, it allows different modules to
contain functions with the same names.

Since the ``Main`` function is the entry point of our program, the generated
script ends with a call to the generated function.

In most cases, it might be preferable to create a file with the generated code,
instead of printing it to the terminal. We can use the ``-o`` option for
specifying an output file::

    koscc -L <kosc-path>/kosc-prelude -o Main.ks Main.kc

This command creates a file named ``Main.ks`` in the current directory. You can
place this file into the kOS script folder and run it on a ship just like any
other kOS script file.

Step 3: Controlling a ship
--------------------------

The following program steers a ship skywards at full throttle until it runs out
of fuel. Then it waits until its close to the ground again before deploying the
parachutes.

.. code-block:: c

                module Main;

                import KOS::Builtin unqualified;
                import KOS::Cockpit as Cockpit;
                import KOS::Vessel as Vessel;

                Void Main() {
                  lock Cockpit::Throttle = 1;
                  lock Cockpit::Steering = Vessel::Ship.Up;
                  Cockpit::Stage();
                  wait until Vessel::Ship.AvailableThrust < 1;
                  unlock Cockpit::Throttle;
                  lock Cockpit::Steering = Vessel::Ship.SrfRetrograde;
                  wait until Vessel::Ship.Altitude < Vessel::Ship.GeoPosition.TerrainHeight + 1500;
                  Cockpit::Chutes = True;
                  unlock all;
                }

The main function starts by locking the throttle to 100% and the steering to the
upwards direction of the ship. This corresponds directly to the
``LOCK Throttle TO ...`` and ``LOCK Steering TO ...`` directives used in kOS
scripts, except that the identifiers are now declared in the module
``KOS::Cockpit`` and can therefore be accessed using a qualified name.

Besides throttle and steering, the cockpit module provides variables for
accessing all the knobs one would expect to find in the actual cockpit of a
vessel. This includes action groups, SAS, brakes, etc.

The following line activates the next stage. The current stage can be referred
to by ``KOS::Vessel::CurrentStage``.

If you have created vessel with a suitable staging setup, it should be heading
straight for the sky at this point.

The next line is the kOS-C equivalent of the ``WAIT UNTIL ... .`` kOS directive.
As soon as the ship loses thrust, we unlock the throttle and lock the steering
to the ship's retrograde direction relative to the surface.

Then we wait again until the vessel is 1500m above ground, at which point the
parachutes are deployed and the steering is unlocked.
