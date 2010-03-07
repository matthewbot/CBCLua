CBCLua
======
By Matthew Thompson

Requirements
------------
* Lua 5.1.4
* LuaSocket
* Python
* WxPython
* [Chumby Toolchain][]
* Local GCC toolchain

Building
--------

	make             # builds a local install at local/
	make usbinstall  # builds a cbc install at cbclua/, then packages it into a usb installer inside usbinstall/
	make wifiinstall # builds a cbc install at cbclua/, then copies it via ssh to a networked cbc

Installation
------------

For a wifi install, just enter the CBC's IP address when prompted and the installation is performed automatically. For a usb install, you must copy robot.c and cbclua.tgz into the root of a USB drive, then mount it to compile and run robot.c. You must be sure to leave the thumb drive mounted in the file manager when you run robot.c.

Running
-------

Using the file manager, compile and run cbclua.c from the cbclua folder. Unlike a normal program, CBCLua is designed to be left running continually. When run, it acts like a daemon, and begins waiting for interaction connections. The black button can be used to start or stop the currently loaded Lua program without using interact. If you do use the E-Stop button, any active interaction connections are also terminated.

To run CBCLua on your local computer, simply run local/run.sh after first building a local build using make.

At the present, interact over a network connection is the only way to load a CBCLua program. This is expected to change after the Florida regional competition is over.

Interaction
-----------

The interaction utility, written in Python, lets you download, run, and interact with Lua programs. Its fairly simple to use, and very similar to IC's interaction. You can customize interaction by writing an interact.lua file in your program, which is automatically imported into an interact session, providing short hands or extra interaction utilities. You can also access the CBC's console display using the interact's console window, on the Window menu. You can push the D-pad/A/B buttons, as well as start and stop the Lua program and view its output just as it appears on the CBC screen. To download programs, use Download on the Program menu and select the folder containing all of your code. To reload the same directory after making a change, just use the Reload option.

Organization
------------

* cbclua -- holds the current cbc build
* local -- holds the current local build, which will use your system's Lua and LuaSocket.
* interact -- the interact utility. Run interact.py.
* usbinstall -- holds the cbc usb installer's robot.c and cbclua.tgz
* src/cmods -- cbclua modules written in C. This includes the cbc module, which wraps KIPR's userlib
* src/includes -- includes shared for all cmods
* src/mods -- cbclua modules written in Lua.
* src/stdmods -- standard lua modules that CBCLua needs to run. At present this is only luasocket's lua side.
* src/startup -- CBCLua startup scripts
* src/lua-5.1.4-arm -- Lua 5.1.4 with [LNum][] and [CoCo][] patches to run on the CBC.
* src/luasocket-2.0.2-arm -- LuaSocket for CBC

[Chumby Toolchain]: http://wiki.chumby.com/mediawiki/index.php/GNU_Toolchain
[LNum]: http://luaforge.net/projects/lnum/
[CoCo]: http://coco.luajit.org/

