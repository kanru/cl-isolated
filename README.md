Eval-bot
========

**An IRC bot for Common Lisp code evaluation**


Introduction
------------

_Eval-bot_ is an Internet Relay Chat robot program (a bot) which aims to
help discussions related to the Common Lisp language. The bot sits on an
IRC channel and can evaluate Common Lisp expressions and send their
return value to IRC channel or user.

The bot program is implemented in the Common Lisp language. It must be
used with an implementation that supports threads through
[Bordeaux Threads][BT] library. Minor parts in the bot's start and
setting scripts use features specific to [SBCL][] implementation.

[BT]:   http://common-lisp.net/project/bordeaux-threads/
[SBCL]: http://www.sbcl.org/


IRC
---

Common Lisp package `EVAL-BOT` contains the IRC part of the program.
Function `make-client` creates a client object which is used with
IRC-related functions, such as `irc-connect`, `irc-join`, `irc-quit`
etc.


Sandbox
-------

Common Lisp expressions from an IRC channel are evaluated in a sandbox
environment which provides a subset of Common Lisp features. In general,
many features related to symbols, packages and operating system have
been disabled. The sandbox is implemented in packages `SANDBOX-IMPL`,
`SANDBOX-CL` and `SANDBOX-EXTRA`. Function `sandbox-impl:repl` is the
interface for sandbox code evaluation.

Each IRC user has automatically her own sandbox package. User-defined
variables and functions are not shared between users. Users have their
own REPL variables too: `* ** *** / // /// + ++ +++`. The user-specific
sandbox package is temporary and is automatically deleted if not used
for a while.


The source code
---------------

GitHub repository: <https://github.com/tlikonen/cl-eval-bot>


Copyright and license
---------------------

Copyright (C) 2012-2013 Teemu Likonen <<tlikonen@iki.fi>>

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero
General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see
<<http://www.gnu.org/licenses/agpl.html>>.
