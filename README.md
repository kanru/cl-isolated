Eval-bot
========

**An IRC bot for Common Lisp code evaluation**


Introduction
------------

_Eval-bot_ is an Internet Relay Chat (IRC) robot program (a bot) which
aims to help discussions related to the Common Lisp language. The bot
sits on an IRC channel and can evaluate Common Lisp expressions and send
their return value to IRC channel or user.

The bot program is implemented in the Common Lisp language. It must be
used with an implementation that supports threads through
[Bordeaux Threads][BT] library. Minor parts in the bot's start and
setting scripts use features specific to [SBCL][] implementation.

[BT]:   http://common-lisp.net/project/bordeaux-threads/
[SBCL]: http://www.sbcl.org/


IRC
---

Common Lisp package `EVAL-BOT` contains the IRC part of the program.
Function `make-client` creates a client object which can be used with
IRC-related functions, such as `irc-connect`, `irc-join`, `irc-quit`
etc. Here is an example on how to run the bot.

 1. Start the bot from shell.

        $ ./start-bot-sbcl

 2. Use SBCL's REPL from the terminal or connect to the Swank server
    with Emacs's Slime.

        M-x slime-connect RET 127.0.0.1 RET 50000 RET

    If you use Slime you probably want to see bot's messages in the
    Slime buffer. Bot's message stream can be changed with variable
    `eval-bot::*local-stream*`. Write this in the Slime REPL buffer:

        (setf eval-bot::*local-stream* *standard-output*)

 3. Switch to the `EVAL-BOT` package.

        (in-package #:eval-bot)

 4. Create a client object for connections.

        (defparameter *client*
          (make-client :server "some.server.org"
                       :nickname "eval-bot"
                       :username "eval-bot"
                       :realname "Common Lisp Eval Bot"
                       :listen-targets '("#mychannel")
                       :auto-join '("#mychannel")))

    There is `*freenode*` client already. If you choose to use it, you
    may just change some of the slots:

        (setf (nickname *freenode*) "eval-bot")
        (push "#mychannel" (listen-targets *freenode*))
        (push "#mychannel" (auto-join *freenode*))

 5. Connect.

        (irc-connect *client*)

    The bot will connect and automatically join to `#mychannel`. You can
    also use `(irc-join client channel &optional password)` function.
    Functions with `irc-` prefix are the IRC commands for the server.
    Raw IRC protocol commands can be sent with `(irc-raw client
    raw-message)`.

 6. Use the bot!

        somenick> ,(values 1 2 3)
        eval-bot> => 1, 2, 3
        somenick> ,help
        eval-bot> [bot prints information]

    Comma `,` is the default prefix for code evaluation. It can be
    changed with variable `*eval-prefix*`. Not all Common Lisp's
    features are supported. See the Sandbox section below.

If you want to make the startup process automatic you could create a
Lisp file for your commands and start the bot with `./start-bot-sbcl
--load mysettings.lisp`.


Sandbox
-------

Common Lisp expressions from IRC channels are evaluated in a restricted
sandbox environment which provides a subset of Common Lisp's features.
In general, many features related to symbols, packages and operating
system have been disabled. Some standard functions and macros have been
replaced with safer versions. The sandbox is implemented in packages
`SANDBOX-IMPL`, `SANDBOX-CL` and `SANDBOX-EXTRA`. Function
`sandbox-impl:repl` is the interface for sandbox code evaluation.

A single eval message from IRC results in a single answer message from
the bot. Each IRC user has automatically her own sandbox package. So
user-defined variables and functions are not shared between users. Users
have their own REPL variables too: `* ** *** / // /// + ++ +++`. The
user-specific sandbox package is temporary and is automatically deleted
if not used for a while.


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
