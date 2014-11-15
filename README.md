Sandbox
========

**A restricted environment for Common Lisp code evaluation**

Introduction
------------

Common Lisp expressions are evaluated in a restricted environment
which provides a subset of Common Lisp's features. In general, many
features related to symbols, packages and operating system have been
disabled. Some standard functions and macros have been replaced with
safer versions. The sandbox is implemented in packages `SANDBOX-IMPL`
and `SANDBOX-CL`. Function `sandbox:read-eval-print` is the interface
for sandbox code evaluation.

Variable `sandbox:*sandbox*` is the name of the sandbox package used
for evaluation. Use function `sandbox:reset` to reset the package.

Disabled symbols
-----------------

To find the list of disabled symbols/features

    (loop :for symbol :being :the :symbol :in (find-package :sandbox-cl)
          :when (get symbol :sandbox-locked)
            :collect symbol)

The source code
---------------

GitHub repository: <https://github.com/kanru/cl-sandbox>


Copyright and license
---------------------

Copyright (C) 2012-2013 Teemu Likonen <<tlikonen@iki.fi>>  
Copyright (C) 2014 Kan-Ru Chen <<kanru@kanru.info>>

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
