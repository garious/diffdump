Unit-testing with diffdump and patchdump
========================================

[![Build Status](https://secure.travis-ci.org/garious/DiffDump.png)](http://travis-ci.org/garious/DiffDump)

Introduction
------------

diffdump and patchdump are generalized versions of standard command-line utilities diff and patch.  The biggest
difference is that both utilities accept input on stdin, making it possible to pipe data from one utility to
the other without introducing intermediary files.

Usage
-----

    diffdump [OPTIONS] [OLD_DIR] [NEW_DIR]
    
    Common flags:
      -o --outfile=FILE     Output path.
         --outdir=DIR       Directory for tempory files.
         --cmd=FILE         Command to run on both arguments.
         --arg=ARG          Argument to pass to both commands.
         --cmd1=FILE        Command to run on first argument.  Overrides --cmd
         --arg1=ARG         Argument appended to first command.
         --cmd2=FILE        Command to run on second argument.  Overrides --cmd
         --arg2=ARG         Argument appended to second command.
         --filetype=ITEM    Filter by files that include this string in the
                            output of the 'file' command.
         --fileext=ITEM     Filter by files that have this file extension.
         --diff=FILE        Path to diff.
         --diff-arg=ITEM    Pass <arg> on to diff.
      -f --force            Run despite the output directories already existing.
      -i --ignore=DIR       Ignore contents of the given directory.
      -r --recursive        Recursively compare any subdirectories found.
      -u --unified          Output 3 lines of unified context.
         --color            Force colorized diff in terminal output.
         --no-color         Do not colorize diff in terminal output.
      -v --verbose          Verbose output.
      -k --keep-files       Keep intermediary files.
      -s --succeed-on-diff  Unlike diff, return zero exit code when there are
                            differences.
      -? --help             Display help message
      -V --version          Print version information


Building the sources
--------------------

DiffDump is written in Haskell.  To build it, get Haskell Platform from http://www.haskell.org/ghc/.  Then:

    $ git clone <url>
    $ cd DiffDump
    $ cabal install


Case Study, Unit-testing
------------------------

In the example below, we will start with a simplest way to unit-test
text-to-text transformations and show how that can be fragile.  Then
we will show how storing a file diff can be a nice incremental
improvement.   Lastly, we will demonstrate how 'diffdump' and
'patchdump' help make this new testing strategy easy to implement.

Goal:

    Write a unit-test to ensure LLVM's Constant Propagation eliminates
    runtime constant evaluation.

First a simple solution, given a test input file, run the constprop optimization on it and
save the output as a "gold" file.

    $ opt -S -constprop basictest.ll > basictest.ll.gold

As you make changes to the compiler, verify the output has not changed:

    $ opt -S -constprop basictest.ll > tmp.ll
    $ diff basictest.ll.gold tmp.ll
    $ rm tmp.ll

The tool 'diff' conveniently returns non-zero if there are any differences.  But this
strategy is still not without its limitations.  Using "gold" copies as test cases
is very fragile!  If a code fragment unrelated to constant propagation changes, your unit
tests fail.

A good unit test should not fail when output unrelated to what it is your trying to test
changes.  Implementing this is where diffdump and LLVM's unit-test suite diverge.  LLVM's
solution is the FileCheck utility.  You provide it with instructions on how to scan the
compiler output to see if the output contains or does not contain what you expect.  The
author finds this tedious, typically incomplete, error-prone, and unnecessarily complicated
 to debug.  Here is an example from basictest.ll.  It says "iterate down to @test1" and
 "verify the line contains zero and not %Val".  Note this test is incomplete and does
 not verify the line that set %Val has been removed as well.

    ; CHECK: @test1
    ; CHECK: %Ret = phi i32 [ 0, %BB1 ], [ 1, %BB2 ]


Instead, we recommend creating unit-tests based on diff'ing and patch'ing compiler output.
First, when you are satisfied the compiler is working well, create a gold diff.  We will
first demonstrate how to do this with existing tools, and then show the one-liner with
diffdump.

    $ opt -S basictest.ll > normalized.ll
    $ opt -S -constprop basictest.ll > tmp.ll
    $ diff -u normalized.ll tmp.ll > basictest.ll.patch

And now with diffdump:

    $ diffdump -u --cmd=opt --arg=-S --arg2=-constprop basictest.ll > basictest.ll.patch 


Here's the output.  It shows that %Val was removed in two places.

    diff -u a/dump.txt b/dump.txt
    --- a/dump.txt  2012-10-31 11:28:31.000000000 -0700
    +++ b/dump.txt  2012-10-31 11:28:31.000000000 -0700
    @@ -6,34 +6,28 @@
       br i1 %B, label %BB1, label %BB2
     
     BB1:                                              ; preds = %0
    -  %Val = add i32 0, 0
       br label %BB3
     
     BB2:                                              ; preds = %0
       br label %BB3
     
     BB3:                                              ; preds = %BB2, %BB1
    -  %Ret = phi i32 [ %Val, %BB1 ], [ 1, %BB2 ]
    +  %Ret = phi i32 [ 0, %BB1 ], [ 1, %BB2 ]
       ret i32 %Ret
     }
    
And now, to verify a new compiler has not broken our test, we use patch to generate a
"gold" copy, and diff it against the compiler output.  Note this gold copy can be 
quite different than the original gold copy.  If unrelated output has changed, 'patch'
comfortably manages the file changes and applies the patch in the correct location.
If, however, the unrelated changes have changed too much, patch will bail out, letting
you know you will need to create a new gold patch.

Like before, we'll first do this the long way with 'diff' and 'patch' and then show
how to do it with 'diffdump' and 'patchdump'.  Both examples, assume we have already
created and saved the "gold" patch mentioned earlier.

    $ opt -S basictest.ll > normalized.ll
    $ patch -p1 -i basictest.ll.patch normalized.ll > basictest.ll.gold
    $ opt -S -constprop basictest.ll > tmp.ll
    $ diff -u basictest.ll.gold tmp.ll

And with diffdump and patchdump:

    $ diffdump -u --cmd1=./MakeGold.sh --cmd2=opt --arg2=-S --arg2=-constprop basictest.ll

Where MakeGold.sh contains:

    opt -S $1 | patchdump -p1 -i $1.patch

diffdump wraps diff, but allows you to execute different commands on each input
file before computing the diff.  In this case, we want the first parameter to
diff to be the expected output and the second parameter to be the actual output.
The '--cmd1' parameter is executed with the first argument to diffdump and the
'--cmd2' parameter is executed with the second argument.  If no second argument
is provided, as is here, then '--cmd2' also acts on the first argument.  We can
unwrap each script to see exactly how this works.  Starting with our one-liner:

    $ diffdump -u --cmd1=./MakeGold.sh --cmd2=opt --arg2=-S --arg2=-constprop basictest.ll

Without diffdump is:

    $ ./MakeGold.sh basictest.ll > basictest.ll.gold
    $ opt -S -constprop basictest.ll > tmp.ll
    $ diff -u basictest.ll.gold tmp.ll

Without MakeGold.sh is:

    $ opt -S basictest.ll | patchdump -p1 -i basictest.ll.patch > basictest.ll.gold
    $ opt -S -constprop basictest.ll > tmp.ll
    $ diff -u basictest.ll.gold tmp.ll

Without patchdump is:

    $ opt -S basictest.ll > normalized.ll
    $ patch -p1 -i basictest.ll.patch normalized.ll > basictest.ll.gold
    $ opt -S -constprop basictest.ll > tmp.ll
    $ diff -u basictest.ll.gold tmp.ll

patchdump is used to output a patched file to stdout, and  diffdump is used
to diff the stdout from two commands.  Together, all temporary files
are tucked away.  While temporary files are still created behind the scenes,
diffdump ensures they are cleaned up properly and that they are given unique
names so that multiple diffdump instances can run concurrently.


More diffdump examples
----------------------

Compare the disassembly of two object files:

    $ diffdump old.o new.o -- objdump -d

Note that when diffdump is given two filenames, you can also pass
it a third command argument, the command to execute on each.
Use '--' to tell diffdump that the all following options, such
as '-d', are intended for the given command and not for diffdump.
Using the command argument is synonymous with '--cmd' and '--arg':

    $ diffdump --cmd=objdump --arg=-d old.o new.o

Like diff, use the '-u' option to report a diff in "unified" format.

    $ diffdump -u old.o new.o -- objdump -d

To keep the dump files around afterwards, use --keep-files and an explicit
output directory.

    $ diffdump --keep-files --outdir=mydir old.o new.o -- objdump -d

To compare the disassembly of two directories with matching files trees:

    $ diffdump -r old_dir new_dir -- objdump -d

To use 'readelf -s' instead of objdump:

    $ diffdump -u old.o new.o -- readelf -s


