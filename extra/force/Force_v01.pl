#!/usr/bin/perl
#
# Force_v01.pl:
#     - This perl script calls the three files
#       cnf2hgraph.pl, NetPlacer, cnf_rename_vars.pl
#     - Reads a CNF file, generates a NEW CNF file
#       with the variables re-ordered.
#
# Created: 5 June 2003, Fadi Aloul (c)
#
#

use Getopt::Std;

if(@ARGV < 1) {
    print << "END";
    USAGE: Force_v01.pl file

    (c) Copyright, 2003 Fadi Aloul, University of Michigan
    Email: faloul@eecs.umich.edu

END
  exit;
}

$file = $ARGV[0];

#
# 1) Run cnf2hgraph.pl
#

$cmd = "cnf2hgraph.pl $file\n";
print "$cmd\n";
system("$cmd");

#
# 2) Run NetPlacer
#

$cmd = "NetPlacer -c 3 -s 10\n";
print "$cmd\n";
system("$cmd");


#
# 3) Run cnf_rename_vars.pl
#

$newFile = $file;
$newFile =~ s/.cnf//;
$newFile .= "_new.cnf";

$cmd = "cnf_rename_vars.pl out.pl $file $newFile\n";
print "$cmd\n";
system("$cmd");










