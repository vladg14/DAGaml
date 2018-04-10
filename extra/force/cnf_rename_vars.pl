#!/usr/bin/perl
#
# cnf_rename_vars.pl:
#        This perl script renames the variables in the clause according 
#        to order file.
#
# Created: 5 June. 2001, Fadi Aloul (c)
#
#

if(@ARGV != 3) {
  die "USAGE: cnf_rename_vars.pl order_file input_file output_file\n\t- Var seperated by space or new line in order_file\n\n";
}

open(VAR, $ARGV[0]) || die ("Cant open $argv[0]");
open(CNF, $ARGV[1]) || die ("Cant open $argv[1]");
open(OUT, ">$ARGV[2]") || die ("Cant open $argv[2]");


@new_var = ();

#
# Header file
#

chomp($today=`date`);
chomp($Iam  =`whoami`);
chomp($host =`uname -a`);

print OUT "c\n";
print OUT "c Renamed file of $ARGV[1] \n";
print OUT "c Variables are renamed according to $ARGV[0] \nc\n";
print OUT "c Date: $today by ($Iam)\n";
print OUT "c Host: $host\nc\n";
print OUT "c Copyright (c) 2001 Fadi A. Aloul.\nc\n";

#
# Setup new variables
#

$counter = 1;

while(<VAR>) {
    $_ =~ s/^\s+//;
    $_ =~ s/v//g;
    @arr = split /\s+/, $_;
    
    foreach $number (@arr) {
	#print "$number -> $counter\n";
	$new_var[$number] = $counter;
	$counter++;
    }
}

#
# Read CNF variable & rename, still need to
# rename not-mentioned variables
#

while(<CNF>) {
  
  next if /^\s*c/;

  if (/^\s*p/) {
    print OUT;
    $_ =~ s/^\s+//;
    @arr = split /\s+/, $_;
    $max_var = $arr[2];
    #print "inc value = $inc_value \n";
    
    for($i=1; $i<= $max_var; $i++) {
	if($new_var[$i] == 0) {
	    $new_var[$i] = $counter;
	    $counter++;
	}
    }

    next;
  }

  $_ =~ s/^\s+//;
  @arr = split /\s+/, $_;


  foreach $number (@arr) {

    if($number > 0) {    
      print OUT $new_var[$number], " ";  
    }
    elsif ($number < 0) {
      print OUT ($new_var[(-1*$number)] * -1), " ";  
    }
    else {
      print OUT "0\n";
    }
  }
}
