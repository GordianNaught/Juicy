#!/usr/bin/perl
use warnings;
use strict;

my @tests =
  (
    ['Emit',0],
    ['Emitint',0],
    ['Fact',6],
    ['Facts',0],
    ['Void',17],
    ['ReturnValue',255],
    ['AssignArg',0],
    ['DigitCount',10],
    ['EmitN',49],
    ['LeftPadNumber',50],
    ['NL',32],
    ['HigherOrder',7],
    ['Lambda', 7],
    ['Closure', 7],
    ['Class', 3],
    ['Vector', 5],
    ['String', 98],
    ['Reduce', 10],
    ['Tagged', 3]
  );

sub find {
  my ($file) = @_;
  `find $file 2> /dev/null`;
  return not $?;
}

my $maxsize = 0;
foreach my $test (@tests) {
  (my $folder, my $expectedReturn) = @$test;
  my $length = length($folder);
  $maxsize = $length if $length > $maxsize;
}

sub indicate {
  my ($test, $result) = @_;
  printf "  %-@{[$maxsize+1]}s[%s]\n", $test, $result;
}

foreach my $test(@tests) {
  (my $folder,my $expectedReturn) = @$test;
  my $source = "Tests/$folder/@{[lc $folder]}.juicy";
  my $build = "juicy $source 2> /dev/null";
  my $out = `$build`;
  my $buildStatus = $out =~ m/a\.out created/;
  my $outputFile = "Tests/$folder/@{[lc $folder]}.txt";
  my $expectedOutput;
  my $executable = 0;
  undef $expectedOutput;
  if (find($outputFile)) {
    $expectedOutput = `cat $outputFile`;
    $expectedOutput = substr($expectedOutput,0,-1);
  }
  if ($buildStatus) {
    if (find('a.out')) {
      $executable = 1;
      my ($output, $return);
      undef $output;
      undef $return;
      $output = `./a.out;`;
      $return = $? >> 8;
      if (not defined $output) {
        indicate $folder, "RUN FAILED";
      } 
      elsif (not defined $return) {
        indicate $folder, "FAIL NO RETURN VALUE";
      }
      elsif (defined($expectedOutput) && $output ne $expectedOutput) {
        indicate $folder, "FAIL WRONG OUTPUT";
      }
      elsif ($return  eq $expectedReturn) {
        indicate $folder, "SUCCESS";
      }
      else {
        indicate $folder, "FAIL $return returned, $expectedReturn expected";
      }
    }
    else {
      indicate $folder, "FAIL EXECUTABLE NOT FOUND";
    }
  }
  else {
      indicate $folder, "FAIL BUILD";
  }
  `rm a.out` if find('a.out');
  `rm a.out.s` if find('a.out.s');
}
