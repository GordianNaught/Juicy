#!/usr/bin/perl
use warnings;
use strict;

=begin comment
  This program is for running tests against the Juicy compiler.
  The tests array below expects a name of a folder residing in
  ./Tests/ and a return value for the program to exit with.
  It is implied that there is a juicy program by the same name
  in lowercase (with .juicy at the end) in that folder.
  If there is also a file with that name and the `.txt' extension,
  then the output of the program is checked against the content
  of that file.
  If no `.txt' file is present, only the return value the program
  exits with is checked (against the provided value in the pair
  within the @tests array).
=end comment
=cut

my @tests =
  (
    ['Emit', 0],
    ['Emitint', 0],
    ['Fact', 6],
    ['Facts', 0],
    ['Void', 17],
    ['ReturnValue', 255],
    ['AssignArg', 0],
    ['DigitCount', 10],
    ['EmitN', 49],
    ['LeftPadNumber', 50],
    ['NL', 32],
    ['HigherOrder', 7],
    ['Lambda', 7],
    ['Closure', 7],
    ['Class', 3],
    ['Vector', 5],
    ['String', 98],
    ['Reduce', 10],
    ['Tagged', 3],
    ['CallOverload', 7]
  );

# This attempts to find a file and returns
# a boolean indicating if it was found.
sub find {
  my ($file) = @_;
  `find $file 2> /dev/null`;
  return not $?;
}

# This finds the maximum size of a test name
# so that things can be padded nicely
my $maxsize = 0;
foreach my $test (@tests) {
  (my $folder, my $expectedReturn) = @$test;
  my $length = length($folder);
  $maxsize = $length if $length > $maxsize;
}

# Indicate that a certain test had a certain result.
sub indicate {
  my ($test, $result) = @_;
  printf "  %-@{[$maxsize+1]}s[%s]\n", $test, $result;
}

foreach my $test(@tests) {
  (my $folder,my $expectedReturn) = @$test;
  my $source = "Tests/$folder/@{[lc $folder]}.juicy";
  my $build = "juicy $source 2> /dev/null";
  my $out = `$build`;
  my $buildSuccess = $out =~ m/a\.out created/;
  my $outputFile = "Tests/$folder/@{[lc $folder]}.txt";
  my $expectedOutput;
  my $executable = 0;
  undef $expectedOutput;
  if (find($outputFile)) {
    $expectedOutput = `cat $outputFile`;
    # cut off last character added by editor
    $expectedOutput = substr($expectedOutput,0,-1);
  }
  if ($buildSuccess) {
    if (find('a.out')) {
      $executable = 1;
      my ($output, $return);
      undef $output;
      undef $return;
      $output = `./a.out;`;
      $return = $? >> 8;
      if (not defined $output) {
        # couldn't retrieve the output from the program
        # We will assume it failed to run.
        indicate $folder, "RUN FAILED";
      } 
      elsif (not defined $return) {
        indicate $folder, "FAIL NO RETURN VALUE";
      }
      # output was expected and the output we got did not match
      # what we expected
      elsif (defined($expectedOutput) && $output ne $expectedOutput) {
        indicate $folder, "FAIL WRONG OUTPUT";
      }
      # the returned value was what we expected
      elsif ($return  eq $expectedReturn) {
        indicate $folder, "SUCCESS";
      }
      # the returned value was not what we expected
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
