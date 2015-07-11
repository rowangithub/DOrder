#!/usr/bin/perl -w
use strict;
use Data::Dumper;

# Convert arff format data into something dlist can understand.
# Nominal attributes will simply be converted to name=value strings.
# Numeric attributes will need to be represented with splits.

my $classattr;
if (@ARGV and $ARGV[0] =~ /^-?\d+$/) {
    $classattr = shift;
    $classattr = -$classattr if $classattr < 0;
} 

my $relation;
my @attribute;
my @attrtype;
while(<>) {
    if (/^\%/ or /^\s*$/) {
	next;
    } elsif (/^\@relation/i) {
	die "Relation already defined" if defined $relation;
	my ($a, $b) = mysplit($_);
	$relation = $b;
    } elsif (/^\@attribute/i) {
	my ($a, $b, $c) = mysplit($_);
	push @attribute, $b;
	push @attrtype, $c;
    } elsif (/^\@data/i) {
	last;
    }
}
# warn "Read ". scalar(@attribute) . " attributes\n";

my @data;
while(<>) {
    next if /^\%/;
    next if /^\s*$/;
    s/,/ /g;
    my @d = mysplit($_);
    if (@d != @attribute) {
	die "Wrong number of attributes: [$_]";
    }
    push @data, \@d;
}
# warn "Read ". scalar(@data). " instances";

# process numeric attributes:
my @sorted = ();
for (my $i = 0; $i <= $#attrtype; $i++) {
    next unless $attrtype[$i] =~ /^(numeric|integer|real)$/i;
    # collect and sort all unique values for attr $i:
    my @a;
    my @b;
    my %h;
    for my $d (@data) {
	my $di = $d->[$i];
	next if $di eq '?';
	push @b, $di;
	next if defined $h{$di};
	$h{$di}++;
	push @a, $di;
    }
    
#    warn "Splitting numerical attribute $i\n";
    # if there are too few unique values bin each one:
    if (@a <= sqrt(@data)) {
	@a = sort {$a<=>$b} @a;
	$sorted[$i] = \@a;
    }

=pod
    # equal-interval binning:
    # split the max-min into maxsplit equal intervals
    else {
	my @b;
	my $d = ($a[$#a] - $a[0])/($maxsplit);
	for (my $j = $a[0] + $d; $j < $a[$#a]; $j += $d) {
	    push @b, $j;
	}
	$sorted[$i] = \@b;
    }
=cut

    # equal frequency binning:
    # split the sorted data into maxsplit equal subsets
    else {
	my $maxsplit = sqrt(@data); # maybe this should be sqrt(@b)
	@b = sort {$a<=>$b} @b;
	my @c;
	for (my $j = 1; $j < $maxsplit; $j++) {
	    my $k = $j*@b/($maxsplit);
	    next if @c and $c[$#c] == $b[$k];
	    push @c, $b[$k];
	}
	$sorted[$i] = \@c;
    }
}

# warn "Writing instances\n";

$classattr = $#attribute 	# this is usually the last attribute,
    if not defined $classattr;	# but should make it an option. dlist
				# wants it in first column.

for my $d (@data) {
    print nospace($attribute[$classattr], '=', $d->[$classattr]);
    for (my $i = 0; $i <= $#attribute; $i++) {
	next if $i == $classattr;
	# next if $d->[$i] eq '?';
	print " ".nospace($attribute[$i], '=', $d->[$i]);
	if ($attrtype[$i] =~ /^(numeric|integer|real)$/i) {
	    my $s = $sorted[$i];
	    my $v = $d->[$i];
	    for my $si (@$s) {
		if ($v >= $si) {
		    print " ".nospace($attribute[$i], '>=', $si);
		}
		if ($v <= $si) {
		    print " ".nospace($attribute[$i], '<=', $si);
		}
	    }
	}
    }
    print "\n";
}

sub nospace {
    my $s = join('', @_);
    $s =~ s/ /_/g;
    return $s;
}

sub mysplit {
    # handle quotes and curly braces etc.
    my ($s) = shift;
    my @a = ();
    while ($s =~ /\S/) {
	if ($s =~ s/^\s*\{(.+?)\}//) {
	    my $y = $1;
	    $y =~ s/,/ /g;	# this will fail if comma in quotes
	    my @b = mysplit($y);
	    push @a, \@b;
	} elsif ($s =~ s/^\s*\'(.+?)\'//) {
	    push @a, $1;
	} elsif ($s =~ s/^\s*\"(.+?)\"//) {
	    push @a, $1;
	} elsif ($s =~ s/^\s*(\S+)//) {
	    push @a, $1;
	}
    }
#    print Dumper($_, \@a);
    return @a;
}

