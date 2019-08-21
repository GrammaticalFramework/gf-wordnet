#!/usr/bin/perl -w

use strict;
use utf8;

die "Usage: $0 fr.wikipage.sql.gz fr.langlinks.sql.gz (en|fa|zh|..)+\n" unless scalar @ARGV >= 3;

binmode(STDIN,":utf8");
binmode(STDOUT,":utf8");

my $pages = shift @ARGV;
my $links = shift @ARGV;
my @langs = @ARGV;

my $d = extract_page($pages);
   $d = extract_langlinks($links, \%$d);

for my $k (sort keys %$d) {
  my $fl = $d->{$k};
  my $s  = "$k";
  foreach (@langs)
  {
   	  my $f = $fl->{$_} // "";
      $s .= "\t$f";
  }
  $s .= "\n";
  print $s
}

# TODO:
# select p.page_title,l.ll_title from page as p, langlinks as l where p.page_id = l.ll_from and l.ll_lang='en';
sub extract_page {
  my $f    = shift;
  my $lang = substr $f, 0, 2;

  print STDERR "Reading page data from $f...\n";
  open F, "gunzip -c $f| iconv -f utf8 -t utf8 -c|" or die "Pipe failed zcat $f: $!";
  binmode(F,":utf8");
  my %d;
  my $dc = 0;
  while(<F>){
    while(/\((\d+),[^']+'((?:[^'\\]|\\.)*)'[^)]+/g) {
      my ($p, $m) = ($1,$2);
      $m =~ s/\\'/'/g;
      $m =~ s/\\"/"/g;
      $m =~ s/_/ /g;
      $m =~ s/–/-/g;
      $d{$p}{$lang}=$m;
      $dc++;
    }
  };
  close F;
  print STDERR "  read $dc documents\n";
  return \%d;
}

sub extract_langlinks {
  my $f = $_[0];
  my %d = %{$_[1]};

  print STDERR "Reading langlinks data from $f...\n";
  open F, "gunzip -c $f| iconv -f utf8 -t utf8 -c|" or die "Pipe failed zcat $f: $!";
  binmode(F,":utf8");
  my $dc = 0;
  #(27289,'aa','User:Marcin Łukasz Kiejzik'),
  while(<F>){
    while(/\((\d+),'([^']+)','((?:[^'\\]|\\.)*)'/g) {
      my ($p, $lang, $m) = ($1,$2, $3);
      next if $m =~ /^$/;
      $m =~ s/\\'/'/g;
      $m =~ s/\\"/"/g;
      $m =~ s/_/ /g;
      $m =~ s/–/-/g;
      $d{$p}{$lang}=$m;
      $dc++;
    }
  };
  close F;
  print STDERR "  read $dc documents\n";
  return \%d;
}

