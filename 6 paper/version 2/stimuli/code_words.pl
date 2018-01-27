
open(FIN,"tocode.txt") or die "could not open file";
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
next if /^date/;
next if /writewords/;

@values = split(//);
#print @values, "\n";
$word=$_;
#print $word, "\n";

$total_freq{$word} = 0;

###avg frequency
for($r =0; $r<(@values); $r++)
{
	if ($values[$r] eq "a"){$total_freq{$word} = $total_freq{$word} + 8.167;}
	if ($values[$r] eq "b"){$total_freq{$word} = $total_freq{$word} + 1.492;}
	if ($values[$r] eq "c"){$total_freq{$word} = $total_freq{$word} + 2.782;}
	if ($values[$r] eq "d"){$total_freq{$word} = $total_freq{$word} + 4.253;}
	if ($values[$r] eq "e"){$total_freq{$word} = $total_freq{$word} + 12.702;}
	if ($values[$r] eq "f"){$total_freq{$word} = $total_freq{$word} + 2.228;}
	if ($values[$r] eq "g"){$total_freq{$word} = $total_freq{$word} + 2.015;}
	if ($values[$r] eq "h"){$total_freq{$word} = $total_freq{$word} + 6.094;}
	if ($values[$r] eq "i"){$total_freq{$word} = $total_freq{$word} + 6.966;}
	if ($values[$r] eq "j"){$total_freq{$word} = $total_freq{$word} + 0.153;}	
	if ($values[$r] eq "k"){$total_freq{$word} = $total_freq{$word} + 0.772;}
	if ($values[$r] eq "l"){$total_freq{$word} = $total_freq{$word} + 4.025;}
	if ($values[$r] eq "m"){$total_freq{$word} = $total_freq{$word} + 2.406;}
	if ($values[$r] eq "n"){$total_freq{$word} = $total_freq{$word} + 6.749;}
	if ($values[$r] eq "o"){$total_freq{$word} = $total_freq{$word} + 7.507;}
	if ($values[$r] eq "p"){$total_freq{$word} = $total_freq{$word} + 1.929;}
	if ($values[$r] eq "q"){$total_freq{$word} = $total_freq{$word} + 0.095;}
	if ($values[$r] eq "r"){$total_freq{$word} = $total_freq{$word} + 5.987;}
	if ($values[$r] eq "s"){$total_freq{$word} = $total_freq{$word} + 6.327;}
	if ($values[$r] eq "t"){$total_freq{$word} = $total_freq{$word} + 9.056;}
	if ($values[$r] eq "u"){$total_freq{$word} = $total_freq{$word} + 2.758;}
	if ($values[$r] eq "v"){$total_freq{$word} = $total_freq{$word} + 0.978;}
	if ($values[$r] eq "w"){$total_freq{$word} = $total_freq{$word} + 2.360;}
	if ($values[$r] eq "x"){$total_freq{$word} = $total_freq{$word} + 0.150;}
	if ($values[$r] eq "y"){$total_freq{$word} = $total_freq{$word} + 1.974;}
	if ($values[$r] eq "z"){$total_freq{$word} = $total_freq{$word} + 0.074;}


}

$avg_freq{$word} = $total_freq{$word}/length($word);

####

###switch code L R

$LRcode{$word} = $word;

###left
$LRcode{$word} =~ s/[qwertasdfgzxcvb]/1/gi;
$LRcode{$word} =~ s/[yuiophjklnm]/2/gi;

$finger{$word} = $word;

$finger{$word} =~ s/[qaz]/1/gi;
$finger{$word} =~ s/[wsx]/2/gi;
$finger{$word} =~ s/[edc]/3/gi;
$finger{$word} =~ s/[rfvtgb]/4/gi;
$finger{$word} =~ s/[yhnujm]/5/gi;
$finger{$word} =~ s/[ik]/6/gi;
$finger{$word} =~ s/[ol]/7/gi;
$finger{$word} =~ s/[p]/8/gi;


###switches

$switch{$word} = 0;

@values = split //, $finger{$word};

for ($i = 0; $i < (@values-1); $i++)
{
	#print $values[$i], " ", $values[($i+1)], " \n";
	if ($values[$i] == $values[$i+1])
	{	
		$switch{$word} = 0 + $switch{$word};
		#print $switch, " eq ";
	}
	else
	{
		$switch{$word} = 1 + $switch{$word};
		#print $switch, " no ";
	}
}

#print $word, " ", $switch{$word}, " ";

$RHA{$word} = 0;
$switch2{$word} = 0;

@values = split //, $LRcode{$word};

for ($i = 0; $i < (@values-1); $i++)
{
	
	#print $values[$i], " ", $values[($i+1)], " \n";
	if ($values[$i] == $values[$i+1])
	{	
		$switch2{$word} = 0 + $switch2{$word};
		#print $switch, " eq ";
	}
	else
	{
		$switch2{$word} = 1 + $switch2{$word};
		#print $switch, " no ";
	}
}

#print $switch2{$word}, "\n";

for ($i = 0; $i < (@values); $i++)
{
	if ($values[$i] == 1)
	{
		#print $word, " ", $values[$i], " ", $RHA{$word}, " ";
		$RHA{$word} = $RHA{$word} - 1;
		#print $RHA{$word}, "\n";
	}
	if ($values[$i] == 2)
	{
		#print $word, " ", $values[$i], " ", $RHA{$word}, " ";
		$RHA{$word} = $RHA{$word} +1;
		#print $RHA{$word}, "\n";
	}
}
	


}
#####



foreach $word (sort keys %LRcode)
{
	print $word, " ", $LRcode{$word}, " ", $finger{$word}, " ", $switch2{$word}, " ", $switch{$word}, " ", $avg_freq{$word}, " ", length($word), " ", $RHA{$word}, "\n";
}


