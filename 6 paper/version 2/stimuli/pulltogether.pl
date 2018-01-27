
open(FIN,"tocode.txt") or die "could not open file";
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
next if /^date/;
next if /writewords/;

@values = split(//);
#print @values, "\n";


$switch = 0;

for ($i = 0; $i < (@values-1); $i++)
{
	#print $values[$i], " ", $values[($i+1)], " \n";
	if ($values[$i] == $values[$i+1])
	{	
		$switch = 0 + $switch;
		#print $switch, " eq ";
	}
	else
	{
		$switch = 1 + $switch;
		#print $switch, " no ";
	}
}

print $switch, "\n";


}

