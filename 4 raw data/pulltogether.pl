opendir my $dir, "/Users/buchanan/OneDrive - Missouri State University/RESEARCH/2 projects/QWERTY/9 15 stuff/exp analyses/raw data 1/" or die "Cannot open directory: $!";
my @files = readdir $dir;
closedir $dir;

#for ($i = 0; $i<@files; $i++)
#{
#	 print $files[$i], "\n";
#}

for ($i=3; $i<(@files-1); $i++)
{
$howtype = "NA";
#print $files[$i];

open(FIN,"/Users/buchanan/OneDrive - Missouri State University/RESEARCH/2 projects/QWERTY/9 15 stuff/exp analyses/raw data 1/$files[$i]") or die "could not open file";
while(<FIN>) {
next if /#/;
chomp; tr/ \t/ /s;
next if /^date/;
next if /writewords/;

#~ date	time	subject	trialcode	trialnum	latency	response
#~ 020212	13:34	200	old	1	14085	wrinkles
#~ 020212	13:34	200	lost	2	4664	found
#~ 020212	13:34	200	magazine	3	11734	celebrity

##dealing with the codes that need to be on all of them
#110112 13:11 54687 speed 26
#110112 13:11 54687 error 96%
#110112 13:11 54687 whichhand right_left

($date, $time, $subject, $trialcode, $trialnumber, $latency, $response) = split();

if ($trialcode eq "whichhand") 
    {
    $hand = $response; 
    }
elsif ($trialcode eq "speed") { $speed = $response; }
elsif ($trialcode eq "howtype") { $howtype = $response; }
elsif ($trialcode eq "error") { $error = $response; }
else{
    print $date, " ", $time, " ", $subject, " ", $response, " ", $hand, " ", $speed, " ", $error, " ", $howtype, " ", $trialcode, "\n";
    }

#$responses{$trialcode}{$files[$i]} = $response;

#print $subject, " ", $trialcode, " ", $response, "\n";
}
}

#foreach $trial (sort keys %responses)
#{
#	print $trial, " ";
	
#	foreach $person (sort keys %{$responses{$trial}})
#	{
#		print $responses{$trial}{$person}, " ";
#	}
#	print "\n";
#}
