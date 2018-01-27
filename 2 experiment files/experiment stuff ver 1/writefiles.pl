###stuff to do:
###look up what the gunk should be

sub scramble {
  local @randtemp;
  local @list = @_;
  push(@randtemp, splice(@list, rand(@list),1)) while @list;
  return @randtemp;
}


open (FOUT, ">text.exp");

print FOUT <<ERIN;

<picture sam>
/ items = ("sam.jpg")
/ valign = top
/ erase = false
/ position = (50,10)
/ size = (80%, 30%)
</picture>

<text handintro>
/ erase = true(220,220,220)
/ txbgcolor = (220,220,220)
/ font = ("Verdana", -18, 400, 0, 34)
/ items = ("Which hand do you write with the most?")
/ numitems = 1
/ position = (50,30)
</text>

<text speed>
/ erase = true(220,220,220)
/ txbgcolor = (220,220,220)
/ font = ("Verdana", -18, 400, 0, 34)
/ items = ("What was your typing speed?")
/ numitems = 1
/ position = (50,30)
</text>

<text error>
/ erase = true(220,220,220)
/ txbgcolor = (220,220,220)
/ font = ("Verdana", -18, 400, 0, 34)
/ items = ("What was your typing error?")
/ numitems = 1
/ position = (50,30)
</text>

ERIN

###pull in words here
#~  label first second

open(FIN,"finalpairs2.txt");
while(<FIN>) {
next if /#/;
  chomp; tr/ \t/ /s;
@line = split;

push @trialcode, $line[0];
push @judge1, $line[1];


}
close(FIN);

for ($i=0; $i<@judge1; $i++)
{

print FOUT <<TEXT;
<text $judge1[$i]>
/ erase = true(220,220,220)
/ font = ("Verdana", -18, 400, 0, 34)
/ items = ("$judge1[$i]")
/ numitems = 1
/ position = (50,50)
/ txbgcolor = (220,220,220)
</text>

TEXT

}

close (FOUT);

open (FOUT, ">trials.exp");

for ($k=0; $k<@trialcode; $k++)
{
#~ print $trialcode[$k], "\n";

print FOUT <<TYPE;
<likert $trialcode[$k]>
/ anchors = [1 = "unpleasant"; 2 = " "; 3 = " "; 4 =" "; 5 = "neutral"; 6=" "; 7=" "; 8=" "; 9="pleasant";]
/ correctmessage = false 
/ errormessage = false 
/ mouse = true
/ numpoints = 9
/ position = (50,60)
/ posttrialpause = 500
/ stimulusframes = [1 =$judge1[$k], sam;]
/ trialcode = "$trialcode[$k]"
</likert>

TYPE
}

print FOUT <<blah;

<openended handedness1>
/ frames = [1=speed;]
/ buttonlabel =  "CONTROL ENTER"
/ charlimit = 40
/ linelength = 40
/ trialcode = "speed"
</openended>

<openended handedness2>
/ frames = [1=error;]
/ buttonlabel =  "CONTROL ENTER"
/ charlimit = 40
/ linelength = 40
/ trialcode = "error"
</openended>

<openended write>
/ frames = [1=handintro;]
/ buttonlabel =  "CONTROL ENTER"
/ charlimit = 40
/ linelength = 40
/ trialcode = "whichhand"
</openended>

blah

close (FOUT);


