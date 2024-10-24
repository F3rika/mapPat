#################################################
# input: fine periodo (settimane).
# input: file con i metadati e mutazioni
# file richiesti: allADM_CountryRegion_AssocTab.txt LinDefMut.csv 

open(IN,'<:encoding(UTF-8)',"allADM_CountryRegion_AssocTab.txt");
while(<IN>)
{
	chomp();
	($country,$isoc,$countryG,$region,$SynR)=(split(/\t/));
	#next unless $isoc eq "FRA";
	#$region=~s/\s+//g;
	#$region=~s/\'//g;
	$listRegTarget{$region}=1;
	$country=~s/\s+//g;
	$country=$isoc;
	@gregions=(split(/\,/,$SynR));
	foreach $greg (@gregions)
        {
                next if $greg eq "NA";
		$greg=~s/\s+//g;
                $CVR{$greg}=$region;
                #print "\t$greg -> $region\n" if $isoc eq "FRA";
        }
	#$countries{$isoc}++;
	push(@{$keepEr{$isoc}},$region);
}
close(IN);

$ldf="LinDefMut.csv";
open(IN,$ldf);
while(<IN>)
{
	chomp;
	($lin,@muts)=(split());
	foreach $m (@muts)
	{
		$ldf{$lin}{$m}=1;
	}
	
}
close(IN);


$infile=shift;
$cov=shift;
die ("cov deve essere SI o NO\n") unless $cov eq "NO" || $cov eq "SI";
$start=1;
$end=`tail -n 1 $infile | cut -f 5`;
chomp($end);
@weeks=();
@init=();
for ($i=$start;$i<=$end;$i++)
{
        push(@weeks,$i);
        push(@init,0);
}
%missed=();
%countLin=();
%countMut=();
open(IN,$infile);
while(<IN>)
{
	chomp();
	($week,$country,$region,$mut,$lin)=(split(/\t/))[4,7,8,10,9];
	$region=$CVR{$region} if $CVR{$region};
	#print "$country\n";
	#print "$week $country $region $mut $lin\n"; #if $country ne "FRA";		
	#print("non trovo $region, il paese era $country\n") if  (!$listRegTarget{$region} && $region ne "NA");
	next if $week <$start;
	next if $lin eq "Unassigned";
	next if $lin eq "";
	if  (!$listRegTarget{$region} && $region ne "NA")
	{
		$missed{$country}++;
	}else{
		$notMissed{$country}++
	}
	$count{$lin}{$country}++;
	#print "$week\n";
	@muts=(split(/\,/,$mut));
	$counTc{$country}{$lin}{$week}++;
	$counTReg{$lin}{$country}{$region}{$week}++;
	$total{$lin}++ unless $lin eq "NA";
	$countries{$country}++;
	foreach $m (@muts)
	{
		if ($cov eq "SI")
		{
			next unless $m=~/Spike/;
		}
		$muts{$m}++;
		$muTc{$country}{$m}{$week}++;
		unless ($ldf{$lin}{$m})
		{
			$countMut{$country}{$m}++;
			$mutLinC{$country}{$m}{$region}{$lin}{$week}++;
			$mutLinC{$country}{$m}{$country}{$lin}{$week}++;
			$haveMore{$country}{$lin}{$m}++; #quante
		}
	}
	
}

open(TBL,">:utf8","inTab_avCheck_tmp.txt");
print TBL "Country\tLin\tMut\tHeatChoroMap\tTotReg\n";

foreach $country (sort {$a<=>$b} keys %countries)
{
	#print "$country\t$countries{$country}\n";
	next unless $keepEr{$country};
	if ($countries{$country}<=1000)
	{
		print TBL "$country\tNA\tNA\tNA\tNA\n";
	}else{
		print TBL "$country\tEpiweek.$country.csv\t$country\_muts_perLin.csv\tHeatmapRegLin\_$country.csv\tTotal\_$country\_regions.csv\n";
	
	#print "$country\n";
	open(OUT,">:utf8","Epiweek.$country.csv");
	print OUT " @weeks\n";
	foreach $lin (sort keys %total)
	{
		print OUT "\"$lin\"";
		foreach $w (@weeks)
		{
			$val=$counTc{$country}{$lin}{$w} ? $val=$counTc{$country}{$lin}{$w} : 0;
			print OUT " $val";
		}
		print OUT "\n";
	}
	close(OUT);
	#open(OUT,">Epiweek.MutsSpike.$country.csv");
	#print OUT " @weeks\n";
	#foreach $mut (sort keys %muts)
	#{
	#	print OUT "$mut";
	#	foreach $w (@weeks)
	#	{
	#		$val=$muTc{$country}{$mut}{$w} ? $muTc{$country}{$mut}{$w}  : 0;
	#		print OUT " $val";
	#	}
	#	print OUT "\n";
	#	
	#}
	#close(OUT)
	open(CMUT,">:utf8","$country\_muts_perLin.csv");
	print CMUT  "lineage mutation region @weeks\n";
	foreach $mut (keys %{$mutLinC{$country}}) #$mutLinC{$country}{$lin}{$m}{$week}
	{
		@lins=keys %{$haveMore{$country}};
		@regions= @{$keepEr{$country}};
		unshift(@regions,$country);
		foreach $lin (@lins)
		{
			
			next unless $haveMore{$country}{$lin}{$mut}>=10; #almeno 10 genomi;
			foreach $reg (@regions)
			{
				print CMUT "\"$lin\" \"$mut\" \"$reg\"";
				foreach $w (@weeks)
				{
					$val=$mutLinC{$country}{$mut}{$reg}{$lin}{$w} ? $mutLinC{$country}{$mut}{$reg}{$lin}{$w} : 0;
                                        print CMUT " $val";
				}
				print CMUT "\n";
			}
		}		
	}
	close(CMUT);
	if ($keepEr{$country})
	{
		open(NWO,">:utf8","HeatmapRegLin\_$country.csv");
		print NWO "lin reg @weeks\n";
		open(TOT,">:utf8","Total\_$country\_regions.csv");
		print TOT " @weeks\n";	
		foreach $lin (sort{$a<=>$b} keys %total)
		{
			next unless $counTReg{$lin}{$country};
			@regions=@{$keepEr{$country}};
			#open(OUT,">$lin\_$country\_regions.csv");
			#print OUT " @weeks\n";
			foreach $reg (@regions)
			{
				#print OUT "$reg";
				print NWO "\"$lin\" \"$reg\"";
				unless( $tots{$reg})
				{
					for (my $wk=0;$wk<=$#weeks;$wk++)
					{
						#$w=$weeks[$wk];
						$tots{$reg}[$wk]=0;
					}
					#$tots{$reg}=\@init;
				}
				for (my $wk=0;$wk<=$#weeks;$wk++)
				{
					my $w=$weeks[$wk];

					$val=$counTReg{$lin}{$country}{$reg}{$w} ? $counTReg{$lin}{$country}{$reg}{$w}  : 0;
					$tots{$reg}[$wk]+=$val;
					#print OUT " $val";
					print NWO " $val";
				}
				#$iso=$decode{$reg} ? $decode{$reg} : "NA";
				#print OUT "\n";
				print NWO "\n";
			}
			
		}
		@regions=@{$keepEr{$country}};
		foreach $reg (@regions)
		{
			next unless $tots{$reg};
			@TV=@{$tots{$reg}};
			$vals=join(" ",@TV);
			#$reg=~s/\s+//;
			print TOT "\"$reg\" $vals\n" #@$vals\n";
		}
		close(NWO);
		close(TOT);
	}
	}
} 
