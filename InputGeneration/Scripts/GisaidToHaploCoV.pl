use strict;
use POSIX;

my $fileConvert="coutryToISO.txt";
open(IN,$fileConvert);
my %converter=();
while(<IN>)
{
	chomp();
	my ($country,$ISO)=(split(/\t/));
	$converter{$country}=$ISO;	
}

############################################################
### Arguments
#
my %arguments=
(
"--metadata"=>"na",      	# max time
"--dayFrom"=>"-3300",
"--outfile"=>"na" 		# min time
);
#
#############################################################
###Process input arguments and check if valid
check_arguments();
check_input_arg_valid();
##


my $metadata=$arguments{"--metadata"}; #file con varianti
my $ofile=$arguments{"--outfile"};
my $dayFrom=$arguments{"--dayFrom"};

###########################################################
# process the data

metadataToLists($metadata,$ofile);
#linearize("tmpOfile.txt",$data,$ofile);


#########################################################
# subs


sub metadataToPos
{
	my $keepFile="metaDkeep";
	my %lock=();
	if (-e $keepFile)
	{
		open(ON,$keepFile);
		while(<ON>)
		{
			chomp();
			$lock{$_}=1;	
		}
	}else{
		%lock=(
			"Virus name"=>"na",
			"Collection date"=>"na",
			"Submission date"=>"na",
			"Location"=>"na",
			"Pango lineage"=>"na",
			"AA Substitutions"=>"na"
		);
	}
	return (\%lock);	
}

sub areas
{
        my $areaFile="areaFile";
	unless (-e $areaFile)
	{
		download_areas();
	}
        open(AR,$areaFile);
        my %areas=();
        while(<AR>)
        {
                chomp();
                my ($country,$area)=(split(/\t/));
                $areas{$country}=$area;
        }
        return(\%areas);
}


sub metadataToLists
{
	my $metadataFile=$_[0];
	my $ofile=$_[1];
	my %cities=process_cities();
	open (OUT,">$ofile");	
	open(IN,$metadataFile);
	my $header=<IN>;
	chomp($header);
	my @vl=(split(/\t/,$header));
	my %keep=();
	my %lock=%{metadataToPos()};
	my %areas=%{areas()};
	my %data=();
	for (my $i=0;$i<=$#vl;$i++)
	{
		my $v=$vl[$i];
		if ($lock{$v})
		{
			$lock{$v}=$i 
		}
	}

	foreach my $MV (keys %lock)
	{
		print "$MV $lock{$MV}\n";
		die ("Could not find the required column $MV in your metadata file\nPlease check carefully\nExecution will halt now due to missing data\n") if $lock{$MV} eq "na";
	}
	my $Iv=$lock{"Virus name"};
	my $Ir=$lock{"Location"};	
	my $Id=$lock{"Collection date"};
	my $Is=$lock{"Submission date"};
	my $Ip=$lock{"Pango lineage"};
	my $Ivar=$lock{"AA Substitutions"};
	while (<IN>)
	{
		chomp();
		my @data=(split(/\t/));
		my $id=$data[$Iv];
		$id=fix_strain($id);
		my $d=$data[$Id];
		my $s=$data[$Is];
		my $p=$data[$Ip];
		next if $p eq "None";
		next if $p eq "Unassigned";
		next if $p eq "";
		$p="NoPango" if $p eq "NA";
		my $location=$data[$Ir];
		my $var=$data[$Ivar];
		$var=~s/\(//;
		$var=~s/\)//;
		my ($continent,$country,$region)=(split(/\//,$location));
        	$country=~s/\s+//g;
		my $area=$areas{$country} ? $areas{$country} : "NA";
		$continent=~s/\s+//g;
		$region=~s/\s+//g;
		my $lregion=lc $region;
		$region=$cities{$lregion} if $cities{$lregion};
		$continent="NA" if $continent eq "";
		$country="NA" if $country eq "";
		$region="NA" if $region eq "";
		$p="NA" if $p eq "";
		$country= $converter{$country} ? $converter{$country} : "na";
		$d=fix_date($d);
		$s=fix_date($s);
		my $delta=diff_d($d);
		next if $delta<0;
		my $delta_sub=ceil($delta/7);
		print OUT "$id\t$d\t$delta\t$s\t$delta_sub\t$continent\t$area\t$country\t$region\t$p\t$var\n";
	}
	system("sort -nk 5 $ofile > $ofile.tmp")==0||die("could not sort file\n");
	system("mv $ofile.tmp $ofile")==0||die();
}

sub fix_strain
{
        my $strain=$_[0];
        $strain=~s/hCoV-19\///;
        $strain=~s/\//\_/g;
        $strain=~s/\s+//g;
        $strain=~s/Lu\`an/Luan/g;
        $strain=~s/Lu\'an/Luan/g;
        $strain=~s/\$//g;
        $strain=~s/\(//g;
        $strain=~s/\)//g;
        $strain=~s/\'//g;
        return($strain);
}

sub fix_date
{
	my $date=$_[0];
	my @vl=(split(/\-/,$date));
	if ($#vl!=2)
	{
		$date="NA";
	}
	return($date);
}

sub diff_d
{
	my $diff="NA";
	my $date=$_[0];
        my @vl=(split(/\-/,$date));
	if ($date eq "NA")
	{
		return($diff);
	}else{
		$diff=0;
		my $diffY=($vl[0]-2019)*365;
		my $diffM=(int($vl[1]-12)*30.42);
		my $diffD=$vl[2]-30;
		$diff=$diffY+$diffM+$diffD;
		return(int($diff));
	}
}

######################################################################
## Functions for input control and help
##

sub check_arguments
{
        my @arguments=@ARGV;
        for (my $i=0;$i<=$#ARGV;$i+=2)
        {
                my $act=$ARGV[$i];
                my $val=$ARGV[$i+1];
                if (exists $arguments{$act})
                {
                        $arguments{$act}=$val;
                }else{
                        warn("$act: unknown argument\n");
                        my @valid=keys %arguments;
                        warn("Valid arguments are @valid\n");
                        warn("All those moments will be lost in time, like tears in rain.\n Time to die!\n");
                        print_help();
                }
        }
}

sub download_areas
{
        print "Area file, not found in the current folder\n";
        print "addToTable.pl will try to Download the file from github\n";
        print "Please download this file manually, if this fails\n";
        check_exists_command('wget') or die "$0 requires wget to download areafile\nHit <<which wget>> on the terminal to check if you have wget\n";
        system("wget https://raw.githubusercontent.com/matteo14c/HaploCoV/master/areaFile")==0||die("Could not retrieve the required file areafile. The file is not in the current folder. Please download it!\n");

}


sub check_exists_command {
    my $check = `sh -c 'command -v $_[0]'`;
    return $check;
}

sub check_input_arg_valid
{
        if ($arguments{"--metadata"} eq "na" ||  (! -e ($arguments{"--metadata"})))
        {
                print_help();
                my $f=$arguments{"--metadata"};
                die("No valid input file provided. $f does not exist!");
	}
	if($arguments{"--dayFrom"}<-3500){
                print_help();
                my $m=$arguments{"--dayFrom"};
                die("Start day can not be <-3500. $m provided\n");
        }
}

sub print_help
{
        print " This utility is meant to 1) read a metadata table file; 2) read a fasta file\n"; 
	print " of SARS-CoV-2 genome sequences; 3) identify sequences that have already been\n";
	print " processed by align.pl and 4) process the novel sequences and 5) provide an\n";
	print " output file with up to date data and metadata to be used by other main utilities \n";
	print " provided by HaploCoV\n";
	print " The final output will consist in a metadata table in HaploCov format\n";
	print " Temporary output files are written to a user specified folder, \"--genomes\"\n";
	print " The aling.pl utility form HaploCov is used to identify genetic variants\n";
	print " with respect to the reference genome of SARS-CoV-2. This process is executed\n";
	print " in parallel on 8 threads by default. Number of thread to use can be specified\n";
	print " by --nproc.\n";
	print " Users do also have the option to exclude sequences collected before a user specified\n";
	print " date from their analysis. This is controlled by the --dayFrom option. Default is -2500\n";
	print " Please see the main documentation of HaploCov to lear more about how dates are handled\n\n"; 
	

	print "##INPUT PARAMETERS\n\n";
        print "--metadata <<filename>>\t metadata file\n";
        print "--dayFrom <<integer>>\t keep only genomes isolated after this day. Default -2500\n";
	print "--outfile <<filename>>\t output metadata file in HaploCoV format. If the file is not empty\n";
	print "novel data/metadata will be appended to the bottom of the file\n";
        print "Mandatory parameters are --seq and --metadata \n";
        print "the file needs to be in the current folder.\n\n";
        print "\n##EXAMPLE:\n\n";
        print "1# input is metadata.tsv:\nperl addToTable.pl --metadata metadata.tsv --outfile HaploCoV_formattedMetadata\n\n";
}
