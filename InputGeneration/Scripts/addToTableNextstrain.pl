use strict;
use POSIX;

###########################################################
# country to ISO
my $fileConvert="./Config/coutryToISO.txt";
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
"--ref"=>"na",
"--seq"=>"na",         # name of the input file
"--metadata"=>"na",      	# max time
"--dayFrom"=>"-3500",
"--nproc"=>8,
"--outfile"=>"na" 		# min time
);
#
#############################################################
###Process input arguments and check if valid
check_arguments();
check_input_arg_valid();
##


my $seq=$arguments{"--seq"};
my $metadata=$arguments{"--metadata"}; #file con varianti
my $ofile=$arguments{"--outfile"};
my $N=$arguments{"--nproc"};
my $dayFrom=$arguments{"--dayFrom"};
my $refile=$arguments{"--ref"};


check_exists_command('mkdir') or die "$0 requires mkdir to create a temporary directory\n";
check_exists_command('sort') or die  "$0 requires sort to order the output file\n";
check_exists_command('split') or die "$0 requires split to split the input fasta file\n";
check_exists_command('cat') or die "$0 requires cat to concatenate input fasta files\n";
check_exists_command('nucmer') or die "$0 requires nucmer from the mummer package to align genome sequences. Please install nucmer\n";


###########################################################
# process the data


my ($data,$fileTosplit)=metadataToLists($seq,$metadata,$ofile);
#
parallel_align($fileTosplit,"tmpOfile.txt",$N);
linearize("tmpOfile.txt",$data,$ofile);


#########################################################
# subs


sub metadataToPos
{
	my $keepFile="./Config/metaDkeep";
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
			"Pango lineage"=>"na"

		);
	}
	return (\%lock);	
}

sub areas
{
        my $areaFile="./Config/areaFile";
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
	my $seq=$_[0];
	my $metadataFile=$_[1];
	my $ofile=$_[2];
	
	my $fileTosplit=$seq;
	
	my %indexO=();
	#if output contain data. read the data. will skip all the genome I do have
	
	if (-e $ofile)
	{
		print "indexing $ofile\n";
		open(IN,$ofile);
		my $header=<IN>;
		my @sizes=(split(/\t/,$header));
		unless ($#sizes==10)
		{
			die("\n Your file in HaploCoV format does not have 11 columns as expected.\nExecution will halt.\nThis is the header of your input file:\t$header.\n\nPlease provide a valid file in HaploCoV format\n");
		}
		my $c=0;
		while(<IN>)
		{
			$c++;
			print "#$c sequences indexed\n" if $c %1000000==0;
			my ($id,$date)=(split(/\t/))[0,2];
			$indexO{$id}=1;
		}
	}
	

	open(IN,$metadataFile);
	my $header=<IN>;
	chomp($header);
	my @vl=(split(/\t/,$header));
	my %keep=();
	my %lock=%{metadataToPos()};
	my @l=keys %lock;
	my %areas=%{areas()};
	my %data=();
	for (my $i=0;$i<=$#vl;$i++)
	{
		my $v=$vl[$i];
		if ($lock{$v})
		{
			$lock{$v}=$i;
		#to deal with inconsistent naming in gisaid metadata table
		}elsif ($v eq "Pango lineage" || $v eq "Lineage" || $v eq "pangolin_lineage"){
			$lock{"Pango lineage"}=$i;
		}elsif ($v eq "date" || $v eq "Collection date"){
			$lock{"Collection date"}=$i;
		}elsif ($v eq "country" || $v eq "Location"){
			$lock{"Location"}=$i;
		}elsif ($v eq "strain" || $v eq "Virus name"){
			 $lock{"Virus name"}=$i;
		}	

	}
	#  again to deal with inconsistent naming in gisaid metadata
	$lock{"Submission date"}=$lock{"Collection date"} if $lock{"Submission date"} eq "na";

	foreach my $MV (keys %lock)
	{
		die ("Could not find the required column $MV in your metadata file\nPlease check carefully\nExecution will halt now due to missing data\n") if $lock{$MV} eq "na";
	}

	my $Iv=$lock{"accession"};
	my $Ir=$lock{"region"};
	my $Ic=$lock{"country"};
	my $Idiv=$lock{"division"};	
	my $Id=$lock{"date"};
	my $Is=$lock{"date_submitted"};
	my $Ip=$lock{"lineage"};
	#print "$Iv $Ir $Ic $Idiv $Id $Is $Ip\n";
	#die();
	#open(OUT,">tmpTable.csv");
	#print OUT "name\tdate\tpango\tcontinent\tarea\tcountry\tregion\tdays\n";
	my $c=0;
	print "indexing done\nNow reading input metadata file\n";
	while (<IN>)
	{
		$c++;
		print "#$c sequences acquired\n" if $c%1000000==0;
		my @data=(split(/\t/));
		my $id=$data[$Iv];
		$id=fix_strain($id);
		next if $indexO{$id}; 
		my $d=$data[$Id];
		my $s=$data[$Is];
		my $p=$data[$Ip];
		my $continent=$data[$Ir];
		my $country=$data[$Ic];
		my $region=$data[$Idiv];

		my $countryCopy=$country;
		$countryCopy=~s/\s+//g;


		my $area=$areas{$countryCopy} ? $areas{$countryCopy} : "NA";
		#$continent=~s/\s+//g;
		#$region=~s/\s+//g;
		$continent="NA" if $continent eq "";
		$country="NA" if $country eq "";
		$region="NA" if $region eq "";
		$p="NA" if $p eq "";
		$d=fix_date($d);
		$s=fix_date($s);
		my $delta=diff_d($d);
		next if $delta<$dayFrom;
		my $delta_sub=diff_d($s);
		#print "I got $id\n";
		$country= $converter{$country} ? $converter{$country} : "na";
		$data{$id}="$d\t$delta\t$s\t$delta_sub\t$continent\t$area\t$country\t$region\t$p";
		#print "$id\t$d\t$delta\t$s\t$delta_sub\t$continent\t$area\t$country\t$region\t$p\n";
	}
	if (-e $ofile)
	{
		print "metadata read done\nNow subsetting the fasta file\n";
		my $c=0;
		open(IN,$seq);
		open(OUT,">tmpMissingSeq.fa");
		my $print=0;
		my $id="na";
		my $seqOUT=0;
		while(<IN>)
		{
			if ($_=~/^>(.*)/)
			{
				print OUT "\n" if $print==1;
				$print=0;
				$c++;
				print "#$c genomes processed\n" if $c%100000==0;
				my $fid=$1;
				$fid=(split(/\|/,$fid))[0];
				$fid=fix_strain($fid);
				unless ($indexO{$fid})
				{
					#print "$fid\n";
					$print=1;
					$seqOUT++;
				}
			}else{
				chomp();
			}
			print OUT if $print==1;
		}
		print OUT "\n" if $print==1;

		$fileTosplit="tmpMissingSeq.fa";
		if ($seqOUT==0)
		{
			die("No novel sequences were detected in your fasta file\nExecution will stop here\nYour Haplocov metadata file already contains all the metadata for all the genomes in the fasta file\nOr the start date you set is way far ahead in the future. Please check\n");
		}
		print "Adding $seqOUT sequences\n";
	}
	return(\%data,$fileTosplit);
}

sub fix_strain
{
        my $strain=$_[0];
        $strain=~s/\//\_/g;
        $strain=~s/\s+//g;
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
		my $diffY=($vl[0]-2022)*365;
		my $diffM=(int($vl[1]-1)*30.42); #Months
		my $diffD=$vl[2]-1;
		$diff=$diffY+$diffM+$diffD;
		$diff=ceil($diff/30.42);
		return($diff);
	}
}

sub parallel_align
{
	my $infile=$_[0];
	my $ofile=$_[1];
	my $numP=$_[2];
	my @infiles=@{split_file($infile,$numP)};
	print "@infiles\n";
	unless (-d "Tgenomes")
	{
		system ("mkdir Tgenomes")==0||die();
	}
	my @childs=();
	my @outfiles=();
	for (my $i=0;$i<$numP;$i++)
        {
                my $pid=fork();
                my $file=@infiles[$i];
		push(@outfiles,"$file\_variants");
                unless (defined($pid))
                {
                        die "Cannot fork a child: $!";
                }elsif ($pid == 0) {
                        exec("perl ./Scripts/align.pl --infile $file --ref $refile --outfile $file\_variants") || die "can't exec $!";
                        exit(0);
                }else {
                        push(@childs,$pid);
                }
        }
	print "Now aligning with $numP processes\n";
        foreach(@childs){
                my $tmp=waitpid($_,0);
        }
	#print "o:@outfiles\n";
	merge(\@outfiles,$ofile);
	if ($infile eq "tmpMissingSeq.fa")
	{
		system("rm $infile")==0||die("could not remove temporary file with missing sequences\n");
	}
	foreach my $ifile (@infiles)
	{
		system("rm $ifile")==0||die("could not remove temporary file $ifile\n");
	}
}

sub split_file
{
        my $ifile=$_[0];
	#print "$ifile\n";
        my $proc=$_[1];
        my $Totlines=`wc -l $ifile`;
        chomp($Totlines);
        $Totlines=(split(/\s+/,$Totlines))[0];
        my $nlines=int($Totlines/($proc))+2;
	$nlines++ if $nlines % 2!=0;
	##print "$Totlines $nlines\n";
        system("split -d -l $nlines $ifile SPLITalnSeqFaCOVID")==0||die();
        my @files=<SPLITalnSeqFaCOVID*>;
        return(\@files);
}

sub merge
{
        my @files=@{$_[0]};
        my $ifile=$_[1];
        foreach my $f (@files)
        {
                system ("cat $f >> $ifile")==0||die();
		system ("rm $f")==0||die();
        }
}



sub linearize
{

	my $file=$_[0];
	#print "$file\n";
	my %data=%{$_[1]};
	my $ofile=$_[2];
	#print "$ofile\n";
	my %vars=();
	open(OUT,">$ofile.tmp");
	my $NAst="NA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA";
	
	open(LIN,$file);
	while(<LIN>)
	{
		#print;
        	my ($curID,$vars)=(split(/\t/))[0,1];
		$vars="none" if $vars eq ".";
		my $metadata=$data{$curID} ? $data{$curID} : $NAst;
                print OUT "$curID\t$metadata\t$vars";
	}
	system("rm $file")==0||die("could not remove temporary file $file\n");	
	system("sort -n -k 3 $ofile.tmp  > $ofile.srt")==0||die("could not sort the output file by date\n");
	close(OUT);
	unless (-e $ofile)
	{
		open(OUT,">$ofile");
		print OUT "genomeID\tcollectionD\toffsetCD\tdepositionD\toffsetDD\tcontinent\tarea\tcountry\tregion\tpangoLin\tlistV\n";
		close(OUT);
	}
	system("cat $ofile.srt >> $ofile")==0||die("Could not write the output file\n");
	system("rm $ofile.tmp $ofile.srt")==0||die("Could not remove temporary files\n");

}

sub download_areas
{
        print "Area file, not found in the current folder\n";
        print "addToTable.pl will try to Download the file from github\n";
        print "Please download this file manually, if this fails\n";
        check_exists_command('wget') or die "$0 requires wget to download areafile\nHit <<which wget>> on the terminal to check if you have wget\n";
        system("https://raw.githubusercontent.com/matteo14c/HaploCoV/updates/areaFile")==0||die("Could not retrieve the required file areafile. The file is not in the current folder. Please download it!\n");

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
			die("Reason:\nInvalid parameter $act provided\n");
                }
        }
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
                die("Reason:\nNo valid input file provided. Please provide one!");
        }
	if ($arguments{"seq"} eq "na" || (! -e $arguments{"--seq"}))
        {
                print_help();
                my $f=$arguments{"--seq"};
                die("Reason: $f\nNo valid sequence file provided. Please provide one!");
        }
	if ($arguments{"ref"} eq "na" || (! -e $arguments{"--ref"}))
        {
                print_help();
                my $f=$arguments{"--ref"};
                die("Reason: $f\nNo valid sequence file provided. Please provide one!");
        }

	if ($arguments{"--nproc"}<0)
	{
		print_help();
		my $m=$arguments{"--nproc"};
		die("Reason:\nNum threads can not be <0. $m provided\n");
	}
	if ($arguments{"--dayFrom"}<-3500)
        {
                print_help();
                my $m=$arguments{"--dayFrom"};
                die("Reason:\nStart day can not be <-3500. $m provided\n");
        }
	if ($arguments{"--outfile"} eq "na" || $arguments{"--outfile"} eq "." )
        {
                print_help();
                my $f=$arguments{"--outfile"};
                die("Reason:\n$f is not a valid name for the output file. Please provide a valide name using --outfile");
        }

}

sub print_help
{
        print "\n This utility is meant to 1) read a metadata table file; 2) read a fasta file\n"; 
	print " of SARS-CoV-2 genome sequences; 3) identify sequences that have already been\n";
	print " processed by align.pl and 4) process the novel sequences and 5) provide an\n";
	print " output in HaploCoV format to be used by other main utilities\n";
	print " provided by HaploCoV.\n";
	print " Temporary output files are written to the user specified folder, \"--genomes\".\n";
	print " The align.pl utility form HaploCov is used to identify genetic variants\n";
	print " with respect to the reference genome of SARS-CoV-2. This process is executed\n";
	print " in parallel on 8 threads by default. Number of thread to use can be specified\n";
	print " by --nproc.\n";
	print " Users do also have the option to exclude sequences collected before a user specified\n";
	print " date from their analysis. This is controlled by the --dayFrom option. Default is -2500\n";
	print " Please see the main documentation of HaploCov to lear more about how dates are handled.\n\n"; 
	

	print "##INPUT PARAMETERS\n\n";
        print "--metadata <<filename>>\t metadata file;\n";
        print "--ref <<filename>>\t reference genome sequence file;\n";
	print "--seq <<filename>>\t fasta file;\n";
        print "--dayFrom <<integer>>\t keep only genomes isolated after this day. Default -2500;\n";
	print "--nproc <<integer>>\t number of processes to align genomes and call variants. Default 8;\n";
	print "--outfile <<filename>>\t output metadata file in HaploCoV format. If the file is not empty\n";
	print "                      \t novel data/metadata will be appended to the bottom of the file.\n\n";
        print "Mandatory parameters are --seq, --metadata  and --outfile\n";
        print "the files needs to be in the current folder.\n\n";
        print "\n##EXAMPLE:\n\n";
        print "1# input is metadata.tsv:\nperl addToTable.pl --metadata metadata.tsv --seq sequences.fasta --ref referencegenomesequence.fasta --outfile HaploCoV.tsv\n\n";
}
