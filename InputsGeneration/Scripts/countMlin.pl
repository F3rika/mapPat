# script per contare le mutazione associate a ciascun lineage.
# l'input è il tabellone preso da gisaid
# in perl while(<>) legge riga per riga un file passato come input
# lo script si esegue in questo modo:
# l'output è stampato sull stdout. perciò va rediretto con il >
# perl countMlin.pl metadata.tsv > mutazioniLineage.csv
$l=<>;

while(<>)
{
	#per ogni riga/genoma
	#leggo le colonne con lineage (11) e lista mutazioni (14)
	($lin,$muts)=(split(/\t/))[9,10];
	next if $lin eq "";
	next if $lin eq "Unassigned";
	$lin="NoPango" if $lin eq "NA";
	#uso un hash per contare quanti genomi sono assegnati al lineage
	#uso il lineage come chiave. e aumento il valore di 1 ogni volta
	#che lo vedo has{chiave}++ fa esattamente questa cosa
	$count{$lin}++;
	#tolgo le parentesi dalla stringa con le mutazioni
	$muts=~s/\(//;
	$muts=~s/\)//;
	# divido la stringa con le mutazioni in un un array chiamato @muts
	# uso la virgola per dividere: split(come,cosa)
	@muts=split(/\,/,$muts);
	#per ciascuna mutazione m nel array muts
	foreach $m (@muts)
	{
		#uso un hash con 2 chiavi per salvare quali mutazioni sono nel lineage
		#la prima chiave è il lineage
		#la seconda chiave la mutazione
		#countM è un hash di hash.
		#alla prima chiave (lineage) è  associato un hash con la lista di mutazioni di quel lineage
		#l'operatore ++ aumenta di uno il conto di ciascuna mutazione associata al lineage
		#ogni volta che la vedo aggiungo 1
		$countM{$lin}{$m}++;
	}
}

# per ogni lineage/chiave di countM

foreach $lin (keys %countM)
{
	next if $lin eq "NA";
	next if $lin eq "None";
	#stampa il nome 
	print "$lin\t";
	# leggi usando il nome del lineage come chiave, quanti genomi sono associati a quel lineage (hash count)
	$tot=$count{$lin};
	#per ogni mut (chiave secondaria)
	foreach $mut (keys %{$countM{$lin}})
	{
		#calcolo la prevalenza: ossevazioni totali della mutazione/totale genomi nel lineage
		# se prevalenza >= 0.5
		$ppc=$countM{$lin}{$mut}/$tot;
		if ($ppc>=0.5)
		{
			#stampo la mutazione
			print "$mut\t"
		}
	}
	# finito di processare la lista delle mutazioni metto un a capo e passo al prossimo lineage
	print "\n";
}
