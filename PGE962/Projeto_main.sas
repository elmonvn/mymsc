/******************************************************************************
 * PGE962 - Projeto IDHM
 * Autor:	Elmon de Vasconcelos Noronha (apud Cristiano Ferraz)
 * Resumo:	Este script efetua a amostragem estratificada do cadastro de
 *			municipios brasileiros, estratificando-os pela coluna regiao,
 *			e gerando amostras de Bernoulli para cada estrato, com pi
 *			arbitrario de 5%.
 *
 *			Tambem serao geradas as estimativas para o cadastro de municipios
 *			abordados nos Atlas de Desenvolvimento Humano no Brasil, ediçoes
 *			1998 e 2003.
 *
 ******************************************************************************/
/*options cmplib=idhm.lib;*/

/* Limpeza da work */
proc datasets library=work kill nolist;
quit;

/* Padrao de formatacao dos valores numericos */
proc format;
	value decimais (default=6.3);
run;

/* Importando o arquivo de acadastro */
filename reffile '~/PGE962_Projeto/CAD.xlsx';

proc import datafile=reffile dbms=xlsx out=cad;
	getnames=yes;
run;

/* Ordenando o cadastro para estratificar por regiao */
proc sort data=cad;
	by regiao;
run;

proc freq data=cad;
	tables regiao /out=regcad;
run;

data regcad;
	set regcad;
	keep regiao _total_ _rate_;
	_total_=count;
	_rate_=0.05;
run;	

/* Selecionando a amostra do cadastro */
proc surveyselect data=cad samprate=regcad method=bernoulli seed=1234 out=amostra stats;
	strata regiao;
run;

/* Captura totais, nh (tamanho das amostras), seus valores esperados e variancias */
proc sql;
	create table regtamamostra as
	select
		distinct(regiao),
		count(*) as nh,
		ExpectedN,
		(ExpectedN * (1 - 0.05)) label='Variance of Sample Size' as VarianceN
	from
		amostra
	group by
		regiao;
run;

/* Consolida totais dos estratos */
data totais;
	merge regcad regtamamostra;
	by regiao;
run;

proc print data=totais;
run;

/* Obtendo as variaveis respostas: importando o arquivo RESP */
filename reffile1 '~/PGE962_Projeto/RESP.xlsx';

proc import datafile=reffile1 dbms=xlsx out=resp;
	getnames=yes;
run;

/* Incorporando a variavel resposta ao arquivo amostra */
proc sort data=amostra;
	by codmun;

proc sort data=resp;
	by codmun;

data dadosamostrais;
	merge resp(in=a) amostra(in=b);
	by codmun;

	if b=1;
run;

/* Ordenando os dados amostrais por regiao */
proc sort data=dadosamostrais;
	by regiao;
run;

/* Analisando valores de parametros */
title 'Analises descritivas dos parametros por estrato';
proc means data=resp n min max mean stddev;
	class regiao;
	vars idh10 espvi10 fect10 mort10 estudo10;

/* Analisando valores de parametros nas amostras */
title 'Analises descritivas dos parametros estimados por estrato';
proc means data=dadosamostrais n min max mean stddev;
	class regiao;
	vars idh10 espvi10 fect10 mort10 estudo10;
run;

/* Analisando valores de parametros na populacao */
title 'Analises descritivas dos parametros na populacao';
proc means data=resp n min max mean stddev;
	vars idh10 espvi10 fect10 mort10 estudo10;
run;

/* Gerando estimativas por estrato */
title 'Estimativas por estrato';
proc surveymeans data=dadosamostrais total=totais;
	var idh10 espvi10 fect10 mort10 estudo10;
	strata regiao/list;
	by regiao;
	weight SamplingWeight;
	ods output Statistics=estimaestratos StrataInfo=estratos Summary=resumo;
run;

/* Gerando estimativas para toda populacao */
title 'Estimativas para toda populaçao';
proc surveymeans data=dadosamostrais total=totais;
	var idh10 espvi10 fect10 mort10 estudo10;
	strata regiao/list;
	weight SamplingWeight;
	ods output Statistics=estimapop;
run;
