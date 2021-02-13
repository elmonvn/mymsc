/******************************************************************************
 *
 *
 ******************************************************************************/

proc fcmp outlib=work.lib.idhm;
	function ReverseName(name $) $;
	length newname $ 40;
	newname = catx(' ', scan(name, 2, ','), scan(name, 1, ','));
	return (newname);
	endsub;
quit;

/*proc surveyselect method=bernoulli ...*/
