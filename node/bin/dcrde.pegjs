process "process" = 
	_ name:id 
	_ "process" 
	_ '{' events:evt+ _ '}' 
	{return {type:'process', name:name, events:events}}

evt "event" =
	_ required:('!' {return true})?
	_ included:('+' {return true})?
	_ name:id 
	_ params:('<' vars:vars _ '>' {return vars})? 
	_ '{' _ rels:relation+ _ '}' 
	{
		required = (required==true)? required : false
		included = (included==true)? included : false
		var o = {type:'event', required:required, included:included, name:name, params:params, relations:rels}; console.log('event',o); return o
	}

vars "vars" = 
	fst:prm 
	r:(_ ',' v:prm {return v})* 
	{var o = [fst].concat(r); console.log('params',o); return o}
prm "param" = 
	_ '$' v:id 
	{return {type:'var',name:v}}

relation "relation" = 
	_ rname:id 
	_ '{' reacts:reactions _ '}' 
	{var o = {type:'relation', name:rname, reactions:reacts}; console.log('relation',o); return o}

reactions "reactions" = 
	fst:reaction 
	r:(_ ',' v:reaction {return v})* 
	{var o = [fst].concat(r); console.log('reactions',o); return o}

reaction "reaction" = 
	_ name:id 
	_ params:('<' vars:vars _ '>' {return vars})? 
	{return {evtname:name,params:params}}

id "identifier"
	= fstchar:('_'/[a-zA-Z]) rest:(('_'/[a-zA-Z0-9])*) {var id = [fstchar].concat(rest).join(''); console.log('read id '+id); return id}

_ "spaces" = space*
space = ' ' / [\t\n\r]
