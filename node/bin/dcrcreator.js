var  dcrde = require('../lib/dcrde')
	,fs = require('fs')
	,util = require('util')
	,wu = require('wu').wu
	,futures = require('futures')
	,Client = require('../lib/dcrdeclient')

var fname = process.argv[2] || 'treatment.dcr'
var contents = fs.readFileSync('./' + fname, 'utf8')
var parsed = dcrde.parse(contents)

var client = new Client()

var makeProcess = function(p,cl) {
	return function(n) {
		console.log('new',p.name)
		cl.new(p.name,n)
	}
}
var makeEvents = function(evts,name,cl,s) {
	var newevts = wu(evts).map(function(ev) {
		var prms = wu(ev.params).map(function(p){
			return p.name
		}).toArray()
		return {name:ev.name,fields:prms}
	}).toArray()
	var included = wu(evts)
	.filter(function(ev){
		return ev.included==true
	})
	.map(function(ev){
		return ev.name
	}).toArray()
	
	s.then(function(n){
		console.log('init events',newevts)
		cl.init_events(name,newevts,n)
	}).then(function(n){
		console.log('init included',included)
		cl.init_included(name, included,n)
	})
}
var makeRelations = function(evts,name,cl,s) {
	var evparams = {}
	wu(evts).each(function(ev){
		evparams[ev.name] = wu(ev.params).map(function(p){return p.name}).toArray()
	})
	var rels = []
	wu(evts).each(function(ev){
		var from = ev.name
		wu(ev.relations).each(function(rel){
			var type = rel.name
			reacts = wu(rel.reactions).map(function(r){
				var to = r.evtname
				var pred = []
				for(var i=0;i<r.params.length;i++) {
					pred.push(evparams[from][i] + "=" + r.params[i].name)
				}
				return {k:type,f:from,t:to,pred:pred.join(' & ')}
			}).toArray()
			rels = rels.concat(reacts)
		})
	})
	rels = wu(rels).map(function(rel){
		if(rel.k=='condition'||rel.k=='milestone') {
			return {
				k:rel.k,
				f:rel.t,
				t:rel.f,
				pred:rel.pred
			}
		}
		return rel
	})
	rels.each(function(rel){
		s.then(function(n){
			console.log('make',rel)
			cl.make(name,rel.k,rel.f,rel.t,rel.pred,n)
		})
	})
}

console.log('parsed',require('util').inspect(parsed,true,100))
var seq = futures.sequence()
if(parsed && parsed.type == 'process') {
	seq
	.then(function(n){client.connect(n)})
	.then(makeProcess(parsed,client))
	makeEvents(parsed.events,parsed.name,client,seq)
	makeRelations(parsed.events,parsed.name,client,seq)
	seq.then(function(){
		console.log('done')
		process.exit()
	})
}