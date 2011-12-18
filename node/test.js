var futures = require('futures')
  , Client = require('./lib/dcrdeclient')

var cl = new Client()

var c = function(fn) {
	return function(d) {
		console.log(d),
		fn(d)
	}
}

var seq = futures.sequence()
seq.then(function(n){
	cl.connect(n)
}).then(function(n){
	cl.new("foo",c(n))
}).then(function(n){
	cl.init_events("foo",[{name:'a',fields:['foo']},{name:'b',fields:['bar','baz']}],c(n))
}).then(function(n){
	cl.init_included("foo",['a','b'],c(n))
}).then(function(n){
	cl.exec('foo','a',{foo:5},c(n))
}).then(function(n){
	cl.state('foo',n)
}).then(function(n,state){
	console.log(state)
	cl.dcr('foo',n)
}).then(function(n,dcr){
	console.log(dcr)
	cl.disconnect()
})
