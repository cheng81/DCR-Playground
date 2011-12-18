var Client = require('../lib/dcrdeclient')

var id = process.argv[2]
if(!id) {
	console.log('usage: node dcrexit.js <dcrid>')
	return
}
console.log('exit dcr',id)

var client = new Client()
client.connect(function(){
	client.exit(id,function(r){
		console.log('done',r)
		process.exit()
	})
})