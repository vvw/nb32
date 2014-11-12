var spawn = require('child_process').spawn;
var child = spawn('./t.m', ['t.jpg']);

child.stdout.on('data', function(chunk) {
	console.log('hi,,,'+chunk);  
// output here
});
