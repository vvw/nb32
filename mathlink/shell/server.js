var express = require('express');
var app = express();
var spawn = require('child_process').spawn;

app.get('/:jpgname', function(req, res){
	var reconize=function(jpgname){
       		 //var child = spawn('./t.m', ['t.jpg']);
		 //var jpgname=req.param('jpgname');	
                 var child = spawn('./t.m', [jpgname]);
       		 child.stdout.on('data', function(chunk) {
			 res.send('hi,,,'+chunk);
       		 });

	}
	var jpgname=req.param('jpgname');
	reconize(jpgname);
});

app.listen(3000);
