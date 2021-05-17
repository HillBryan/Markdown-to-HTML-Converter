var express = require('express');
const util = require('util');
const execFile = util.promisify(require('child_process').execFile);
var router = express.Router();

/* GET home page. */
router.get('/', function(req, res, next) {
  res.render('index', { title: 'Express' });
});

router.post('/api/convert', function(req,res,next) {
  getHTML(req.body.Text).then((value) => res.send(value));
});

async function getHTML(req) {
  const { stdout } = await execFile('./converter-proj', [req]);
  return stdout;
}


module.exports = router;
