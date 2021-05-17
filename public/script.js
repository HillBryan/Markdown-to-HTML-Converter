const url = "http://localhost:3000/api/convert";
let html = "";

$(document).ready(function () {
  var textareas = document.getElementsByTagName('textarea');
  var count = textareas.length;
  for(var i=0;i<count;i++){
      textareas[i].onkeydown = function(e){
          if(e.keyCode==9 || e.which==9){
              e.preventDefault();
              var s = this.selectionStart;
              this.value = this.value.substring(0,this.selectionStart) + "\t" + this.value.substring(this.selectionEnd);
              this.selectionEnd = s+1;
          }
      }
  }

  $('#textbox').keyup(() => {
    if ($('#textbox').val() == "") {
      $('#result').val('');
    }
    else {
          apiCall($('#textbox').val());
    }
  });
});

function apiCall(input) {
  fetch(url, {
    method: "POST",
    headers: {
      'Accept': 'application/json',
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({Text: input})
  }).then(response => response.json())
    .then(data => ($('#result').val(data)));
}
