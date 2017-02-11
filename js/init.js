document.onreadystatechange = function () {
  if ("complete" === document.readyState) {
    window.onresize = function () {
      canvas = document.getElementById('game');
      context = canvas.getContext('2d');
      container = document.getElementById('container');

      // TODO: redraw on resize
      //canvas.width = container.width;
      //canvas.height = container.height;

      // call something to redraw
    }

    var span = document.getElementById('close');

    span.onclick = function() {
      var modal = document.getElementById('modal-overlay');
      modal.style.display = "none";
    }

  }
}

function gameInit() {
  var vs = new ViewState(document.getElementById('game'));

  /* Execute */

  var request = new XMLHttpRequest();
  request.open("GET", "./getSentence");

  request.onload = function() {
    if (200 === request.status) {
      var sentence = JSON.parse(request.responseText);

      var parser = new Parser(sentence.text);
      var s = parser.parseSentence();

      s.createErrors(sentence.studentLevel);
      
      vs.ingestSentence(s, {
        "studentId": sentence.studentId,
        "studentLevel": sentence.studentLevel,
        "sentenceId": sentence.sentenceId
      });
    }
    else {
      console.log('Sentence init request failed');
    }
  }

  request.send();
}

function updateSentenceAttempt(sentenceId, studentId, attemptStatus, data) {
  var request = new XMLHttpRequest();

  request.open("POST", "./updateStats");
  request.setRequestHeader('Content-Type', 'application/json');
  
  data['studentId'] = studentId;
  data['sentenceId'] = sentenceId;
  data['attemptStatus'] = attemptStatus;

  request.onload = function() {
    if (200 === request.status && attemptStatus) {
      // Right now, go in and pull another sentence.
      gameInit();
    }
    else if (200 === request.status) {
      // just an update for a failed attempt
      console.log('success, but sentence failure');
    }
    else {
      //alert("Submission failed. Try again. If this happens repeatedly please contact support.");
      displayModal("Submission failed. Try again. If this happens repeatedly please contact support.");

    }
  };

  request.send(JSON.stringify(data));
}

function displayModal(message) {
  var modal = document.getElementById('modal-overlay');
  modal.style.display = "block";
  var textarea = document.getElementById('modal-text');
  textarea.innerText = message;

}