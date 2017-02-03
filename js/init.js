function init() {
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
      init();
    }
    else if (200 === request.status) {
      // just an update for a failed attempt
      console.log('success, but sentence failure');
    }
    else {
      alert("Submission failed. Try again. If this happens repeatedly please contact support.");
    }
  };

  request.send(JSON.stringify(data));
}