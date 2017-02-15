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

var vs = new ViewState(document.getElementById('game'));

var gameInit = function() {
  var request = new XMLHttpRequest();
  request.open("GET", "./getSentence");

  request.onload = function() {
    if (200 === request.status) {
      var sentence = JSON.parse(request.responseText);

      if (typeof sentence.Right !== 'undefined') {
        var parser = new Parser(sentence.Right.text);
        var s = parser.parseSentence();

        s.createErrors(sentence.Right.studentLevel);
      
        vs.ingestSentence(s, {
          "studentId": sentence.Right.studentId,
          "studentLevel": sentence.Right.studentLevel,
          "sentenceId": sentence.Right.sentenceId
        });
      }
      else if (typeof sentence.Left !== 'undefined') {
        displayFinal(sentence.Left.text);
      }
      else {
        displayModal("Error with getting sentence.");
      }
      
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

function displayFinal(countArray) {
  var textarea = document.getElementById('modal-text');
  arrayLen = countArray.length;

  var ul = document.createElement('ul');
  var img = document.createElement('img');

  img.setAttribute('src', 'images/parrot.png');

  if (arrayLen === 0) {
    ul.innerText = "Perfect score!";
    img.setAttribute('id', 'img');
  }
  else {
    ul.innerText = "Your errors:";
  }

  for (var i = 0; i < arrayLen; i++) {
    var element = document.createElement('li');
    element.innerText = countArray[i];
    ul.appendChild(element);
  }

  textarea.appendChild(ul);
  textarea.appendChild(img);

  var modal = document.getElementById('modal-overlay');
  modal.style.display = "block";
}