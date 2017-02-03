function Sentence(array) {
	this.sentenceArray = array;
	// hack to clone and separate the two arrays
	this.correctSentenceArray = JSON.parse(JSON.stringify(array));
	this.length = 0;
}

Sentence.prototype.checkForErrors = function() {
	// For all i and j, d[i,j] will hold the Levenshtein distance between
	// the first i characters of s and the first j characters of t.
	// Note that d has (m+1)  x(n+1) values.
	var j = 0;
	var i = 0;
	var testLen = this.sentenceArray.length;
	var correctLen = this.correctSentenceArray.length;
	var memo = [];
	var errorCache = [];
	var capsErrors = 0;

	for (var x = 0; x <= correctLen; x++) {
		memo.push(Array(testLen));
		errorCache.push(Array(testLen));
	}
  
	for (var i = 0; i <= correctLen; i++) {
   	memo[i][0] = i; // the distance of any first string to an empty second string
   	errorCache[i][0] = {'caps':0, 'period': 0, 'quotes': 0, 'spacing': 0, 'misc': 0, 'apostrophe': 0}; // Not exactly true, but should work
	}

	for (var j = 0; j <= testLen; j++) {
		memo[0][j] = j;// the distance of any second string to an empty first string
		errorCache[0][j] = {'caps':0, 'period': 0, 'quotes': 0, 'spacing': 0, 'misc': 0, 'apostrophe': 0}; // see above
	}

	for (var i = 1; i <= correctLen; i++) {
		for (var j = 1; j <= testLen; j++) {
			if (this.sentenceArray[j-1]['tokenValue'] === this.correctSentenceArray[i-1]['tokenValue']) {
				memo[i][j] = memo[i-1][j-1];
				errorCache[i][j] = JSON.parse(JSON.stringify(errorCache[i-1][j-1]));
			}
			else if (this.sentenceArray[j-1]['tokenValue'].toLowerCase() === this.correctSentenceArray[i-1]['tokenValue'].toLowerCase()) {
				// want this special case to be 1 error, not 2
				memo[i][j] = memo[i-1][j-1] + 1;
				errorCache[i][j] = JSON.parse(JSON.stringify(errorCache[i-1][j-1]));
				errorCache[i][j]['caps']++;
			}
			else {
				// need to get error type
				// We know that words can't be added or deleted, so that simplifies the problem somewhat.

				var testTokenType = this.sentenceArray[j-1]['tokenType'];
				var testTokenValue = this.sentenceArray[j-1]['tokenValue'];
				var correctTokenType = this.correctSentenceArray[i-1]['tokenType'];
				var correctTokenValue = this.correctSentenceArray[i-1]['tokenValue'];
				var errorType = '';
				
				// 1. If correct type = word and the other is not a word, then the error type must be 
				// the the type of character in the user-submitted sentence
				if (correctTokenType === 'CapsWord' || correctTokenType === 'LowerWord') {
					errorType = this._getErrorTypeFromToken(testTokenType);
				}
				// 2. If the correct type isn't a word and but the incorrect type is, the error type is
				// the type in the correct sentence
				else if ('CapsWord' === testTokenType || 'LowerWord' === testTokenType) {
					errorType = this._getErrorTypeFromToken(correctTokenValue);
				}
				// 3. And if neither of them are words?
				else if ((memo[i-1][j] + 1) <= memo[i][j-1] + 1) {
					errorType = this._getErrorTypeFromToken(testTokenType);
				}
				else {
					errorType = this._getErrorTypeFromToken(correctTokenType);
				}

				if ((memo[i-1][j] + 1) <= memo[i][j-1] + 1) {
					memo[i][j] = memo[i-1][j] + 1;
					errorCache[i][j] = JSON.parse(JSON.stringify(errorCache[i-1][j]));
				}
				else {
					memo[i][j] = memo[i][j-1] + 1;
					errorCache[i][j] = JSON.parse(JSON.stringify(errorCache[i][j-1]));
				}
				errorCache[i][j][errorType]++;
			}
		}
	}
	
	return {
		"types": errorCache[correctLen][testLen],
		"count": (memo[correctLen][testLen])
	};
}

Sentence.prototype._getErrorTypeFromToken = function(tokenValue) {
	if ('.' === tokenValue) {
		return 'period';
	}
	else if ("\'" === tokenValue) {
		return 'apostrophe';
	}
	else if ('"' === tokenValue || '\"' === tokenValue) {
		return 'quotes';
	}
	else if (' ' === tokenValue) {
		return 'spacing';
	}
	else {
		//console.log("Picking misc for: " + tokenValue);
		return 'misc';
	}
}

Sentence.prototype.acceptChangedSentence = function(sentenceArray) {
	this.sentenceArray = sentenceArray;
}

Sentence.prototype.createErrors = function(userLevel) {
	var len = this.sentenceArray.length;
	var outputArray = [];

	for (var i = 0; i < len; i++) {
		var chanceCorrect = Math.random();

		if (chanceCorrect <= 0.05 * userLevel) { // useful to think of this as "within"
			if ('LowerWord' === this.sentenceArray[i]['tokenType']) {
				var value = this.sentenceArray[i]['tokenValue'];
				outputArray.push({
					"length": this.sentenceArray[i]['length'],
					"tokenType": "CapsWord",
					"tokenValue": value[0].toUpperCase() + value.slice(1, value.length)
				});
			}
			else if ('CapsWord' === this.sentenceArray[i]['tokenType']) {
				outputArray.push({
					"length": this.sentenceArray[i]['length'],
					"tokenType": "LowerWord",
					"tokenValue": this.sentenceArray[i]['tokenValue'].toLowerCase()
				});
			}
			else if ('Punctuation' === this.sentenceArray[i]['tokenType']) {
				// skip - remove this item
			}
			else if ('Space' === this.sentenceArray[i]['tokenType']) {
				// skip - remove this item
			}
			else if ('Possessive' === this.sentenceArray[i]['tokenType']) {
				// Currently not implemented, but on the drawing board
				outputArray.push(this.sentenceArray[i]);
			}
			else {
				// Uncompleted type. Stay safe and just push it on
				outputArray.push(this.sentenceArray[i]);
			}
		}
		else {
			outputArray.push(this.sentenceArray[i]);
		}
	}

	this.sentenceArray = outputArray;
}


function Parser(sentence) {
	this.sentence = sentence;
}

Parser.prototype.isLetter = function(char) {
	var re = /[a-zA-Z]/;
	return char.match(re) ? true : false;
}

Parser.prototype.isLowerCase = function(char) {
	var re = /^[a-z]/;
	return char.match(re) ? true : false;
}

Parser.prototype.isUpperCase = function(char) {
	var re = /^[A-Z]/;
	return char.match(re) ? true : false;
}

Parser.prototype.isPunctuation = function(char) {
	var re = /[\s\,\.\!\"\'\$\?]/;
	return char.match(re) ? true : false;
}

Parser.prototype.parseSentence = function() {
	var sentenceArray = [];

	var previous = '';
	var previousIsLetter = false;
	var token = '';
	var item = '';
	var len = this.sentence.length;

	var totalLength = 0;
	var CONST_CHAR_LEN = 33; // will need something better than this

	for (var i = 0; i < len; i++) {
		item = this.sentence[i];

		totalLength += CONST_CHAR_LEN;

		if (i === len - 1 && this.isPunctuation(item)) {
			// special case for the last item:
			// parse and put both on the sentenceArray

			sentenceArray.push(this.parseItem(token));
			sentenceArray.push(this.parseItem(item));
		}

		else if (previousIsLetter && this.isLetter(item)) {
			// part of the same word
			token += item;
			previous = item;
		}
		else if (previousIsLetter && this.isPunctuation(item)) {
			// end of the word
			previous = item;
			sentenceArray.push(this.parseItem(token));
			previousIsLetter = false;
		}
		else if (!previousIsLetter && '' !== previous && this.isLetter(item)) {
			// start of a new word
			sentenceArray.push(this.parseItem(previous));
			previous = item;
			previousIsLetter = true;
			token = item;
		}
		else if (!previousIsLetter && '' !== previous && this.isPunctuation(item)) {
			// doubled punctuation (e.g. "test?")
			sentenceArray.push(this.parseItem(previous));
			previous = item;
			previousIsLetter = false;
			token = item;
		}

		else if (previous === '') {
			// finally, a special case for the first item
			token = item;
			previous = item;

			if (this.isLetter(item)) {
				previousIsLetter = true;
			}
		}

	}
	//console.log('total length: ' + totalLength);
	var s = new Sentence(sentenceArray);
	s.length = totalLength;
	return s;
}

Parser.prototype.parseItem = function(item) {
	/*
		returns token object
		type: CapsWord | LowerWord | Space | Punctuation
		value: item
	*/

	var token = {
		'tokenType': '',
		'tokenValue': item,
		'length': item.length
	};

	if (this.isLowerCase(item)) {
		token.tokenType = 'LowerWord';
	}
	else if (this.isUpperCase(item)) {
		token.tokenType = 'CapsWord';
	}
	else if (' ' === item) {
		token.tokenType = 'Space';
	}
	else {
		token.tokenType = 'Punctuation';
	}

	return token;
}