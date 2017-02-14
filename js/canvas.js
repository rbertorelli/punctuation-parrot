/*
	Note, this (especially ViewState) is an expansion of the HTML5 canvas tutorial code from Simon Sarris.
	See https://github.com/simonsarris/HTML5Unleashed/blob/master/Chapter%206/6.11.html
*/

function Shape(x, y, w, h, fill, ctx, padding, direction) {

	this.x = x || 0; // perhaps some validation
	this.y = y || 0;
	this.w = w || 1;
	this.h = h || 1;
	this.ctx = ctx;
	this.fill = fill || 'rgba(111, 111, 111, .1)';
	this.SUBSHAPE_PADDING = padding || 0;

	this.subShapes = [];
	this.subShapeCount = 0;
	this.subShapesTotalLength = 0;

	this.direction = direction || 'vertical';

	// determine which dimensions to use depending on direction
	this.balanceDimension = this.direction === 'vertical' ? 'y' : 'x';
	this.dimensionLen = this.balanceDimension === 'y' ? 'h' : 'w';
	this.constDimension = this.balanceDimension === 'y' ? 'x' : 'y';
	this.constDimLen = this.constDimension === 'y' ? 'h' : 'w';

}

Shape.prototype.isShape = function() {
	return typeof this.x !== 'undefined'
		&& typeof this.y !== 'undefined'
		&& typeof this.w !== 'undefined'
		&& typeof this.h !== 'undefined';
}

Shape.prototype.contains = function(mx, my) {
	// All we have to do is make sure the Mouse X,Y fall in the area between
	// the shape's X and (X + Width) and its Y and (Y + Height)
	return  (this.x <= mx) && (this.x + this.w >= mx) &&
		(this.y <= my) && (this.y + this.h >= my);
}

Shape.prototype.overlaps = function(shape) {
	// the shapes are all rectangles that are wider
	// than they are tall, so this is simple

	return (shape !== this 
		&& (shape.contains(this.x, this.y)
			|| shape.contains(this.x, this.y + this.h)
			|| shape.contains(this.x + this.w, this.y)
			|| shape.contains(this.x + this.w, this.y + this.h)
			|| this.contains(shape.x, shape.y)
			|| this.contains(shape.x, shape.y + shape.h)
			|| this.contains(shape.x + shape.w, shape.y)
			|| this.contains(shape.x + shape.w, shape.y + shape.h)
	));
}

Shape.prototype.draw = function() {
	this.clear();
	this.ctx.fillStyle = this.fill;
	this.ctx.fillRect(this.x, this.y, this.w, this.h);

	for (var i = 0; i < this.subShapeCount; i++) {
		this.subShapes[i].draw();
	}
}

Shape.prototype.clear = function() {
	this.ctx.clearRect(this.x, this.y, this.w, this.h);
}

Shape.prototype.addSubShape = function(shape, position) {
	if (Shape.prototype.isPrototypeOf(shape)) {

		if (0 === this.subShapeCount || -1 === position) {
			this.subShapes.push(shape);
		}
		else {
			var first = this.subShapes.slice(0, position);
			var second = this.subShapes.slice(position, this.subShapeCount);

			this.subShapes = first.concat([shape]).concat(second);
		}

		this.subShapeCount++;
		this.subShapesTotalLength += shape[this.dimensionLen];

		// set the x and y values for the new Shape
		shape.y = this.y + this.padding;
		this.balance();
		this.draw();
		
	}
	else {
		// is not a Shape-like object
		console.log('Error: trying to add a non-Shape');
	}
}

Shape.prototype.removeSubShape = function(index) {
	//var first = this.subShapes.slice(0, index);
	//var second = this.subShapes.slice(index + 1, this.subShapeCount - 1);
	//this.subShapes = first.concat(second);

	this.subShapesTotalLength -= this.subShapes[index][this.dimensionLen];
	this.subShapes.splice(index, 1);
	--this.subShapeCount;
}

Shape.prototype.balance = function() {
	// two ways to balance - equi-distant, clustered
	// default should be equi-distant (sub-prototypes can override)

	// padding needs to be kept constant
	var padding = (this[this.dimensionLen] - this.subShapesTotalLength) / (this.subShapeCount + 1);
	var start = padding;

	for (var i = 0; i < this.subShapeCount; i++) {
		this.subShapes[i][this.constDimension] = this[this.constDimension] + (this[this.constDimLen] - this.subShapes[i][this.constDimLen]) / 2;		
		this.subShapes[i][this.balanceDimension] = start;
		start += this.subShapes[i][this.dimensionLen] + padding;
	}

}

/**
 *	Setting up Types:
 */

// Vertical container for rows
function Container(x, y, w, h, ctx) {
	var fill = 'rgba(127, 255, 212, .01)';
	Shape.call(this, x, y, w, h, fill, ctx, 0, 'vertical');

	this.ROW_HEIGHT = 60; // temp - inject when appropriate
	this.ROW_PADDING = 0;
	this.ROW_WIDTH = w * .8;
}

Container.prototype = new Shape();
Container.prototype.constructor = Container;

Container.prototype.createRow = function() {
	// based on presence (or not) of other rows,
	// instantiate row with defined height, width, (x,y), and fill
	// set padding
	// have some idea of letter/token size
	var newRow = new Row(
		0,
		0,
		this.ROW_WIDTH,
		this.ROW_HEIGHT,
		this.ctx
	);

	this.addSubShape(newRow, -1);
}

Container.prototype.ingestSentence = function(sentence, metadata) {
	
	this.sentence = sentence;
	this.metadata = metadata;

	var characterSize = '30px'; // might have to replace/improve on this
	// need pixel measure of sentence length
	// need to round up for row count - remainders go in their own row, after all
	var rowCount = Math.ceil(this.sentence.length / this.ROW_WIDTH); 

	for (var i = 1; i <= rowCount; i++) {
		this.createRow();
	}

	var len = this.sentence.sentenceArray.length;
	var pixelCount = 0;

	for (var j = 0; j < len; j++) {
		pixelCount += (this.sentence.sentenceArray[j].length * 33);
		var currentRow = Math.floor(pixelCount / this.ROW_WIDTH);
		this.subShapes[currentRow].insertValueAsToken(this.sentence.sentenceArray[j]);
	}
}

Container.prototype.updateSentence = function() {
	var newSentenceArray = [];
	for (var r = 0; r < this.subShapeCount; r++) {
		var row = this.subShapes[r];
		for (var t = 0; t < row.subShapeCount; t++) {
			newSentenceArray.push(row.subShapes[t].value);
		}
	}

	this.sentence.acceptChangedSentence(newSentenceArray);
}


// Horizontal container for tokens
function Row(x, y, w, h, ctx) {
	var fill = 'rgba(40, 40, 60, .2)';
	Shape.call(this, x, y, w, h, fill, ctx, 0, 'horizontal');
	this.padding = 5;
	this.TOKEN_HEIGHT = this.h - 2 * this.padding;
}

Row.prototype = new Shape();
Row.prototype.constructor = Row;

Row.prototype.insertValueAsToken = function(value) {
	this.addSubShape(
		new Token(0, this.y + this.padding, this.TOKEN_HEIGHT, value, this.ctx),
		-1
	);
}

Row.prototype.balance = function() {
	// take all objects and get the proper x values for them
	// what happens if it is too long?
	// let the container handle that (for now)

	// this should not be equi-distant, but rather keep inter-token distances the same

	var start = this.x + (this.w - this.subShapesTotalLength) / 2;
	this.clear();

	for (var i = 0; i < this.subShapeCount; i++) {
		this.subShapes[i].x = start;
		start = start + this.subShapes[i].w + this.padding;
	}

	this.draw();
};


// Shape holding the grammatical tokens
function Token(x, y, h, value, ctx) {
	var w = ctx.measureText(value.tokenValue).width;
	//this.fill = '#ab3c17';
	this.fill = 'rgb(230,230,230)';
	Shape.call(this, x, y, w, h, this.fill, ctx, 0, 'horizontal');

	this.value = value;
	this.ctx.fillStyle = this.fill;
	this.ctx.textBaseline = 'hanging';
	this.ctx.font = "48px Courier New"; // this should depend on height
	ctx.strokeStyle = 'red';
	ctx.lineWidth = 1;
}

Token.prototype = new Shape();
Token.prototype.constructor = Token;

Token.prototype.draw = function() {
	this.ctx.fillStyle = this.fill;
	this.ctx.fillRect(this.x, this.y, this.w, this.h);

	this.ctx.fillStyle = 'rgb(171,60,23)';
	this.ctx.fillText(this.value.tokenValue, this.x, this.y);
	//this.ctx.strokeRect(this.x, this.y, this.w, this.h);
}

Token.prototype.click = function() {
	if ('LowerWord' === this.value.tokenType) {
		// capitalize first letter and redraw

		var v = this.value.tokenValue;
		this.value.tokenValue = v[0].toUpperCase() + v.slice(1, v.length);
		this.value.tokenType = 'CapsWord';
		return 'switch case';
	}
	else if ('CapsWord' === this.value.tokenType) {
		this.value.tokenValue = this.value.tokenValue.toLowerCase();
		this.value.tokenType = 'LowerWord';
		return 'switch case';
	}
	else if ('Punctuation' === this.value.tokenType) {
		// delete the punctuation mark from the array ... super-parent should then know to re-balance
		return 'delete';
	}
	else {
		// spaces - do nothing ??? perhaps remove?
	}
}



// Selections - Bottom container for draggable add-ons to game

function Selections(x, y, w, h, ctx) {
	var fill = 'rgba(245, 222, 179, .5)';
	var direction = 'horizontal';
	Shape.call(this, x, y, w, h, fill, ctx, 0, direction);
	this.padding = 10;

	this.ADD_ON_HEIGHT = 50;
	this.ADD_ON_WIDTH = 100;

	this.createAddOns(); // need to initialize this

	// Draw upon creation
	this.draw();

}

Selections.prototype = new Shape();
Selections.prototype.constructor = Selections;

Selections.prototype.createAddOns = function() {
	var xStart = this.x + 5;

	var xStart2 = xStart + this.ADD_ON_WIDTH + 5;
	var xStart3 = xStart2 + this.ADD_ON_WIDTH + 5;
	var xStart4 = xStart3 + this.ADD_ON_WIDTH + 5;
	var xStart5 = xStart4 + this.ADD_ON_WIDTH + 5;
	var xStart6 = xStart5 + this.ADD_ON_WIDTH + 5;
	var xStart7 = xStart6 + this.ADD_ON_WIDTH + 5;
	var xStart8 = xStart7 + this.ADD_ON_WIDTH + 5;

	var period = {
		'tokenValue': '.',
		'tokenType': 'Punctuation',
		'length': 1
	};

	var comma = {
		'tokenValue': ',',
		'tokenType': 'Punctuation',
		'length': 1
	};

	var quote = {
		'tokenValue': '"',
		'tokenType': 'Punctuation',
		'length': 1
	};

	var apostrophe = {
		'tokenValue': '\'',
		'tokenType': 'Punctuation',
		'length': 1
	};

	var qmark = {
		'tokenValue': '?',
		'tokenType': 'Punctuation',
		'length': 1
	};

	var exclam = {
		'tokenValue': '!',
		'tokenType': 'Punctuation',
		'length': 1
	};

	var space = {
		'tokenValue': ' ',
		'tokenType': 'Space',
		'length': 1
	};

	this.addSubShape(new AddOn(xStart2, this.y + this.padding, this.ADD_ON_HEIGHT, comma, this.ctx), 0);
	this.addSubShape(new AddOn(xStart3, this.y + this.padding, this.ADD_ON_HEIGHT, space, this.ctx), 0);
	this.addSubShape(new AddOn(xStart4, this.y + this.padding, this.ADD_ON_HEIGHT, period, this.ctx), 0);
	this.addSubShape(new AddOn(xStart5, this.y + this.padding, this.ADD_ON_HEIGHT, quote, this.ctx), 0);
	this.addSubShape(new AddOn(xStart6, this.y + this.padding, this.ADD_ON_HEIGHT, apostrophe, this.ctx), 0);
	this.addSubShape(new AddOn(xStart7, this.y + this.padding, this.ADD_ON_HEIGHT, qmark, this.ctx), 0);
	this.addSubShape(new AddOn(xStart8, this.y + this.padding, this.ADD_ON_HEIGHT, exclam, this.ctx), 0);

}

Selections.prototype.create = function(addOn) {
	// duplicate the selected add-on
	var newAddOn = new AddOn(addOn.x, addOn.y, addOn.h, addOn.value, addOn.ctx);
	newAddOn.draw();
	return newAddOn;
}


// Add Ons - The draggable items themselves
function AddOn(x, y, h, value, ctx) {
	this.testing = 'y';
	this.fill = 'red';
	var direction = '';
	ctx.font = "48px Courier New"; // this should depend on height. Also need this here to standardize width
	var w = ctx.measureText(value.tokenValue).width;

	Shape.call(this, x, y, w, h, this.fill, ctx, 0, direction);
	this.value = value || {
		'tokenValue': '',
		'tokenType': ''
	};

	this.ctx.fillStyle = this.fill;
	this.ctx.textBaseline = 'hanging';
	ctx.strokeStyle = 'red';
	ctx.lineWidth = 1;
}

AddOn.prototype = new Shape();
AddOn.prototype.constructor = AddOn;

AddOn.prototype.convertToToken = function() {
	return new Token(this.x, this.y, this.h, this.value, this.ctx);
}

AddOn.prototype.draw = function() {
	this.ctx.fillStyle = this.fill;
	this.ctx.fillText(this.value.tokenValue, this.x, this.y);
	this.ctx.strokeRect(this.x, this.y, this.w, this.h);
}

/**
 *	Submit Button
 *
 */

function SubmitButton(x, y, w, h, ctx) {
	var fill = 'green';
	var direction = '';
	this.text = 'Ask the parrot!';
	Shape.call(this, x, y, w, h, fill, ctx, 0, direction);
}

SubmitButton.prototype = new Shape();
SubmitButton.prototype.constructor = SubmitButton;

SubmitButton.prototype.draw = function() {
	this.clear();
	this.ctx.fillStyle = 'rgb(166,218,42)';
	this.ctx.fillRect(this.x, this.y, this.w, this.h);
	this.ctx.fillStyle = this.fill;
	this.ctx.fillStyle = this.fill;
	var textWidth = this.ctx.measureText(this.text).width
	this.ctx.fillText(this.text, this.x + .5 * this.w - .5 * textWidth, this.y + 10);
}

/**
 *	ViewState
 *	Top-level container for game area parts
 */

function ViewState(canvas) {
	// Overall object that handles the entire board

	this.canvas = canvas;
	this.width = canvas.width;
	this.height = canvas.height;
	this.ctx = canvas.getContext('2d');
	//this.addOns = []; // INITIALIZE THIS ARRAY - needs to be the list of draggable objects (punctuation)

	// TO DO:
	this.container = new Container(0, 0, this.width, this.height * .8, this.ctx);
	this.selections = new Selections(0, this.height * .8, this.width, this.height * .1, this.ctx);
	this.submitButton = new SubmitButton(0, this.height * .9, this.width, this.height * .1, this.ctx);

	var stylePaddingLeft, stylePaddingTop, styleBorderLeft, styleBorderTop;
 	if (document.defaultView && document.defaultView.getComputedStyle) {
	    this.stylePaddingLeft = parseInt(document.defaultView.getComputedStyle(canvas, null)['paddingLeft'], 10)      || 0;
	    this.stylePaddingTop  = parseInt(document.defaultView.getComputedStyle(canvas, null)['paddingTop'], 10)       || 0;
	    this.styleBorderLeft  = parseInt(document.defaultView.getComputedStyle(canvas, null)['borderLeftWidth'], 10)  || 0;
	    this.styleBorderTop   = parseInt(document.defaultView.getComputedStyle(canvas, null)['borderTopWidth'], 10)   || 0;
  	}


	// Some pages have fixed-position bars (like the stumbleupon bar) at the top or left of the page
	// They will mess up mouse coordinates and this fixes that
	var html = document.body.parentNode;
	this.htmlTop = html.offsetTop;
	this.htmlLeft = html.offsetLeft;

	this.valid = false; // when set to false, the canvas will redraw everything
	this.shapes = [];  // the collection of things to be drawn
	this.dragging = false; // Keep track of when we are dragging
	this.selection = null; // the current selected object.
	this.dragoffx = 0; // See mousedown and mousemove events for explanation
	this.dragoffy = 0;

	var myState = this;

	/**
	 *	Event Listeners
	 */
	canvas.addEventListener('selectstart', function(e) { e.preventDefault(); return false; }, false);

	canvas.addEventListener('mousedown', function(e) {
		var mouse = myState.getMouse(e);
		var mx = mouse.x;
		var my = mouse.y;

		// what can be interacted with?

		if (myState.selections.contains(mx, my)) {
			// one of the draggables at the bottom
			var addOns = myState.selections.subShapes;

			var l = addOns.length;
			for (var i = l-1; i >= 0; i--) {
				if (addOns[i].contains(mx, my)) {
					var mySel = myState.selections.create(addOns[i]); // Need to create duplicate object for dragging

					// Keep track of where in the object we clicked
					// so we can move it smoothly (see mousemove)
					myState.dragoffx = mx - mySel.x;
					myState.dragoffy = my - mySel.y;
					myState.dragging = true;
					myState.selection = mySel;
					myState.valid = false;
					return;
				}
			}
		}
		else {
			// in the sentence area above
			for (var rowIndex = 0; rowIndex < myState.container.subShapeCount; rowIndex++) {
				for (var tokenIndex = 0; tokenIndex < myState.container.subShapes.subShapeCount; tokenIndex++) {
					if (myState.container.subShapes[rowIndex].subShapes[tokenIndex].contains(mx, my)) {
						// then split action based on token type
						if (false) {}
					}
				}
			}

		}

		// havent returned means we have failed to select anything.
		// If there was an object selected, we deselect it
		if (myState.selection) {
			myState.selection = null;
			myState.valid = false; // Need to clear the old selection border
		}
	}, true);

	canvas.addEventListener('mousemove', function(e) {
		if (myState.dragging) {
			var mouse = myState.getMouse(e);
			// We don't want to drag the object by its top-left corner, we want to drag it
			// from where we clicked. Thats why we saved the offset and use it here
			myState.selection.x = mouse.x - myState.dragoffx;
			myState.selection.y = mouse.y - myState.dragoffy;
			myState.selection.xEnd = myState.selection.x + myState.selection.w;
			myState.selection.yEnd = myState.selection.y + myState.selection.h;
			myState.valid = false; // Something's dragging so we must redraw

		}
  }, true);

  canvas.addEventListener('mouseup', function(e) {
		var mouse = myState.getMouse(e);
		var mx = mouse.x;
		var my = mouse.y;

		// dragging and dropped item overlaps row tokens
		if (myState.dragging && myState.container.overlaps(myState.selection)) {
			myState.dragging = false;

			// find if it overlaps with a row
			for (var i = 0; i < myState.container.subShapeCount; i++) {
				var row = myState.container.subShapes[i];
				if (row.overlaps(myState.selection)) {

					var finalRow = row;
					var indexes = []; // need to keep track of which instances are overlapped

					for (var j = 0; j < row.subShapeCount; j++) {
						if (row.subShapes[j].overlaps(myState.selection)) {
							// find the likely position
							indexes.push(j);
						}
					}

					// might need to check if it overlaps multiple rows
					// also check here if it overlaps a drawn row but not any tokens 
				}
			}
			
			if (typeof indexes !== 'undefined' && indexes.length > 0) {
				// convert selection to Token
				var newToken = myState.selection.convertToToken();
				var position = 0;

				// specify the likely position
				if (1 === indexes.length && 0 === indexes[0]) {
					position = 0;
					// this is a little ugly .. perhaps refactor?
					if (finalRow.subShapes[indexes[0]].x > myState.selection.x) {
						position = 0;
					}
					else {
						position = 1;
					}
					// need to further specify here for singletons:
					// is this in the first half or second half?
				}
				else if (1 === indexes.length && (finalRow.subShapeCount - 1) === indexes[0]) {
					position = indexes[0] + 1;
				}
				else if (1 === indexes.length) {
					// edge case with a slight overlap of just one token
					// do later - this is definitely not the conditional
				}
				else {
					// overlaps two
					position = indexes[1];
				}
				// insert Token at specified location

				finalRow.addSubShape(newToken, position);
			}

			myState.valid = false;
			myState.selection = null;

			// Pass sentence changes on to container
			myState.container.updateSentence();

		}
		
		else if ( myState.submitButton.contains(mx, my) ) {
			//check correct number
			var result = myState.container.sentence.checkForErrors();
			var attemptStatus;

			if (result.count > 0) {
				displayModal("Your sentence has " + result.count + " errors!");
				attemptStatus = false;
			}
			else {
				displayModal("The sentence is correct!");
				attemptStatus = true;
			}

			updateSentenceAttempt(
				myState.container.metadata.sentenceId,
				myState.container.metadata.studentId,
				attemptStatus,
				result.types);

		}
		
		else {
			// need to get location - if it's a token, execute click()
			var mouse = myState.getMouse(e);
			
			for (var i = 0; i < myState.container.subShapeCount; i++) {
				var row = myState.container.subShapes[i];
				for (var j = 0; j < row.subShapeCount; j++) {
					var token = row.subShapes[j];
					var result = '';

					if (token.contains(mouse.x, mouse.y)) {
						result = token.click();
						
						if ('delete' === result) {
							row.removeSubShape(j);
						}

						row.balance();
						myState.container.updateSentence();
						return;
					}
				}
			}

		}

  }, true);


	this.selectionColor = '#CC0000';
	this.selectionWidth = 2;  
	this.interval = 30;
	setInterval(function() { myState.draw(); }, myState.interval);
}

// While draw is called as often as the INTERVAL variable demands,
// It only ever does something if the canvas gets invalidated by our code
ViewState.prototype.draw = function() {
	// if our state is invalid, redraw and validate!
	// draw with width?

	if (!this.valid) {
		var ctx = this.ctx;

		var shapes = this.shapes;

		this.clear();
		this.container.draw();
		this.selections.draw();
		this.submitButton.draw();
		//this.container.drawSubShapes();

		// draw selection
		// right now this is just a stroke along the edge of the selected Shape
		if (this.selection != null) {
			ctx.strokeStyle = this.selectionColor;
			ctx.lineWidth = this.selectionWidth;
			var mySel = this.selection;
			ctx.strokeRect(mySel.x,mySel.y,mySel.w,mySel.h);
		}

		this.valid = true;
	}
}

ViewState.prototype.getMouse = function(e) {
	var element = this.canvas, offsetX = 0, offsetY = 0, mx, my;

	// Compute the total offset
	if (element.offsetParent !== undefined) {
		do {
			offsetX += element.offsetLeft;
			offsetY += element.offsetTop;
		} while (element = element.offsetParent);
	}

	// Add padding and border style widths to offset
	// Also add the <html> offsets in case there's a position:fixed bar
	offsetX += this.stylePaddingLeft + this.styleBorderLeft + this.htmlLeft;
	offsetY += this.stylePaddingTop + this.styleBorderTop + this.htmlTop;

	mx = e.pageX - offsetX;
	my = e.pageY - offsetY;

	return {x: mx, y: my};
}

ViewState.prototype.clear = function() {
	this.shapes = [];
	this.ctx.clearRect(this.x, this.y, this.w, this.h);
}

ViewState.prototype.ingestSentence = function(sentence, metadata) {
	this.shapes = [];
	this.clear();
	this.container = new Container(0, 0, this.width, this.height * .8, this.ctx);
	this.container.ingestSentence(sentence, metadata);
}