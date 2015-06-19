// Based on https://github.com/ajaxorg/ace/blob/master/lib/ace/ext/static_highlight.js

var EditSession = ace.require("ace/edit_session").EditSession;
var TextLayer = ace.require("ace/layer/text").Text;

function highlightCodeHTML(mode, input) {
  var session = new EditSession("");
  session.setUseWorker(false);
  session.setMode(mode);

  var textLayer = new TextLayer(document.createElement("div"));
  textLayer.setSession(session);
  textLayer.config = {
      characterWidth: 10,
      lineHeight: 20
  };

  session.setValue(input);

  var results = [];
  var length = session.getLength();
  for(var ix = 0; ix < length; ix++) {
      var lineBuilder = [];
      textLayer.$renderLine(lineBuilder, ix, true, false);
      results.push(lineBuilder.join(""));
  }

  textLayer.destroy();

  return results;
}

// Given a container of spans (such as a line of code highlighted by
// ace), yields a list of text intervals with class..
function spanContainerToSpans(el) {
  var results = [];
  var j = 0;
  for(var i = 0; i < el.childNodes.length; i++) {
    var child = el.childNodes[i];
    switch (child.nodeType) {
      case 1: // element
        var txt = child.innerText;
        var className = child.getAttribute("class")
        var to = j + txt.length;
        results.push([j, to, className]);
        j = to;
        break;
      case 3: // text node
        j += child.nodeValue.length;
        break;
    }
  }
  return results;
}

function positionControlsOnResize(sohControls, sohColumn) {
  $(sohControls).css('position', 'fixed');
  $(sohControls).css('bottom', '0');
  var positionControls = debounce(100, function () {
    $(sohControls).css('left', $(sohColumn).offset().left - $(window).scrollLeft());
    $(sohControls).css('width', $(sohColumn).width());
  });
  positionControls();
  $(window).resize(positionControls);
  $(window).scroll(positionControls);
}

// Debounce the given function
function debounce(ms, f){
  var t;
  return function(x){
    clearTimeout(t);
    t = setTimeout(function(){
      f(x);
    },ms);
  };
}
