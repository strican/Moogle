// Colorize SML code.
//
// colorize(id) recolors the code found in the <pre> element 'id'
//
// fetch_code(id, url) fetches code from the URL 'url', colors it, and inserts
// it into the <pre> element 'id'.
// 

var keyword_list = new Array("val", "fun", "signature", "sig", "struct",
  "if", "else", "then", "case", "of", "let", "in", "end", "structure", "type",
  "andalso", "orelse", "datatype", "exception", "fn", "handle", "raise",
  "where", "local", "with", "functor", "funsig", "and", "open");

var keywords = new Object;

for (k in keyword_list) {
    keywords[keyword_list[k]] = 1;
}

function colorize(id) {
    colorize_node(document.getElementById(id));
}

function colorize_text(t) {
    if (window.navigator.appVersion.match(/Safari/) && n.tagName !="CODE") {
      t=t.replace(/&/g,'&amp;').                                         
          replace(/>/g,'&gt;').                                           
          replace(/</g,'&lt;').                                           
          replace(/"/g,'&quot;');      
    }
    var tokens = t.split(/\b/g);
    var newt = "";
    var commenting = false;
	var instring = false;
    for (var i in tokens) {
	if (!commenting && tokens[i].match(/\(\*/)) {
	    tokens[i] = tokens[i].replace(/\(\*/, "<span-class=\"comment\">(*");
	    newt += tokens[i];
	    commenting = true;
	} else if (keywords[tokens[i]] && !commenting && !instring) {
	    newt += '<span-class="keyword">' + tokens[i] + '</span>';
	} else if (tokens[i].match(/\*\)\s+\(\*/)) {
	    newt += tokens[i];
	    // commenting unchanged
	} else if (tokens[i].match(/\*\)/)) {
	    newt += tokens[i].replace(/\*\)/, "*)</span>");
	    commenting = false;
	} else if (tokens[i].match(/"/)) {
	    instring = !instring;
	    newt += tokens[i];
	} else {
	    newt += tokens[i];
	}
    }

    if (window.navigator.appName.match(/Internet Explorer/)) {
	// IE consumes whitespace when the innerHTML property is
	// assigned. So replace newlines with explicit breaks,
	// spaces with nbsps, tabs with eight nbsps.
	newt = newt.replace(/\r\n/g, "<br>");
	newt = newt.replace(/\n/g, "<br>");
	newt = newt.replace(/ /g, "&nbsp;");
	newt = newt.replace(/\t/g, "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
    }
    // this substitution is needed to support the IE compatibility hacks.
    newt = newt.replace(/span-class/g, "span class");
    return newt;
}

function colorize_node(n) {
    var t = n.innerHTML;
    if (t == null) return;
    n.innerHTML = colorize_text(t);
}

function colorize_all() {
    var pres = document.getElementsByTagName("pre");
    for (i=0; i < pres.length; i++) {
	colorize_node(pres[i]);
    }
    pres = document.getElementsByTagName("code");
    for (i=0; i < pres.length; i++) {
	colorize_node(pres[i]);
    }
}

function redact(s) {
    return s.replace(/\(\* Rest not in notes \*\)(.|\r|\n)*/, '');
    return s.replace(/(.|\r|\n)*\(\* Above not in notes \*\)/, '');
    return s.replace(/\(\* Begin ... in notes \*\)(.|\r|\n)*\(\* End ... \*\)/, '...');
}

function fetch_code(id, url) {
    fetch_code_redacted(id, url, redact);
}

function fetch_code_region(id, url, tag) {
    function extract_region(s) {
	var pat = "^([\\b\\r\\n]|.)*\\(\\* Begin " + tag + " \\*\\)(.|[\\b])*\\n";
	var re = new RegExp(pat);
	s = s.replace(re, "");
	pat = "\\(\\* End " + tag + " \\*\\)([\\b\\r\\n]|.)*";
	s = s.replace(new RegExp(pat), "");
	return s;
    }
    fetch_code_redacted(id, url, extract_region);
}

function fetch_code_redacted(id, url, redactor) {
    var node = document.getElementById(id);
    if (node == null) alert("No code element named " + id);
    var req;
    if (window.XMLHttpRequest) { // Mozilla, Safari, ...
	req = new XMLHttpRequest();
	req.overrideMimeType('text/xml');
    } else if (window.ActiveXObject) { // IE
	req = new ActiveXObject("Microsoft.XMLHTTP");
    }
    req.onreadystatechange = function() {
	if (req.readyState == 4) {
	    if (req.status == 200) {
		node.innerHTML = '<a class=pre href="' + url + '">' +
		    colorize_text(redactor(req.responseText)) +
		    '</a>';
	    } else {
		node.innerHTML = 'Could not read source code file at ' + url +
				    ': Error ' + req.status;
	    }
	}
    }
    if (!url.match(/^http:/)) {
	var prefix = location.href;
	url = prefix.replace(/\/[^\/]*$/, '/') + url;
    }
    req.open("GET", url, true);
    req.send(null);
}
