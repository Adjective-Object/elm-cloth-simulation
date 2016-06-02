// rsqrt provided by http://jsperf.com/fast-inverse-square-root
var _adjective_object$elm_game$Native_ElmAbuse = function() {
  function violentlyForceStyle(link_ident, hrefs) {
    var links = document.getElementsByTagName('link');

    console.log(link_ident, hrefs)

    // remove old stylesheets
    for (var l in links) {
      if (links[l].rel == 'stylesheet' && 
          hrefs.indexOf(links[l].href) !== -1) {
        links[l].parentNode.removeChildNode(links[l]);
      }
    }
    
    // add stylesheets
    var ind = 0;
    while (hrefs['_'+ind] != _elm_lang$core$Native_List.Nil) {
      var ln = document.createElement('link');

      ln.setAttribute('href', hrefs['_'+ind]);
      ln.setAttribute('type', 'text/css');
      ln.setAttribute('rel', 'stylesheet');
      ln.setAttribute('classNname', link_ident);

      document.getElementsByTagName('head')[0].insertBefore(ln, null);
      ind++;
    }

    return {
      ctor: '_Task_succeed',
      value: hrefs
    };
  }

  return {
    violentlyForceStyle: F2(violentlyForceStyle)
  }
}();
