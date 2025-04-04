var last_highlight = '';

function set_script_tab(script_num, line) {
  tablink = "#script_tab_"+script_num;
  $('#script_tabs a[href="'+tablink+'"]').tab('show');
  if (line !== undefined) {
    showline = parseInt(line)-3;
    if (showline < 1) showline = 1;
    if (last_highlight !== '') {
      $(last_highlight).removeClass("code-highlight");
    }
    $("#L"+line+"___"+script_num).addClass("code-highlight");
    last_highlight = "#L"+line+"___"+script_num;
    document.querySelector("#L"+showline+"___"+script_num).scrollIntoView({
      behavior: 'smooth'
    });
  }
}



$(document).ready(function() {
  $(document).on("click",".arttabcell",function(event) {
    let el = event.currentTarget;
    let id = el.id;
    // cell_5
    let cellid =  parseInt(id.substring(5));
    let script_num = $(el).data("script_num");
    let code_line = $(el).data("code_line");
    set_script_tab(script_num, code_line)
  });
});



const urlpar = new URLSearchParams(window.location.search);

if (urlpar.has('script')) {
  script_num = urlpar.get('script');
  code_line = urlpar.get('L');
  set_script_tab(script_num, code_line);
}


if (urlpar.has('tabid')) {
  var tabid = urlpar.get('tabid');
  $('#tabtabs a[href="#tabtab'+tabid+'"]').tab('show');
}


