rtt_html_page = function(body, title="", header=rtt_html_header(), footer_scripts = "static_code_links.js" ) {
  if (length(footer_scripts)>0) {
    footer_code = paste0('<script src="', footer_scripts,'"></script>', collapse="\n")
  } else {
    footer_code = ""
  }
  paste0("<html><title>", title,"</title>\n", header,"\n<body>", merge.lines(body),footer_code,"</body>\n</html>")
}


rtt_add_html_header = function(html, path.prefix = "") {
  paste0(rtt_html_header(path.prefix), html)
}

rtt_add_html_header_standalone = function(html) {
  paste0(rtt_html_header_standalone, html)
}


rtt_html_header = function(path.prefix = "") {
  paste0('
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
<script src="',path.prefix,'shared/jquery.min.js"></script>
<link href="',path.prefix,'shared/bootstrap.min.css" rel="stylesheet"/>
<link href="',path.prefix,'shared/bootstrap-accessibility.min.css" rel="stylesheet" />
<script src="',path.prefix,'shared/bootstrap.min.js"></script>
<script src="',path.prefix,'shared/bootstrap-accessibility.min.js"></script>
<link href="',path.prefix,'repbox.css" rel="stylesheet"/>
</head>
  ')
#  <link href="',path.prefix,'link-menu.css" rel="stylesheet"/>
}

rtt_html_header_standalone = function() {
  dir = system.file("www", package="repboxHtml")
  HTML(paste0(
    includeScript(file.path(dir,"shared/jquery.min.js")),
    includeCSS(file.path(dir,"shared/bootstrap.min.css")),
    includeScript(file.path(dir,"shared/bootstrap.min.js")),
    includeCSS(file.path(dir,"repbox.css"))
    #includeCSS(file.path(dir,"link-menu.css"))
  ))
}
