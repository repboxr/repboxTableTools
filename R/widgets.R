
# Bootstrap tabset panels for none-shiny html pages
rtt_tabset_panel = function(id=paste0("tabset_",random.string()), tabids=paste0("tab_",random.string(length(contents))), tabnames, contents, type=c("tabs", "pills")[1], active = tabids[1], ul.class="", fix.top=FALSE) {
  restore.point("rtt_tabset_panel")
  head = paste0('
  <ul id="',id,'" class="nav nav-',type,' ',if (fix.top) 'navbar-fixed-top ', ul.class,'" role="tablist">')
  inner = paste0(
    '<li', ifelse(tabids==active,' class="active">','>'),
    '<a href="#',tabids,'" role="tab" data-toggle="tab">',tabnames,'</a>',
    '</li>', collapse="\n")
  foot = paste0('</ul>')
  navList=paste0(head,"\n",inner,"\n", foot)

  # content div
  head = paste0('<div class="tab-content">')
  inner = paste0(
    '<div class="tab-pane ', ifelse(tabids==active,' active',''),'"',
    ' id = "',tabids,'">\n',
    contents,
    '\n</div>',
    collapse="\n")
  foot = paste0("</div>")
  content = paste0(head,"\n",inner,"\n", foot)

  paste0(navList,"\n", content)
}
