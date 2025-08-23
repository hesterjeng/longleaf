open Tyxml.Html

let html_head =
  head
    (title (txt "Longleaf C&C"))
    [
      script
        ~a:
          [
            a_src "https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js"; a_defer ();
          ]
        (txt "");
    ]

let html_body =
  body
    [
      div ~a:[ a_id "app" ] []; script ~a:[ a_src "static/script.js" ] (txt "");
    ]

let page = html html_head html_body
