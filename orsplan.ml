(* orsplan.ml - An OCaml RSS/Atom aggregator *)

open Rss

(* Function to fetch RSS feed *)
let fetch_rss_feed url =
  try
    let channel = Curl.init () in
    Curl.set_url channel url;
    let buffer = Buffer.create 4096 in
    Curl.set_writefunction channel (fun data ->
        Buffer.add_string buffer data;
        String.length data);
    Curl.perform channel;
    Curl.cleanup channel;
    let feed_content = Buffer.contents buffer in
    Some (Rss_channel.parse_string feed_content)
  with
  | _ -> None

(* Function to process a list of feeds and return their items *)
let process_feeds urls =
  let rec collect_items acc = function
    | [] -> acc
    | url :: rest -> (
        match fetch_rss_feed url with
        | Some feed -> collect_items (acc @ feed.channel_items) rest
        | None -> collect_items acc rest)
  in
  collect_items [] urls

(* Function to generate an HTML page from RSS items *)
let generate_html items output_file =
  let open Printf in
  let oc = open_out output_file in
  fprintf oc "<!DOCTYPE html>\n<html>\n<head>\n<title>OrsPlan</title>\n</head>\n<body>\n";
  fprintf oc "<h1>OrsPlan - Aggregated Feeds</h1>\n<ul>\n";
  List.iter
    (fun item ->
      fprintf oc "<li><a href=\"%s\">%s</a> - %s</li>\n"
        (Option.value ~default:"#" item.item_link)
        (Option.value ~default:"No Title" item.item_title)
        (Option.value ~default:"No Description" item.item_description))
    items;
  fprintf oc "</ul>\n</body>\n</html>";
  close_out oc

(* Main function *)
let () =
  let feeds = [ "https://example.com/feed1.xml"; "https://example.com/feed2.xml" ] in
  let items = process_feeds feeds in
  generate_html items "output.html";
  Printf.printf "Generated output.html with %d items.\n" (List.length items)
