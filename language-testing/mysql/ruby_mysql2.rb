#! /usr/bin/env ruby

require 'mysql2'

def insert_mysql_data
  dbh = Mysql2::Client.new(:host => "localhost", :username => "read_only")

  last_genre = nil
  dbh.query("SELECT title, author, genre, comment FROM mysql.books " +
            "ORDER BY genre, author;").each do |db_row|
    if db_row["genre"] != last_genre
      print "</ol>" unless last_genre.nil?
      print "<h3>#{db_row["genre"]}</h3><ol>"
      last_genre = db_row["genre"]
    end
    print "<li><b>#{db_row["title"]}</b> av <b>#{db_row["author"]}</b>"
    print " (#{db_row["comment"]})" unless db_row["comment"].empty?
    print "</li>"
  end
  print "</ol>"

  dbh.close
end

if __FILE__ == $0
  # Change stdout so we can use the print statement
  $stdout = File.open("index.html", "w")

  <<-EOF
<html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <title>Title</title>
  </head>
  <body>
  <h1>Title</h1>
  <h2>Books</h2>
  <div id="books">
  </div>
  </body>
</html>
  EOF
  .split("\n").each do |line|
    # Write each line, and add mysql data inside the "books" div tag
    print line.lstrip
    insert_mysql_data if line =~ /<div id="books">/
  end
end
