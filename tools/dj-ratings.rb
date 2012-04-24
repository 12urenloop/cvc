require 'rubygems'
require 'sqlite3'

db = SQLite3::Database.new '/var/spool/gammu/db.sqlite'
rows = db.execute "select ReceivingDateTime, TextDecoded from inbox"
r = rows.group_by { |t, _| DateTime.parse(t).hour }

require 'statistics2'

def ci_lower_bound(pos, n, confidence)
  return 0 if n == 0

  z = Statistics2.pnormaldist(1-(1-confidence)/2)
  phat = 1.0*pos/n
  (phat + z*z/(2*n) - z * Math.sqrt((phat*(1-phat)+z*z/(4*n))/n))/(1+z*z/n)
end

r = r.map do |k,v|
  pos = v.select {|t, text| text =~ /hot/i }.size
  neg = v.select {|t, text| text =~ /not/i }.size
  {
    :hour => k,
    :rating => ci_lower_bound(pos, pos + neg, 0.95),
    :hot => pos,
    :not => neg
  }
end
r.sort_by { |x| x[:rating] }.reverse.each.with_index do |dj, i|
   puts "#{sprintf("%2d", i + 1)}: #{sprintf("%2d", dj[:hour])}h, " +
     "(#{dj[:hot]} hot, #{dj[:not]} not, #{dj[:hot] + dj[:not]} " +
     "total, #{dj[:rating]})"
end
