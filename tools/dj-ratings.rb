require 'rubygems'
require 'sqlite3'

db = SQLite3::Database.new 'db.sqlite'
rows = db.execute "select ReceivingDateTime,TextDecoded from inbox"
r = rows.group_by { |t,_| DateTime.parse(t).hour }

require 'statistics2'

def ci_lower_bound(pos, n, confidence)
  return 0 if n == 0

  z = Statistics2.pnormaldist(1-(1-confidence)/2)
  phat = 1.0*pos/n
  (phat + z*z/(2*n) - z * Math.sqrt((phat*(1-phat)+z*z/(4*n))/n))/(1+z*z/n)
end

r = r.map do |k,v|
  hot1 = v.select {|t,text| text =~ /hot/i }.size
  not1 = v.select {|t,text| text =~ /not/i }.size
  [k, ci_lower_bound(hot1, hot1 + not1, 0.95)]
end
r.sort_by(&:last).reverse.each.with_index { |arr, i| puts "%2d: (%2dh) #{arr.last}" % [i + 1, arr.first] }
