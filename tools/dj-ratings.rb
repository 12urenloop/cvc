require 'rubygems'
require 'sqlite3'

db = SQLite3::Database.new '/var/spool/gammu/db.sqlite'
rows = db.execute(
    "select ReceivingDateTime, SenderNumber, TextDecoded from inbox")

# Create votes structure. votes[hour][sender] => text
votes = {}
rows.each do |time, sender, text|
  hour = DateTime.parse(time).hour
  votes[hour] = votes[hour] || Hash.new
  votes[hour][sender] = text
end

require 'statistics2'

def ci_lower_bound(pos, n, confidence)
  return 0 if n == 0

  z = Statistics2.pnormaldist(1-(1-confidence)/2)
  phat = 1.0*pos/n
  (phat + z*z/(2*n) - z * Math.sqrt((phat*(1-phat)+z*z/(4*n))/n))/(1+z*z/n)
end

# Count hot/not to make rating for each dj
djs = votes.collect do |hour, senders|
  pos = senders.select { |_, text| text =~ /hot/i }.size
  neg = senders.select { |_, text| text =~ /not/i }.size
  {
    :hour => hour,
    :rating => ci_lower_bound(pos, pos + neg, 0.95),
    :hot => pos,
    :not => neg
  }
end

djs.sort_by { |x| x[:rating] }.reverse.each.with_index do |dj, i|
   puts "#{sprintf("%2d", i + 1)}: #{sprintf("%2d", dj[:hour])}h, " +
     "(#{dj[:hot]} hot, #{dj[:not]} not, #{dj[:hot] + dj[:not]} " +
     "total, #{dj[:rating]})"
end
