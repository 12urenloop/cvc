#!/usr/bin/env ruby -w
require 'net/ssh'

hosts = File.open('hosts').readlines.map(&:strip)

def error(host, message)
  if $error_state == 0
    puts "=" * 20
  end
  puts "#{Time.now}: #{host}: #{message}"
  $error_state = 1
end

$error_state = 0

loop do
  errors = 0
  hosts.each do |host|
    begin
      Timeout::timeout(10) do
        Net::SSH.start(host, 'root') do |ssh|
          usb_check = ssh.exec!("hcitool dev | grep -v Devices")
          unless usb_check.lines.count == 1
            error(host, "No Bluetooth-receiver detected (#{usb_check})")
            errors = 1
          end

          gyrid_check = ssh.exec!("ps -ewwo args | grep gyrid | grep -v grep")
          unless gyrid_check.lines.count == 2
            error(host, "Gyrid is not running (#{gyrid_check})")
            errors = 1
          end

          conn_check = ssh.exec!("netstat -t -n | tail -n +3 | tr -s ' ' | cut -d' ' -f5 | grep 9001")
          if !conn_check || conn_check.lines.count != 1
            error(host, "No remote connection to port 9001 (#{conn_check})")
            errors = 1
          end

          load = ssh.exec!("cat /proc/loadavg | cut -d' ' -f1")
          if load.to_f > 1
            error(host, "High load (#{load})")
            errors = 1
          end
        end
      end
    rescue SignalException => e
      raise e
    rescue Exception => e
      error(host, e)
      errors = 1
    end
    if errors == 0
      $error_state = 0
    end
  end
end
