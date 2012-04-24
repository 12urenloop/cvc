#!/usr/bin/env ruby -w
require 'net/ssh'
require './sihemo.rb'

class Check
  def initialize(sihemo, name, alive = 20)
    @sihemo = sihemo
    @name = name
    @alive = alive
  end

  def check(host, ssh)
    errs = []
    begin
      errs = perform_check(ssh)
    rescue SignalException => e
      raise e
    rescue Exception => e
      errs = e.to_s
    end

    begin
      report_check_result(host, errs)
    end
  end

  def report_check_result(host, errors)
    if errors.nil? or errors.empty?
      with_sihemo { |s| s.heartbeat(host, @name, @alive) }
    else
      puts "-" * 78
      errors.each { |err| puts "#{host}: #{err}" }
      with_sihemo { |s| s.down(host, @name) }
    end
  end

  def with_sihemo
    begin
      yield @sihemo
    rescue Exception => e
      puts "Error talking to sihemo: #{e}"
    end
  end
end

class BluetoothCheck < Check
  def perform_check(ssh)
    usb_check = ssh.exec!("hcitool dev | grep -v Devices")
    unless usb_check.lines.count == 1
      return ["No Bluetooth-receiver detected (#{usb_check})"]
    end
  end
end

class GyridCheck < Check
  def perform_check(ssh)
    gyrid_check = ssh.exec!("ps -ewwo args | grep gyrid | grep -v grep")
    unless gyrid_check.lines.count == 2
      return ["Gyrid is not running (#{gyrid_check})"]
    end
  end
end

class ConnCheck < Check
  def perform_check(ssh)
    conn_check = ssh.exec!(
        "netstat -t -n | tail -n +3 | tr -s ' ' | cut -d' ' -f5 | grep 9001")
    if !conn_check || conn_check.lines.count != 1
      return ["No remote connection to port 9001 (#{conn_check})"]
    end
  end
end

class LoadCheck < Check
  def load_check(ssh)
    load = ssh.exec!("cat /proc/loadavg | cut -d' ' -f1")
    if load.to_f > 1
      return ["High load (#{load})"]
    end
  end
end

def main
  sihemo = Sihemo.new('localhost', 8001)
  gyrids = File.open('gyrids').readlines.map(&:strip)

  checks = [
    BluetoothCheck.new(sihemo, 'bluetooth', 60),
    GyridCheck.new(sihemo, 'gyrid', 60),
    ConnCheck.new(sihemo, 'conn', 60),
    LoadCheck.new(sihemo, 'load', 60),
  ]

  loop do
    gyrids.each do |gyrid|
      begin
        Timeout::timeout(10) do
          Net::SSH.start(gyrid, 'root') do |ssh|
            checks.each do |check|
              check.check(gyrid, ssh)
            end
          end
        end
      rescue Timeout::Error => e
        puts "#{gyrid}: Timeout: #{e}"
      rescue SignalException => e
        raise e
      rescue Exception => e
        puts "#{gyrid}: Uncaught exception: #{e}"
      end
    end
  end
end

main
