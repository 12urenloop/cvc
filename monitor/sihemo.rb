require 'net/http'

class Sihemo
  attr_reader :host, :port, :path

  def initialize(host = 'localhost', port = 80, path = '')
    @host = host
    @port = port
    @path = path
  end

  def heartbeat(group, name, alive = 30)
    run_http do |http|
      request = Net::HTTP::Post.new service_path(group, name)
      request.set_form_data('alive' => alive)
      http.request request
    end
  end

  def down(group, name)
    run_http do |http|
      request = Net::HTTP::Post.new service_path(group, name)
      request.set_form_data('state' => 'down')
      http.request request
    end
  end

  def shutdown(group, name)
    run_http do |http|
      request = Net::HTTP::Delete.new service_path(group, name)
      http.request request
    end
  end

  private

  def run_http
    Net::HTTP.start(@host, @port) { |http| yield http }
  end

  def service_path(group, name)
    "#{@path}/services/#{group}/#{name}"
  end
end
