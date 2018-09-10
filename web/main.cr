# A very basic HTTP server
require "http/server"

server = HTTP::Server.new do |context|
  context.response.content_type = "text/plain"
  context.response.print "Hello world, got #{context.request.path}!"
end

# Set env var PORT to a default if not already set
PORT = ENV["PORT"] ||= "5000"

puts "Listening on http://127.0.0.1:#{PORT}"
server.listen(PORT.to_i)
