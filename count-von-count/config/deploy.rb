require 'pathname'
set :application, "count-von-count"
set :repository,  "git@github.com:ZeusWPI/12Urenloop.git"

set :scm, :git
# Or: `accurev`, `bzr`, `cvs`, `darcs`, `git`, `mercurial`, `perforce`, `subversion` or `none`

# Change this
server "eva" , :web, :db, :app, primary: true
ssh_options[:forward_agent] = true
set :user, 'zeus'

set :deploy_to, "/home/zeus/#{application}"
set :use_sudo, false


after 'deploy:update_code', 'cabal:build'
before 'cabal:build', 'cabal:update', 'cabal:install'

def run_in(path, command)
  run "cd #{Pathname.new(latest_release).join(path)} && #{command}"
end

namespace :cabal do
  ["build", "configure", "update", "install"].each do |cmd|
    task cmd do
      run_in "count-von-count", "cabal #{cmd} > /dev/null"
    end
  end
end
