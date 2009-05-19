require 'rake'
require 'rake/clean'

task :default => :stats

Dir['tasks/**/*.rake'].each { |rake| load rake }

CLEAN.include %w[
  **/*.o
  **/*.hi
  Main.exe*
  **/*.pid
  **/*.sock
  **/*.fcgi
  manifest
  db/public/*.fcgi
]

desc "kibro refresh"
task :r => [:b] do
  sh 'kibro refresh'
end

desc "kibro restart"
task :rs => [:b] do
  sh 'kibro restart'
end

desc "kibro start"
task :s => [:b] do
  sh 'kibro start'
end

desc "kibro build"
task :b do
  # sh 'kibro build'
end

desc "kibro stop"
task :st do
  sh 'kibro stop'
end

desc "console"
task :console do
  sh 'ghci -isrc src/Hack/Handler/Hyena.hs'
end

desc "clean refresh"
task :cr => [:clean, :r]

desc "kill"
task :kill do
  kill :fcgi
  kill :lighttpd
end

def kill(that)
  `ps -A | grep #{that} | awk '{print $1}' | xargs kill -9`
end
