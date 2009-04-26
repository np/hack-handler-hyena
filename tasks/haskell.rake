desc "manifest"
task :ma do
  sh 'find . | grep "hs$" > manifest'
end

desc "build"
task :build do
  sh 'cabal clean; cabal configure; cabal build'
end


desc "dist"
task :dist do
  sh 'cabal clean'
  sh 'cabal configure'
  sh 'cabal sdist'
end