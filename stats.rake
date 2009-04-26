def count_regex(glob, regex)
  Dir[glob].map{|file|
    File.open(file){|io| io.grep(regex).size }
  }.inject{|s,v| s + v }
end

desc 'Show some stats about the code'
task :stats do
  loc = [
    [ 'code', app_code = count_regex('src/**/*.hs',  /^\s*[^-^\{\s]/)],
    [ 'comment', app_comments = count_regex('src/**/*.hs',  /^\s*(--|\{-)/)],
#    [ 'total', app_total = count_regex('app/**/*.rb',  /\S/)],
#    [ 'spec', spec_total = count_regex('spec/**/*.rb', /\S/)],
  ]

  ratio = [
    [ 'comment/code', app_comments / app_code.to_f ],
 #   [ 'spec/code', spec_total / app_code.to_f ],
  ]

  print '/' << '=' * 30 << "\\\n"

  loc.each do |date|
    puts "| %-21s: %5d |" % date
  end

  puts "|" << '-' * 30 << "|"
  puts "| %-28s |" % 'ratio:' 
  
  ratio.each do |date|
    puts "|   %-19s: %5.2f |" % date
  end

  print '\\' << '=' * 30 << "/\n"
end
