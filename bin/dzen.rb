class DZen
  def self.menu(title, items, options=nil)
    events = [
      'entertitle=uncollapse,grabkeys,grabmouse',
      'enterslave=grabkeys,grabmouse',
      'leaveslave=collapse,ungrabkeys,ungrabmouse',
      'button1=menuprint',
      'button3=collapse',
      'button4=scrollup:10',
      'button5=scrolldown:10',
      'key_Escape=collapse,ungrabkeys,ungrabmouse'
    ].join(';')

    dzen = "dzen2 -p -m -l #{[ items.size + 1, 50 ].min} -e '#{events}' -fn fixed -w 85 -sa c -bg '#333' -fg white #{options}"

    IO.popen(dzen, 'a+') do |pipe|
      pipe.puts("^r(5x5) ^r(5x5) #{title} ^r(5x5) ^r(5x5)")
      pipe.puts(items.join("\n"))
      pipe.puts(("^r(5x5) " * 6).chop)

      loop { yield pipe.readline.chomp }
    end
  end
end
