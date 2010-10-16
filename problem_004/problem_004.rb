# project euler: problem 4
# Keita Yamaguchi, 2010

class Integer
  def is_palindromic?
    ary = to_s.split("")
    ary == ary.reverse
  end
end

class Palindrome
  include Enumerable

  def initialize(min,max)
    @r = min..max
  end

  def each
    @r.each do |d1|
      @r.each do |d2|
        next unless d1 >= d2
        n = d1 * d2
        yield n if n.is_palindromic?
      end
    end
  end
end

puts Palindrome.new(100,1000).max
