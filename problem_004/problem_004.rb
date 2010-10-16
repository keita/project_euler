class Integer
  def is_palindromic?
    ary = to_s.split("")
    ary == ary.reverse
  end
end

class Multiplier
  include Enumerable

  def initialize(min,max)
    @r = min..max
  end

  def each
    @r.each do |d1|
      @r.each do |d2|
       yield d1 * d2 if d1 >= d2
      end
    end
  end
end

m = Multiplier.new(100,1000)
puts m.find_all{|n| n.is_palindromic?}.max
