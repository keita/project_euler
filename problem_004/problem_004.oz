% project euler: problem 4
% Keita Yamaguchi, 2010

functor
import
   System(show:Show)
   Application(exit:Exit)
define
   MaxN
   Min = 100
   Max = 1000

   fun {Palindromic N}
      S = {Int.toString N}
      RevS = {List.reverse S}
   in
      S == RevS
   end

   proc {Palindrome N1 N2 R}
      N = N1 * N2
   in
      local RecR in
	 cond N2 = Max then R = 0
	 [] N1 = Max then
	    {Palindrome Min (N2 + 1) RecR}
	    R = if ({Palindromic N} andthen N > RecR) then N else RecR end
	 else
	    {Palindrome (N1 + 1) N2 RecR}
	    R = if ({Palindromic N} andthen N > RecR) then N else RecR end
	 end
      end
   end
in
   {Palindrome Min Min MaxN}
   {Show MaxN}
   {Exit 0}
end
