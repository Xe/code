#!/usr/bin/env ruby

1.upto(100) do |i|
	if (i % 3) == 0 and (i % 5) == 0 # Divisible by 3 and 5
		puts "FizzBuzz"
	elsif (i % 3) == 0 # Divisible by 3
		puts "Fizz"
	elsif (i % 5) == 0 # Divisible by 5
		puts "Buzz"
	else
		puts i
	end
end