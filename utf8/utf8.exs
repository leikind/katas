@moduledoc """

The code below validates a list of bytes as valid or invalid UTF-8

Reading this this code and the comments you'll learn two things

1. How UTF-8 works
2. How easy it is to deal with binary data in Elixir

Also check out the file with tests: utf8_test.exs

#####  VIEWING DATA AS BINARY IN ELIXIR #####

binary number
IO.puts 0b101
> 5

print decimal number as binary
IO.puts Integer.to_string(12, 2)
> 1100

or
IO.puts inspect(12, base: :binary)
> 0b1100

see the number of bytes a character takes
IO.puts byte_size("愛")
> 3

see the number of bits a character takes
IO.puts bit_size("愛")
> 24

see the codepoint behind a character
IO.puts ?h
> 104

IO.puts inspect(?h, base: :binary)
> 0b1101000

IO.puts inspect(?Ш, base: :binary)
> 0b10000101000

IO.puts inspect(?愛, base: :binary)
> 0b110000100011011

IO.puts inspect(?💩, base: :binary)
> 0b11111010010101001

#####  BINARIES  IN ELIXIR ##########

IO.puts <<>> == ""

packing bytes into a binary (string)
IO.puts << 104 :: size(8), 101 :: size(8), 108 :: size(8), 108 :: size(8), 111 :: size(8) >>
> hello

IO.puts << 104, 101, "ll", 0b1101111 >>
hello

there are many size expression
a = 4.234
IO.inspect << a :: size(64)-float-little >>

concatenate two binaries:
a = << 0b00100000, 0b11100110 >>
b = << 0b10000100, 0b10011011, 0b00100000 >>
IO.inspect << a :: bitstring, b :: bitstring >>
> " 愛 "

#####  BINARY PATTERN-MATCHING IN ELIXIR ##########

<< first_byte :: size(8),  second_byte :: size(8) >> = "Щ"
IO.puts first_byte
> 208

IO.puts second_byte
> 169

If you only want to match the first byte binary
<< first_byte :: size(8),  _rest :: bitstring >> = "Щ"
IO.puts first_byte
> 208

Get bits out of a byte
<<  first_three :: size(3), rest :: size(5) >>  = << 0b10100001 >>
IO.inspect(first_three, base: :binary)
> 0b101

IO.inspect(rest, base: :binary)
> 0b1

defmodule Ascii do
  def is_ascii_char?(<<0b0::size(1), _rest::size(7)>>), do: true
  def is_ascii_char?(_), do: false
end

IO.inspect(Ascii.is_ascii_char?("a"))
> true
IO.inspect(Ascii.is_ascii_char?(<<0b10100001>>))
> false

#####  The genius of UTF8 #####

0xxxxxxx
110xxxxx 10xxxxxx
1110xxxx 10xxxxxx 10xxxxxx
11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

* Backward compatibility
* Single bytes (0xxxxxxx), leading bytes (11xxxxxx), and continuation bytes (10xxxxxx) do not share values,
  in other word, it is impossible to confuse a leafing byte with a continuation byte or a single byte
* The number of high-order 1's in the leading byte of a multi-byte sequence indicates the number of bytes in the sequence.

"""

defmodule UTF8 do
  def valid?(bytes) do
    case parse(bytes) do
      {:ok, _parsed} -> true
      {:invalid, _parsed} -> false
    end
  end

  def extract_utf(bytes) do
    {_, parsed} = parse(bytes)

    parsed
    |> List.foldr(<<>>, fn x, acc -> <<acc::bitstring, x::bitstring>> end)
  end

  defp parse(bytes) do
    bytes
    |> Enum.map(&<<&1>>)
    |> scan([], :find_leading_byte, :ok)
  end

  # success!
  defp scan([], accu, :find_leading_byte, validity) do
    {validity, accu}
  end

  # < FIND A LEADING BYTE >

  # find the first byte of 0xxxxxxx
  defp scan(
         [byte = <<0b0::size(1), _significant_bits::size(7)>> | tail],
         accu,
         :find_leading_byte,
         validity
       ) do
    scan(tail, [byte | accu], :find_leading_byte, validity)
  end

  # find the first byte of 110xxxxx 10xxxxxx
  defp scan(
         [byte = <<0b110::size(3), _significant_bits::size(5)>> | tail],
         accu,
         :find_leading_byte,
         validity
       ) do
    scan(tail, [byte | accu], {:get_continuation_byte, 1}, validity)
  end

  # find the first byte of 1110xxxx 10xxxxxx 10xxxxxx
  defp scan(
         [byte = <<0b1110::size(4), _significant_bits::size(4)>> | tail],
         accu,
         :find_leading_byte,
         validity
       ) do
    scan(tail, [byte | accu], {:get_continuation_byte, 2}, validity)
  end

  # find the first byte of 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
  defp scan(
         [byte = <<0b11110::size(5), _significant_bits::size(3)>> | tail],
         accu,
         :find_leading_byte,
         validity
       ) do
    scan(tail, [byte | accu], {:get_continuation_byte, 3}, validity)
  end

  # </ FIND A LEADING BYTE >

  # we've got all trailing bytes, let's find another leading byte
  defp scan(bytes, accu, {:get_continuation_byte, 0}, validity) do
    scan(bytes, accu, :find_leading_byte, validity)
  end

  # Find the Nth trailing byte 110xxxxx
  defp scan(
         [byte = <<0b10::size(2), _significant_bits::size(6)>> | tail],
         [codepoint_in_progress | accu_tail],
         {:get_continuation_byte, n},
         validity
       ) do
    b = <<codepoint_in_progress::bitstring, byte::bitstring>>
    scan(tail, [b | accu_tail], {:get_continuation_byte, n - 1}, validity)
  end

  # Failures!

  # we just skip a byte setting the validity to :invalid
  # retaining the state and accu
  defp scan([_ | tail], accu, state, _validity) do
    scan(tail, accu, state, :invalid)
  end

  # ending the scan with the wrong status, not having completed a codepoint
  defp scan([], accu, _state, _validity) do
    {:invalid, accu}
  end
end
