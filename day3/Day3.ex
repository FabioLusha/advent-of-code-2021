defmodule Day3 do
  @filename "input.txt"

  def read_input() do
    case :file.read_file(@filename) do
      {:ok, str} -> 
        str
        |> String.split("\n", [{:trim, true}])
        |> Enum.map(&String.to_charlist/1)
      {:error, why} -> raise(["Unable to open file", why])
    end
  end

  def count_bits(bits, old), do: count_bits(bits,old, [])

  def count_bits(
    [b_h|b_tl],
    [{ones = {:ones,val_1}, zeros = {:zeros,val_0}} | c_tl],
    new)
  do
    case b_h do
      ?0 -> count_bits(b_tl, c_tl, [{ones, {:zeros, val_0+1}} | new])
      ?1 -> count_bits(b_tl, c_tl, [{{:ones, val_1+1}, zeros} | new])
    end
  end
  def count_bits([b_h|b_tl], [], new) do
    case b_h do
     ?0 -> count_bits(b_tl, [], [{{:ones, 0}, {:zeros, 1}} | new])
     ?1 -> count_bits(b_tl, [], [{{:ones, 1}, {:zeros, 0}} | new])
    end
  end
  def count_bits([],[], new), do: Enum.reverse(new)


  def bit_to_integer(bit_list), do: bit_to_integer(bit_list, 0)

  def bit_to_integer([], val), do: val
  def bit_to_integer([hd | tl], val), do: bit_to_integer( tl, val * 2 + hd - ?0)


  def most_least_common(bit_counter), do: most_least_common(bit_counter, {[],[]})

  def most_least_common([], {most,least}), do: {Enum.reverse(most), Enum.reverse(least)}
  def most_least_common([{{:ones, val_1}, {:zeros, val_0}} | tl], {most,least}) do
    case val_1 > val_0 do
      true -> most_least_common(tl, {[?1|most], [?0|least]})
      false -> most_least_common(tl, {[?0|most], [?1|least]})
    end
  end

  def part1() do
    read_input()
    |> List.foldl([], fn(elem, acc) -> count_bits(elem, acc) end)
    |> most_least_common()
    |> (fn({fst, snd}) -> bit_to_integer(fst) * bit_to_integer(snd) end).()
  end
end
