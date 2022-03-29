defmodule Day1 do
@filename "input.txt"

def read_input() do
    case :file.read_file(@filename) do
        {:ok, bin} -> String.split(bin, "\n")
                        |> Stream.filter(fn elem -> elem != "" end)
                        |> Stream.map(&:string.to_integer/1)
                        |> Stream.map(fn {fst, _} -> fst end)
                        |> Enum.to_list
        {:error, why} -> throw(["Unable to open file", why])
    end
end

def count([]), do: 0
def count([_]), do: 0
def count([h|t]) do
  List.foldl(
    t,
    {h, 0},
    fn(elem, {prev, count}) when elem > prev -> {elem, count+1};
      (elem, {_prev, count}) -> {elem, count}
    end)
    |> Kernel.elem(1) # Attention, zero based indexing
end

def part1(), do: read_input() |> count

def three_mes([_,_], acc), do: Enum.reverse(acc)
def three_mes([a,b,c | tail], acc) do
  three_mes([b, c | tail], [a+b+c | acc])
end

def part2(), do: read_input() |> three_mes([]) |> count

end
