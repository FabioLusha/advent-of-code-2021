defmodule Day4 do
  @filename "input.txt"

  def read_input() do
    case :file.read_file(@filename) do
      {:ok, bitstr} -> bitstr;
      {:error, why} -> raise(["Unable to open file", why])
    end
  end

  def winning_strike(bitstr) do

  end
end
