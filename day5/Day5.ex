defmodule Day5 do
  @filename "input.txt"

  def read_input() do
    case File.open(@filename) do
      {:ok, file} ->
        case IO.binread(file, :eof) do
          {:error, why} -> raise(["Unable to read file", why]);
          bin -> bin
        end
      {:error, why} -> raise(["Unable to open file", why])
    end
  end


  def extract_points(str) do
    str
    |> String.splitter("\n", trim: true)
    |> Stream.map(
      fn str_elem ->
        [a, b] = String.split(str_elem, "->", trim: true)
        an_fn =
          fn x ->
            String.splitter(x, ",", trim: true)
            |> Stream.map(&String.trim/1)
            |> Stream.map(&String.to_integer/1)
            |> Enum.to_list()
          end
        [xa, ya] = an_fn.(a)
        [xb, yb] = an_fn.(b)
        {{xa,ya},{xb,yb}}
      end)
    |> Enum.to_list()
  end

  def only_hz_vt(points) do
    Enum.filter(
      points,
      fn {{x1,y1},{x2,y2}} ->
        x1 == x2 || y1 == y2
      end)
  end


  def draw_line(board, {x1,y1},{x2,y2}) do
      fn_dir =
        fn x,y ->
          case dir = x - y do
            0 -> dir;
            _ -> div( dir, abs(dir))
          end
        end
      x_dir = fn_dir.(x2,x1)
      y_dir = fn_dir.(y2,y1)
      # Because our matrix is 1-based index but
      # the input is 0-based we add +1 to the coordinate
      draw_line(board, {x1+1,y1+1}, {x_dir, y_dir}, {x2+1, y2+1})
  end

  def draw_line(board, {x,y} = dest, _, dest) do
    {get, set_f} = :linalg.get_and_set(board, y, x)
    set_f.(get+1)
  end
  def draw_line(board, {x,y}, {x_dir, y_dir} = dir, dest) do
      # we invert the position of x and y because in the matrix the
      # first arg represents the vertical axis and the second arg
      # represents the horizontal axis
      {get, set_f} = :linalg.get_and_set(board, y, x)
      new_board = set_f.(get+1)
      draw_line(new_board, {x + x_dir, y + y_dir}, dir, dest)
  end

  def part1() do
    points =
      read_input()
      |> extract_points()
      |> only_hz_vt()

    res =
      List.foldl(
      points,
      :linalg.zeros(1000,1000),
      fn {p1,p2}, acc ->
        draw_line(acc, p1, p2)
      end)

    res
    |> List.flatten()
    |> List.foldl(
      0,
      fn elem, acc -> if elem >= 2 do acc+1 else acc end end)

  end

  def part2() do
    points =
      read_input()
      |> extract_points()

    res =
      List.foldl(
      points,
      :linalg.zeros(1000,1000),
      fn {p1,p2}, acc ->
        draw_line(acc, p1, p2)
      end)

    res
    |> List.flatten()
    |> List.foldl(
      0,
      fn elem, acc -> if elem >= 2 do acc+1 else acc end end)

  end
end
