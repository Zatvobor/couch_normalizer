defmodule CouchNormalizer.Scenario do

  @moduledoc """
  Data normalization scenario.
  """

  @doc false
  def define(title, scenario) do
    true = CouchNormalizer.Registry.acquire(title, scenario)
  end



  defmacro field(body, name) do
    quote do: :proplists.get_value(to_b(unquote(name)), unquote(body), :nil)
  end


  defmacro remove_field(name) do
    quote do: var!(body) = :proplists.delete(to_b(unquote(name)), var!(body))
  end


  defmacro remove_fields(names) do
    lc name inlist names, do: quote(do: remove_field(unquote(name)))
  end


  defmacro rename_field(name, new_name) do
    quote do: update_field(unquote(name), unquote(new_name), field(var!(body), unquote(name)))
  end


  defmacro update_field(name, value) do
    quote do
      if field(var!(body), unquote(name)) do
        remove_field unquote(name)
        var!(body) = [{to_b(unquote(name)), unquote(value)}|var!(body)]
      end
    end
  end


  defmacro update_field(name, new_name, value) do
    quote do
      if field(var!(body), unquote(name)) do
        var!(body) = [{to_b(unquote(new_name)), unquote(value)}|var!(body)]
        remove_field unquote(name)
      end
    end
  end


  def to_b(name) when is_atom(name), do: atom_to_binary(name)
  def to_b(name), do: name

end