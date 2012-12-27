defmodule CouchNormalizer.Scenario.FieldMethods do


  @doc false
  defmacro field(name) do
    quote do: field(var!(body), unquote(name))
  end

  @doc false
  defmacro field(:not_found, _name) do
    quote do: nil
  end

  @doc false
  defmacro field(body, name) do
    quote do
      { body, name } = { unquote(body), unquote(name) }
      :proplists.get_value(to_binary(name), body, :nil)
    end
  end

  @doc false
  defmacro remove_field(name) do
    quote do
      var!(body) = :proplists.delete(to_binary(unquote(name)), var!(body))
    end
  end

  @doc false
  defmacro remove_fields(names) do
    removers = lc name inlist names, do: quote(do: remove_field(unquote(name)))
    quote do: unquote_splicing(removers)
  end

  @doc false
  defmacro rename_field(name, new_name) do
    quote do
      { name, new_name } = { unquote(name), unquote(new_name) }
      update_field(name, new_name, field(var!(body), name))
    end
  end

  @doc false
  defmacro update_field(name, value) do
    quote do
      { name, value } = { unquote(name), unquote(value) }
      if field(var!(body), name) do
        remove_field name
        var!(body) = [ {to_binary(name), value} | var!(body) ]
      end
    end
  end

  @doc false
  defmacro update_field(name, new_name, value) do
    quote do
      { name, new_name, value } = { unquote(name), unquote(new_name), unquote(value) }
      if field(var!(body), name) do
        var!(body) = [ {to_binary(new_name), value} | var!(body) ]
        remove_field name
      end
    end
  end

  @doc false
  defmacro create_field(name, value) do
    quote do
      { name, value } = { unquote(name), unquote(value) }
       var!(body) = var!(body) ++ [{to_binary(name), value}]
    end
  end

end