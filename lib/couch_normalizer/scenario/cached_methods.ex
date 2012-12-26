defmodule CouchNormalizer.Scenario.CachedMethods do


  @doc false
  defmacro doc(db, id, :cached!) do
    quote do
      { db, id } = { unquote(db), unquote(id) }
      caches "#{db}-#{id}", fn -> doc(db, id) end
    end
  end

  @doc false
  defmacro doc_field(db, id, name, :cached!) do
    quote do
      { db, id, name } = { unquote(db), unquote(id), unquote(name) }
      caches "#{db}-#{id}-#{to_binary(name)}", fn -> doc_field(db, id, name) end
    end
  end

  @doc false
  defp caches(key, fun) do
    if Process.get(key) == nil, do: Process.put(key, fun.())
    Process.get(key)
  end

end