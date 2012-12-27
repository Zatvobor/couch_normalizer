defmodule CouchNormalizer.Scenario.CachedMethods do


  @doc false
  # defmacro doc(id, :cached!) do
    # quote do: doc(var!(db), unquote(id), :cached!)
  # end

  @doc false
  defmacro doc(db, id, :cached!) do
    quote do
      { db, id } = { unquote(db), unquote(id) }
      caches "#{db}-#{id}", fn -> doc(db, id) end
    end
  end

  @doc false
  # defmacro doc_field(id, name, :cached!) do
    # quote do
      # { id, name } = { unquote(id), unquote(name) }
      # doc_field(var!(db), id, name, :cached!)
    # end
  # end

  @doc false
  defmacro doc_field(db, id, name, :cached!) do
    quote do
      { db, id, name } = { unquote(db), unquote(id), unquote(name) }
      caches "#{db}-#{id}-#{to_binary(name)}", fn -> doc_field(db, id, name) end
    end
  end


  # Internal functions.
  # You shouldn't call them directly from scenario.

   @doc false
  def caches(key, fun) when is_function(fun) do
    if cached(key) == nil, do: caches(key, fun.()), else: cached(key)
  end

  @doc false
  def caches(key, value) do
    Process.put(key, value)
    value
  end

  @doc false
  def cached(key), do: Process.get(key)

end