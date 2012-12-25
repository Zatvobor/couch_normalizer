defmodule CouchNormalizer.Scenario do

  @moduledoc """
  Data normalization scenario.
  """

  @doc false
  def define(title, scenario) do
    true = CouchNormalizer.Registry.acquire(title, scenario)
  end


  # Manipulation methods

  @doc false
  def doc(db, id) do
    :couch_normalizer_util.document_body(db, id)
  end

  @doc false
  def doc(db, id, :cached!) do
    caches "#{db}-#{id}", fn -> doc(db, id) end
  end

  @doc false
  def doc_field(db, id, name) do
    field(doc(db, id), name)
  end

  @doc false
  def doc_field(db, id, name, :cached!) do
    caches "#{db}-#{id}-#{to_binary(name)}", fn -> doc_field(db, id, name) end
  end

  @doc false
  def field(:not_found, _name), do: nil

  @doc false
  def field(body, name) do
    :proplists.get_value(to_binary(name), body, :nil)
  end

  @doc false
  def remove_document!(db, id) do
    document = :couch_normalizer_util.mark_as_deleted(doc(db, id))
    :couch_normalizer_util.update_doc(document)
  end


  @doc false
  defp caches(key, fun) do
    if Process.get(key) == nil, do: Process.put(key, fun.())
    Process.get(key)
  end


  @doc false
  defmacro remove_field(name) do
    quote do: var!(body) = :proplists.delete(to_binary(unquote(name)), var!(body))
  end

  @doc false
  defmacro remove_fields(names) do
    removers = lc name inlist names, do: quote(do: remove_field(unquote(name)))
    quote do: unquote_splicing(removers)
  end

  @doc false
  defmacro rename_field(name, new_name) do
    quote do: update_field(unquote(name), unquote(new_name), field(var!(body), unquote(name)))
  end

  @doc false
  defmacro update_field(name, value) do
    quote do
      if field(var!(body), unquote(name)) do
        remove_field unquote(name)
        var!(body) = [{to_binary(unquote(name)), unquote(value)}|var!(body)]
      end
    end
  end

  @doc false
  defmacro update_field(name, new_name, value) do
    quote do
      if field(var!(body), unquote(name)) do
        var!(body) = [{to_binary(unquote(new_name)), unquote(value)}|var!(body)]
        remove_field unquote(name)
      end
    end
  end

  @doc false
  defmacro create_field(name, value) do
    quote do: var!(body) = var!(body) ++ [{to_binary(unquote(name)), unquote(value)}]
  end

  @doc false
  defmacro mark_as_deleted!() do
    quote do: var!(body) = :couch_normalizer_util.mark_as_deleted(var!(body))
  end

end