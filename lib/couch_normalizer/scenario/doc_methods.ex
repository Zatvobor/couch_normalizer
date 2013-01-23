defmodule CouchNormalizer.Scenario.DocMethods do


  @doc false
  defmacro doc(id) do
    quote do: doc(var!(db), unquote(id))
  end

  @doc false
  defmacro doc(db, id) do
    quote do
      { db, id } = { unquote(db), unquote(id) }
      body = couchdb.document_body(db, id)
      unless body == :not_found, do: body = HashDict.new(body)
      body
    end
  end

  @doc false
  defmacro doc_field(id, name) do
    quote do
      { id, name } = { unquote(id), unquote(name) }
      doc_field(var!(db), id, name)
    end
  end

  @doc false
  defmacro doc_field(db, id, name) do
    quote do
      { db, id, name } = { unquote(db), unquote(id), unquote(name) }
      field(doc(db, id), name)
    end
  end

  @doc false
  defmacro remove_document!(id) do
    quote do: remove_document!(var!(db), unquote(id))
  end

  @doc false
  defmacro remove_document!(db, id) do
    quote do
      { db, id } = { unquote(db), unquote(id) }
      doc        = doc(db, id)
      couchdb.update_doc(db, mark_as_deleted(doc))
    end
  end

  @doc false
  defmacro mark_as_deleted!() do
    quote do: var!(body) = mark_as_deleted(var!(body))
  end

  @doc false
  defmacro mark_as_deleted(not_found) when is_atom(not_found) do
    quote do: :not_found
  end

  @doc false
  defmacro mark_as_deleted(body) do
    quote do: create_field(unquote(body), "_deleted", true)
  end


  # Internal fuctions.
  # You shouldn't call them directly from scenario.


  @doc false
  def couchdb() do
    if :code.is_loaded(Mix) && Mix.env == :test do
      CouchNormalizer.Scenario.under_test
    else
      :couch_normalizer_utils
    end
  end

end