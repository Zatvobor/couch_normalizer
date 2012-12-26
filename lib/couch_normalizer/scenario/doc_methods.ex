defmodule CouchNormalizer.Scenario.DocMethods do


  @doc false
  defmacro doc(db, id) do
    quote do
      { db, id } = { unquote(db), unquote(id) }
      couchdb.document_body(db, id)
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
  defmacro remove_document!(db, id) do
    quote do
      { db, id } = { unquote(db), unquote(id) }
      document = couchdb.mark_as_deleted(doc(db, id))
      couchdb.update_doc(document)
    end
  end

  @doc false
  defmacro mark_as_deleted!() do
    quote do: var!(body) = couchdb.mark_as_deleted(var!(body))
  end

  @doc false
  def couchdb() do
    if Mix.env == :test, do: :couch_normalizer_utils, else: :couch_normalizer_utils
  end

end