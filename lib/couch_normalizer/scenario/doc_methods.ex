defmodule CouchNormalizer.Scenario.DocMethods do
  @moduledoc """
  This module provides convenience functions for fething/removing documents from
  current or second dbs.
  """

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
  defmacro mark_as_deleted(body) do
    quote do
      { body } = { unquote(body) }
      case body do
        :not_found -> :not_found
        _          -> create_field(body, "_deleted", true)
      end
    end
  end


  # [INTERNAL] You shouldn't call them directly from your scenario.

  @doc false
  def couchdb() do
    if :code.is_loaded(Mix) && Mix.env == :test do
      CouchNormalizer.Scenario.under_test
    else
      :couch_normalizer_db
    end
  end
end