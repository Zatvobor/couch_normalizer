defmodule CouchNormalizer.Scenario do

  @moduledoc """
  Data normalization scenarion. This is main behaviour for data migration
  """

  @doc false
  def define(title, scenario) do
    true = CouchNormalizer.Registry.acquire(title, scenario)
  end


  # defmacro __using__(_) do
  #   quote do
  #     import CouchNormalizer.Scenario
  #
  #     def normalize(env, handle_update) do
  #
  #       db      = Keyword.get(env, :db, nil)
  #       doc_id  = Keyword.get(env, :doc_id, nil)
  #       rev     = Keyword.get(env, :rev, nil)
  #       body    = Keyword.get(env, :body, nil)
  #
  #       case apply(db, doc_id, rev, body) do
  #         {:update, body} -> handle_update.(db, doc_id, body)
  #         _               -> :skiped
  #       end
  #     end
  #
  #
  #
  #
  #   end # quote do
  # end # defmacro __using__


  defmacro field(body, name) do
    quote do: Keyword.get(unquote(body), to_b(unquote(name)))
  end


  defmacro remove_field(name) do
    quote do: var!(body) = Keyword.delete(var!(body), to_b(unquote(name)))
  end


  defmacro remove_fields(names) do
    quote do: lc f inlist unquote(names), do: remove_field(f)
  end


  defmacro rename_field(name, new_name) do
    quote do: update_field(unquote(name), unquote(new_name), field(var!(body), unquote(name)))
  end


  defmacro update_field(name, value) do
    quote do: update_field(unquote(name), unquote(name), unquote(value))
  end


  defmacro update_field(name, new_name, value) do
    quote do
      case Keyword.key?(var!(body), to_b(unquote(name))) do
        true  ->
                  body = Keyword.delete(var!(body), to_b(unquote(name)))
                  var!(body) = [{to_b(unquote(new_name)), unquote(value)}|body]
        _     ->
                  false
      end
    end
  end



  def to_b(name) when is_atom(name), do: atom_to_binary(name)
  def to_b(name), do: name

end