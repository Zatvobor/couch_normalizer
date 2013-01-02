defmodule CouchNormalizer.Scenario do

  @moduledoc """
  This module specifies the Normalization DSL and brings
  manipulation functions to your scenario.

  Check for more information:

  * `CouchNormalizer.Scenario.DocMethods`

  * `CouchNormalizer.Scenario.CachedMethods`

  * `CouchNormalizer.Scenario.FieldMethods`
  """

  @doc false
  defmacro __using__(_) do
    quote do
      import CouchNormalizer.Scenario.DocMethods
      import CouchNormalizer.Scenario.CachedMethods
      import CouchNormalizer.Scenario.FieldMethods
    end
  end


  @doc false
  def call(scenario_fun, {db_name, id, rev, body}) do
    # calls scenario function
    case scenario_fun.(db_name, id, rev, HashDict.new(body)) do
      { :update, body } -> { :update, HashDict.to_list(body) }
      _                 -> nil
    end
  end

  @doc """
  Gets the Scenario used by default under test.
  """
  def under_test() do
    { :ok, mod } = :application.get_env(:couch_normalizer, :under_test)
    mod
  end

  @doc """
  Sets the Scenario to be used under test.
  """
  def under_test(mod) do
    :application.set_env(:couch_normalizer, :under_test, mod)
  end

end