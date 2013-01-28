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


  @doc """
  Invokes particular scenario with specific document's invariants.
  This function used as a main entry point for performing all scenarios.
  Check `:couch_normalizer_process.apply_scenario/3` for more details.
  """
  def call(scenario, {db, id, rev, body}) when is_function(scenario) do
    # calls scenario function
    scenario.(db, id, rev, HashDict.new(body))
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