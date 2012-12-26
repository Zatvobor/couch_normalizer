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

end