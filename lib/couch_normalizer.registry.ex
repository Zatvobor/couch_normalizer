defmodule CouchNormalizer.Registry do


  @doc false
  def acquire(title, scenario) do
    [h|_] = String.split(title, "-")
    case is_number(h) do
      true  ->
        acquire(h, title, scenario)
      false ->
        raise "Can't find the normalization position in '#{title}' title (it should be defined as '1-example-scenario' where '1' is the normalization position (:norompos))."
    end
  end


  def acquire(norpos, title, scenario) do
    case Erlang.application.get_env(:couch_normalizer, :scenarios) do
      {:ok, ticket} -> Erlang.ets.insert(ticket, {norpos, ticket, scenario})
      :undefined    -> raise "Can't find 'scenarios' registry"
    end
  end
end