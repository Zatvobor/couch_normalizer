defmodule CouchNormalizer.Registry do

  @doc false
  def init() do
    registry = :ets.new(__MODULE__, [:ordered_set, {:keypos, 1}])
    :application.set_env(:couch_normalizer_manager, :registry, registry)
  end

  @doc false
  def release() do
    :ets.delete to_ets()
    :application.set_env(:couch_normalizer_manager, :registry, :undefined)
  end

  @doc false
  def acquire(title, scenario) do
    matches = Regex.run %r/(\d+)-/, title

    if is_list(matches) do
      [_,norpos|_] = matches
      acquire(norpos, title, scenario)
    else
      raise "Can't find the normalization position in '#{title}' title (it should be defined as '1-example-scenario' where '1' is the normalization position field)."
    end
  end

  @doc false
  def acquire(norpos, title, scenario) do
    case :application.get_env(:couch_normalizer_manager, :registry) do
      {:ok, to_registry}  -> :ets.insert(to_registry, {binary_to_integer(norpos), title, scenario})
      :undefined          -> raise "Env didn't initialized"
    end
  end

  @doc false
  def to_ets() do
    {:ok, registry} = :application.get_env(:couch_normalizer_manager, :registry)
    registry
  end

  @doc false
  def load_all(path) do
    Enum.each Path.wildcard(to_binary(path) <> "/*.exs"), fn(f) -> Code.load_file(f) end
  end

  @doc false
  def load(file) do
    to_binary(file) |> Code.load_file
  end
end