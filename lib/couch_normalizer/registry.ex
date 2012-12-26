defmodule CouchNormalizer.Registry do


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
      {:ok, to_registry}  -> :ets.insert(to_registry, {norpos, title, scenario})
      :undefined          -> raise "Env didn't initialized"
    end
  end

  @doc false
  def load_all(path) do
    Enum.each File.wildcard(File.expand_path(to_binary(path)) <> "/*.exs"), fn(f) -> Code.load_file(f) end
  end

  @doc false
  def load(file) do
    Code.load_file File.expand_path(to_binary(file))
  end

end