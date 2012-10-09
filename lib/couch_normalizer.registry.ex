defmodule CouchNormalizer.Registry do


  @doc false
  def acquire(title, scenario) do
    norpos = String.at(title, 0)
    case Regex.match?(%r/\d+/, norpos) do
      true  ->
        acquire(norpos, title, scenario)
      false ->
        raise "Can't find the normalization position in '#{title}' title (it should be defined as '1-example-scenario' where '1' is the normalization position (:norompos))."
    end
  end


  @doc false
  def acquire(norpos, title, scenario) do
    case :application.get_env(:couch_normalizer_manager, :registry) do
      {:ok, ticket} -> :ets.insert(ticket, {norpos, title, scenario})
      :undefined    -> raise "Can't find environment for :couch_normalizer_manager, :registry"
    end
  end


  @doc false
  def load_all(path) do
    path = convert_list_to_binary(path)
    Enum.each File.wildcard(File.expand_path(path) <> "/*.exs"), fn(f) -> Code.load_file(f) end
  end

  @doc false
  def load(file) do
    Code.load_file File.expand_path(convert_list_to_binary(file))
  end



  @doc false
  defp convert_list_to_binary(string) do
    if is_list(string) do
      list_to_binary(string)
    else
      string
    end
  end

end