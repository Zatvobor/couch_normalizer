Code.require_file "../test_helper.exs", __FILE__

## Output for both elixir 0.7.0 and 0.7.2 versions published on
## `https://gist.github.com/4547444`
##

defmodule CouchNormalizer.ElixirUpdateIssueTest do
  use ExUnit.Case, async: false

  defmodule StubbedCouchDb do
    def body, do: [ {"link", "example.com"} ]
    def document_body(_db, _id), do: body
    def update_doc(_body), do: :done!
  end

  CouchNormalizer.Scenario.under_test(StubbedCouchDb)

  @subject "examples/1-example-scenario.exs"

  def setup(_) do
    # tries to load scenario from exs file
    CouchNormalizer.Registry.init
    CouchNormalizer.Registry.load(@subject)

    # loads scenario locally
    CouchNormalizer.Registry.acquire "2-test-scenario", fn(_db, _doc_id, _rev, body) ->
      use CouchNormalizer.Scenario

      # 1. update/improve document structure
      # update_field    :field, String.downcase(field(body, :b))
      # rename_field    :old_name, :new_name

      # 2. cleanup unused/deprecated fields
      # remove_field    :unused_a
      # remove_field    :unused_b

      {:update, body}
    end

  end

  def teardown() do
    CouchNormalizer.Registry.release
  end

  def subject_fun(i) do
    [{ _, _, subject }] = :ets.lookup(CouchNormalizer.Registry.to_ets, i)
    subject
  end


  test "shows function info" do
    IO.puts inspect(:erlang.fun_info(subject_fun(1)))
    IO.puts inspect(:erlang.fun_info(subject_fun(2)))
  end

  test "does work as loaded" do
    body = HashDict.new([{"type", "track"}])
    assert subject_fun(1).("db", "ddoc", "rev", body) == nil
  end

  test "does work as locally defined" do
    assert subject_fun(2).("db", "ddoc", "rev", "body") == {:update, "body"}
  end

  test "does fix function clause error" do
    CouchNormalizer.Scenario.call(subject_fun(1), {"db", "ddoc", "rev", [{"type", "track"}]})
  end

end