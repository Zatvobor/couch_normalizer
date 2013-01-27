use CouchNormalizer.Scenario
# Sets/Updates missing `artist_uri` and `artist` fields for `album` and `track` documents
# via special 'queque' documents.
CouchNormalizer.Registry.acquire "2-acquire-missing-artists-fields", fn(_db, doc_id, _rev, body) ->
  if  (body["type"] == "album" || body["type"] == "track") do
    # contains 'ids' and 'fields' which will be updated
    name_and_id_doc = doc("changes-queue", "tracks_albums_artist_name_and_id", :cached!)

    # checks if `name_and_id_doc` has updates for the current `doc_id`
    name_and_id_field = name_and_id_doc[doc_id]
    if name_and_id_field != nil do
      [artist_name, artist_id] = name_and_id_field

      artist_uri = "medianet:artist:#{artist_id}"

      # creates or updates a missing `artist_uri` field
      if body["artist_uri"] == nil do
        create_field :artist_uri, artist_uri
      else
        update_field :artist_uri, artist_uri
      end

      # creates or updates a missing `artist` field
      if body["artist"] == nil do
        create_field :artist, artist_name
      else
        update_field :artist, artist_name
      end

       {:update, body}
    end

  end
end