use CouchNormalizer.Scenario
# Some tracks have missing `artist_uri` field. Hovewer, we could acquire missing field from `album`
# that belongs to track.
CouchNormalizer.Registry.acquire "3-add-artist-uri-to-track-if-album-has-it", fn(db, _doc_id, _rev, body) ->
  if body["type"] == "track" do

      album_id = body["album_uri"]
      unless album_id == :nil do

        album_doc = doc(album_id)
        unless album_doc == :not_found do

          artist_uri = album_doc["artist_uri"]
          if artist_uri != :nil && body["artist_uri"] == :nil do
            create_field :artist_uri, artist_uri
            {:update, body}
          end

        end
      end

  end
end