{application,couch_normalizer,
             [{description,"Couch DB Normalization manager"},
              {vsn,"git"},
              {modules,[couch_normalizer_app,couch_normalizer_httpd_db,
                        couch_normalizer_manager]},
              {registered,[couch_migration_manager]},
              {applications,[kernel,stdlib,couch]},
              {mod,{couch_normalizer_app,[]}},
              {env,[]}]}.
