mnesia_json
=====

An OTP library to dump information about a running mnesia instance to a JSON which can then be sent to the user. Note that the JSON is pretty printed

To add this to your project in the `rebar.config` file add this to the deps record.
```erlang
{mnesia_json, {git, "https://github.com/Unity-Technologies/mnesia_json.git"}}
```

Note that this is a library application, it does not start any processes in the Erlang VM. It just contains the 1 module

To invoke ensure that the mnesia application is started and run the function `mnesia_json:json/0` it will output a pretty printed json with a fair amount of information about the mnesia instance. You can also use `mnesia_json/info/0` to create raw terms which can be turned into JSON with `jsx:encode/1`, this is useful if you wish to embed this in a larger data structure, or pull out specific fields.

Fields that might be useful:
* `directory`, the directory where mnesia stores its data
* `running_db_nodes`, the nodes on which mnesia is running.
* `tables`, a list of tables
* `local_tables`, tables on the local node
* `disc_copies`, `ram_copies`, `disc_only_copies` lists which tables are stored in what way
* `table_info:table_name` Information about a specific table

   * `active_replicas`, nodes on which a table is active
   * `index`, list of table indexes
   * `memory`, memory used by a table (in bytes)
   * `size`, size of the table (number of records)
   * `type`, one of set, bag, duplicate_bag
   * `disc_copies`, `disc_only_copies`, `ram_copies`, show where the table is stored and how
   * `arity`, number of fields (you should subtract 1 from this number)
   * `attributes`, what the fields are in the table
   * `record_name`, the name of the stored record.

Note, the arity of a table will always be the attributes + 1. So the shopping mall table holds erlang records of the type shopping mall (see code). Which are actually tuples in which the first element is the atom `shopping_mall`. So There are 3 fields, but the first one is just the type of the record.

```erlang
-record(shopping_mall, {name,
                        address = unknown}).
%% Actual erlang tuple
{shopping_mall, "name", "address"}

-record(person, {name,
                 age = 0,
                 address = unknown,
                 salary = 0,
                 children = []}).

```

Here is the full output JSON from the test, which creates 2 mnesia
tables from the example `person` and `shopping_mall` records and adds
some sample data.

```javascript
{
  "debug": "none",
  "directory": "/Users/zachary.kessin/Sandbox/mnesia_json/Mnesia.eunit-test@zachary",
  "fallback_activated": false,
  "schema_location": "opt_disc",
  "tables": [
    "shopping_mall",
    "schema",
    "person"
  ],
  "transaction_commits": 3,
  "transaction_failures": 4,
  "transaction_log_writes": 2,
  "transaction_restarts": 0,
  "use_dir": true,
  "version": "4.16.1",
  "uncertain_transactions": [],
  "running_nodes": [
    "eunit-test@zachary"
  ],
  "db_nodes": [
    "eunit-test@zachary"
  ],
  "stopped_db_nodes": "",
  "held_locks": 0,
  "lock_queue": 0,
  "master_tables": [],
  "remote": [],
  "ram_copies": [
    "person"
  ],
  "disc": [
    "schema",
    "shopping_mall"
  ],
  "disc_only": [],
  "table_info": {
    "shopping_mall": {
      "access_mode": "read_write",
      "active_replicas": [
        "eunit-test@zachary"
      ],
      "all_nodes": [
        "eunit-test@zachary"
      ],
      "arity": 3,
      "attributes": [
        "name",
        "address"
      ],
      "disc_copies": [
        "eunit-test@zachary"
      ],
      "disc_only_copies": [],
      "index": [],
      "load_node": "eunit-test@zachary",
      "memory": 337,
      "ram_copies": [],
      "record_name": "shopping_mall",
      "size": 1,
      "storage_type": "disc_copies",
      "type": "set"
    },
    "schema": {
      "access_mode": "read_write",
      "active_replicas": [
        "eunit-test@zachary"
      ],
      "all_nodes": [
        "eunit-test@zachary"
      ],
      "arity": 3,
      "attributes": [
        "table",
        "cstruct"
      ],
      "disc_copies": [
        "eunit-test@zachary"
      ],
      "disc_only_copies": [],
      "index": [],
      "load_node": "eunit-test@zachary",
      "memory": 660,
      "ram_copies": [],
      "record_name": "schema",
      "size": 3,
      "storage_type": "disc_copies",
      "type": "set"
    },
    "person": {
      "access_mode": "read_write",
      "active_replicas": [
        "eunit-test@zachary"
      ],
      "all_nodes": [
        "eunit-test@zachary"
      ],
      "arity": 6,
      "attributes": [
        "name",
        "age",
        "address",
        "salary",
        "children"
      ],
      "disc_copies": [],
      "disc_only_copies": [],
      "index": [],
      "load_node": "eunit-test@zachary",
      "memory": 422,
      "ram_copies": [
        "eunit-test@zachary"
      ],
      "record_name": "person",
      "size": 5,
      "storage_type": "ram_copies",
      "type": "set"
    }
  }
}

```

This code is tested with eunit, elvis and dialyzer
