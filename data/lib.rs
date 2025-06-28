use fully_pub::fully_pub;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;

structstruck::strike! {
    struct Main {
        #![structstruck::each[fully_pub]]
        #![structstruck::each[derive(Deserialize, Serialize, Default, Debug)]]
        evals: HashMap<String, struct {
            lock: Option<NixDeserialized<MetadataInfo>>,
            hosts: Option<NixDeserialized<Vec<String>>>,
            eval: Option<HashMap<String, struct {
                drv: Option<String>,
                msgs: Vec<String>,
                stderr: Vec<String>,
            }>>,
        }>,
        builds: Option<struct {
            built: IndexMap<String, bool>,
            failed: Vec<String>,
            stderr: Vec<String>,
            msgs: Vec<String>,
        }>,
        build_times: struct {
            start: Option<i64>,
            end: Option<i64>,
        },
    }
}

structstruck::strike! {
    struct MetadataInfo {
        #![structstruck::each[fully_pub]]
        #![structstruck::each[derive(Deserialize, Serialize, Default, Debug)]]

        #[serde(alias = "resolvedUrl")]
        resolved_url: String,
        locked: struct {
            rev: String,
            #[serde(alias = "lastModified")]
            last_modfied: i64,
        }
    }
}

#[derive(Deserialize, Serialize, Default, Debug)]
#[fully_pub]
struct NixDeserialized<T> {
    log: String,
    data: Option<Result<T, String>>,
}
