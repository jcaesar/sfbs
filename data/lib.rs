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
            stderr: Vec<String>,
            msgs: Vec<String>,
            deps_failed: HashMap<String, Vec<String>>,
            failed_logs: HashMap<String, Option<(String, String)>>,
        }>,
        #[serde(default, alias = "build_times")]
        meta: struct {
            #[serde(alias = "start")]
            start_time: Option<i64>,
            #[serde(alias = "end")]
            end_time: Option<i64>,
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
            last_modfied: Option<i64>,
        }
    }
}

#[derive(Deserialize, Serialize, Default, Debug)]
#[fully_pub]
struct NixDeserialized<T> {
    log: String,
    data: Option<Result<T, String>>,
}
