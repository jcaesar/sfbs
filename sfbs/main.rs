use std::collections::HashMap;
use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

use data::*;
use serde::Deserialize;
use serde::de::DeserializeOwned;

fn main() {
    let mut out = Main::default();
    out.build_times.start = unix_ts();
    let flakes = std::env::var("SFBS_SYSFLAKES")
        .expect("SFBS_SYSFLAKES env var not set")
        .split(",")
        .map(Into::into)
        .collect::<Vec<String>>();
    for flake in &flakes {
        out.evals.insert(flake.into(), evals(flake));
    }
    out.builds = Some(build(
        out.evals
            .values()
            .filter_map(|e| e.eval.as_ref())
            .flat_map(|e| e.values())
            .filter_map(|e| e.drv.as_deref()),
    ));
    out.build_times.end = unix_ts();

    match std::env::var_os("SFBS_OUTPUT") {
        None => println!(
            "{}",
            serde_json::to_string_pretty(&out).expect("Output struct should serialize")
        ),
        Some(outpath) => std::fs::write(
            outpath,
            serde_json::to_string(&out).expect("Output struct should serialize"),
        )
        .expect("Failed to write result"),
    }
}

fn unix_ts() -> Option<i64> {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .ok()
        .and_then(|d| d.as_secs().try_into().ok())
}

#[derive(serde::Deserialize)]
struct NixMsg {
    msg: Option<String>,
}

fn get_nix_des<T>(inp: &Option<NixDeserialized<T>>) -> Option<&T> {
    inp.as_ref()
        .and_then(|inp| inp.data.as_ref())
        .and_then(|inp| inp.as_ref().ok())
}

fn evals(flake: &str) -> Evals {
    let mut ret = Evals::default();
    ret.lock = Some(nix_and_deserialize(&[
        "flake",
        "metadata",
        "--refresh",
        "--json",
        flake,
    ]));
    if let Some(mi) = get_nix_des(&ret.lock) {
        let locked = format!("{}?rev={}", mi.resolved_url, mi.locked.rev);
        ret.hosts = Some({
            nix_and_deserialize(&[
                "eval",
                "--json",
                &format!("{locked}#nixosConfigurations"),
                "--apply",
                "builtins.attrNames",
            ])
        });
        if let Some(hosts) = get_nix_des(&ret.hosts) {
            ret.eval = Some(hosts.iter().map(|host| eval_host(&locked, host)).collect());
        }
    }
    ret
}

fn eval_host(locked: &str, host: &str) -> (String, Eval) {
    let mut ret = Eval::default();
    let host_path =
        format!("{locked}#nixosConfigurations.{host}.config.system.build.toplevel.drvPath");
    if host.chars().any(|c| !c.is_alphanumeric()) {
        ret.msgs
            .push(format!("META: Ignoring non-alphanumeric hostname {host}"));
    } else {
        let shellout = Command::new("nix")
            .args(["eval", "--log-format", "internal-json", "--raw", &host_path])
            .output()
            .expect("failed to execute process");
        for line in shellout.stderr.split(|&c| c == b'\n') {
            if line.is_empty() {
                continue;
            }
            let msg = line
                .strip_prefix(b"@nix ")
                .and_then(|msg| serde_json::from_slice::<NixMsg>(msg).ok());
            match msg {
                Some(msg) => ret.msgs.append(&mut msg.msg.into_iter().collect()),
                None => ret.stderr.push(String::from_utf8_lossy(line).into()),
            }
        }
        if shellout.status.success() {
            ret.drv = Some(
                String::from_utf8(shellout.stdout)
                    .expect("Non-UTF8 derivation path. Try annoying somebody else."),
            );
        }
    }
    (host.to_owned(), ret)
}

fn nix_and_deserialize<T>(args: &[&str]) -> NixDeserialized<T>
where
    T: DeserializeOwned,
    T: Default,
{
    let mut ret = NixDeserialized::<T>::default();
    let shellout = Command::new("nix")
        .args(args)
        .output()
        .expect("failed to execute process");
    ret.log = String::from_utf8_lossy(&shellout.stderr).into();
    if shellout.status.success() {
        ret.data =
            Some(serde_json::from_slice::<T>(&shellout.stdout).map_err(|e| format!("{e:?}")));
    }
    ret
}

structstruck::strike! {
    #[structstruck::each[derive(Deserialize)]]
    #[structstruck::long_names]
    struct BuildOutput {
        action: #[serde(rename_all = "lowercase")] enum { Result, Stop, Start, Msg },
        id: Option<u64>,
        msg: Option<String>,
        #[serde(rename = "type")]
        typ: Option<u16>,
        fields: Option<enum {
            #![serde(untagged)]
            #![allow(dead_code)]
            StartBuild(String, String, i32, i32),
            FourInt(i32, i32, i32, i32),
            TwoInt(i32, i32),
            Whatev(serde_json::Value),
        }>
    }
}
fn build<'a>(root_drvs: impl Iterator<Item = &'a str>) -> Builds {
    let mut ret = Builds::default();
    let mut running = HashMap::<u64, String>::new();
    ret.built = root_drvs.map(|d| (d.to_owned(), false)).collect();
    let root_outs = ret
        .built
        .keys()
        .map(|s| format!("{s}^out"))
        .collect::<Vec<_>>();
    let mut shellout = Command::new("nix")
        .args([
            "build",
            "--log-format",
            "internal-json",
            "--no-link",
            "--keep-going",
        ])
        .args(root_outs)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to execute process");
    for line in BufReader::new(shellout.stderr.take().unwrap()).split(b'\n') {
        let line = match line {
            Ok(line) => line,
            Err(e) => {
                eprintln!("{e:?}");
                break;
            }
        };
        if line.is_empty() {
            continue;
        }
        let out = line
            .strip_prefix(b"@nix ")
            .and_then(|msg| serde_json::from_slice::<BuildOutput>(msg).ok());
        use BuildOutputAction::*;
        match out {
            Some(BuildOutput {
                action: Msg,
                msg: Some(msg),
                ..
            }) => ret.msgs.push(msg),
            Some(BuildOutput {
                action: Start,
                id: Some(id),
                typ: Some(105),
                fields: Some(BuildOutputFields::StartBuild(drv, _, _, _)),
                ..
            }) => drop(running.insert(id, drv)),
            Some(BuildOutput {
                action: Stop,
                id: Some(id),
                ..
            }) => {
                if let Some(drv) = running.remove(&id) {
                    match outputs_exist(&drv) {
                        Ok(true) => {
                            if let Some(built) = ret.built.get_mut(&drv) {
                                *built = true;
                            }
                        }
                        Ok(false) => ret.failed.push(drv),
                        Err(e) => eprintln!("{e:?}"),
                    }
                }
            }
            Some(_) => (),
            None => ret.stderr.push(String::from_utf8_lossy(&line).into()),
        }
    }
    for (drv, built) in ret.built.iter_mut() {
        if !*built {
            match outputs_exist(drv) {
                Ok(v) => *built = v,
                Err(e) => eprintln!("{e:?}"),
            }
        }
    }
    ret
}

fn outputs_exist(drv: &str) -> Result<bool, Box<dyn std::error::Error>> {
    // nix's json output only reports that the build ended, but not whether it succeeded
    // nom parses the actual human-facing output messages, console escape codes including, to get the status. brr.
    // while it's a little more resource intensive, i'll just parse the derivation and check whether the outputs exist.
    // (not that that's foolproof either)
    use nix_compat::derivation::Derivation;
    let drv = std::fs::read(drv)?;
    let drv = Derivation::from_aterm_bytes(&drv).map_err(|e| format!("{e:?}"))?;
    for output in drv.outputs.values() {
        if let Some(path) = &output.path {
            if !std::fs::exists(path.to_absolute_path())? {
                return Ok(false);
            }
        }
    }
    Ok(true)
}
