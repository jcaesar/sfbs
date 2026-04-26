use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{BufRead, BufReader, Write as _};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::rc::Rc;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::mpsc::RecvTimeoutError;
use std::sync::{LazyLock, mpsc};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use data::*;
use serde::Deserialize;
use serde::de::DeserializeOwned;

type DepInfo = HashMap<Rc<String>, Vec<Rc<String>>>;

#[derive(clap::Parser, Debug, Clone)]
#[command(version, about)]
struct Args {
    #[arg(short, long, required = true)]
    sysflake: Vec<String>,
    #[arg(short, long, env = "SFBS_OUTPUT", value_parser)]
    output: PathBuf,
    #[arg(short, long, env = "SFBS_BUILD_ARGS")]
    build_args: Option<String>,

    /// Keep links to toplevel derivations
    ///
    /// If you have keep-outputs enabled in nix.conf, this will also mean that partial builds won't be GC'ed. Helpful if you run out of disk space during building.
    #[arg(short, long, env = "SFBS_LINKS", value_parser = clap::value_parser!(clio::ClioPath).exists().is_dir())]
    links: Option<clio::ClioPath>,
    #[arg(long, env = "SFBS_GC_AFTER_LINK")]
    gc_after_link: bool,
}

static ARGS: LazyLock<Args> = LazyLock::new(clap::Parser::parse);

enum BuildStateE {
    Start,
    Success,
    Fail,
}
struct BuildState {
    drv: String,
    state: BuildStateE,
}

fn main() {
    LazyLock::force(&ARGS);
    let mut out = Main::default();
    out.meta.start_time = unix_ts();
    write_out(&out);
    let flakes = ARGS.sysflake.to_owned();
    let mut deps = HashMap::new();
    for flake in &flakes {
        out.evals.insert(flake.into(), evals(flake, &mut deps));
        write_out(&out);
    }
    if ARGS.gc_after_link {
        Command::new("nix")
            .args(["store", "gc"])
            .output()
            .expect("failed to execute gc");
    }
    let rdeps = make_machine_deps(&out.evals, &deps);

    let (building_send, building_recv) = mpsc::channel();
    std::thread::spawn(move || debounce_report(building_recv));
    build(&mut out, rdeps, building_send);
    out.meta.end_time = unix_ts();

    write_out(&out);
}

fn write_out(out: &Main) {
    let mut tmp = ARGS.output.clone().into_os_string();
    tmp.push(OsStr::new(".tmp"));
    let out = serde_json::to_string_pretty(&out).expect("Output struct should serialize");
    File::create(&tmp)
        .expect("Failed to create temp output file")
        .write_all(out.as_bytes())
        .expect("Failed to write result");
    std::fs::rename(tmp, &ARGS.output).expect("Failed to emplace output file");
}

fn make_machine_deps(
    evals: &HashMap<String, Evals>,
    deps: &DepInfo,
) -> HashMap<String, HashSet<Rc<String>>> {
    let mut r = HashMap::new();
    for eval in evals.values() {
        if let Some(eval) = &eval.eval {
            for host in eval.values() {
                if let Some(drv) = &host.drv {
                    let drv = Rc::new(drv.to_owned());
                    add_me_to_deps(&mut r, deps, drv.clone(), drv);
                }
            }
        }
    }
    fn add_me_to_deps(
        r: &mut HashMap<String, HashSet<Rc<String>>>,
        dep_info: &DepInfo,
        host: Rc<String>,
        node: Rc<String>,
    ) {
        stacker::maybe_grow(2048, 1 << 20, || {
            if let Some(deps) = dep_info.get(&node) {
                for dep in deps {
                    let new = r.entry(dep.to_string()).or_default().insert(host.clone());
                    if new {
                        add_me_to_deps(r, dep_info, host.clone(), dep.clone());
                    }
                }
            }
        })
    }
    r
}

fn read_deps(drv: String, deps: &mut DepInfo) -> Rc<String> {
    use std::collections::hash_map::Entry::*;
    let key = Rc::new(drv);
    if let Occupied(entry) = deps.entry(key.clone()) {
        entry.key().clone()
    } else {
        stacker::maybe_grow(2048, 1 << 20, || {
            let rec = read_drv(&key)
                .iter()
                .flat_map(|drv| drv.input_derivations.keys())
                .map(|inp| read_deps(inp.to_absolute_path(), deps))
                .collect::<Vec<_>>();
            deps.insert(key.clone(), rec);
            key
        })
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

fn evals(flake: &str, deps: &mut DepInfo) -> Evals {
    let mut ret = Evals::default();
    ret.lock = Some(nix_and_deserialize(&[
        "flake",
        "metadata",
        "--refresh",
        "--json",
        flake,
    ]));
    if let Some(mi) = get_nix_des(&ret.lock) {
        // i should add proper url parsing…
        let sep = match mi.resolved_url.contains('?') {
            false => '?',
            true => '&',
        };
        let locked = format!("{}{sep}rev={}", mi.resolved_url, mi.locked.rev);
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
            println!("Flake {flake} ({locked}) has hosts: {}", hosts.join(", "));
            ret.eval = Some(
                hosts
                    .iter()
                    .map(|host| eval_host(&locked, flake, host, deps))
                    .collect(),
            );
        }
    }
    ret
}

fn eval_host(locked: &str, flake: &str, host: &str, deps: &mut DepInfo) -> (String, Eval) {
    let mut ret = Eval::default();
    let host_path = format!("{locked}#nixosConfigurations.{host}.config.system.build");
    if host.chars().any(|c| !c.is_alphanumeric()) {
        ret.msgs
            .push(format!("META: Ignoring non-alphanumeric hostname {host}"));
    } else {
        #[derive(Deserialize, Debug)]
        struct EH {
            drv: String,
            grp: Option<String>,
        }
        let shellout = nix_and_deserialize::<EH>(&[
            "eval",
            "--log-format",
            "internal-json",
            "--json",
            &host_path,
            "--apply",
            "s: {drv = s.toplevel.drvPath; grp = s.sfbs-group or null; }",
        ]);
        for line in shellout.log.split(|c| c == '\n') {
            if line.is_empty() {
                continue;
            }
            let msg = line
                .strip_prefix("@nix ")
                .and_then(|msg| serde_json::from_str::<NixMsg>(msg).ok());
            match msg {
                Some(msg) => ret.msgs.append(&mut msg.msg.into_iter().collect()),
                None => ret.stderr.push(line.to_owned()),
            }
        }
        match shellout.data {
            Some(eval) => {
                let EH { drv, grp } =
                    eval.expect("Eval succeeded but didn't return required structure");
                println!("{flake}#{host} evaluated to {drv}");
                keep_drv(&drv);
                read_deps(drv.clone(), deps);
                ret.drv = Some(drv);
                ret.group = grp;
            }
            None => {
                println!(
                    "{locked}#{host} failed to evaluate:\n{}\n",
                    ret.msgs.join("\n")
                )
            }
        }
    }
    (host.to_owned(), ret)
}

fn nix_and_deserialize<T>(args: &[&str]) -> NixDeserialized<T>
where
    T: DeserializeOwned,
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

fn build<'a>(
    out: &mut Main,
    rdep_info: HashMap<String, HashSet<Rc<String>>>,
    running_modified: mpsc::Sender<BuildState>,
) {
    let mut root_drvs = out
        .evals
        .values()
        .filter_map(|e| e.eval.as_ref())
        .flat_map(|e| e.values())
        .filter(|e| e.drv.is_some())
        .collect::<Vec<_>>();
    root_drvs.sort_by_key(|d| (d.group.is_none(), &d.group));
    assert!(out.builds.is_none());
    out.builds = Some(Default::default());
    macro_rules! builds {
        () => {
            out.builds.as_mut().unwrap()
        };
    }
    builds!().built = root_drvs
        .iter()
        .map(|d| (d.drv.clone().unwrap(), false))
        .collect();
    for (i, root_drv) in root_drvs.into_iter().rev().enumerate().rev() {
        let drv = root_drv.drv.as_ref().unwrap();
        if builds!().deps_failed.contains_key(drv) {
            println!("Skipping attempt for {drv}, already has failed deps");
            continue;
        }
        build_host(drv, &rdep_info, &running_modified, builds!());
        dbg!(i);
        if i != 0 {
            write_out(&*out);
        }
    }
}

fn build_host(
    drv: &String,
    rdep_info: &HashMap<String, HashSet<Rc<String>>>,
    running_modified: &mpsc::Sender<BuildState>,
    ret: &mut Builds,
) {
    let mut running = HashMap::<u64, String>::new();
    println!("Building {drv}");
    let mut shellout = Command::new("nix")
        .args(["build", "--log-format", "internal-json", "--no-link"])
        .args(
            ARGS.build_args
                .as_ref()
                .map(|args| unescape_split(args))
                .unwrap_or(vec![]),
        )
        .arg(format!("{drv}^out"))
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
            }) => {
                println!("{}", msg);
                ret.msgs.push(msg);
            }
            Some(BuildOutput {
                action: Start,
                id: Some(id),
                typ: Some(105),
                fields: Some(BuildOutputFields::StartBuild(drv, _, _, _)),
                ..
            }) => {
                running.insert(id, drv.clone());
                running_modified
                    .send(BuildState {
                        drv,
                        state: BuildStateE::Start,
                    })
                    .ok();
            }
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
                            running_modified
                                .send(BuildState {
                                    drv,
                                    state: BuildStateE::Success,
                                })
                                .ok();
                        }
                        Ok(false) => {
                            if let Some(rdep) = rdep_info.get(&drv) {
                                for host in rdep {
                                    ret.deps_failed
                                        .entry(String::clone(host))
                                        .or_default()
                                        .push(drv.clone());
                                }
                            }
                            let log = build_log(&drv);
                            ret.failed_logs.insert(drv.clone(), log);
                            running_modified
                                .send(BuildState {
                                    drv,
                                    state: BuildStateE::Fail,
                                })
                                .ok();
                        }
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
}

fn build_log(drv: &str) -> Option<(String, String)> {
    let shellout = Command::new("nix")
        .args(["log", drv])
        .output()
        .expect("failed to execute process");
    let max_log = 20480;
    fn last(data: &[u8], len: usize) -> String {
        let mut chunk = data.rchunks(len).next().unwrap_or(b"");
        fn is_utf8_char_boundary(n: u8) -> bool {
            n < 128 || n >= 192
        }
        while chunk.len() > 0 && !is_utf8_char_boundary(chunk[0]) {
            chunk = &chunk[1..];
        }
        String::from_utf8_lossy(chunk).into_owned()
    }
    Some((
        last(&shellout.stdout, max_log),
        last(&shellout.stderr, max_log),
    ))
}

fn outputs_exist(drv: &str) -> Result<bool, Box<dyn std::error::Error>> {
    // nix's json output only reports that the build ended, but not whether it succeeded
    // nom parses the actual human-facing output messages, console escape codes including, to get the status. brr.
    // while it's a little more resource intensive, i'll just parse the derivation and check whether the outputs exist.
    // (not that that's foolproof either)
    for output in read_drv(drv)?.outputs.values() {
        if let Some(path) = &output.path {
            if !std::fs::exists(path.to_absolute_path())? {
                return Ok(false);
            }
        }
    }
    Ok(true)
}

fn read_drv(drv: &str) -> Result<nix_compat::derivation::Derivation, Box<dyn Error + 'static>> {
    use nix_compat::derivation::Derivation;
    let drv = std::fs::read(drv)?;
    let drv = Derivation::from_aterm_bytes(&drv).map_err(|e| format!("{e:?}"))?;
    Ok(drv)
}

fn unescape_split(inp: &str) -> Vec<String> {
    if inp.is_empty() {
        return vec![];
    }
    let mut ret = vec![String::new()];
    let mut escaped = false;
    for c in inp.chars() {
        if c == '\\' && !escaped {
            escaped = true;
            continue;
        }
        if c == ' ' && !escaped {
            ret.push(String::new());
            continue;
        }
        let len = ret.len();
        ret[len - 1].push(c);
        escaped = false;
    }
    ret
}

fn keep_drv(drv: &str) {
    static COUNT: AtomicU32 = AtomicU32::new(0);
    if let Some(dir) = ARGS.links.as_ref() {
        let i = COUNT.fetch_add(1, Ordering::SeqCst);
        let o = dir.path().to_owned().join(format!("b{i}"));
        Command::new("nix")
            .arg("build")
            .arg("-o")
            .arg(o.as_os_str())
            .arg(drv)
            .status()
            .ok();
    };
}

fn debounce_report(c: mpsc::Receiver<BuildState>) {
    let mut timeout = Option::<Instant>::None;
    let mut running = HashSet::<String>::new();
    loop {
        let recv = match timeout {
            Some(timeout) => c.recv_timeout(timeout.saturating_duration_since(Instant::now())),
            None => c.recv().map_err(|_| RecvTimeoutError::Disconnected),
        };
        timeout.get_or_insert_with(|| Instant::now() + Duration::from_secs(10));
        match recv {
            Ok(BuildState { drv, state }) => match state {
                BuildStateE::Start => {
                    running.insert(drv);
                }
                BuildStateE::Success => {
                    running.remove(&drv);
                    println!("Built: {drv}");
                }
                BuildStateE::Fail => {
                    running.remove(&drv);
                    println!("Failed to build {drv}.");
                }
            },
            Err(e) => {
                let mut report = String::new();
                if !running.is_empty() {
                    report += "Running:\n";
                    for d in &running {
                        report += &format!("  {d}\n");
                    }
                }
                print!("{report}");
                match e {
                    RecvTimeoutError::Timeout => timeout = None,
                    RecvTimeoutError::Disconnected => return,
                }
            }
        }
    }
}
