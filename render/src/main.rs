use std::sync::Arc;

use chrono::{DateTime, Local, Utc};
use futures_signals::{
    signal::{Mutable, SignalExt},
    signal_map::MutableBTreeMap,
    signal_vec::SignalVecExt,
};
use reqwasm::http::Request;
use silkenweb::{
    clone,
    elements::html::{Th, Tr, div, p, span, style, table, td, th, tr},
    log_panics, mount,
    node::{
        Node,
        element::{Element, ParentElement, TextParentElement},
    },
    task,
    value::Sig,
};

// CSS is lava
const RED: &'static str = "red";
const YELLOW: &'static str = "#f1ad0e"; // TODO
const GREEN: &'static str = "#00a92d";

async fn data(url: &str) -> Result<data::Main, Box<dyn std::error::Error>> {
    let data = Request::get(url).send().await?.text().await?;
    let data = serde_json::from_str(&data)?;
    Ok(data)
}

#[derive(Clone, Copy, Debug)]
enum EvalStatus {
    Failed,
    Notice,
    Ok,
}

#[allow(unused)]
#[derive(Clone, Copy, Debug)]
struct BuildStatus {
    eval: EvalStatus,
    build_ok: Option<bool>,
}

type SysStates = Arc<Vec<Mutable<Option<BuildStatus>>>>;

#[derive(Clone, Copy)]
enum RunStatus {
    NotYetLoaded,
    BuiltOn(DateTime<Utc>),
    DateUnknown,
    LoadFail,
}

fn n_of<T>(n: usize, f: impl Fn() -> T) -> Vec<T> {
    let mut ret = Vec::new();
    ret.resize_with(n, f);
    ret
}

// yeah, using fine-grained state for this is not sane.
// but I wanted to take silkenweb fr a spin
fn app(results: &str) -> Node {
    let links = results.split(",").collect::<Vec<_>>();
    let link_count = links.len();
    let runs = n_of(link_count, || Mutable::new(RunStatus::NotYetLoaded));
    let systems = MutableBTreeMap::<SysKey, SysStates>::new();
    for (i, &link) in links.iter().enumerate().rev().rev() {
        let link: String = link.into();
        let run = runs[i].clone();
        clone!(systems);
        task::spawn_local(async move {
            match data(&link).await {
                Ok(data) => {
                    let end = data
                        .build_times
                        .end
                        .and_then(|end| DateTime::from_timestamp(end, 0));
                    run.set(match end {
                        Some(end) => RunStatus::BuiltOn(end),
                        None => RunStatus::DateUnknown,
                    });
                    let mut systems = systems.lock_mut();
                    for (_flake, feval) in data.evals {
                        for (sys, seval) in feval.eval.iter().flat_map(|feval| feval.iter()) {
                            let sys: String = sys.into();
                            let entry = match systems.get(&sys) {
                                Some(e) => e[i].clone(),
                                None => {
                                    let states = Arc::new(n_of(link_count, || Mutable::new(None)));
                                    let entry = states[i].clone();
                                    systems.insert_cloned(sys, states); // insert_cloned takes an owned?
                                    entry
                                }
                            };
                            entry.set(Some(BuildStatus {
                                eval: match (seval.drv.is_some(), seval.msgs.is_empty()) {
                                    (false, _) => EvalStatus::Failed,
                                    (_, false) => EvalStatus::Notice,
                                    _ => EvalStatus::Ok,
                                },
                                build_ok: seval.drv.as_ref().and_then(|drv| {
                                    data.builds
                                        .as_ref()
                                        .and_then(|builds| builds.built.get(drv).copied())
                                }),
                            }));
                        }
                    }
                }
                Err(e) => {
                    web_log::eprintln!("Loading {link}: {e:?}");
                    run.set(RunStatus::LoadFail)
                }
            }
        });
    }
    let ret = table()
        .style_property("margin", "auto")
        .child(tr().child(th()).children(render_run(runs)))
        .children_signal(systems.entries_cloned().map(render_sys));
    ret.into()
}

type SysKey = String; // I tried worrying about duplicated configuration keys that are not the same host. I don't think it has merit.
// I sure would like to keep them grouped by flake and in the order of keys…

fn render_sys((sys, states): (SysKey, SysStates)) -> Tr {
    tr().child(td().text(sys)).children(
        states
            .iter()
            .map(|sm| td().child(Sig(sm.signal().map(render_status)))),
    )
}

fn render_status(s: Option<BuildStatus>) -> Node {
    match s {
        Some(s) => p()
            .style_property("margin", "0.2em")
            .child(span().text("◖").style_property(
                "color",
                match s.eval {
                    EvalStatus::Failed => RED,
                    EvalStatus::Notice => YELLOW,
                    EvalStatus::Ok => GREEN,
                },
            ))
            .child(match s.build_ok {
                Some(build_ok) => span().text("◗").style_property(
                    "color",
                    match build_ok {
                        true => GREEN,
                        false => RED,
                    },
                ),
                None => span(),
            }),
        None => p(), // another case of dummy elements to not fight the type checker
    }
    .into()
}

fn render_run(dates: Vec<Mutable<RunStatus>>) -> Vec<Th> {
    dates
        .iter()
        .map(|d| {
            th().text(Sig(d.signal().map(|d| match d {
                RunStatus::NotYetLoaded => format!("…"),
                RunStatus::BuiltOn(t) => {
                    format!("{}", t.with_timezone(&Local).format("%Y年%m月%d日%H時"))
                }
                RunStatus::DateUnknown => format!("????年??月??日??時"),
                RunStatus::LoadFail => format!("×"),
            })))
            .style_property("writing-mode", "vertical-lr")
            .style_property("text-orientation", "upright")
        })
        .collect::<Vec<_>>()
}

fn main() {
    log_panics();
    mount(
        "app",
        div().child(Sig(
            silkenweb::router::url_path().signal_ref(|url_path| app(url_path.hash()))
        )),
    );
}
