use std::sync::Arc;

use chrono::{DateTime, Local};
use futures_signals::{
    signal::{Mutable, SignalExt},
    signal_map::MutableBTreeMap,
    signal_vec::SignalVecExt,
};
use reqwasm::http::Request;
use silkenweb::{
    clone,
    elements::html::{Th, Tr, div, style, table, td, th, tr},
    log_panics, mount,
    node::{
        Node,
        element::{Element, ParentElement, TextParentElement},
    },
    task,
    value::Sig,
};

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

fn n_of<T>(n: usize, f: impl Fn() -> T) -> Vec<T> {
    let mut ret = Vec::new();
    ret.resize_with(n, f);
    ret
}

fn app(results: &str) -> Node {
    let links = results.split(",").collect::<Vec<_>>();
    let link_count = links.len();
    let dates = n_of(link_count, || Mutable::new(Option::<i64>::None));
    let systems = MutableBTreeMap::<(String, String), SysStates>::new();
    for (i, &link) in links.iter().enumerate() {
        let link: String = link.into();
        let date = dates[i].clone();
        clone!(systems);
        task::spawn_local(async move {
            // TODO handle Request fail
            if let Ok(data) = data(&link).await {
                if let Some(end) = data.build_times.end {
                    date.set(Some(end));
                }
                let mut systems = systems.lock_mut();
                for (flake, feval) in data.evals {
                    for (sys, seval) in feval.eval.iter().flat_map(|feval| feval.iter()) {
                        let sys: String = sys.into();
                        let fst = (flake.clone(), sys);
                        let entry = match systems.get(&fst) {
                            Some(e) => e[i].clone(),
                            None => {
                                let states = Arc::new(n_of(link_count, || Mutable::new(None)));
                                let entry = states[i].clone();
                                systems.insert_cloned(fst, states); // insert_cloned takes an owned?
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
        });
    }
    let ret = table()
        .child(tr().child(th()).children(render_dates(dates)))
        .children_signal(systems.entries_cloned().map(render_sys));
    ret.into()
}

fn render_sys(((_flake, sys), states): ((String, String), SysStates)) -> Tr {
    tr().child(td().text(sys)).children(
        states
            .iter()
            .map(|sm| td().text(Sig(sm.signal().map(render_status)))),
    )
}

fn render_status(s: Option<BuildStatus>) -> String {
    format!("{:?}", s)
}

fn render_dates(dates: Vec<Mutable<Option<i64>>>) -> Vec<Th> {
    dates
        .iter()
        .map(|d| {
            th().text(Sig(d.signal().map(|d| {
                let d = d.and_then(|d| DateTime::from_timestamp(d, 0));
                match d {
                    Some(d) => format!(
                        "{}",
                        d.with_timezone(&Local).format("%Y年%m月%d日%H時%M分%S秒")
                    ),
                    None => format!("?"),
                }
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
        div()
            .child(Sig(
                silkenweb::router::url_path().signal_ref(|url_path| app(url_path.hash()))
            ))
            .child(style().text(
                "
table, th, td {
  border:1px solid black;
}
            ",
            )),
    );
}
