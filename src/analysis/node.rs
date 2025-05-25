use rtlola_frontend::mir::{
    Offset, Origin, PacingType, RtLolaMir, StreamAccessKind, StreamReference,
};
use serde::Serialize;
use std::{collections::HashSet, hash::Hash, usize};

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord, Serialize)]
pub enum Node {
    InputStream(usize),
    OutputStream(usize),
    SlidingWindow(usize),
}
impl Node {
    pub fn from_stream(stream_ref: &StreamReference) -> Node {
        match stream_ref {
            StreamReference::In(x) => Node::InputStream(x.clone()),
            StreamReference::Out(x) => Node::OutputStream(x.clone()),
        }
    }

    pub fn from_access(access: &(StreamReference, Vec<(Origin, StreamAccessKind)>)) -> Node {
        match access.1.first().unwrap().1 {
            StreamAccessKind::SlidingWindow(x) => Node::SlidingWindow(x.idx()),
            _ => Node::from_stream(&access.0),
        }
    }

    pub fn get_children(&self, mir: &RtLolaMir) -> Vec<(Node, usize)> {
        let mut children: Vec<(Node, usize)> = Vec::new();
        match self {
            Node::InputStream(x) => {
                for child in &mir.inputs[x.clone()].accessed_by {
                    for (_, access_kind) in &child.1 {
                        let offset = match access_kind {
                            StreamAccessKind::Offset(off) => match off {
                                Offset::Past(x) => x.clone() as usize,
                                _ => unreachable!(),
                            },
                            _ => 0,
                        };
                        children.push((Node::from_access(child), offset));
                    }
                }
            }
            Node::OutputStream(x) => {
                for child in &mir.outputs[x.clone()].accessed_by {
                    for (_, access_kind) in &child.1 {
                        let offset = match access_kind {
                            StreamAccessKind::Offset(off) => match off {
                                Offset::Past(x) => x.clone() as usize,
                                _ => unreachable!(),
                            },
                            _ => 0,
                        };
                        children.push((Node::from_access(child), offset));
                    }
                }
            }
            Node::SlidingWindow(x) => {
                children.push((Node::from_stream(&mir.sliding_windows[x.clone()].caller), 0));
            }
        }
        children
            .into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
    }

    pub fn get_non_offset_children(&self, mir: &RtLolaMir) -> Vec<Node> {
        let mut children: Vec<Node> = Vec::new();
        match self {
            Node::InputStream(x) => {
                for child in &mir.inputs[x.clone()].accessed_by {
                    for (_, access_kind) in &child.1 {
                        match access_kind {
                            StreamAccessKind::Offset(_) => {}
                            _ => {
                                children.push(Node::from_access(child));
                            }
                        }
                    }
                }
            }
            Node::OutputStream(x) => {
                for child in &mir.outputs[x.clone()].accessed_by {
                    for (_, access_kind) in &child.1 {
                        match access_kind {
                            StreamAccessKind::Offset(_) => {}
                            _ => {
                                children.push(Node::from_access(child));
                            }
                        }
                    }
                }
            }
            Node::SlidingWindow(x) => {
                children.push(Node::from_stream(&mir.sliding_windows[x.clone()].caller));
            }
        }
        children
            .into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
    }

    pub fn get_non_offset_parents(&self, mir: &RtLolaMir) -> Vec<Node> {
        let mut parents: Vec<Node> = Vec::new();
        match self {
            Node::InputStream(x) => (),
            Node::OutputStream(x) => {
                for parent in &mir.outputs[x.clone()].accesses {
                    for (_, access_kind) in &parent.1 {
                        match access_kind {
                            StreamAccessKind::Offset(_) => {}
                            _ => {
                                parents.push(Node::from_access(parent));
                            }
                        }
                    }
                }
            }
            Node::SlidingWindow(x) => {
                parents.push(Node::from_stream(&mir.sliding_windows[x.clone()].target));
            }
        }
        parents
            .into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
    }

    pub fn get_offset_parents(&self, mir: &RtLolaMir) -> Vec<(Node, usize)> {
        let mut parents: Vec<(Node, usize)> = Vec::new();
        match self {
            Node::OutputStream(x) => {
                for parent in &mir.outputs[x.clone()].accesses {
                    match parent.1.first().unwrap().1 {
                        StreamAccessKind::Offset(off) => match off {
                            Offset::Past(x) => {
                                parents.push((Node::from_access(parent), x.clone() as usize));
                            }
                            _ => {
                                unreachable!()
                            }
                        },
                        _ => {}
                    }
                }
            }
            _ => {}
        }
        parents
    }

    pub fn get_hold_parents(&self, mir: &RtLolaMir) -> Vec<Node> {
        let mut parents: Vec<Node> = Vec::new();
        match self {
            Node::OutputStream(x) => {
                for parent in &mir.outputs[x.clone()].accesses {
                    match parent.1.first().unwrap().1 {
                        StreamAccessKind::Hold => {
                            parents.push(Node::from_access(parent));
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
        parents
    }

    pub fn prettify(&self, mir: &RtLolaMir) -> String {
        match self {
            Node::InputStream(x) => mir.inputs[x.clone()].name.clone(),
            Node::OutputStream(x) => mir.outputs[x.clone()].name.clone(),
            Node::SlidingWindow(x) => {
                let caller = {
                    mir.outputs[mir.sliding_windows[x.clone()].caller.out_ix()]
                        .name
                        .clone()
                };
                let target = {
                    let target = mir.sliding_windows[x.clone()].target;
                    if target.is_output() {
                        mir.outputs[target.out_ix()].name.clone()
                    } else {
                        mir.inputs[target.in_ix()].name.clone()
                    }
                };
                format!("sw({},{})", target, caller)
            }
        }
    }

    pub fn is_periodic(&self, mir: &RtLolaMir) -> bool {
        match self {
            Node::InputStream(_) => false,
            Node::OutputStream(x) => match mir.outputs[x.clone()].eval.eval_pacing {
                PacingType::GlobalPeriodic(_) => true,
                PacingType::Event(_) => false,
                PacingType::Constant => false,
                _ => unimplemented!(),
            },
            Node::SlidingWindow(_) => true,
        }
    }
}
