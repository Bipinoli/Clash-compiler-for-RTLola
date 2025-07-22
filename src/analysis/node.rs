use rtlola_frontend::mir::{
    Offset, Origin, PacingType, RtLolaMir, StreamAccessKind, StreamReference,
};
use serde::Serialize;
use std::{collections::HashSet, hash::Hash, usize};

type ChildWithOffset = (Node, usize);

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

    fn get_children_from_accessed_by(
        accesses: &Vec<(StreamReference, Vec<(Origin, StreamAccessKind)>)>,
    ) -> Vec<ChildWithOffset> {
        let mut children: Vec<(Node, usize)> = Vec::new();
        for child in accesses {
            for (_, access_kind) in &child.1 {
                let (final_child, offset) = match access_kind {
                    StreamAccessKind::Sync => (Node::from_access(child), 0),
                    StreamAccessKind::Offset(off) => match off {
                        Offset::Past(x) => (Node::from_access(child), x.clone() as usize),
                        _ => unreachable!(),
                    },
                    StreamAccessKind::SlidingWindow(sw) => (Node::SlidingWindow(sw.idx()), 0),
                    StreamAccessKind::Hold => (Node::from_access(child), 0),
                    _ => unimplemented!(),
                };
                children.push((final_child, offset));
            }
        }
        children
    }

    pub fn get_children(&self, mir: &RtLolaMir) -> Vec<ChildWithOffset> {
        let children = match self {
            Node::InputStream(x) => {
                Node::get_children_from_accessed_by(&mir.inputs[x.clone()].accessed_by)
            }
            Node::OutputStream(x) => {
                Node::get_children_from_accessed_by(&mir.outputs[x.clone()].accessed_by)
            }
            Node::SlidingWindow(x) => {
                vec![(Node::from_stream(&mir.sliding_windows[x.clone()].caller), 0)]
            }
        };
        children
            .into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
    }

    pub fn get_non_offset_children(&self, mir: &RtLolaMir) -> Vec<Node> {
        self.get_children(mir)
            .iter()
            .filter(|(_, offset)| *offset == 0 as usize)
            .map(|(child, _)| child.clone())
            .collect()
    }

    pub fn get_non_offset_parents(&self, mir: &RtLolaMir) -> Vec<Node> {
        let mut parents: Vec<Node> = Vec::new();
        match self {
            Node::InputStream(_) => (),
            Node::OutputStream(x) => {
                for parent in &mir.outputs[x.clone()].accesses {
                    for (_, access_kind) in &parent.1 {
                        match access_kind {
                            StreamAccessKind::Offset(_) => {}
                            StreamAccessKind::SlidingWindow(sw) => {
                                parents.push(Node::SlidingWindow(sw.idx()))
                            }
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

    pub fn is_input(&self) -> bool {
        match self {
            Node::InputStream(_) => true,
            _ => false,
        }
    }
}
