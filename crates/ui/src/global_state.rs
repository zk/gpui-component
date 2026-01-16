use std::rc::Rc;

use gpui::{App, Entity, Global, Pixels, Point, Window};

use crate::text::TextViewState;

/// Callback type for link click events (UI thread only).
pub(crate) type OnLinkClickCallback = Rc<dyn Fn(&str, &mut Window, &mut App)>;
/// Callback type for link right-click events (UI thread only).
pub(crate) type OnLinkRightClickCallback = Rc<dyn Fn(&str, Point<Pixels>, &mut Window, &mut App)>;

pub(crate) fn init(cx: &mut App) {
    cx.set_global(GlobalState::new());
}

impl Global for GlobalState {}

pub(crate) struct GlobalState {
    pub(crate) text_view_state_stack: Vec<Entity<TextViewState>>,
    /// Stack of link click callbacks for current text view rendering context
    pub(crate) on_link_click_stack: Vec<Option<OnLinkClickCallback>>,
    /// Stack of link right-click callbacks for current text view rendering context
    pub(crate) on_link_right_click_stack: Vec<Option<OnLinkRightClickCallback>>,
}

impl GlobalState {
    pub(crate) fn new() -> Self {
        Self {
            text_view_state_stack: Vec::new(),
            on_link_click_stack: Vec::new(),
            on_link_right_click_stack: Vec::new(),
        }
    }

    pub(crate) fn global(cx: &App) -> &Self {
        cx.global::<Self>()
    }

    pub(crate) fn global_mut(cx: &mut App) -> &mut Self {
        cx.global_mut::<Self>()
    }

    pub(crate) fn text_view_state(&self) -> Option<&Entity<TextViewState>> {
        self.text_view_state_stack.last()
    }

    pub(crate) fn on_link_click(&self) -> Option<OnLinkClickCallback> {
        self.on_link_click_stack.last().and_then(|c| c.clone())
    }

    pub(crate) fn on_link_right_click(&self) -> Option<OnLinkRightClickCallback> {
        self.on_link_right_click_stack.last().and_then(|c| c.clone())
    }
}
