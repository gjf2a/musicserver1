use std::{ops::RangeInclusive, sync::Arc, cmp::{min, max}, collections::{VecDeque, HashMap}};

use bare_metal_modulo::{ModNum, MNum};
use crossbeam_utils::atomic::AtomicCell;
use eframe::{epaint::{Stroke, Color32, Vec2, Pos2, Rect, FontId, FontFamily}, egui::{Ui, Painter, Sense, self}, emath::Align2};

use crate::analyzer::{MusicMode, KeySignature, MidiByte, Melody, Accidental, MelodicFigure, Note};

const MIDDLE_C: MidiByte = 60;
const STAFF_PITCH_WIDTH: MidiByte = 19;
const LOWEST_STAFF_PITCH: MidiByte = MIDDLE_C - STAFF_PITCH_WIDTH;
const HIGHEST_STAFF_PITCH: MidiByte = MIDDLE_C + STAFF_PITCH_WIDTH;
const BORDER_SIZE: f32 = 8.0;
const Y_OFFSET: f32 = BORDER_SIZE * 2.0;
const X_OFFSET: f32 = BORDER_SIZE * 5.0;
const ACCIDENTAL_SIZE_MULTIPLIER: f32 = 5.0;
const KEY_SIGNATURE_OFFSET: f32 = 28.0;
const NUM_STAFF_LINES: MidiByte = 5;
const LINE_STROKE: Stroke = Stroke {
    width: 1.0,
    color: Color32::BLACK,
};

pub fn font_id(size: f32) -> FontId {
    FontId {
        size,
        family: FontFamily::Proportional,
    }
}

/// Musical symbols are a very tricky issue. Here are resources I've used:
/// * Font: [Bravura](https://github.com/steinbergmedia/bravura)
/// * [Unicode for a few symbols](https://www.compart.com/en/unicode/block/U+2600)
/// * [Unicode for the remaining symbols](https://unicode.org/charts/PDF/U1D100.pdf)
pub struct MelodyRenderer {
    scale: MusicMode,
    sig: KeySignature,
    x_range: RangeInclusive<f32>,
    y_range: RangeInclusive<f32>,
    y_per_pitch: f32,
    y_middle_c: f32,
    hi: MidiByte,
    melody_progress: Arc<AtomicCell<Option<f32>>>,
}

impl MelodyRenderer {
    fn staff_line_space(&self) -> f32 {
        self.y_per_pitch * 2.0
    }

    fn space_above_staff(&self) -> f32 {
        1.0 + self
            .scale
            .diatonic_steps_between_round_up(HIGHEST_STAFF_PITCH, self.hi) as f32
    }

    fn min_x(&self) -> f32 {
        *self.x_range.start()
    }

    fn total_note_x(&self) -> f32 {
        *self.x_range.end() - self.note_offset_x()
    }

    fn note_offset_x(&self) -> f32 {
        self.min_x() + X_OFFSET + KEY_SIGNATURE_OFFSET + self.y_per_pitch * self.sig.len() as f32
    }

    pub fn render(
        ui: &mut Ui,
        size: Vec2,
        melodies: &Vec<(Melody, Color32)>,
        show_sections: bool,
        show_figures: bool,
        melody_progress: Arc<AtomicCell<Option<f32>>>,
    ) {
        if melodies.len() > 0 {
            let (response, painter) = ui.allocate_painter(size, Sense::hover());
            let scale = melodies[0].0.best_scale_for();
            let (lo, hi) = Self::min_max_staff(&scale, melodies);
            let num_diatonic_pitches =
                1 + scale.diatonic_steps_between(lo, hi).pure_degree().unwrap();
            let y_per_pitch = ((response.rect.max.y - response.rect.min.y) - BORDER_SIZE * 2.0)
                / num_diatonic_pitches as f32;
            let y_border = Y_OFFSET + response.rect.min.y;
            let renderer = MelodyRenderer {
                hi,
                scale,
                y_per_pitch,
                x_range: response.rect.min.x + BORDER_SIZE..=response.rect.max.x - BORDER_SIZE,
                y_range: response.rect.min.y + BORDER_SIZE..=response.rect.max.y - BORDER_SIZE,
                sig: scale.key_signature(),
                y_middle_c: y_border
                    + y_per_pitch * scale.diatonic_steps_between_round_up(MIDDLE_C, hi) as f32,
                melody_progress,
            };
            renderer.draw_progress(&painter);
            let y_treble = y_border + y_per_pitch * renderer.space_above_staff();
            renderer.draw_staff(&painter, Clef::Treble, y_treble);
            let y_bass = renderer.y_middle_c + renderer.staff_line_space();
            renderer.draw_staff(&painter, Clef::Bass, y_bass);
            for (i, (melody, color)) in melodies.iter().enumerate().rev() {
                renderer.draw_melody(
                    &painter,
                    melody,
                    show_sections,
                    show_figures && i == 0,
                    *color,
                );
            }
        }
    }

    fn draw_progress(&self, painter: &Painter) {
        if let Some(progress) = self.melody_progress.load() {
            let x = self.note_offset_x() + self.total_note_x() * progress;
            let y1 = *self.y_range.start();
            let y2 = *self.y_range.end();
            painter.line_segment(
                [Pos2 { x, y: y1 }, Pos2 { x, y: y2 }],
                Stroke {
                    width: 5.0,
                    color: Color32::GREEN,
                },
            );
        }
    }

    fn draw_melody(
        &self,
        painter: &Painter,
        melody: &Melody,
        show_sections: bool,
        show_figures: bool,
        color: Color32,
    ) {
        let mut note_renderer =
            IncrementalNoteRenderer::new(self, painter, melody, show_sections, show_figures, color);
        for (i, note) in melody.iter().enumerate() {
            let x = self.note_offset_x()
                + self.total_note_x() * note_renderer.total_duration / melody.duration() as f32;
            note_renderer.note_update(note, &self.scale);
            let y = self.y_middle_c - note_renderer.staff_offset as f32 * self.y_per_pitch;
            if !note.is_rest() {
                note_renderer.show_note(i, x, y);
            }
            if note_renderer.can_show_figures() {
                note_renderer.show_figures(i, x, y);
            }
        }
    }

    fn draw_staff(&self, painter: &Painter, clef: Clef, start_y: f32) {
        let mut y = start_y;
        clef.render(painter, self.min_x(), y, self.y_per_pitch);
        for _ in 0..NUM_STAFF_LINES {
            painter.hline(self.x_range.clone(), y, LINE_STROKE);
            y += self.staff_line_space();
        }
        for (i, position) in clef.key_signature_positions(&self.sig).iter().enumerate() {
            let x = self.min_x() + KEY_SIGNATURE_OFFSET + self.y_per_pitch * i as f32;
            let y = self.y_middle_c - *position as f32 * self.y_per_pitch;
            self.draw_accidental(painter, self.sig.symbol(), x, y, Color32::BLACK);
        }
    }

    fn draw_accidental(
        &self,
        painter: &Painter,
        text: Accidental,
        x: f32,
        y: f32,
        text_color: Color32,
    ) {
        painter.text(
            Pos2 { x, y },
            Align2::CENTER_CENTER,
            text.symbol(),
            font_id(ACCIDENTAL_SIZE_MULTIPLIER * self.y_per_pitch),
            text_color,
        );
    }

    fn draw_extra_dashes(&self, painter: &Painter, x: f32, staff_offset: MidiByte) {
        let staff_extra_threshold = (NUM_STAFF_LINES + 1) * 2;
        if staff_offset == 0 {
            self.draw_extra_dash(painter, x, staff_offset);
        } else if staff_offset >= staff_extra_threshold {
            for offset in staff_extra_threshold..=staff_offset {
                self.draw_extra_dash(painter, x, offset);
            }
        } else if staff_offset <= -staff_extra_threshold {
            for offset in staff_offset..=-staff_extra_threshold {
                self.draw_extra_dash(painter, x, offset);
            }
        }
    }

    fn draw_extra_dash(&self, painter: &Painter, x: f32, staff_offset: MidiByte) {
        let x_offset = self.y_per_pitch * 1.5;
        let x1 = x - x_offset;
        let x2 = x + x_offset;
        let y = self.y_middle_c - staff_offset as f32 * self.y_per_pitch;
        painter.line_segment([Pos2 { x: x1, y }, Pos2 { x: x2, y }], LINE_STROKE);
    }

    fn min_max_staff(scale: &MusicMode, melodies: &Vec<(Melody, Color32)>) -> (MidiByte, MidiByte) {
        let mut lo = LOWEST_STAFF_PITCH;
        let mut hi = HIGHEST_STAFF_PITCH;
        for (melody, _) in melodies.iter() {
            let (mlo, mhi) = melody.min_max_pitches();
            lo = min(lo, mlo);
            hi = max(hi, mhi);
        }
        (scale.closest_pitch_below(lo), scale.closest_pitch_above(hi))
    }
}

static FIGURE_COLORS: [Color32; 8] = [
    Color32::DARK_GREEN,
    Color32::DARK_RED,
    Color32::DARK_BLUE,
    Color32::GOLD,
    Color32::GREEN,
    Color32::RED,
    Color32::BLUE,
    Color32::BROWN,
];

struct ColorMaker {
    next: ModNum<usize>,
}

impl ColorMaker {
    fn new() -> Self {
        Self {
            next: ModNum::new(0, FIGURE_COLORS.len()),
        }
    }

    fn next(&mut self) -> Color32 {
        let result = FIGURE_COLORS[self.next.a()];
        self.next += 1;
        result
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
struct PendingFigureBox {
    color: Color32,
    x: f32,
    y_min: f32,
    y_max: f32,
    last_note: usize,
}

impl PendingFigureBox {
    fn new(color: Color32, x: f32, y: f32, last_note: usize) -> Self {
        Self {
            color,
            x,
            y_min: y,
            y_max: y,
            last_note,
        }
    }

    fn active(&self, i: usize) -> bool {
        i <= self.last_note
    }

    fn update_y(&mut self, y: f32) {
        if y < self.y_min {
            self.y_min = y;
        }
        if y > self.y_max {
            self.y_max = y;
        }
    }
}

struct IncrementalNoteRenderer<'a> {
    renderer: &'a MelodyRenderer,
    melody: &'a Melody,
    painter: &'a Painter,
    total_duration: f32,
    figure_boundaries: VecDeque<(usize, usize, MelodicFigure)>,
    figure_colors: HashMap<MelodicFigure, Color32>,
    show_sections: bool,
    show_figures: bool,
    staff_offset: i16,
    note_color: Color32,
    auxiliary_symbol: Option<Accidental>,
    pending_figures: Vec<PendingFigureBox>,
}

impl<'a> IncrementalNoteRenderer<'a> {
    fn new(
        renderer: &'a MelodyRenderer,
        painter: &'a Painter,
        melody: &'a Melody,
        show_sections: bool,
        show_figures: bool,
        note_color: Color32,
    ) -> Self {
        let figure_boundaries = melody.figure_boundaries();
        let mut figure_colors = HashMap::new();
        let mut colors = ColorMaker::new();
        for (_, _, f) in figure_boundaries.iter() {
            if !figure_colors.contains_key(f) {
                figure_colors.insert(*f, colors.next());
            }
        }
        Self {
            renderer,
            total_duration: 0.0,
            melody,
            painter,
            show_figures,
            show_sections,
            figure_boundaries,
            figure_colors,
            auxiliary_symbol: None,
            staff_offset: 0,
            note_color,
            pending_figures: vec![],
        }
    }

    fn note_update(&mut self, note: &Note, scale: &MusicMode) {
        self.total_duration += note.duration() as f32;
        let (staff_offset, auxiliary_symbol) = scale.staff_position(note.pitch());
        self.staff_offset = staff_offset;
        self.auxiliary_symbol = auxiliary_symbol;
    }

    fn show_note(&self, i: usize, x: f32, y: f32) {
        if self.show_sections {
            self.show_sections(i, x, y);
        } else {
            self.painter
                .circle_filled(Pos2 { x, y }, self.renderer.y_per_pitch, self.note_color);
        }
        if let Some(auxiliary_symbol) = self.auxiliary_symbol {
            let x = x + self.renderer.staff_line_space();
            self.renderer
                .draw_accidental(self.painter, auxiliary_symbol, x, y, self.note_color);
        }
        self.renderer
            .draw_extra_dashes(self.painter, x, self.staff_offset);
    }

    fn show_sections(&self, i: usize, x: f32, y: f32) {
        match self.melody.section_number_for(i) {
            None => self.painter.circle_filled(
                Pos2 { x, y },
                self.renderer.y_per_pitch,
                self.note_color,
            ),
            Some(s) => {
                self.painter.text(
                    Pos2 { x, y },
                    Align2::CENTER_CENTER,
                    format!("{s}"),
                    font_id(ACCIDENTAL_SIZE_MULTIPLIER * self.renderer.y_per_pitch),
                    self.note_color,
                );
            }
        };
    }

    fn can_show_figures(&self) -> bool {
        self.show_figures && self.figure_boundaries.len() > 0
    }

    fn show_figures(&mut self, i: usize, x: f32, y: f32) {
        self.set_up_figures(i, x, y);
        self.update_active_figures(i, y);
        self.resolve_completed_figures(i, x);
    }

    fn set_up_figures(&mut self, i: usize, x: f32, y: f32) {
        for (start, end, figure) in self.figure_boundaries.iter() {
            if i == *start {
                self.pending_figures.push(PendingFigureBox::new(
                    self.figure_colors.get(figure).copied().unwrap(),
                    x,
                    y,
                    *end,
                ));
            }
        }
    }

    fn update_active_figures(&mut self, i: usize, y: f32) {
        for pending in self.pending_figures.iter_mut() {
            if pending.active(i) {
                pending.update_y(y);
            }
        }
    }

    fn resolve_completed_figures(&self, i: usize, x: f32) {
        for pending in self.pending_figures.iter() {
            if pending.last_note == i {
                let rect = Rect::from_min_max(
                    Pos2::new(
                        pending.x - self.renderer.y_per_pitch,
                        pending.y_min - self.renderer.y_per_pitch,
                    ),
                    Pos2::new(
                        x + self.renderer.y_per_pitch,
                        pending.y_max + self.renderer.y_per_pitch,
                    ),
                );
                self.painter
                    .rect_stroke(rect, 0.0, egui::Stroke::new(3.0, pending.color));
            }
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Clef {
    Treble,
    Bass,
}

impl Clef {
    pub fn symbol(&self) -> char {
        match self {
            Self::Treble => '\u{1d11e}',
            Self::Bass => '\u{1d122}',
        }
    }

    pub fn key_signature_positions(&self, sig: &KeySignature) -> Vec<MidiByte> {
        match self {
            Self::Treble => sig.treble_clef(),
            Self::Bass => sig.bass_clef(),
        }
    }

    fn size(&self) -> f32 {
        match self {
            Self::Treble => 13.5,
            Self::Bass => 8.0,
        }
    }

    fn x_offset(&self) -> f32 {
        10.0
    }

    fn y_offset(&self) -> f32 {
        match self {
            Self::Treble => 5.0,
            Self::Bass => -0.45,
        }
    }

    fn render(&self, painter: &Painter, x: f32, y: f32, y_per_pitch: f32) {
        painter.text(
            Pos2 {
                x: x + self.x_offset(),
                y: y + self.y_offset() * y_per_pitch,
            },
            Align2::CENTER_CENTER,
            self.symbol(),
            font_id(self.size() * y_per_pitch),
            Color32::BLACK,
        );
    }
}
