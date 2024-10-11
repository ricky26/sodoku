use bitvec::order::Lsb0;
use bitvec::{bitarr, BitArr};
use clap::Parser;
use nom::combinator::fail;
use nom::error::context;
use nom::{Finish, IResult};
use smallvec::SmallVec;
use std::fmt::{Debug, Display, Formatter};
use std::io::{stdin, Read};
use std::ops::{Deref, DerefMut};
use tracing::{error, trace};

#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Validity {
    Complete,
    Incomplete,
    Invalid,
}

impl Validity {
    pub fn and(self, other: Validity) -> Validity {
        self.max(other)
    }

    pub fn and_then(self, f: impl FnOnce() -> Validity) -> Validity {
        if self == Validity::Invalid {
            Validity::Invalid
        } else {
            self.max(f())
        }
    }
}

type PossibilitySetStorage = BitArr!(for 9, in u16, Lsb0);

#[derive(Clone, Copy, Default)]
pub struct PossibilitySet(PossibilitySetStorage);

impl PossibilitySet {
    pub const ALL_POSSIBILITIES: PossibilitySet = PossibilitySet(bitarr![const u16, Lsb0; 1, 1, 1, 1, 1, 1, 1, 1, 1]);
}

impl Deref for PossibilitySet {
    type Target = PossibilitySetStorage;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PossibilitySet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Debug for PossibilitySet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;

        let mut it = self.0.iter_ones();
        if let Some(first) = it.next() {
            write!(f, "{}", first + 1)?;

            for v in it {
                write!(f, ", {}", v + 1)?;
            }
        }

        write!(f, "]")
    }
}

#[derive(Clone, Copy)]
pub enum GridCell {
    Solved(u8),
    Unsolved(PossibilitySet),
}

impl GridCell {
    pub fn as_display_char(&self) -> char {
        match self {
            GridCell::Solved(v) => (b'0' + *v) as char,
            GridCell::Unsolved(_) => ' ',
        }
    }

    pub fn validate<'a>(iter: impl Iterator<Item=&'a GridCell>) -> (Validity, PossibilitySet) {
        let mut remaining = PossibilitySet::ALL_POSSIBILITIES;
        let mut valid = true;

        for cell in iter {
            if let GridCell::Solved(value) = cell {
                if !remaining.replace(*value as usize - 1, false) {
                    valid = false;
                }
            }
        }

        if !valid {
            return (Validity::Invalid, remaining);
        }

        if remaining.first_one().is_some() {
            return (Validity::Incomplete, remaining);
        }

        (Validity::Complete, remaining)
    }
}

impl Default for GridCell {
    fn default() -> Self {
        GridCell::Unsolved(PossibilitySet::ALL_POSSIBILITIES)
    }
}

impl Debug for GridCell {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GridCell::Solved(value) => write!(f, "{value}"),
            GridCell::Unsolved(value) => write!(f, "{value:?}"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SubGrid {
    pub cells: [GridCell; 9],
}

impl SubGrid {
    pub const fn example() -> SubGrid {
        SubGrid {
            cells: [
                GridCell::Solved(1),
                GridCell::Solved(2),
                GridCell::Solved(3),
                GridCell::Solved(4),
                GridCell::Solved(5),
                GridCell::Solved(6),
                GridCell::Solved(7),
                GridCell::Solved(8),
                GridCell::Solved(9),
            ],
        }
    }

    fn cell_index(x: usize, y: usize) -> Option<usize> {
        if x >= 3 || y >= 3 {
            return None;
        }

        Some(x + y * 3)
    }

    pub fn cell(&self, x: usize, y: usize) -> Option<&GridCell> {
        Self::cell_index(x, y).map(|i| &self.cells[i])
    }

    pub fn cell_mut(&mut self, x: usize, y: usize) -> Option<&mut GridCell> {
        Self::cell_index(x, y).map(|i| &mut self.cells[i])
    }

    pub fn prune(&mut self, sub_grid: &PossibilitySet, rows: &[PossibilitySet], cols: &[PossibilitySet]) -> bool {
        let mut ok = true;
        for (i, cell) in self.cells.iter_mut().enumerate() {
            let row = i / 3;
            let col = i - (row * 3);
            if let GridCell::Unsolved(ref mut set) = cell {
                *set.0 &= cols[col].0 & rows[row].0 & sub_grid.0;
                if set.0.first_one().is_none() {
                    ok = false;
                }
            }
        }
        ok
    }

    pub fn validate(&self) -> (Validity, PossibilitySet) {
        GridCell::validate(self.cells.iter())
    }
}

impl Default for SubGrid {
    fn default() -> Self {
        Self {
            cells: [GridCell::default(); 9],
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Grid {
    pub sub_grids: [SubGrid; 9],
}

impl Grid {
    pub const fn example() -> Grid {
        Grid {
            sub_grids: [
                SubGrid::example(),
                SubGrid::example(),
                SubGrid::example(),
                SubGrid::example(),
                SubGrid::example(),
                SubGrid::example(),
                SubGrid::example(),
                SubGrid::example(),
                SubGrid::example(),
            ],
        }
    }

    pub fn copy_from(&mut self, grid: &Grid) {
        self.sub_grids.copy_from_slice(&grid.sub_grids);
    }

    fn sub_grid_index_at(x: usize, y: usize) -> Option<(usize, usize, usize)> {
        if x >= 9 || y >= 9 {
            return None;
        }

        let sub_grid_x = x / 3;
        let sub_grid_y = y / 3;
        let sub_grid_index = sub_grid_x + sub_grid_y * 3;
        Some((sub_grid_index, x - sub_grid_x * 3, y - sub_grid_y * 3))
    }

    pub fn cells(&self) -> impl Iterator<Item=&GridCell> {
        self.sub_grids.iter().flat_map(|g| g.cells.iter())
    }

    pub fn cells_mut(&mut self) -> impl Iterator<Item=&mut GridCell> {
        self.sub_grids.iter_mut().flat_map(|g| g.cells.iter_mut())
    }

    pub fn cell(&self, x: usize, y: usize) -> Option<&GridCell> {
        Self::sub_grid_index_at(x, y).and_then(|(i, x, y)| self.sub_grids[i].cell(x, y))
    }

    pub fn cell_mut(&mut self, x: usize, y: usize) -> Option<&mut GridCell> {
        Self::sub_grid_index_at(x, y).and_then(|(i, x, y)| self.sub_grids[i].cell_mut(x, y))
    }

    pub fn row(&self, row: usize) -> impl Iterator<Item=&GridCell> {
        (0..9).map(move |col| self.cell(col, row).unwrap())
    }

    pub fn column(&self, col: usize) -> impl Iterator<Item=&GridCell> {
        (0..9).map(move |row| self.cell(col, row).unwrap())
    }

    pub fn solve(&mut self) -> Validity {
        type Stack = Vec<(Grid, usize, usize, PossibilitySet)>;
        let mut stack: Stack = Vec::new();

        let pop_stack = |stack: &mut Stack, grid: &mut Grid| {
            if let Some((saved_grid, x, y, mut set)) = stack.pop() {
                let v = set.first_one().unwrap() + 1;
                set.set(v - 1, false);
                grid.copy_from(&saved_grid);
                *grid.cell_mut(x, y).unwrap() = GridCell::Solved(v as u8);
                trace!("[{}] trying {x},{y}={v} ({})\n{grid}", stack.len(), set.count_ones() + 1);

                if !set.first_one().is_none() {
                    stack.push((saved_grid, x, y, set));
                }

                true
            } else {
                false
            }
        };

        loop {
            let (validity, grid_possibilities) = self.check_prune();
            match validity {
                Validity::Complete => {
                    return Validity::Complete;
                }
                Validity::Incomplete => {}
                Validity::Invalid => {
                    trace!("invalid!\n{self:#?}\n{self}");

                    if pop_stack(&mut stack, self) {
                        continue;
                    }

                    return Validity::Invalid;
                }
            }

            let mut did_solve = false;

            for (grid, mut mask) in self.sub_grids.iter_mut().zip(grid_possibilities) {
                for cell in &mut grid.cells {
                    if let GridCell::Unsolved(set) = cell {
                        let masked = set.0 & mask.0;
                        if masked.count_ones() == 1 {
                            did_solve = true;
                            let v = masked.first_one().unwrap();
                            *cell = GridCell::Solved((v + 1) as u8);
                            mask.set(v, false);
                        }
                    }
                }
            }

            if !did_solve {
                let mut next = None;
                let mut max_possibilities = 10;

                for y in 0..9 {
                    for x in 0..9 {
                        let cell = self.cell(x, y).unwrap();
                        if let GridCell::Unsolved(set) = cell {
                            assert!(set.first_one().is_some());
                            let ones = set.count_ones();
                            if ones < max_possibilities {
                                max_possibilities = ones;
                                next = Some((x, y, *set));
                            }
                        }
                    }
                }

                let Some((x, y, set)) = next else { unreachable!() };
                trace!("next {x},{y} = {set:?}");
                stack.push((self.clone(), x, y, set));

                if pop_stack(&mut stack, self) {
                    continue;
                }

                return Validity::Incomplete;
            }

            trace!("solved some\n{self:#?}\n{self}");
        }
    }

    fn prune_phase(iter: impl Iterator<Item=(Validity, PossibilitySet)>) -> (Validity, SmallVec<[PossibilitySet; 9]>) {
        iter.fold((Validity::Complete, SmallVec::new()), |(av, mut sets), (v, new_set)| {
            sets.push(new_set);
            (av.and(v), sets)
        })
    }

    fn check_prune(&mut self) -> (Validity, SmallVec<[PossibilitySet; 9]>) {
        let (vg, grids) = Self::prune_phase(self.sub_grids.iter().map(SubGrid::validate));
        let (vc, cols) = Self::prune_phase((0..9).map(|col| GridCell::validate(self.column(col))));
        let (vr, rows) = Self::prune_phase((0..9).map(|row| GridCell::validate(self.row(row))));

        let mut validity = vg.and(vc).and(vr);
        for (index, grid) in self.sub_grids.iter_mut().enumerate() {
            let y = index / 3;
            let x = index - y * 3;
            let row_start = y * 3;
            let col_start = x * 3;
            let row_range = row_start..(row_start + 3);
            let col_range = col_start..(col_start + 3);
            let grid_rows = &rows[row_range];
            let grid_cols = &cols[col_range];
            if !grid.prune(&grids[index], grid_rows, grid_cols) {
                validity = Validity::Invalid;
            }
        }

        (validity, grids)
    }

    pub fn check(&mut self) -> Validity {
        self.check_prune().0
    }

    pub fn parse(src: &str) -> IResult<&str, Grid> {
        let mut chars = src.chars();
        let mut grid = Grid::default();

        let mut x = 0;
        let mut y = 0;

        loop {
            let Some(c) = chars.next() else { break };
            match c {
                '1'..='9' => {
                    if let Some(cell) = grid.cell_mut(x, y) {
                        *cell = GridCell::Solved(c as u8 - b'0');
                    } else {
                        return context("out of bounds", fail)(chars.as_str());
                    }
                    x += 1;
                }
                '_' | '.' | '0' => {
                    if let Some(cell) = grid.cell_mut(x, y) {
                        *cell = GridCell::default();
                    } else {
                        return context("out of bounds", fail)(chars.as_str());
                    }
                    x += 1;
                }
                '\n' => {
                    x = 0;
                    y += 1;
                }
                _ => {
                    // Skip unknown characters
                }
            }
        }

        Ok((chars.as_str(), grid))
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "╔═══════╤═══════╤═══════╗\n")?;

        let mut first = true;
        for chunk in self.sub_grids.chunks_exact(3) {
            if !first {
                write!(f, "╟───────┼───────┼───────╢\n")?;
            } else {
                first = false;
            }

            let [a, b, c] = chunk else { unreachable!() };
            for row in 0..3 {
                let offset = row * 3;
                let v0 = a.cells[offset].as_display_char();
                let v1 = a.cells[offset + 1].as_display_char();
                let v2 = a.cells[offset + 2].as_display_char();
                let v3 = b.cells[offset].as_display_char();
                let v4 = b.cells[offset + 1].as_display_char();
                let v5 = b.cells[offset + 2].as_display_char();
                let v6 = c.cells[offset].as_display_char();
                let v7 = c.cells[offset + 1].as_display_char();
                let v8 = c.cells[offset + 2].as_display_char();
                write!(f, "║ {v0} {v1} {v2} │ {v3} {v4} {v5} │ {v6} {v7} {v8} ║\n")?;
            }
        }

        write!(f, "╚═══════╧═══════╧═══════╝\n")
    }
}

#[derive(Parser)]
struct Options {
    #[clap(short, long)]
    pub verbose: bool,

    #[clap(short, long)]
    pub debug: bool,
}

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let opts = Options::parse();

    let mut input = String::new();
    stdin().read_to_string(&mut input)?;

    let (_, mut grid) = Grid::parse(&input)
        .map_err(|e| e.to_owned())
        .finish()?;

    if opts.verbose {
        println!("input:\n{grid}");
    }

    if grid.solve() != Validity::Complete {
        error!("failed to solve grid");
    }

    if opts.debug {
        println!("{:#?}", grid);
    }

    println!("output:\n{}", grid);
    Ok(())
}
