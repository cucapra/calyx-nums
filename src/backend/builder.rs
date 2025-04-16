//! IR builder based on [`calyx_ir::Builder`].

use calyx_ir as ir;

pub struct IRBuilder<'a> {
    pub component: &'a mut ir::Component,
    pub lib: &'a mut ir::LibrarySignatures,
}

impl<'a> IRBuilder<'a> {
    pub fn new(
        component: &'a mut ir::Component,
        lib: &'a mut ir::LibrarySignatures,
    ) -> IRBuilder<'a> {
        IRBuilder { component, lib }
    }

    pub fn add_constant(
        &mut self,
        value: u64,
        width: u64,
    ) -> ir::RRC<ir::Cell> {
        let name = ir::Cell::constant_name(value, width);

        if let Some(cell) = self.component.cells.find(name) {
            return cell;
        }

        let prototype = ir::CellType::Constant { val: value, width };

        let ports = [ir::PortDef::new(
            ir::Id::new("out"),
            width,
            ir::Direction::Output,
            Default::default(),
        )];

        let cell = ir::rrc(ir::Cell::new(name, prototype));
        add_ports_to_cell(&cell, ports);

        self.component.cells.add(cell.clone());

        cell
    }

    pub fn add_comb_group<S>(
        &mut self,
        prefix: S,
        assignments: Vec<ir::Assignment<ir::Nothing>>,
    ) -> ir::RRC<ir::CombGroup>
    where
        S: Into<ir::Id>,
    {
        fn inner(
            builder: &mut IRBuilder,
            prefix: ir::Id,
            assignments: Vec<ir::Assignment<ir::Nothing>>,
        ) -> ir::RRC<ir::CombGroup> {
            let group = builder.ir_builder().add_comb_group(prefix);
            group.borrow_mut().assignments = assignments;

            group
        }

        inner(self, prefix.into(), assignments)
    }

    pub fn add_primitive<S, T>(
        &mut self,
        prefix: S,
        primitive: T,
        parameters: &[u64],
    ) -> ir::RRC<ir::Cell>
    where
        S: Into<ir::Id>,
        T: Into<ir::Id>,
    {
        fn inner(
            builder: &mut IRBuilder,
            prefix: ir::Id,
            primitive: ir::Id,
            parameters: &[u64],
        ) -> ir::RRC<ir::Cell> {
            let definition = builder.lib.get_primitive(primitive);
            let (parameters, ports) = definition.resolve(parameters).unwrap();

            let name = builder.component.generate_name(prefix);

            let prototype = ir::CellType::Primitive {
                name: primitive,
                param_binding: Box::new(parameters),
                is_comb: definition.is_comb,
                latency: definition.latency,
            };

            let cell = ir::rrc(ir::Cell::new(name, prototype));
            add_ports_to_cell(&cell, ports);

            builder.component.cells.add(cell.clone());

            cell
        }

        inner(self, prefix.into(), primitive.into(), parameters)
    }

    pub fn add_component<S, T>(
        &mut self,
        prefix: S,
        component: T,
        ports: Vec<ir::PortDef<u64>>,
    ) -> ir::RRC<ir::Cell>
    where
        S: Into<ir::Id>,
        T: Into<ir::Id>,
    {
        fn inner(
            builder: &mut IRBuilder,
            prefix: ir::Id,
            component: ir::Id,
            ports: Vec<ir::PortDef<u64>>,
        ) -> ir::RRC<ir::Cell> {
            let name = builder.component.generate_name(prefix);
            let prototype = ir::CellType::Component { name: component };

            let cell = ir::rrc(ir::Cell::new(name, prototype));
            add_ports_to_cell(&cell, ports);

            builder.component.cells.add(cell.clone());

            cell
        }

        inner(self, prefix.into(), component.into(), ports)
    }

    #[inline]
    pub fn add_continuous_assignment(
        &mut self,
        assignment: ir::Assignment<ir::Nothing>,
    ) {
        self.component.continuous_assignments.push(assignment);
    }

    #[inline]
    pub fn add_continuous_assignments<I>(&mut self, assignments: I)
    where
        I: IntoIterator<Item = ir::Assignment<ir::Nothing>>,
    {
        self.component.continuous_assignments.extend(assignments);
    }

    /// For compatibility with [`calyx_ir::build_assignments`].
    pub fn build_assignment<T>(
        &self,
        dst: ir::RRC<ir::Port>,
        src: ir::RRC<ir::Port>,
        guard: ir::Guard<T>,
    ) -> ir::Assignment<T> {
        ir::Assignment {
            dst,
            src,
            guard: Box::new(guard),
            attributes: Default::default(),
        }
    }

    pub fn invoke_with<S>(
        &mut self,
        component: ir::RRC<ir::Cell>,
        inputs: Vec<(ir::Id, ir::RRC<ir::Port>)>,
        prefix: S,
        assignments: Vec<ir::Assignment<ir::Nothing>>,
    ) -> ir::Control
    where
        S: Into<ir::Id>,
    {
        let comb_group = (!assignments.is_empty())
            .then(|| self.add_comb_group(prefix, assignments));

        ir::Control::Invoke(ir::Invoke {
            comp: component,
            inputs,
            outputs: Vec::new(),
            attributes: Default::default(),
            comb_group,
            ref_cells: Vec::new(),
        })
    }

    fn ir_builder(&mut self) -> ir::Builder {
        ir::Builder::new(self.component, self.lib).not_generated()
    }

    pub fn collapse<I, F>(statements: I, f: F) -> ir::Control
    where
        I: IntoIterator<Item = ir::Control>,
        F: FnOnce(Vec<ir::Control>) -> ir::Control,
    {
        let statements: Vec<_> = statements
            .into_iter()
            .filter(|control| !matches!(control, ir::Control::Empty(_)))
            .collect();

        match statements.len() {
            0 => ir::Control::empty(),
            1 => statements.into_iter().next().unwrap(),
            _ => f(statements),
        }
    }
}

fn add_ports_to_cell<I>(cell: &ir::RRC<ir::Cell>, ports: I)
where
    I: IntoIterator<Item = ir::PortDef<u64>>,
{
    let wrc = ir::WRC::from(cell);
    let cell_ports = &mut cell.borrow_mut().ports;

    for port in ports {
        let port = ir::rrc(ir::Port {
            name: port.name(),
            width: port.width,
            direction: port.direction,
            parent: ir::PortParent::Cell(wrc.clone()),
            attributes: port.attributes,
        });

        cell_ports.push(port);
    }
}
