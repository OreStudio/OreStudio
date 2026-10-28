/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "conventions_family_pack.hpp"
#include "conventions_family_units.hpp"
#include "conventions_fields.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include <optional>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

namespace conventions_fields {

std::optional<double> opt_real(const record_reader& v, std::string_view name) {
    return v.real(name);
}

}

refdata::domain::bond_yield_convention conventions_mapper::map_bond_yield(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping bond yield convention: " << text_of(v, "Id");

    refdata::domain::bond_yield_convention r;
    r.id = text_of(v, "Id");
    r.compounding = text_of(v, "Compounding");
    assign(r.frequency, opt_frequency(v, "Frequency"));
    assign(r.price_type, opt_text(v, "PriceType"));
    assign(r.accuracy, opt_real(v, "Accuracy"));
    assign(r.max_evaluations, opt_integer(v, "MaxEvaluations"));
    assign(r.guess, opt_real(v, "Guess"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_bond_yield(const refdata::domain::bond_yield_convention& v,
                                            record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Compounding", v.compounding);
    put_enum(w, "Frequency", v.frequency, "frequency type");
    w.set_optional_text("PriceType", v.price_type);
    w.set_optional_real("Accuracy", v.accuracy);
    w.set_optional_integer("MaxEvaluations", as_integer(v.max_evaluations));
    w.set_optional_real("Guess", v.guess);
}

namespace conventions_family {

void pack_bond_yield(const record_reader& root, refdata::messaging::conventions_bundle& bundle) {
    pack_family(
        bundle, "bond_yield", root.children("BondYield"), &conventions_mapper::map_bond_yield);
}

void join_bond_yield(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_bond_yield(unpack_row<refdata::domain::bond_yield_convention>(row),
                                           root.append("BondYield"));
}

}

}
