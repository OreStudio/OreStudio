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

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

refdata::domain::tenor_basis_two_swap_convention
conventions_mapper::map_tenor_basis_two_swap(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping two-tenor basis swap convention: " << text_of(v, "Id");

    refdata::domain::tenor_basis_two_swap_convention r;
    r.id = text_of(v, "Id");
    r.calendar = text_of(v, "Calendar");
    r.long_fixed_frequency = frequency_of(v, "LongFixedFrequency");
    r.long_fixed_convention = bdc_of(v, "LongFixedConvention");
    r.long_fixed_day_count_fraction = day_counter_of(v, "LongFixedDayCounter");
    r.long_index = text_of(v, "LongIndex");
    r.short_fixed_frequency = frequency_of(v, "ShortFixedFrequency");
    r.short_fixed_convention = bdc_of(v, "ShortFixedConvention");
    r.short_fixed_day_count_fraction = day_counter_of(v, "ShortFixedDayCounter");
    r.short_index = text_of(v, "ShortIndex");
    assign(r.long_minus_short, opt_flag(v, "LongMinusShort"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_tenor_basis_two_swap(
    const refdata::domain::tenor_basis_two_swap_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Calendar", v.calendar);
    put_enum(w, "LongFixedFrequency", v.long_fixed_frequency, "frequency type");
    put_bdc(w, "LongFixedConvention", v.long_fixed_convention);
    put_day_counter(w, "LongFixedDayCounter", v.long_fixed_day_count_fraction);
    w.set_text("LongIndex", v.long_index);
    put_enum(w, "ShortFixedFrequency", v.short_fixed_frequency, "frequency type");
    put_bdc(w, "ShortFixedConvention", v.short_fixed_convention);
    put_day_counter(w, "ShortFixedDayCounter", v.short_fixed_day_count_fraction);
    w.set_text("ShortIndex", v.short_index);
    w.set_optional_flag("LongMinusShort", v.long_minus_short);
}

namespace conventions_family {

void pack_tenor_basis_two_swap(const record_reader& root,
                               refdata::messaging::conventions_bundle& bundle) {
    pack_family(bundle,
                "tenor_basis_two_swap",
                root.children("TenorBasisTwoSwap"),
                &conventions_mapper::map_tenor_basis_two_swap);
}

void join_tenor_basis_two_swap(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_tenor_basis_two_swap(
        unpack_row<refdata::domain::tenor_basis_two_swap_convention>(row),
        root.append("TenorBasisTwoSwap"));
}

}

}
