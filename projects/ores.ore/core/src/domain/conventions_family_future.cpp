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

refdata::domain::future_convention conventions_mapper::map_future(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping future convention: " << text_of(v, "Id");

    refdata::domain::future_convention r;
    r.id = text_of(v, "Id");
    r.index = text_of(v, "Index");
    assign(r.date_generation_rule, opt_spelling(v, "DateGenerationRule"));
    assign(r.netting_type, opt_spelling(v, "OvernightIndexFutureNettingType"));
    assign(r.calendar, opt_text(v, "Calendar"));
    assign(r.overnight_index_tenor, opt_text(v, "OvernightIndexTenor"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_future(const refdata::domain::future_convention& v,
                                        record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Index", v.index);
    put_enum(w, "DateGenerationRule", v.date_generation_rule, "future date generation rule");
    put_enum(w,
             "OvernightIndexFutureNettingType",
             v.netting_type,
             "overnight index future netting type");
    w.set_optional_text("Calendar", v.calendar);
    w.set_optional_text("OvernightIndexTenor", v.overnight_index_tenor);
}

namespace conventions_family {

void pack_future(const record_reader& root, refdata::service::conventions_bundle& bundle) {
    pack_family(bundle, "future", root.children("Future"), &conventions_mapper::map_future);
}

void join_future(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_future(unpack_row<refdata::domain::future_convention>(row),
                                       root.append("Future"));
}

}

}
