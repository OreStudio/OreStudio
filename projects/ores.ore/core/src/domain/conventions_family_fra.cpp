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

refdata::domain::fra_convention conventions_mapper::map_fra(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FRA convention: " << text_of(v, "Id");

    refdata::domain::fra_convention r;
    r.id = text_of(v, "Id");
    r.index = text_of(v, "Index");
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_fra(const refdata::domain::fra_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Index", v.index);
}

namespace conventions_family {

void pack_fra(const record_reader& root, refdata::service::conventions_bundle& bundle) {
    pack_family(bundle, "fra", root.children("FRA"), &conventions_mapper::map_fra);
}

void join_fra(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_fra(unpack_row<refdata::domain::fra_convention>(row),
                                    root.append("FRA"));
}

}

}
