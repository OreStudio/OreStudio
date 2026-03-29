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
#include "ores.trading.api/generators/composite_leg_generator.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::trading::generator {

using ores::utility::generation::generation_keys;

domain::composite_leg generate_synthetic_composite_leg(
    const boost::uuids::uuid& instrument_id,
    int leg_sequence,
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");

    domain::composite_leg r;
    r.version = 1;
    r.id = boost::uuids::random_generator()();
    r.instrument_id = instrument_id;
    r.leg_sequence = leg_sequence;
    r.constituent_trade_id = boost::uuids::to_string(
        boost::uuids::random_generator()());
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::composite_leg>
generate_synthetic_composite_legs(const boost::uuids::uuid& instrument_id,
    utility::generation::generation_context& ctx) {
    std::vector<domain::composite_leg> r;
    r.push_back(generate_synthetic_composite_leg(instrument_id, 1, ctx));
    r.push_back(generate_synthetic_composite_leg(instrument_id, 2, ctx));
    return r;
}

}
