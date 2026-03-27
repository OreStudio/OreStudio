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
#include "ores.trading.api/generators/swap_leg_generator.hpp"

#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::trading::generator {

using ores::utility::generation::generation_keys;

domain::swap_leg generate_synthetic_swap_leg(
    const boost::uuids::uuid& instrument_id,
    int leg_number,
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");

    domain::swap_leg r;
    r.version = 1;
    r.id = boost::uuids::random_generator()();
    r.instrument_id = instrument_id;
    r.leg_number = leg_number;
    r.notional = 1000000.0;
    r.currency = "USD";
    r.day_count_fraction_code = "A365F";
    r.business_day_convention_code = "MODFOLLOWING";
    r.payment_frequency_code = "Annual";
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();

    if (leg_number == 1) {
        // Fixed leg
        r.leg_type_code = "Fixed";
        r.fixed_rate = 0.05;
    } else {
        // Floating leg
        r.leg_type_code = "Floating";
        r.floating_index_code = "EURIBOR6M";
        r.spread = 0.0;
    }
    return r;
}

std::vector<domain::swap_leg>
generate_synthetic_swap_legs(const boost::uuids::uuid& instrument_id,
    utility::generation::generation_context& ctx) {
    std::vector<domain::swap_leg> r;
    r.push_back(generate_synthetic_swap_leg(instrument_id, 1, ctx));
    r.push_back(generate_synthetic_swap_leg(instrument_id, 2, ctx));
    return r;
}

}
