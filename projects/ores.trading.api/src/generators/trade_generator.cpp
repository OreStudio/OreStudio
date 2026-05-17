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
#include "ores.trading.api/generators/trade_generator.hpp"

#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <boost/uuid/nil_generator.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::trading::generator {

using ores::utility::generation::generation_keys;

domain::trade generate_synthetic_trade(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");

    domain::trade r;
    r.identity.get().version = 1;
    r.identity.get().id = ctx.generate_uuid();
    r.identity.get().external_id = std::string();
    r.parties.get().book_id = ctx.generate_uuid();
    r.parties.get().portfolio_id = ctx.generate_uuid();
    r.parties.get().successor_trade_id = std::nullopt;
    r.classification.get().trade_type = std::string("Swap");
    r.classification.get().netting_set_id = std::string("NS-001");
    r.classification.get().activity_type_code = std::string("new_booking");
    r.classification.get().status_id = boost::uuids::nil_uuid();
    r.lifecycle.get().trade_date = std::string("2025-01-15");
    r.lifecycle.get().execution_timestamp = std::string("2025-01-15 10:00:00");
    r.lifecycle.get().effective_date = std::string("2025-01-16");
    r.lifecycle.get().termination_date = std::string("2026-01-15");
    r.audit.get().modified_by = modified_by;
    r.audit.get().performed_by = modified_by;
    r.audit.get().change_reason_code = "system.test";
    r.audit.get().change_commentary = "Synthetic test data";
    r.audit.get().recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::trade>
generate_synthetic_trades(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::trade> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_trade(ctx));
    return r;
}

}
