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
#include "ores.synthetic.core/service/trade_population_service.hpp"
#include "ores.synthetic.core/service/trade_population_planner.hpp"

namespace ores::synthetic::service {

trade_population_service::trade_population_service(trade_population_writer& writer)
    : writer_(writer) {}

trade_population_run trade_population_service::run(const boost::uuids::uuid& config_id) {
    const auto scope = writer_.resolve_scope(config_id);
    const auto plan = planner_for(scope.asset_class_code).plan(scope);

    trade_population_run run;
    run.config_id = scope.config_id;
    run.party_id = scope.party_id;
    run.planned = static_cast<std::uint32_t>(plan.slots.size());
    run.outcomes.reserve(plan.slots.size());

    for (const auto& slot : plan.slots) {
        const auto stored = writer_.read_trade(slot.trade_id);
        if (stored.has_value()) {
            ++run.already_present;
            run.outcomes.push_back(
                trade_population_outcome{.trade_id = slot.trade_id,
                                         .internal_identifier = slot.internal_identifier,
                                         .book_id = slot.book_id,
                                         .counterparty_id = slot.counterparty_id,
                                         .trade_type_code = slot.trade_type_code,
                                         .already_present = true,
                                         .version = stored->version,
                                         .external_version = stored->external_version});
            continue;
        }

        trade_population_write write;
        write.slot = slot;
        write.security_id = slot.security_id;
        write.issue_id = slot.issue_id;

        writer_.write_trade(write, scope);

        ++run.written;
        run.outcomes.push_back(
            trade_population_outcome{.trade_id = slot.trade_id,
                                     .internal_identifier = slot.internal_identifier,
                                     .book_id = slot.book_id,
                                     .counterparty_id = slot.counterparty_id,
                                     .trade_type_code = slot.trade_type_code,
                                     .already_present = false});
    }

    return run;
}

}
