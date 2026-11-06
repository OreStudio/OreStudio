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
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <exception>

namespace ores::synthetic::service {

namespace {

auto& population_lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.synthetic.service.trade_population_service");
    return instance;
}

/// How many trades pass between two progress lines, so a long population shows it is moving.
constexpr std::uint32_t progress_every = 25;

double seconds_since(std::chrono::steady_clock::time_point start) {
    return std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
}

}

trade_population_service::trade_population_service(trade_population_writer& writer)
    : writer_(writer) {}

trade_population_run trade_population_service::run(const boost::uuids::uuid& config_id) {
    using namespace ores::logging;
    const auto started = std::chrono::steady_clock::now();
    const auto config = boost::uuids::to_string(config_id);
    BOOST_LOG_SEV(population_lg(), info)
        << "Trade population of config " << config << ": resolving its scope";
    const auto scope = writer_.resolve_scope(config_id);
    auto plan = planner_for(scope.asset_class_code).plan(scope);
    group_into_structures(plan, scope.structures_per_book);
    BOOST_LOG_SEV(population_lg(), info)
        << "Trade population of config " << config << " (" << scope.asset_class_code
        << "): " << plan.slots.size() << " trade(s) in " << plan.structures.size()
        << " structure(s), planned in " << seconds_since(started) << "s";

    // The structures come first, because a leg cannot name a deal that is not
    // there yet, and a run that grouped them before leaves them alone.
    for (const auto& structure : plan.structures)
        if (!writer_.structure_exists(structure.structure_id))
            writer_.write_structure(structure, scope);

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

        try {
            writer_.write_trade(slot, scope);
            if (slot.structure_id)
                writer_.write_structure_member(*slot.structure_id, slot);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(population_lg(), error)
                << "Trade population of config " << config << " stopped at trade "
                << slot.internal_identifier << " (" << slot.trade_type_code << ", "
                << run.written + run.already_present + 1 << " of " << run.planned << ") after "
                << seconds_since(started) << "s: " << e.what();
            throw;
        }

        ++run.written;
        if ((run.written + run.already_present) % progress_every == 0)
            BOOST_LOG_SEV(population_lg(), info)
                << "Trade population of config " << config << ": "
                << run.written + run.already_present << " of " << run.planned << " done ("
                << run.written << " written, " << run.already_present << " present) after "
                << seconds_since(started) << "s";
        run.outcomes.push_back(
            trade_population_outcome{.trade_id = slot.trade_id,
                                     .internal_identifier = slot.internal_identifier,
                                     .book_id = slot.book_id,
                                     .counterparty_id = slot.counterparty_id,
                                     .trade_type_code = slot.trade_type_code,
                                     .already_present = false});
    }

    BOOST_LOG_SEV(population_lg(), info)
        << "Trade population of config " << config << " finished: " << run.written << " written, "
        << run.already_present << " already present, of " << run.planned << ", in "
        << seconds_since(started) << "s";
    return run;
}

}
