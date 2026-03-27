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
#include "ores.reporting.core/service/report_definition_service.hpp"

#include <stdexcept>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::reporting::service {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_string_query;

namespace {

// Returns the UUID of the initial (draft) FSM state for the
// report_definition_lifecycle machine via the dedicated Postgres helper.
std::optional<boost::uuids::uuid>
find_draft_state_id(const ores::database::context& ctx, logging::logger_t& log) {
    const auto rows = execute_parameterized_string_query(
        ctx, "SELECT ores_reporting_initial_definition_state_fn()::text",
        {}, log, "Looking up report_definition_lifecycle initial state");
    if (rows.empty() || rows.front().empty()) {
        BOOST_LOG_SEV(log, warn) << "Initial report definition FSM state not found";
        return std::nullopt;
    }
    return boost::lexical_cast<boost::uuids::uuid>(rows.front());
}

} // namespace

report_definition_service::report_definition_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::report_definition> report_definition_service::list_definitions() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all report definitions";
    return repo_.read_latest(ctx_);
}

std::optional<domain::report_definition>
report_definition_service::find_definition(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding report definition: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void report_definition_service::save_definition(domain::report_definition v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Report Definition id cannot be empty.");
    // On first creation, assign the initial (draft) FSM state so the record
    // enters the lifecycle machine in the correct state.
    if (!v.fsm_state_id)
        v.fsm_state_id = find_draft_state_id(ctx_, lg());
    BOOST_LOG_SEV(lg(), debug) << "Saving report definition: " << v.id;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved report definition: " << v.id;
}

void report_definition_service::remove_definition(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report definition: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed report definition: " << id;
}

std::vector<domain::report_definition>
report_definition_service::get_definition_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for report definition: " << id;
    return repo_.read_all(ctx_, id);
}

}
