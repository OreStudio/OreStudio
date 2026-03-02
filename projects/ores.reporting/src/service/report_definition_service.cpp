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
#include "ores.reporting/service/report_definition_service.hpp"

#include <stdexcept>

namespace ores::reporting::service {

using namespace ores::logging;

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

void report_definition_service::save_definition(const domain::report_definition& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Report Definition id cannot be empty.");
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
