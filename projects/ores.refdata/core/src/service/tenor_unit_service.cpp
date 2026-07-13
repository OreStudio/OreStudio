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
#include "ores.refdata.core/service/tenor_unit_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

tenor_unit_service::tenor_unit_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenor_unit> tenor_unit_service::list_units(std::uint32_t offset,
                                                               std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenor units";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t tenor_unit_service::count_units() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total tenor units count";
    return repo_.get_total_unit_count(ctx_);
}

std::optional<domain::tenor_unit> tenor_unit_service::get_unit_at_version(const std::string& code,
                                                                          std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenor unit at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::tenor_unit> tenor_unit_service::get_unit(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenor unit: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void tenor_unit_service::save_unit(const domain::tenor_unit& v) {
    if (v.code.empty())
        throw std::invalid_argument("Tenor Unit code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving tenor unit: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved tenor unit: " << v.code;
}

void tenor_unit_service::save_units(const std::vector<domain::tenor_unit>& units) {
    for (const auto& e : units)
        if (e.code.empty())
            throw std::invalid_argument("Tenor Unit code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << units.size() << " tenor units";
    auto ts = units;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void tenor_unit_service::delete_unit(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor unit: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed tenor unit: " << code;
}

void tenor_unit_service::delete_units(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::tenor_unit> tenor_unit_service::get_unit_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for tenor unit: " << code;
    return repo_.read_all(ctx_, code);
}

}
