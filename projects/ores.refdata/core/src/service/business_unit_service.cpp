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
#include "ores.refdata.core/service/business_unit_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

business_unit_service::business_unit_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::business_unit> business_unit_service::list_business_units(std::uint32_t offset,
                                                                              std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all business units";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t business_unit_service::count_business_units() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total business units count";
    return repo_.get_total_business_unit_count(ctx_);
}


std::optional<domain::business_unit>
business_unit_service::get_business_unit_at_version(const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting business unit at version: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::business_unit>
business_unit_service::get_business_unit(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting business unit: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void business_unit_service::save_business_unit(const domain::business_unit& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Business Unit id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving business unit: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved business unit: " << v.id;
}

void business_unit_service::save_business_units(
    const std::vector<domain::business_unit>& business_units) {
    for (const auto& e : business_units)
        if (e.id.is_nil())
            throw std::invalid_argument("Business Unit id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << business_units.size() << " business units";
    auto ts = business_units;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void business_unit_service::delete_business_unit(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing business unit: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed business unit: " << id;
}

void business_unit_service::delete_business_units(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::business_unit>
business_unit_service::get_business_unit_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for business unit: " << id;
    return repo_.read_all(ctx_, id);
}

}
