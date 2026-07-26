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
#include "ores.dq.core/service/subject_area_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

subject_area_service::subject_area_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::subject_area> subject_area_service::list_areas(std::uint32_t offset,
                                                                   std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all subject areas";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t subject_area_service::count_areas() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total subject areas count";
    return repo_.get_total_area_count(ctx_);
}

std::optional<domain::subject_area> subject_area_service::get_area_at_version(
    const std::string& name, const std::string& domain_name, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting subject area at version. " << "name: " << name
                               << " domain_name: " << domain_name << " version: " << version;
    return repo_.read_at_version(ctx_, name, domain_name, version);
}

std::optional<domain::subject_area> subject_area_service::get_area(const std::string& name,
                                                                   const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting subject area. " << "name: " << name
                               << " domain_name: " << domain_name;
    auto results = repo_.read_latest(ctx_, name, domain_name);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void subject_area_service::save_area(const domain::subject_area& v) {
    if (v.name.empty())
        throw std::invalid_argument("Subject Area name cannot be empty.");
    if (v.domain_name.empty())
        throw std::invalid_argument("Subject Area domain_name cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving subject area. " << "name: " << v.name
                               << " domain_name: " << v.domain_name;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved subject area. " << "name: " << v.name
                              << " domain_name: " << v.domain_name;
}

void subject_area_service::save_areas(const std::vector<domain::subject_area>& areas) {
    for (const auto& e : areas) {
        if (e.name.empty())
            throw std::invalid_argument("Subject Area name cannot be empty.");
        if (e.domain_name.empty())
            throw std::invalid_argument("Subject Area domain_name cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << areas.size() << " subject areas";
    auto ts = areas;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void subject_area_service::delete_area(const std::string& name, const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing subject area. " << "name: " << name
                               << " domain_name: " << domain_name;
    repo_.remove(ctx_, name, domain_name);
    BOOST_LOG_SEV(lg(), info) << "Removed subject area. " << "name: " << name
                              << " domain_name: " << domain_name;
}

void subject_area_service::delete_areas(const std::vector<std::string>& names,
                                        const std::vector<std::string>& domain_names) {
    repo_.remove(ctx_, names, domain_names);
}

std::vector<domain::subject_area>
subject_area_service::get_area_history(const std::string& name, const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for subject area. " << "name: " << name
                               << " domain_name: " << domain_name;
    return repo_.read_all(ctx_, name, domain_name);
}

}
