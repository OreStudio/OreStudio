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
#include "ores.dq.core/service/catalog_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

catalog_service::catalog_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::catalog> catalog_service::list_catalogs(std::uint32_t offset,
                                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all catalogs";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t catalog_service::count_catalogs() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total catalogs count";
    return repo_.get_total_catalog_count(ctx_);
}

std::optional<domain::catalog> catalog_service::get_catalog_at_version(const std::string& name,
                                                                       std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting catalog at version: " << name << " version: " << version;
    return repo_.read_at_version(ctx_, name, version);
}

std::optional<domain::catalog> catalog_service::get_catalog(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting catalog: " << name;
    auto results = repo_.read_latest(ctx_, name);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void catalog_service::save_catalog(const domain::catalog& v) {
    if (v.name.empty())
        throw std::invalid_argument("Catalog name cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving catalog: " << v.name;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved catalog: " << v.name;
}

void catalog_service::save_catalogs(const std::vector<domain::catalog>& catalogs) {
    for (const auto& e : catalogs)
        if (e.name.empty())
            throw std::invalid_argument("Catalog name cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << catalogs.size() << " catalogs";
    auto ts = catalogs;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void catalog_service::delete_catalog(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing catalog: " << name;
    repo_.remove(ctx_, name);
    BOOST_LOG_SEV(lg(), info) << "Removed catalog: " << name;
}

void catalog_service::delete_catalogs(const std::vector<std::string>& names) {
    repo_.remove(ctx_, names);
}

std::vector<domain::catalog> catalog_service::get_catalog_history(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for catalog: " << name;
    return repo_.read_all(ctx_, name);
}

}
