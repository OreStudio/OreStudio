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
#include "ores.dq.core/service/dataset_bundle_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

dataset_bundle_service::dataset_bundle_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::dataset_bundle> dataset_bundle_service::list_bundles(std::uint32_t offset,
                                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all dataset bundles";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t dataset_bundle_service::count_bundles() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total dataset bundles count";
    return repo_.get_total_bundle_count(ctx_);
}


std::optional<domain::dataset_bundle>
dataset_bundle_service::get_bundle_at_version(const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting dataset bundle at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::dataset_bundle> dataset_bundle_service::get_bundle(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting dataset bundle. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void dataset_bundle_service::save_bundle(const domain::dataset_bundle& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Dataset Bundle id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving dataset bundle. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved dataset bundle. " << "id: " << v.id;
}

void dataset_bundle_service::save_bundles(const std::vector<domain::dataset_bundle>& bundles) {
    for (const auto& e : bundles) {
        if (e.id.is_nil())
            throw std::invalid_argument("Dataset Bundle id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << bundles.size() << " dataset bundles";
    auto ts = bundles;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void dataset_bundle_service::delete_bundle(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing dataset bundle. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed dataset bundle. " << "id: " << id;
}

void dataset_bundle_service::delete_bundles(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::dataset_bundle>
dataset_bundle_service::get_bundle_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for dataset bundle. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
