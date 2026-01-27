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
#include "ores.dq/service/dataset_bundle_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>

namespace ores::dq::service {

using namespace ores::logging;

dataset_bundle_service::dataset_bundle_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::dataset_bundle> dataset_bundle_service::list_bundles() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all dataset bundles";
    return repo_.read_latest();
}

std::optional<domain::dataset_bundle>
dataset_bundle_service::find_bundle(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding dataset bundle: " << id;
    auto results = repo_.read_latest(id);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::optional<domain::dataset_bundle>
dataset_bundle_service::find_bundle_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding dataset bundle by code: " << code;
    auto results = repo_.read_latest_by_code(code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void dataset_bundle_service::save_bundle(const domain::dataset_bundle& bundle) {
    if (bundle.id.is_nil()) {
        throw std::invalid_argument("Dataset Bundle ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving dataset bundle: " << bundle.id;
    repo_.write(bundle);
    BOOST_LOG_SEV(lg(), info) << "Saved dataset bundle: " << bundle.id;
}

void dataset_bundle_service::remove_bundle(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing dataset bundle: " << id;
    repo_.remove(id);
    BOOST_LOG_SEV(lg(), info) << "Removed dataset bundle: " << id;
}

std::vector<domain::dataset_bundle>
dataset_bundle_service::get_bundle_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for dataset bundle: " << id;
    return repo_.read_all(id);
}

}
