/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
    : bundle_repo_(ctx), member_repo_(ctx) {}

// ============================================================================
// Dataset Bundle Management
// ============================================================================

std::vector<domain::dataset_bundle> dataset_bundle_service::list_bundles() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all dataset bundles";
    return bundle_repo_.read_latest();
}

std::optional<domain::dataset_bundle>
dataset_bundle_service::find_bundle(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding bundle: " << id;
    auto bundles = bundle_repo_.read_latest(id);
    if (bundles.empty()) {
        return std::nullopt;
    }
    return bundles.front();
}

std::optional<domain::dataset_bundle>
dataset_bundle_service::find_bundle_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding bundle by code: " << code;
    auto bundles = bundle_repo_.read_latest_by_code(code);
    if (bundles.empty()) {
        return std::nullopt;
    }
    return bundles.front();
}

void dataset_bundle_service::save_bundle(const domain::dataset_bundle& bundle) {
    if (bundle.id.is_nil()) {
        throw std::invalid_argument("Bundle ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving bundle: " << bundle.id;
    bundle_repo_.write(bundle);
    BOOST_LOG_SEV(lg(), info) << "Saved bundle: " << bundle.id;
}

void dataset_bundle_service::remove_bundle(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bundle: " << id;
    bundle_repo_.remove(id);
    BOOST_LOG_SEV(lg(), info) << "Removed bundle: " << id;
}

std::vector<domain::dataset_bundle>
dataset_bundle_service::get_bundle_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for bundle: " << id;
    return bundle_repo_.read_all(id);
}

// ============================================================================
// Dataset Bundle Member Management
// ============================================================================

std::vector<domain::dataset_bundle_member>
dataset_bundle_service::list_members() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all bundle members";
    return member_repo_.read_latest();
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_service::list_members_by_bundle(const std::string& bundle_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing members for bundle: " << bundle_code;
    return member_repo_.read_latest_by_bundle(bundle_code);
}

void dataset_bundle_service::save_member(
    const domain::dataset_bundle_member& member) {
    if (member.bundle_code.empty()) {
        throw std::invalid_argument("Bundle code cannot be empty.");
    }
    if (member.dataset_code.empty()) {
        throw std::invalid_argument("Dataset code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving bundle member: " << member.bundle_code
                               << "/" << member.dataset_code;
    member_repo_.write(member);
    BOOST_LOG_SEV(lg(), info) << "Saved bundle member: " << member.bundle_code
                              << "/" << member.dataset_code;
}

void dataset_bundle_service::remove_member(const std::string& bundle_code,
    const std::string& dataset_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bundle member: " << bundle_code
                               << "/" << dataset_code;
    member_repo_.remove(bundle_code, dataset_code);
    BOOST_LOG_SEV(lg(), info) << "Removed bundle member: " << bundle_code
                              << "/" << dataset_code;
}

}
