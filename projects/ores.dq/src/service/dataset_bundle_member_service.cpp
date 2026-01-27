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
#include "ores.dq/service/dataset_bundle_member_service.hpp"

#include <stdexcept>

namespace ores::dq::service {

using namespace ores::logging;

dataset_bundle_member_service::dataset_bundle_member_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::dataset_bundle_member> dataset_bundle_member_service::list_members() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all dataset bundle members";
    return repo_.read_latest();
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_service::list_members_by_bundle(const std::string& bundle_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing dataset bundle members for bundle: " << bundle_code;
    return repo_.read_latest_by_bundle(bundle_code);
}

void dataset_bundle_member_service::save_member(const domain::dataset_bundle_member& member) {
    if (member.bundle_code.empty()) {
        throw std::invalid_argument("Bundle cannot be empty.");
    }
    if (member.dataset_code.empty()) {
        throw std::invalid_argument("Dataset cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving dataset bundle member: " << member.bundle_code
                               << "/" << member.dataset_code;
    repo_.write(member);
    BOOST_LOG_SEV(lg(), info) << "Saved dataset bundle member: " << member.bundle_code
                              << "/" << member.dataset_code;
}

void dataset_bundle_member_service::remove_member(const std::string& bundle_code,
    const std::string& dataset_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing dataset bundle member: " << bundle_code
                               << "/" << dataset_code;
    repo_.remove(bundle_code, dataset_code);
    BOOST_LOG_SEV(lg(), info) << "Removed dataset bundle member: " << bundle_code
                              << "/" << dataset_code;
}

}
