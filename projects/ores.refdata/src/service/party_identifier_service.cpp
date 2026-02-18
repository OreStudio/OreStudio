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
#include "ores.refdata/service/party_identifier_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::service {

using namespace ores::logging;

party_identifier_service::party_identifier_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::party_identifier> party_identifier_service::list_party_identifiers() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all party identifiers";
    return repo_.read_latest();
}

std::vector<domain::party_identifier>
party_identifier_service::list_party_identifiers_by_party(
    const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Listing party identifiers for party: "
                               << party_id;
    return repo_.read_latest_by_party_id(party_id);
}

std::optional<domain::party_identifier>
party_identifier_service::find_party_identifier(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding party identifier: " << id;
    auto results = repo_.read_latest(id);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::optional<domain::party_identifier>
party_identifier_service::find_party_identifier_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding party identifier by code: " << code;
    auto results = repo_.read_latest_by_code(code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void party_identifier_service::save_party_identifier(const domain::party_identifier& party_identifier) {
    if (party_identifier.id.is_nil()) {
        throw std::invalid_argument("Party Identifier ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving party identifier: " << party_identifier.id;
    repo_.write(party_identifier);
    BOOST_LOG_SEV(lg(), info) << "Saved party identifier: " << party_identifier.id;
}

void party_identifier_service::remove_party_identifier(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party identifier: " << id;
    repo_.remove(id);
    BOOST_LOG_SEV(lg(), info) << "Removed party identifier: " << id;
}

std::vector<domain::party_identifier>
party_identifier_service::get_party_identifier_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for party identifier: " << id;
    return repo_.read_all(id);
}

}
