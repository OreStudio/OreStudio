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
#include "ores.refdata/service/counterparty_identifier_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::service {

using namespace ores::logging;

counterparty_identifier_service::counterparty_identifier_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::counterparty_identifier> counterparty_identifier_service::list_counterparty_identifiers() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all counterparty identifiers";
    return repo_.read_latest();
}

std::optional<domain::counterparty_identifier>
counterparty_identifier_service::find_counterparty_identifier(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding counterparty identifier: " << id;
    auto results = repo_.read_latest(id);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::optional<domain::counterparty_identifier>
counterparty_identifier_service::find_counterparty_identifier_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding counterparty identifier by code: " << code;
    auto results = repo_.read_latest_by_code(code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void counterparty_identifier_service::save_counterparty_identifier(const domain::counterparty_identifier& counterparty_identifier) {
    if (counterparty_identifier.id.is_nil()) {
        throw std::invalid_argument("Counterparty Identifier ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving counterparty identifier: " << counterparty_identifier.id;
    repo_.write(counterparty_identifier);
    BOOST_LOG_SEV(lg(), info) << "Saved counterparty identifier: " << counterparty_identifier.id;
}

void counterparty_identifier_service::remove_counterparty_identifier(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty identifier: " << id;
    repo_.remove(id);
    BOOST_LOG_SEV(lg(), info) << "Removed counterparty identifier: " << id;
}

std::vector<domain::counterparty_identifier>
counterparty_identifier_service::get_counterparty_identifier_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for counterparty identifier: " << id;
    return repo_.read_all(id);
}

}
