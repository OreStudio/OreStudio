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
#include "ores.refdata/service/counterparty_contact_information_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::service {

using namespace ores::logging;

counterparty_contact_information_service::counterparty_contact_information_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::counterparty_contact_information> counterparty_contact_information_service::list_counterparty_contact_informations() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all counterparty contact informations";
    return repo_.read_latest();
}

std::optional<domain::counterparty_contact_information>
counterparty_contact_information_service::find_counterparty_contact_information(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding counterparty contact information: " << id;
    auto results = repo_.read_latest(id);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::optional<domain::counterparty_contact_information>
counterparty_contact_information_service::find_counterparty_contact_information_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding counterparty contact information by code: " << code;
    auto results = repo_.read_latest_by_code(code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void counterparty_contact_information_service::save_counterparty_contact_information(const domain::counterparty_contact_information& counterparty_contact_information) {
    if (counterparty_contact_information.id.is_nil()) {
        throw std::invalid_argument("Counterparty Contact Information ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving counterparty contact information: " << counterparty_contact_information.id;
    repo_.write(counterparty_contact_information);
    BOOST_LOG_SEV(lg(), info) << "Saved counterparty contact information: " << counterparty_contact_information.id;
}

void counterparty_contact_information_service::remove_counterparty_contact_information(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty contact information: " << id;
    repo_.remove(id);
    BOOST_LOG_SEV(lg(), info) << "Removed counterparty contact information: " << id;
}

std::vector<domain::counterparty_contact_information>
counterparty_contact_information_service::get_counterparty_contact_information_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for counterparty contact information: " << id;
    return repo_.read_all(id);
}

}
