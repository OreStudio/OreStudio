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
#include "ores.iam/service/account_party_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::service {

using namespace ores::logging;

account_party_service::account_party_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::account_party> account_party_service::list_account_parties() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all account parties";
    return repo_.read_latest();
}

std::vector<domain::account_party>
account_party_service::list_account_parties_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Listing account parties for account: " << account_id;
    return repo_.read_latest_by_account(account_id);
}

void account_party_service::save_account_party(const domain::account_party& account_party) {
    if (account_party.account_id.is_nil()) {
        throw std::invalid_argument("Account cannot be empty.");
    }
    if (account_party.party_id.is_nil()) {
        throw std::invalid_argument("Party cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving account party: " << account_party.account_id
                               << "/" << account_party.party_id;
    repo_.write(account_party);
    BOOST_LOG_SEV(lg(), info) << "Saved account party: " << account_party.account_id
                              << "/" << account_party.party_id;
}

void account_party_service::remove_account_party(const boost::uuids::uuid& account_id,
    const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account party: " << account_id
                               << "/" << party_id;
    repo_.remove(account_id, party_id);
    BOOST_LOG_SEV(lg(), info) << "Removed account party: " << account_id
                              << "/" << party_id;
}

}
