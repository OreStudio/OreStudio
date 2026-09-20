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
#include "ores.iam.core/service/account_party_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>

namespace ores::iam::service {

using namespace ores::logging;

account_party_service::account_party_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::account_party> account_party_service::list_account_parties() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all account parties";
    return repo_.read_latest();
}

std::vector<domain::account_party> account_party_service::list_account_parties(
    std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all account parties with offset: " << offset
                               << " limit: " << limit;
    return repo_.read_latest(offset, limit);
}

std::uint32_t account_party_service::get_total_account_party_count() {
    return repo_.get_total_account_party_count();
}

std::vector<domain::account_party>
account_party_service::list_account_parties_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Listing account parties for account: " << account_id;
    return repo_.read_latest_by_account(account_id);
}

std::vector<domain::account_party> account_party_service::list_account_parties_by_account(
    const boost::uuids::uuid& account_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing account parties for account: " << account_id
                               << " offset: " << offset << " limit: " << limit;
    return repo_.read_latest_by_account(account_id, offset, limit);
}

std::uint32_t account_party_service::get_total_account_party_count_by_account(
    const boost::uuids::uuid& account_id) {
    return repo_.get_total_account_party_count_by_account(account_id);
}

std::uint32_t account_party_service::get_total_account_party_count_by_party(
    const boost::uuids::uuid& party_id) {
    return repo_.get_total_account_party_count_by_party(party_id);
}

void account_party_service::save_account_party(const domain::account_party& account_party) {
    if (account_party.account_id.is_nil()) {
        throw std::invalid_argument("Account cannot be empty.");
    }
    if (account_party.party_id.is_nil()) {
        throw std::invalid_argument("Party cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving account party: " << account_party.account_id << "/"
                               << account_party.party_id;
    repo_.write(account_party);
    BOOST_LOG_SEV(lg(), info) << "Saved account party: " << account_party.account_id << "/"
                              << account_party.party_id;
}

void account_party_service::remove_account_party(const boost::uuids::uuid& account_id,
                                                 const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account party: " << account_id << "/" << party_id;
    repo_.remove(account_id, party_id);
    BOOST_LOG_SEV(lg(), info) << "Removed account party: " << account_id << "/" << party_id;
}

void account_party_service::replace_account_parties_by_account(
    const boost::uuids::uuid& account_id,
    const std::vector<domain::account_party>& account_parties,
    const std::string& modified_by,
    const std::string& performed_by,
    const std::string& change_reason_code,
    const std::string& change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Replacing account parties for account: " << account_id;
    repo_.replace_by_account(account_id,
                             account_parties,
                             modified_by,
                             performed_by,
                             change_reason_code,
                             change_commentary);
    BOOST_LOG_SEV(lg(), info) << "Replaced account parties for account: " << account_id;
}

}
