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
#include "ores.iam.core/service/account_contact_information_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::iam::service {

using namespace ores::logging;

account_contact_information_service::account_contact_information_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::account_contact_information>
account_contact_information_service::list_account_contact_informations(std::uint32_t offset,
                                                                       std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all account contact informations";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t account_contact_information_service::count_account_contact_informations() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total account contact informations count";
    return repo_.get_total_account_contact_information_count(ctx_);
}

std::vector<domain::account_contact_information>
account_contact_information_service::list_account_contact_informations_by_account_id(
    const std::string& account_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing account contact informations by account_id: "
                               << account_id;
    return repo_.read_latest_by_account_id(ctx_, account_id, offset, limit);
}

std::uint32_t account_contact_information_service::count_account_contact_informations_by_account_id(
    const std::string& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total account contact informations count by account_id: "
                               << account_id;
    return repo_.get_total_account_contact_information_count_by_account_id(ctx_, account_id);
}

std::vector<domain::account_contact_information>
account_contact_information_service::list_account_contact_informations_by_account_id_as_of(
    const std::string& account_id,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug)
        << "Listing account contact informations by account_id as of window: " << account_id;
    return repo_.read_by_account_id_as_of(ctx_, account_id, valid_from_bound, valid_to_bound);
}
std::optional<domain::account_contact_information>
account_contact_information_service::get_account_contact_information_at_version(
    const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting account contact information at version: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::account_contact_information>
account_contact_information_service::get_account_contact_information(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting account contact information: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void account_contact_information_service::save_account_contact_information(
    const domain::account_contact_information& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Account Contact Information id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving account contact information: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved account contact information: " << v.id;
}

void account_contact_information_service::save_account_contact_informations(
    const std::vector<domain::account_contact_information>& account_contact_informations) {
    for (const auto& e : account_contact_informations)
        if (e.id.is_nil())
            throw std::invalid_argument("Account Contact Information id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << account_contact_informations.size()
                               << " account contact informations";
    auto ts = account_contact_informations;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void account_contact_information_service::delete_account_contact_information(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account contact information: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed account contact information: " << id;
}

void account_contact_information_service::delete_account_contact_informations(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::account_contact_information>
account_contact_information_service::get_account_contact_information_history(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for account contact information: " << id;
    return repo_.read_all(ctx_, id);
}

}
