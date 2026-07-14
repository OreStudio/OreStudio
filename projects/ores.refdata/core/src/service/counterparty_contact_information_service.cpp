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
#include "ores.refdata.core/service/counterparty_contact_information_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

counterparty_contact_information_service::counterparty_contact_information_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::counterparty_contact_information>
counterparty_contact_information_service::list_counterparty_contact_informations(
    std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all counterparty contact informations";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t counterparty_contact_information_service::count_counterparty_contact_informations() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total counterparty contact informations count";
    return repo_.get_total_counterparty_contact_information_count(ctx_);
}

std::vector<domain::counterparty_contact_information>
counterparty_contact_information_service::list_counterparty_contact_informations_by_counterparty_id(
    const std::string& counterparty_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing counterparty contact informations by counterparty_id: "
                               << counterparty_id;
    return repo_.read_latest_by_counterparty_id(ctx_, counterparty_id, offset, limit);
}

std::uint32_t counterparty_contact_information_service::
    count_counterparty_contact_informations_by_counterparty_id(const std::string& counterparty_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting total counterparty contact informations count by counterparty_id: "
        << counterparty_id;
    return repo_.get_total_counterparty_contact_information_count_by_counterparty_id(
        ctx_, counterparty_id);
}

std::vector<domain::counterparty_contact_information> counterparty_contact_information_service::
    list_counterparty_contact_informations_by_counterparty_id_as_of(
        const std::string& counterparty_id,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug)
        << "Listing counterparty contact informations by counterparty_id as of window: "
        << counterparty_id;
    return repo_.read_by_counterparty_id_as_of(
        ctx_, counterparty_id, valid_from_bound, valid_to_bound);
}
std::optional<domain::counterparty_contact_information>
counterparty_contact_information_service::get_counterparty_contact_information_at_version(
    const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting counterparty contact information at version: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::counterparty_contact_information>
counterparty_contact_information_service::get_counterparty_contact_information(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting counterparty contact information: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void counterparty_contact_information_service::save_counterparty_contact_information(
    const domain::counterparty_contact_information& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Counterparty Contact Information id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving counterparty contact information: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved counterparty contact information: " << v.id;
}

void counterparty_contact_information_service::save_counterparty_contact_informations(
    const std::vector<domain::counterparty_contact_information>&
        counterparty_contact_informations) {
    for (const auto& e : counterparty_contact_informations)
        if (e.id.is_nil())
            throw std::invalid_argument("Counterparty Contact Information id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << counterparty_contact_informations.size()
                               << " counterparty contact informations";
    auto ts = counterparty_contact_informations;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void counterparty_contact_information_service::delete_counterparty_contact_information(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty contact information: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed counterparty contact information: " << id;
}

void counterparty_contact_information_service::delete_counterparty_contact_informations(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::counterparty_contact_information>
counterparty_contact_information_service::get_counterparty_contact_information_history(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for counterparty contact information: " << id;
    return repo_.read_all(ctx_, id);
}

}
