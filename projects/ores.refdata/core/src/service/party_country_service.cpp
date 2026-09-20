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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.core/service/party_country_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <utility>

namespace ores::refdata::service {

using namespace ores::logging;
using ores::service::messaging::stamp;

namespace {

/**
 * @brief Stamps a party country row without touching its party_id.
 *
 * This junction's party_id names the association's target, which the
 * client supplies, not the caller's own scope. The generic stamp()
 * matches any field of that name by reflection and overwrites it with
 * the caller's current party, which would silently replace the requested
 * association. Mirrors ores.iam's stamp_account_party.
 */
void stamp_party_country(domain::party_country& row, const ores::database::context& ctx) {
    row.tenant_id = ctx.tenant_id().to_string();
    const auto& actor = ctx.actor();
    const auto& svc = ctx.service_account();
    if (!actor.empty())
        row.modified_by = actor;
    else if (!svc.empty())
        row.modified_by = svc;
    if (!svc.empty())
        row.performed_by = svc;
    if (row.change_reason_code.empty())
        row.change_reason_code = std::string(ores::service::messaging::change_reasons::new_record);
}

} // namespace

party_country_service::party_country_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_(ctx_) {}

std::vector<domain::party_country> party_country_service::list_party_countries() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all party countries";
    return repo_.read_latest();
}

std::vector<domain::party_country>
party_country_service::list_party_countries(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all party countries with offset: " << offset
                               << " limit: " << limit;
    return repo_.read_latest(offset, limit);
}

std::uint32_t party_country_service::get_total_party_country_count() {
    return repo_.get_total_party_country_count();
}

std::vector<domain::party_country>
party_country_service::list_party_countries_by_party(const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Listing party countries for party: " << party_id;
    return repo_.read_latest_by_party(party_id);
}

std::vector<domain::party_country> party_country_service::list_party_countries_by_party(
    const boost::uuids::uuid& party_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing party countries for party: " << party_id
                               << " offset: " << offset << " limit: " << limit;
    return repo_.read_latest_by_party(party_id, offset, limit);
}

std::uint32_t
party_country_service::get_total_party_country_count_by_party(const boost::uuids::uuid& party_id) {
    return repo_.get_total_party_country_count_by_party(party_id);
}

std::uint32_t party_country_service::get_total_party_country_count_by_country(
    const std::string& country_alpha2_code) {
    return repo_.get_total_party_country_count_by_country(country_alpha2_code);
}

void party_country_service::save_party_country(const domain::party_country& party_country) {
    if (party_country.party_id.is_nil()) {
        throw std::invalid_argument("Party cannot be empty.");
    }
    if (party_country.country_alpha2_code.empty()) {
        throw std::invalid_argument("Country cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving party country: " << party_country.party_id << "/"
                               << party_country.country_alpha2_code;
    auto t = party_country;
    stamp_party_country(t, ctx_);
    repo_.write(t);
    BOOST_LOG_SEV(lg(), info) << "Saved party country: " << party_country.party_id << "/"
                              << party_country.country_alpha2_code;
}

void party_country_service::remove_party_country(const boost::uuids::uuid& party_id,
                                                 const std::string& country_alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party country: " << party_id << "/"
                               << country_alpha2_code;
    repo_.remove(party_id, country_alpha2_code);
    BOOST_LOG_SEV(lg(), info) << "Removed party country: " << party_id << "/"
                              << country_alpha2_code;
}

}
