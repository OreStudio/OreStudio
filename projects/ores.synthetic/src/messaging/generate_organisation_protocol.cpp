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
#include "ores.synthetic/messaging/generate_organisation_protocol.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::synthetic::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

// ============================================================================
// generate_organisation_request
// ============================================================================

std::vector<std::byte> generate_organisation_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, country);
    writer::write_uint32(buffer, party_count);
    writer::write_uint32(buffer, party_max_depth);
    writer::write_uint32(buffer, counterparty_count);
    writer::write_uint32(buffer, counterparty_max_depth);
    writer::write_uint32(buffer, portfolio_leaf_count);
    writer::write_uint32(buffer, portfolio_max_depth);
    writer::write_uint32(buffer, books_per_leaf_portfolio);
    writer::write_uint32(buffer, business_unit_count);
    writer::write_uint32(buffer, business_unit_max_depth);
    writer::write_bool(buffer, generate_addresses);
    writer::write_bool(buffer, generate_identifiers);
    writer::write_uint32(buffer, contacts_per_party);
    writer::write_uint32(buffer, contacts_per_counterparty);
    writer::write_bool(buffer, seed.has_value());
    if (seed.has_value()) {
        writer::write_uint64(buffer, *seed);
    }
    writer::write_string(buffer, published_by);
    return buffer;
}

std::expected<generate_organisation_request, error_code>
generate_organisation_request::deserialize(std::span<const std::byte> data) {
    generate_organisation_request request;

    auto country_result = reader::read_string(data);
    if (!country_result) return std::unexpected(country_result.error());
    request.country = *country_result;

    auto party_count_result = reader::read_uint32(data);
    if (!party_count_result) return std::unexpected(party_count_result.error());
    request.party_count = *party_count_result;

    auto party_max_depth_result = reader::read_uint32(data);
    if (!party_max_depth_result) return std::unexpected(party_max_depth_result.error());
    request.party_max_depth = *party_max_depth_result;

    auto counterparty_count_result = reader::read_uint32(data);
    if (!counterparty_count_result) return std::unexpected(counterparty_count_result.error());
    request.counterparty_count = *counterparty_count_result;

    auto counterparty_max_depth_result = reader::read_uint32(data);
    if (!counterparty_max_depth_result) return std::unexpected(counterparty_max_depth_result.error());
    request.counterparty_max_depth = *counterparty_max_depth_result;

    auto portfolio_leaf_count_result = reader::read_uint32(data);
    if (!portfolio_leaf_count_result) return std::unexpected(portfolio_leaf_count_result.error());
    request.portfolio_leaf_count = *portfolio_leaf_count_result;

    auto portfolio_max_depth_result = reader::read_uint32(data);
    if (!portfolio_max_depth_result) return std::unexpected(portfolio_max_depth_result.error());
    request.portfolio_max_depth = *portfolio_max_depth_result;

    auto books_per_leaf_result = reader::read_uint32(data);
    if (!books_per_leaf_result) return std::unexpected(books_per_leaf_result.error());
    request.books_per_leaf_portfolio = *books_per_leaf_result;

    auto bu_count_result = reader::read_uint32(data);
    if (!bu_count_result) return std::unexpected(bu_count_result.error());
    request.business_unit_count = *bu_count_result;

    auto bu_max_depth_result = reader::read_uint32(data);
    if (!bu_max_depth_result) return std::unexpected(bu_max_depth_result.error());
    request.business_unit_max_depth = *bu_max_depth_result;

    auto gen_addresses_result = reader::read_bool(data);
    if (!gen_addresses_result) return std::unexpected(gen_addresses_result.error());
    request.generate_addresses = *gen_addresses_result;

    auto gen_identifiers_result = reader::read_bool(data);
    if (!gen_identifiers_result) return std::unexpected(gen_identifiers_result.error());
    request.generate_identifiers = *gen_identifiers_result;

    auto contacts_per_party_result = reader::read_uint32(data);
    if (!contacts_per_party_result) return std::unexpected(contacts_per_party_result.error());
    request.contacts_per_party = *contacts_per_party_result;

    auto contacts_per_cp_result = reader::read_uint32(data);
    if (!contacts_per_cp_result) return std::unexpected(contacts_per_cp_result.error());
    request.contacts_per_counterparty = *contacts_per_cp_result;

    auto has_seed_result = reader::read_bool(data);
    if (!has_seed_result) return std::unexpected(has_seed_result.error());
    if (*has_seed_result) {
        auto seed_result = reader::read_uint64(data);
        if (!seed_result) return std::unexpected(seed_result.error());
        request.seed = *seed_result;
    }

    auto published_by_result = reader::read_string(data);
    if (!published_by_result) return std::unexpected(published_by_result.error());
    request.published_by = *published_by_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const generate_organisation_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// generate_organisation_response
// ============================================================================

std::vector<std::byte> generate_organisation_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);
    writer::write_uint32(buffer, parties_count);
    writer::write_uint32(buffer, counterparties_count);
    writer::write_uint32(buffer, portfolios_count);
    writer::write_uint32(buffer, books_count);
    writer::write_uint32(buffer, business_unit_types_count);
    writer::write_uint32(buffer, business_units_count);
    writer::write_uint32(buffer, contacts_count);
    writer::write_uint32(buffer, identifiers_count);
    return buffer;
}

std::expected<generate_organisation_response, error_code>
generate_organisation_response::deserialize(std::span<const std::byte> data) {
    generate_organisation_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    auto parties_result = reader::read_uint32(data);
    if (!parties_result) return std::unexpected(parties_result.error());
    response.parties_count = *parties_result;

    auto counterparties_result = reader::read_uint32(data);
    if (!counterparties_result) return std::unexpected(counterparties_result.error());
    response.counterparties_count = *counterparties_result;

    auto portfolios_result = reader::read_uint32(data);
    if (!portfolios_result) return std::unexpected(portfolios_result.error());
    response.portfolios_count = *portfolios_result;

    auto books_result = reader::read_uint32(data);
    if (!books_result) return std::unexpected(books_result.error());
    response.books_count = *books_result;

    auto bu_types_result = reader::read_uint32(data);
    if (!bu_types_result) return std::unexpected(bu_types_result.error());
    response.business_unit_types_count = *bu_types_result;

    auto bu_result = reader::read_uint32(data);
    if (!bu_result) return std::unexpected(bu_result.error());
    response.business_units_count = *bu_result;

    auto contacts_result = reader::read_uint32(data);
    if (!contacts_result) return std::unexpected(contacts_result.error());
    response.contacts_count = *contacts_result;

    auto identifiers_result = reader::read_uint32(data);
    if (!identifiers_result) return std::unexpected(identifiers_result.error());
    response.identifiers_count = *identifiers_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const generate_organisation_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
