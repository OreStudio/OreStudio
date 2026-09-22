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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_API_MESSAGING_PARTY_COUNTERPARTY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PARTY_COUNTERPARTY_PROTOCOL_HPP

#include "ores.refdata.api/domain/party_counterparty.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct party_counterparty_key {
    boost::uuids::uuid party_id;
    boost::uuids::uuid counterparty_id;
};

struct party_counterparty_write {
    boost::uuids::uuid party_id;
    boost::uuids::uuid counterparty_id;
};

struct party_counterparty_change {
    party_counterparty_write write;
    ores::utility::domain::precondition precondition;
};

struct party_counterparty_removal {
    party_counterparty_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct party_counterparty_lookup {
    party_counterparty_key key;
    std::optional<ores::refdata::domain::party_counterparty> party_counterparty;
};

struct party_counterparties_filter {
    std::optional<boost::uuids::uuid> party_id;
};

struct party_counterparty_event {
    boost::uuids::uuid event_id;
    party_counterparty_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_party_counterparties_request {
    using response_type = struct list_party_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_counterparties.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<party_counterparties_filter> filter;
};

struct list_party_counterparties_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_counterparty> party_counterparties;
    std::uint64_t total;
};

struct get_party_counterparty_request {
    using response_type = struct get_party_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_counterparties.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_counterparty_key key;
};

struct get_party_counterparty_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::party_counterparty> party_counterparty;
};

struct get_many_party_counterparties_request {
    using response_type = struct get_many_party_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_counterparties.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_counterparty_key> keys;
};

struct get_many_party_counterparties_response {
    ores::utility::domain::result result;
    std::vector<party_counterparty_lookup> entries;
};

struct put_party_counterparty_request {
    using response_type = struct put_party_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_counterparties.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_counterparty_change change;
    ores::utility::domain::change_intent intent;
};

struct put_party_counterparty_response {
    ores::utility::domain::result result;
    ores::refdata::domain::party_counterparty party_counterparty;
};

struct put_many_party_counterparties_request {
    using response_type = struct put_many_party_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_counterparties.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_counterparty_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_party_counterparties_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_counterparty> party_counterparties;
};

struct delete_party_counterparty_request {
    using response_type = struct delete_party_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_counterparties.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_counterparty_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_party_counterparty_response {
    ores::utility::domain::result result;
};

struct delete_many_party_counterparties_request {
    using response_type = struct delete_many_party_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_counterparties.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_counterparty_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_party_counterparties_response {
    ores::utility::domain::result result;
};

struct list_by_party_id_party_counterparties_request {
    using response_type = struct list_by_party_id_party_counterparties_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.party_counterparties.list_by_party_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid party_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<party_counterparties_filter> filter;
};

struct list_by_party_id_party_counterparties_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_counterparty> party_counterparties;
    std::uint64_t total;
};

}

#endif
