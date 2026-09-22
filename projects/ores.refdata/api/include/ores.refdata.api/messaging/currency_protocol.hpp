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
#ifndef ORES_REFDATA_API_MESSAGING_CURRENCY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURRENCY_PROTOCOL_HPP

#include "ores.refdata.api/domain/currency.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct currency_key {
    std::string iso_code;
};

struct currency_write {
    std::string iso_code;
    std::string name;
    std::string numeric_code;
    std::string symbol;
    std::string fraction_symbol;
    int fractions_per_unit;
    std::string rounding_type;
    int rounding_precision;
    std::string format;
    std::string monetary_nature;
    std::string market_tier;
    std::optional<boost::uuids::uuid> image_id;
    int spot_days;
    std::string day_basis;
    int base_precedence;
};

struct currency_change {
    currency_write write;
    ores::utility::domain::precondition precondition;
};

struct currency_removal {
    currency_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct currency_lookup {
    currency_key key;
    std::optional<ores::refdata::domain::currency> currency;
};

struct currency_event {
    boost::uuids::uuid event_id;
    currency_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct currency_version_key {
    currency_key currency;
    std::uint32_t version;
};

struct currency_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_currencies_request {
    using response_type = struct list_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.list";
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
};

struct list_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency> currencies;
    std::uint64_t total;
};

struct get_currency_request {
    using response_type = struct get_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_key key;
};

struct get_currency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::currency> currency;
};

struct get_many_currencies_request {
    using response_type = struct get_many_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_key> keys;
};

struct get_many_currencies_response {
    ores::utility::domain::result result;
    std::vector<currency_lookup> entries;
};

struct put_currency_request {
    using response_type = struct put_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_change change;
    ores::utility::domain::change_intent intent;
};

struct put_currency_response {
    ores::utility::domain::result result;
    ores::refdata::domain::currency currency;
};

struct put_many_currencies_request {
    using response_type = struct put_many_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency> currencies;
};

struct delete_currency_request {
    using response_type = struct delete_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_currency_response {
    ores::utility::domain::result result;
};

struct delete_many_currencies_request {
    using response_type = struct delete_many_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_currencies_response {
    ores::utility::domain::result result;
};

struct list_currency_versions_request {
    using response_type = struct list_currency_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<currency_versions_filter> filter;
};

struct list_currency_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency> versions;
    std::uint64_t total;
};

struct get_currency_version_request {
    using response_type = struct get_currency_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_version_key key;
};

struct get_currency_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::currency version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace currency_event_subjects {
inline constexpr std::string_view created = "refdata.v1.currencies_events.created";
inline constexpr std::string_view updated = "refdata.v1.currencies_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.currencies_events.deleted";
}

}

#endif
