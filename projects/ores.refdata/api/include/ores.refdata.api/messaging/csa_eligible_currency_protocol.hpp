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
#ifndef ORES_REFDATA_API_MESSAGING_CSA_ELIGIBLE_CURRENCY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CSA_ELIGIBLE_CURRENCY_PROTOCOL_HPP

#include "ores.refdata.api/domain/csa_eligible_currency.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct csa_eligible_currency_key {
    std::string currency_code;
};

struct csa_eligible_currency_write {
    boost::uuids::uuid id;
    boost::uuids::uuid csa_id;
    std::string currency_code;
    int position;
};

struct csa_eligible_currency_change {
    csa_eligible_currency_write write;
    ores::utility::domain::precondition precondition;
};

struct csa_eligible_currency_removal {
    csa_eligible_currency_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct csa_eligible_currency_lookup {
    csa_eligible_currency_key key;
    std::optional<ores::refdata::domain::csa_eligible_currency> csa_eligible_currency;
};

struct csa_eligible_currencies_filter {
    std::optional<boost::uuids::uuid> csa_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> csa_id_one_of;
};

struct csa_eligible_currency_event {
    boost::uuids::uuid event_id;
    csa_eligible_currency_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct csa_eligible_currency_version_key {
    csa_eligible_currency_key csa_eligible_currency;
    std::uint32_t version;
};

struct csa_eligible_currency_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_csa_eligible_currencies_request {
    using response_type = struct list_csa_eligible_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.list";
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
    std::optional<csa_eligible_currencies_filter> filter;
    std::optional<std::string> as_of;
};

struct list_csa_eligible_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::csa_eligible_currency> csa_eligible_currencies;
    std::uint64_t total;
};

struct get_csa_eligible_currency_request {
    using response_type = struct get_csa_eligible_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    csa_eligible_currency_key key;
};

struct get_csa_eligible_currency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::csa_eligible_currency> csa_eligible_currency;
};

struct get_many_csa_eligible_currencies_request {
    using response_type = struct get_many_csa_eligible_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<csa_eligible_currency_key> keys;
};

struct get_many_csa_eligible_currencies_response {
    ores::utility::domain::result result;
    std::vector<csa_eligible_currency_lookup> entries;
};

struct put_csa_eligible_currency_request {
    using response_type = struct put_csa_eligible_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    csa_eligible_currency_change change;
    ores::utility::domain::change_intent intent;
};

struct put_csa_eligible_currency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::csa_eligible_currency> csa_eligible_currency;
};

struct put_many_csa_eligible_currencies_request {
    using response_type = struct put_many_csa_eligible_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<csa_eligible_currency_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_csa_eligible_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::csa_eligible_currency> csa_eligible_currencies;
};

struct delete_csa_eligible_currency_request {
    using response_type = struct delete_csa_eligible_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    csa_eligible_currency_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_csa_eligible_currency_response {
    ores::utility::domain::result result;
};

struct delete_many_csa_eligible_currencies_request {
    using response_type = struct delete_many_csa_eligible_currencies_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.csa_eligible_currencies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<csa_eligible_currency_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_csa_eligible_currencies_response {
    ores::utility::domain::result result;
};

struct list_by_csa_id_csa_eligible_currencies_request {
    using response_type = struct list_by_csa_id_csa_eligible_currencies_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.csa_eligible_currencies.list_by_csa_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid csa_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<csa_eligible_currencies_filter> filter;
};

struct list_by_csa_id_csa_eligible_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::csa_eligible_currency> csa_eligible_currencies;
    std::uint64_t total;
};

struct list_csa_eligible_currency_versions_request {
    using response_type = struct list_csa_eligible_currency_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.csa_eligible_currencies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    csa_eligible_currency_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<csa_eligible_currency_versions_filter> filter;
};

struct list_csa_eligible_currency_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::csa_eligible_currency> versions;
    std::uint64_t total;
};

struct get_csa_eligible_currency_version_request {
    using response_type = struct get_csa_eligible_currency_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.csa_eligible_currencies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    csa_eligible_currency_version_key key;
};

struct get_csa_eligible_currency_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::csa_eligible_currency> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace csa_eligible_currency_event_subjects {
inline constexpr std::string_view created = "refdata.v1.csa_eligible_currencies_events.created";
inline constexpr std::string_view updated = "refdata.v1.csa_eligible_currencies_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.csa_eligible_currencies_events.deleted";
}

}

#endif
