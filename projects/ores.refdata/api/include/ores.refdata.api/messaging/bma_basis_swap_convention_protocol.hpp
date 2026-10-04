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
#ifndef ORES_REFDATA_API_MESSAGING_BMA_BASIS_SWAP_CONVENTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BMA_BASIS_SWAP_CONVENTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/bma_basis_swap_convention.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct bma_basis_swap_convention_key {
    std::string id;
};

struct bma_basis_swap_convention_write {
    std::string id;
    std::string index;
    std::string bma_index;
    std::optional<std::string> bma_payment_calendar;
    std::optional<std::string> bma_payment_convention;
    std::optional<int> bma_payment_lag;
    std::optional<std::string> index_payment_calendar;
    std::optional<std::string> index_payment_convention;
    std::optional<int> index_payment_lag;
    std::optional<int> index_settlement_days;
    std::optional<std::string> index_payment_period;
    std::optional<int> overnight_lockout_days;
};

struct bma_basis_swap_convention_change {
    bma_basis_swap_convention_write write;
    ores::utility::domain::precondition precondition;
};

struct bma_basis_swap_convention_removal {
    bma_basis_swap_convention_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bma_basis_swap_convention_lookup {
    bma_basis_swap_convention_key key;
    std::optional<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap_convention;
};

struct bma_basis_swap_conventions_filter {
    std::optional<std::vector<std::string>> id_one_of;
};

struct bma_basis_swap_convention_event {
    boost::uuids::uuid event_id;
    bma_basis_swap_convention_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bma_basis_swap_convention_version_key {
    bma_basis_swap_convention_key bma_basis_swap_convention;
    std::uint32_t version;
};

struct bma_basis_swap_convention_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bma_basis_swap_conventions_request {
    using response_type = struct list_bma_basis_swap_conventions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bma_basis_swap_conventions.list";
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
    std::optional<bma_basis_swap_conventions_filter> filter;
};

struct list_bma_basis_swap_conventions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap_conventions;
    std::uint64_t total;
};

struct get_bma_basis_swap_convention_request {
    using response_type = struct get_bma_basis_swap_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bma_basis_swap_conventions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bma_basis_swap_convention_key key;
};

struct get_bma_basis_swap_convention_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap_convention;
};

struct get_many_bma_basis_swap_conventions_request {
    using response_type = struct get_many_bma_basis_swap_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bma_basis_swap_conventions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bma_basis_swap_convention_key> keys;
};

struct get_many_bma_basis_swap_conventions_response {
    ores::utility::domain::result result;
    std::vector<bma_basis_swap_convention_lookup> entries;
};

struct put_bma_basis_swap_convention_request {
    using response_type = struct put_bma_basis_swap_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bma_basis_swap_conventions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bma_basis_swap_convention_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bma_basis_swap_convention_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap_convention;
};

struct put_many_bma_basis_swap_conventions_request {
    using response_type = struct put_many_bma_basis_swap_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bma_basis_swap_conventions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bma_basis_swap_convention_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bma_basis_swap_conventions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap_conventions;
};

struct delete_bma_basis_swap_convention_request {
    using response_type = struct delete_bma_basis_swap_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bma_basis_swap_conventions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bma_basis_swap_convention_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bma_basis_swap_convention_response {
    ores::utility::domain::result result;
};

struct delete_many_bma_basis_swap_conventions_request {
    using response_type = struct delete_many_bma_basis_swap_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bma_basis_swap_conventions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bma_basis_swap_convention_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bma_basis_swap_conventions_response {
    ores::utility::domain::result result;
};

struct list_bma_basis_swap_convention_versions_request {
    using response_type = struct list_bma_basis_swap_convention_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bma_basis_swap_conventions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bma_basis_swap_convention_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bma_basis_swap_convention_versions_filter> filter;
};

struct list_bma_basis_swap_convention_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bma_basis_swap_convention> versions;
    std::uint64_t total;
};

struct get_bma_basis_swap_convention_version_request {
    using response_type = struct get_bma_basis_swap_convention_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bma_basis_swap_conventions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bma_basis_swap_convention_version_key key;
};

struct get_bma_basis_swap_convention_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bma_basis_swap_convention> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bma_basis_swap_convention_event_subjects {
inline constexpr std::string_view created = "refdata.v1.bma_basis_swap_conventions_events.created";
inline constexpr std::string_view updated = "refdata.v1.bma_basis_swap_conventions_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.bma_basis_swap_conventions_events.deleted";
}

}

#endif
