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
#ifndef ORES_REFDATA_API_MESSAGING_TENOR_BASIS_SWAP_PAY_RECEIVE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENOR_BASIS_SWAP_PAY_RECEIVE_PROTOCOL_HPP

#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct tenor_basis_swap_pay_receive_key {
    boost::uuids::uuid id;
};

struct tenor_basis_swap_pay_receive_write {
    boost::uuids::uuid id;
    boost::uuids::uuid convention_id;
    std::optional<std::string> pay_index;
    std::optional<std::string> pay_frequency;
    std::optional<std::string> receive_index;
    std::optional<std::string> receive_frequency;
    std::optional<bool> spread_on_rec;
    std::optional<bool> include_spread;
    std::optional<std::string> sub_periods_coupon_type;
    std::optional<bool> pay_is_averaged;
    std::optional<bool> rec_is_averaged;
};

struct tenor_basis_swap_pay_receive_change {
    tenor_basis_swap_pay_receive_write write;
    ores::utility::domain::precondition precondition;
};

struct tenor_basis_swap_pay_receive_removal {
    tenor_basis_swap_pay_receive_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct tenor_basis_swap_pay_receive_lookup {
    tenor_basis_swap_pay_receive_key key;
    std::optional<ores::refdata::domain::tenor_basis_swap_pay_receive> tenor_basis_swap_pay_receive;
};

struct tenor_basis_swap_pay_receives_filter {
    std::optional<boost::uuids::uuid> convention_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> convention_id_one_of;
};

struct tenor_basis_swap_pay_receive_event {
    boost::uuids::uuid event_id;
    tenor_basis_swap_pay_receive_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct tenor_basis_swap_pay_receive_version_key {
    tenor_basis_swap_pay_receive_key tenor_basis_swap_pay_receive;
    std::uint32_t version;
};

struct tenor_basis_swap_pay_receive_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_tenor_basis_swap_pay_receives_request {
    using response_type = struct list_tenor_basis_swap_pay_receives_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_basis_swap_pay_receives.list";
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
    std::optional<tenor_basis_swap_pay_receives_filter> filter;
    std::optional<std::string> as_of;
};

struct list_tenor_basis_swap_pay_receives_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_basis_swap_pay_receive> tenor_basis_swap_pay_receives;
    std::uint64_t total;
};

struct get_tenor_basis_swap_pay_receive_request {
    using response_type = struct get_tenor_basis_swap_pay_receive_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_basis_swap_pay_receives.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_basis_swap_pay_receive_key key;
};

struct get_tenor_basis_swap_pay_receive_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor_basis_swap_pay_receive> tenor_basis_swap_pay_receive;
};

struct get_many_tenor_basis_swap_pay_receives_request {
    using response_type = struct get_many_tenor_basis_swap_pay_receives_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_basis_swap_pay_receives.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_basis_swap_pay_receive_key> keys;
};

struct get_many_tenor_basis_swap_pay_receives_response {
    ores::utility::domain::result result;
    std::vector<tenor_basis_swap_pay_receive_lookup> entries;
};

struct put_tenor_basis_swap_pay_receive_request {
    using response_type = struct put_tenor_basis_swap_pay_receive_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_basis_swap_pay_receives.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_basis_swap_pay_receive_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tenor_basis_swap_pay_receive_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor_basis_swap_pay_receive> tenor_basis_swap_pay_receive;
};

struct put_many_tenor_basis_swap_pay_receives_request {
    using response_type = struct put_many_tenor_basis_swap_pay_receives_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_basis_swap_pay_receives.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_basis_swap_pay_receive_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tenor_basis_swap_pay_receives_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_basis_swap_pay_receive> tenor_basis_swap_pay_receives;
};

struct delete_tenor_basis_swap_pay_receive_request {
    using response_type = struct delete_tenor_basis_swap_pay_receive_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_basis_swap_pay_receives.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_basis_swap_pay_receive_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tenor_basis_swap_pay_receive_response {
    ores::utility::domain::result result;
};

struct delete_many_tenor_basis_swap_pay_receives_request {
    using response_type = struct delete_many_tenor_basis_swap_pay_receives_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_basis_swap_pay_receives.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_basis_swap_pay_receive_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tenor_basis_swap_pay_receives_response {
    ores::utility::domain::result result;
};

struct list_by_convention_id_tenor_basis_swap_pay_receives_request {
    using response_type = struct list_by_convention_id_tenor_basis_swap_pay_receives_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_basis_swap_pay_receives.list_by_convention_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid convention_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_basis_swap_pay_receives_filter> filter;
};

struct list_by_convention_id_tenor_basis_swap_pay_receives_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_basis_swap_pay_receive> tenor_basis_swap_pay_receives;
    std::uint64_t total;
};

struct list_tenor_basis_swap_pay_receive_versions_request {
    using response_type = struct list_tenor_basis_swap_pay_receive_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_basis_swap_pay_receives_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_basis_swap_pay_receive_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_basis_swap_pay_receive_versions_filter> filter;
};

struct list_tenor_basis_swap_pay_receive_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_basis_swap_pay_receive> versions;
    std::uint64_t total;
};

struct get_tenor_basis_swap_pay_receive_version_request {
    using response_type = struct get_tenor_basis_swap_pay_receive_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_basis_swap_pay_receives_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_basis_swap_pay_receive_version_key key;
};

struct get_tenor_basis_swap_pay_receive_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor_basis_swap_pay_receive> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace tenor_basis_swap_pay_receive_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.tenor_basis_swap_pay_receives_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.tenor_basis_swap_pay_receives_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.tenor_basis_swap_pay_receives_events.deleted";
}

}

#endif
