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
#ifndef ORES_DQ_API_MESSAGING_DATASET_BUNDLE_MEMBER_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_DATASET_BUNDLE_MEMBER_PROTOCOL_HPP

#include "ores.dq.api/domain/dataset_bundle_member.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct dataset_bundle_member_key {
    std::string bundle_code;
    std::string dataset_code;
};

struct dataset_bundle_member_write {
    std::string bundle_code;
    std::string dataset_code;
    int display_order;
    bool optional;
};

struct dataset_bundle_member_change {
    dataset_bundle_member_write write;
    ores::utility::domain::precondition precondition;
};

struct dataset_bundle_member_removal {
    dataset_bundle_member_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct dataset_bundle_member_lookup {
    dataset_bundle_member_key key;
    std::optional<ores::dq::domain::dataset_bundle_member> dataset_bundle_member;
};

struct dataset_bundle_members_filter {
    std::optional<std::string> bundle_code;
};

struct list_dataset_bundle_members_request {
    using response_type = struct list_dataset_bundle_members_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.list";
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
    std::optional<dataset_bundle_members_filter> filter;
};

struct list_dataset_bundle_members_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset_bundle_member> dataset_bundle_members;
    std::uint64_t total;
};

struct get_dataset_bundle_member_request {
    using response_type = struct get_dataset_bundle_member_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_member_key key;
};

struct get_dataset_bundle_member_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset_bundle_member> dataset_bundle_member;
};

struct get_many_dataset_bundle_members_request {
    using response_type = struct get_many_dataset_bundle_members_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_bundle_member_key> keys;
};

struct get_many_dataset_bundle_members_response {
    ores::utility::domain::result result;
    std::vector<dataset_bundle_member_lookup> entries;
};

struct put_dataset_bundle_member_request {
    using response_type = struct put_dataset_bundle_member_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_member_change change;
    ores::utility::domain::change_intent intent;
};

struct put_dataset_bundle_member_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset_bundle_member> dataset_bundle_member;
};

struct put_many_dataset_bundle_members_request {
    using response_type = struct put_many_dataset_bundle_members_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_bundle_member_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_dataset_bundle_members_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset_bundle_member> dataset_bundle_members;
};

struct delete_dataset_bundle_member_request {
    using response_type = struct delete_dataset_bundle_member_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_member_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_dataset_bundle_member_response {
    ores::utility::domain::result result;
};

struct delete_many_dataset_bundle_members_request {
    using response_type = struct delete_many_dataset_bundle_members_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_bundle_member_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_dataset_bundle_members_response {
    ores::utility::domain::result result;
};

struct list_by_bundle_code_dataset_bundle_members_request {
    using response_type = struct list_by_bundle_code_dataset_bundle_members_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.dataset_bundle_members.list_by_bundle_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string bundle_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<dataset_bundle_members_filter> filter;
};

struct list_by_bundle_code_dataset_bundle_members_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset_bundle_member> dataset_bundle_members;
    std::uint64_t total;
};

}

#endif
