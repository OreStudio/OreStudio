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
#ifndef ORES_INBOX_API_MESSAGING_APPROVAL_REQUEST_PART_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_APPROVAL_REQUEST_PART_PROTOCOL_HPP

#include "ores.inbox.api/domain/approval_request_part.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::messaging {

struct approval_request_part_key {
    boost::uuids::uuid request_id;
    std::string part_code;
};

struct approval_request_part_write {
    boost::uuids::uuid request_id;
    std::string part_code;
};

struct approval_request_part_change {
    approval_request_part_write write;
    ores::utility::domain::precondition precondition;
};

struct approval_request_part_removal {
    approval_request_part_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct approval_request_part_lookup {
    approval_request_part_key key;
    std::optional<ores::inbox::domain::approval_request_part> approval_request_part;
};

struct approval_request_parts_filter {
    std::optional<boost::uuids::uuid> request_id;
    std::optional<std::vector<boost::uuids::uuid>> request_id_one_of;
};

struct list_approval_request_parts_request {
    using response_type = struct list_approval_request_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_request_parts.list";
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
    std::optional<approval_request_parts_filter> filter;
};

struct list_approval_request_parts_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_request_part> approval_request_parts;
    std::uint64_t total;
};

struct get_approval_request_part_request {
    using response_type = struct get_approval_request_part_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_request_parts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_request_part_key key;
};

struct get_approval_request_part_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_request_part> approval_request_part;
};

struct get_many_approval_request_parts_request {
    using response_type = struct get_many_approval_request_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_request_parts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_request_part_key> keys;
};

struct get_many_approval_request_parts_response {
    ores::utility::domain::result result;
    std::vector<approval_request_part_lookup> entries;
};

struct put_approval_request_part_request {
    using response_type = struct put_approval_request_part_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_request_parts.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_request_part_change change;
    ores::utility::domain::change_intent intent;
};

struct put_approval_request_part_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_request_part> approval_request_part;
};

struct put_many_approval_request_parts_request {
    using response_type = struct put_many_approval_request_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_request_parts.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_request_part_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_approval_request_parts_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_request_part> approval_request_parts;
};

struct delete_approval_request_part_request {
    using response_type = struct delete_approval_request_part_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_request_parts.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_request_part_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_approval_request_part_response {
    ores::utility::domain::result result;
};

struct delete_many_approval_request_parts_request {
    using response_type = struct delete_many_approval_request_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_request_parts.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_request_part_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_approval_request_parts_response {
    ores::utility::domain::result result;
};

struct list_by_request_id_approval_request_parts_request {
    using response_type = struct list_by_request_id_approval_request_parts_response;
    static constexpr std::string_view nats_subject =
        "inbox.v1.approval_request_parts.list_by_request_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid request_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<approval_request_parts_filter> filter;
};

struct list_by_request_id_approval_request_parts_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_request_part> approval_request_parts;
    std::uint64_t total;
};

}

#endif
