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
#ifndef ORES_TRADING_API_MESSAGING_DRAFT_REASON_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_DRAFT_REASON_PROTOCOL_HPP

#include "ores.trading.api/domain/draft_reason.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct draft_reason_key {
    std::string code;
};

struct draft_reason_write {
    std::string code;
    std::string description;
    std::string responsible_role;
};

struct draft_reason_change {
    draft_reason_write write;
    ores::utility::domain::precondition precondition;
};

struct draft_reason_removal {
    draft_reason_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct draft_reason_lookup {
    draft_reason_key key;
    std::optional<ores::trading::domain::draft_reason> draft_reason;
};

struct draft_reasons_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct draft_reason_event {
    boost::uuids::uuid event_id;
    draft_reason_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct draft_reason_version_key {
    draft_reason_key draft_reason;
    std::uint32_t version;
};

struct draft_reason_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_draft_reasons_request {
    using response_type = struct list_draft_reasons_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.list";
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
    std::optional<draft_reasons_filter> filter;
    std::optional<std::string> as_of;
};

struct list_draft_reasons_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::draft_reason> draft_reasons;
    std::uint64_t total;
};

struct get_draft_reason_request {
    using response_type = struct get_draft_reason_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    draft_reason_key key;
};

struct get_draft_reason_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::draft_reason> draft_reason;
};

struct get_many_draft_reasons_request {
    using response_type = struct get_many_draft_reasons_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<draft_reason_key> keys;
};

struct get_many_draft_reasons_response {
    ores::utility::domain::result result;
    std::vector<draft_reason_lookup> entries;
};

struct put_draft_reason_request {
    using response_type = struct put_draft_reason_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    draft_reason_change change;
    ores::utility::domain::change_intent intent;
};

struct put_draft_reason_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::draft_reason> draft_reason;
};

struct put_many_draft_reasons_request {
    using response_type = struct put_many_draft_reasons_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<draft_reason_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_draft_reasons_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::draft_reason> draft_reasons;
};

struct delete_draft_reason_request {
    using response_type = struct delete_draft_reason_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    draft_reason_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_draft_reason_response {
    ores::utility::domain::result result;
};

struct delete_many_draft_reasons_request {
    using response_type = struct delete_many_draft_reasons_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<draft_reason_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_draft_reasons_response {
    ores::utility::domain::result result;
};

struct list_draft_reason_versions_request {
    using response_type = struct list_draft_reason_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    draft_reason_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<draft_reason_versions_filter> filter;
};

struct list_draft_reason_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::draft_reason> versions;
    std::uint64_t total;
};

struct get_draft_reason_version_request {
    using response_type = struct get_draft_reason_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    draft_reason_version_key key;
};

struct get_draft_reason_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::draft_reason> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace draft_reason_event_subjects {
inline constexpr std::string_view created = "trading.v1.draft_reasons_events.created";
inline constexpr std::string_view updated = "trading.v1.draft_reasons_events.updated";
inline constexpr std::string_view deleted = "trading.v1.draft_reasons_events.deleted";
}

}

#endif
