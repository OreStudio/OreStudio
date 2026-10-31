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
#ifndef ORES_SYNTHETIC_API_MESSAGING_CALIBRATION_FALLBACK_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_CALIBRATION_FALLBACK_PROTOCOL_HPP

#include "ores.synthetic.api/domain/calibration_fallback.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct calibration_fallback_key {
    std::string code;
};

struct calibration_fallback_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct calibration_fallback_change {
    calibration_fallback_write write;
    ores::utility::domain::precondition precondition;
};

struct calibration_fallback_removal {
    calibration_fallback_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct calibration_fallback_lookup {
    calibration_fallback_key key;
    std::optional<ores::synthetic::domain::calibration_fallback> calibration_fallback;
};

struct calibration_fallbacks_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct calibration_fallback_event {
    boost::uuids::uuid event_id;
    calibration_fallback_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct calibration_fallback_version_key {
    calibration_fallback_key calibration_fallback;
    std::uint32_t version;
};

struct calibration_fallback_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_calibration_fallbacks_request {
    using response_type = struct list_calibration_fallbacks_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_fallbacks.list";
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
    std::optional<calibration_fallbacks_filter> filter;
    std::optional<std::string> as_of;
};

struct list_calibration_fallbacks_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::calibration_fallback> fallbacks;
    std::uint64_t total;
};

struct get_calibration_fallback_request {
    using response_type = struct get_calibration_fallback_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_fallbacks.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_fallback_key key;
};

struct get_calibration_fallback_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::calibration_fallback> calibration_fallback;
};

struct get_many_calibration_fallbacks_request {
    using response_type = struct get_many_calibration_fallbacks_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_fallbacks.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calibration_fallback_key> keys;
};

struct get_many_calibration_fallbacks_response {
    ores::utility::domain::result result;
    std::vector<calibration_fallback_lookup> entries;
};

struct put_calibration_fallback_request {
    using response_type = struct put_calibration_fallback_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_fallbacks.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_fallback_change change;
    ores::utility::domain::change_intent intent;
};

struct put_calibration_fallback_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::calibration_fallback> calibration_fallback;
};

struct put_many_calibration_fallbacks_request {
    using response_type = struct put_many_calibration_fallbacks_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_fallbacks.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calibration_fallback_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_calibration_fallbacks_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::calibration_fallback> fallbacks;
};

struct delete_calibration_fallback_request {
    using response_type = struct delete_calibration_fallback_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_fallbacks.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_fallback_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_calibration_fallback_response {
    ores::utility::domain::result result;
};

struct delete_many_calibration_fallbacks_request {
    using response_type = struct delete_many_calibration_fallbacks_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_fallbacks.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calibration_fallback_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_calibration_fallbacks_response {
    ores::utility::domain::result result;
};

struct list_calibration_fallback_versions_request {
    using response_type = struct list_calibration_fallback_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_fallbacks_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_fallback_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<calibration_fallback_versions_filter> filter;
};

struct list_calibration_fallback_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::calibration_fallback> versions;
    std::uint64_t total;
};

struct get_calibration_fallback_version_request {
    using response_type = struct get_calibration_fallback_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_fallbacks_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_fallback_version_key key;
};

struct get_calibration_fallback_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::calibration_fallback> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace calibration_fallback_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.calibration_fallbacks_events.created";
inline constexpr std::string_view updated = "synthetic.v1.calibration_fallbacks_events.updated";
inline constexpr std::string_view deleted = "synthetic.v1.calibration_fallbacks_events.deleted";
}

}

#endif
