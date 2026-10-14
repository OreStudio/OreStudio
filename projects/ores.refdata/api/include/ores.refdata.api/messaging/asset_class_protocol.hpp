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
#ifndef ORES_REFDATA_API_MESSAGING_ASSET_CLASS_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_ASSET_CLASS_PROTOCOL_HPP

#include "ores.refdata.api/domain/asset_class.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct asset_class_key {
    std::string code;
    std::string coding_scheme_code;
};

struct asset_class_write {
    std::string code;
    std::string coding_scheme_code;
    std::string source;
    std::string description;
};

struct asset_class_change {
    asset_class_write write;
    ores::utility::domain::precondition precondition;
};

struct asset_class_removal {
    asset_class_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct asset_class_lookup {
    asset_class_key key;
    std::optional<ores::refdata::domain::asset_class> asset_class;
};

struct asset_class_event {
    boost::uuids::uuid event_id;
    asset_class_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct asset_class_version_key {
    asset_class_key asset_class;
    std::uint32_t version;
};

struct asset_class_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_asset_classes_request {
    using response_type = struct list_asset_classes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_classes.list";
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
    std::optional<std::string> as_of;
};

struct list_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::asset_class> classes;
    std::uint64_t total;
};

struct get_asset_class_request {
    using response_type = struct get_asset_class_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_classes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_class_key key;
};

struct get_asset_class_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::asset_class> asset_class;
};

struct get_many_asset_classes_request {
    using response_type = struct get_many_asset_classes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_classes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<asset_class_key> keys;
};

struct get_many_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<asset_class_lookup> entries;
};

struct put_asset_class_request {
    using response_type = struct put_asset_class_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_classes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_class_change change;
    ores::utility::domain::change_intent intent;
};

struct put_asset_class_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::asset_class> asset_class;
};

struct put_many_asset_classes_request {
    using response_type = struct put_many_asset_classes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_classes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<asset_class_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::asset_class> classes;
};

struct delete_asset_class_request {
    using response_type = struct delete_asset_class_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_classes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_class_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_asset_class_response {
    ores::utility::domain::result result;
};

struct delete_many_asset_classes_request {
    using response_type = struct delete_many_asset_classes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_classes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<asset_class_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_asset_classes_response {
    ores::utility::domain::result result;
};

struct list_asset_class_versions_request {
    using response_type = struct list_asset_class_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_classes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_class_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<asset_class_versions_filter> filter;
};

struct list_asset_class_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::asset_class> versions;
    std::uint64_t total;
};

struct get_asset_class_version_request {
    using response_type = struct get_asset_class_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_classes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_class_version_key key;
};

struct get_asset_class_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::asset_class> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace asset_class_event_subjects {
inline constexpr std::string_view created = "refdata.v1.asset_classes_events.created";
inline constexpr std::string_view updated = "refdata.v1.asset_classes_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.asset_classes_events.deleted";
}

}

#endif
