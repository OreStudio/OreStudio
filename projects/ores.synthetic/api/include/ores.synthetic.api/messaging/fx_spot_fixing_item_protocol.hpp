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
#ifndef ORES_SYNTHETIC_API_MESSAGING_FX_SPOT_FIXING_ITEM_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FX_SPOT_FIXING_ITEM_PROTOCOL_HPP

#include "ores.synthetic.api/domain/fx_spot_fixing_item.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct fx_spot_fixing_item_key {
    boost::uuids::uuid id;
};

struct fx_spot_fixing_item_write {
    boost::uuids::uuid id;
    boost::uuids::uuid fx_spot_config_id;
    int rank;
    std::string item;
    std::string provider_code;
    std::string policy;
    int window_days;
};

struct fx_spot_fixing_item_change {
    fx_spot_fixing_item_write write;
    ores::utility::domain::precondition precondition;
};

struct fx_spot_fixing_item_removal {
    fx_spot_fixing_item_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct fx_spot_fixing_item_lookup {
    fx_spot_fixing_item_key key;
    std::optional<ores::synthetic::domain::fx_spot_fixing_item> fx_spot_fixing_item;
};

struct fx_spot_fixing_items_filter {
    std::optional<boost::uuids::uuid> fx_spot_config_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> fx_spot_config_id_one_of;
};

struct fx_spot_fixing_item_event {
    boost::uuids::uuid event_id;
    fx_spot_fixing_item_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct fx_spot_fixing_item_version_key {
    fx_spot_fixing_item_key fx_spot_fixing_item;
    std::uint32_t version;
};

struct fx_spot_fixing_item_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_fx_spot_fixing_items_request {
    using response_type = struct list_fx_spot_fixing_items_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.list";
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
    std::optional<fx_spot_fixing_items_filter> filter;
    std::optional<std::string> as_of;
};

struct list_fx_spot_fixing_items_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_spot_fixing_item> fx_spot_fixing_items;
    std::uint64_t total;
};

struct get_fx_spot_fixing_item_request {
    using response_type = struct get_fx_spot_fixing_item_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_fixing_item_key key;
};

struct get_fx_spot_fixing_item_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_spot_fixing_item> fx_spot_fixing_item;
};

struct get_many_fx_spot_fixing_items_request {
    using response_type = struct get_many_fx_spot_fixing_items_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_spot_fixing_item_key> keys;
};

struct get_many_fx_spot_fixing_items_response {
    ores::utility::domain::result result;
    std::vector<fx_spot_fixing_item_lookup> entries;
};

struct put_fx_spot_fixing_item_request {
    using response_type = struct put_fx_spot_fixing_item_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_fixing_item_change change;
    ores::utility::domain::change_intent intent;
};

struct put_fx_spot_fixing_item_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_spot_fixing_item> fx_spot_fixing_item;
};

struct put_many_fx_spot_fixing_items_request {
    using response_type = struct put_many_fx_spot_fixing_items_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_spot_fixing_item_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_fx_spot_fixing_items_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_spot_fixing_item> fx_spot_fixing_items;
};

struct delete_fx_spot_fixing_item_request {
    using response_type = struct delete_fx_spot_fixing_item_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_fixing_item_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_fx_spot_fixing_item_response {
    ores::utility::domain::result result;
};

struct delete_many_fx_spot_fixing_items_request {
    using response_type = struct delete_many_fx_spot_fixing_items_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_fixing_items.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_spot_fixing_item_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_fx_spot_fixing_items_response {
    ores::utility::domain::result result;
};

struct list_fx_spot_fixing_item_versions_request {
    using response_type = struct list_fx_spot_fixing_item_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_fixing_items_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_fixing_item_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fx_spot_fixing_item_versions_filter> filter;
};

struct list_fx_spot_fixing_item_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_spot_fixing_item> versions;
    std::uint64_t total;
};

struct get_fx_spot_fixing_item_version_request {
    using response_type = struct get_fx_spot_fixing_item_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_fixing_items_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_fixing_item_version_key key;
};

struct get_fx_spot_fixing_item_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_spot_fixing_item> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fx_spot_fixing_item_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.fx_spot_fixing_items_events.created";
inline constexpr std::string_view updated = "synthetic.v1.fx_spot_fixing_items_events.updated";
inline constexpr std::string_view deleted = "synthetic.v1.fx_spot_fixing_items_events.deleted";
}

}

#endif
