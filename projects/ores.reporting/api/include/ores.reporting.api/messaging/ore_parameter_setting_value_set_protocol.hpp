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
#ifndef ORES_REPORTING_API_MESSAGING_ORE_PARAMETER_SETTING_VALUE_SET_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_ORE_PARAMETER_SETTING_VALUE_SET_PROTOCOL_HPP

#include "ores.reporting.api/domain/ore_parameter_setting_value_set.hpp"
#include "ores.reporting.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::reporting::messaging {

struct ore_parameter_setting_value_set_key {
    std::string code;
};

struct ore_parameter_setting_value_set_write {
    std::string code;
    std::string scope;
    std::string name;
    std::string value_set_code;
};

struct ore_parameter_setting_value_set_change {
    ore_parameter_setting_value_set_write write;
    ores::utility::domain::precondition precondition;
};

struct ore_parameter_setting_value_set_removal {
    ore_parameter_setting_value_set_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct ore_parameter_setting_value_set_lookup {
    ore_parameter_setting_value_set_key key;
    std::optional<ores::reporting::domain::ore_parameter_setting_value_set>
        ore_parameter_setting_value_set;
};

struct ore_parameter_setting_value_sets_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct ore_parameter_setting_value_set_event {
    boost::uuids::uuid event_id;
    ore_parameter_setting_value_set_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct ore_parameter_setting_value_set_version_key {
    ore_parameter_setting_value_set_key ore_parameter_setting_value_set;
    std::uint32_t version;
};

struct ore_parameter_setting_value_set_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_ore_parameter_setting_value_sets_request {
    using response_type = struct list_ore_parameter_setting_value_sets_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_parameter_setting_value_sets.list";
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
    std::optional<ore_parameter_setting_value_sets_filter> filter;
    std::optional<std::string> as_of;
};

struct list_ore_parameter_setting_value_sets_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_parameter_setting_value_set> setting_value_sets;
    std::uint64_t total;
};

struct get_ore_parameter_setting_value_set_request {
    using response_type = struct get_ore_parameter_setting_value_set_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_parameter_setting_value_sets.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_parameter_setting_value_set_key key;
};

struct get_ore_parameter_setting_value_set_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::ore_parameter_setting_value_set>
        ore_parameter_setting_value_set;
};

struct get_many_ore_parameter_setting_value_sets_request {
    using response_type = struct get_many_ore_parameter_setting_value_sets_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_parameter_setting_value_sets.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ore_parameter_setting_value_set_key> keys;
};

struct get_many_ore_parameter_setting_value_sets_response {
    ores::utility::domain::result result;
    std::vector<ore_parameter_setting_value_set_lookup> entries;
};

struct put_ore_parameter_setting_value_set_request {
    using response_type = struct put_ore_parameter_setting_value_set_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_parameter_setting_value_sets.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_parameter_setting_value_set_change change;
    ores::utility::domain::change_intent intent;
};

struct put_ore_parameter_setting_value_set_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::ore_parameter_setting_value_set>
        ore_parameter_setting_value_set;
};

struct put_many_ore_parameter_setting_value_sets_request {
    using response_type = struct put_many_ore_parameter_setting_value_sets_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_parameter_setting_value_sets.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ore_parameter_setting_value_set_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_ore_parameter_setting_value_sets_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_parameter_setting_value_set> setting_value_sets;
};

struct delete_ore_parameter_setting_value_set_request {
    using response_type = struct delete_ore_parameter_setting_value_set_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_parameter_setting_value_sets.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_parameter_setting_value_set_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_ore_parameter_setting_value_set_response {
    ores::utility::domain::result result;
};

struct delete_many_ore_parameter_setting_value_sets_request {
    using response_type = struct delete_many_ore_parameter_setting_value_sets_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_parameter_setting_value_sets.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ore_parameter_setting_value_set_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_ore_parameter_setting_value_sets_response {
    ores::utility::domain::result result;
};

struct list_ore_parameter_setting_value_set_versions_request {
    using response_type = struct list_ore_parameter_setting_value_set_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_parameter_setting_value_sets_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_parameter_setting_value_set_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<ore_parameter_setting_value_set_versions_filter> filter;
};

struct list_ore_parameter_setting_value_set_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_parameter_setting_value_set> versions;
    std::uint64_t total;
};

struct get_ore_parameter_setting_value_set_version_request {
    using response_type = struct get_ore_parameter_setting_value_set_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_parameter_setting_value_sets_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_parameter_setting_value_set_version_key key;
};

struct get_ore_parameter_setting_value_set_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::ore_parameter_setting_value_set> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace ore_parameter_setting_value_set_event_subjects {
inline constexpr std::string_view created =
    "reporting.v1.ore_parameter_setting_value_sets_events.created";
inline constexpr std::string_view updated =
    "reporting.v1.ore_parameter_setting_value_sets_events.updated";
inline constexpr std::string_view deleted =
    "reporting.v1.ore_parameter_setting_value_sets_events.deleted";
}
/**
 * @brief The members of ore_parameter_setting_value_set_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_parameter_setting_value_set_key&) {
    return std::tuple{
        utility::codec::field{"code", &ore_parameter_setting_value_set_key::code},
    };
}

/**
 * @brief The members of ore_parameter_setting_value_set_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_parameter_setting_value_set_write&) {
    return std::tuple{
        utility::codec::field{"code", &ore_parameter_setting_value_set_write::code},
        utility::codec::field{"scope", &ore_parameter_setting_value_set_write::scope},
        utility::codec::field{"name", &ore_parameter_setting_value_set_write::name},
        utility::codec::field{"value_set_code",
                              &ore_parameter_setting_value_set_write::value_set_code},
    };
}

/**
 * @brief The members of ore_parameter_setting_value_set_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_parameter_setting_value_set_change&) {
    return std::tuple{
        utility::codec::field{"write", &ore_parameter_setting_value_set_change::write},
        utility::codec::field{"precondition",
                              &ore_parameter_setting_value_set_change::precondition},
    };
}

/**
 * @brief The members of ore_parameter_setting_value_set_removal, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_parameter_setting_value_set_removal&) {
    return std::tuple{
        utility::codec::field{"key", &ore_parameter_setting_value_set_removal::key},
        utility::codec::field{"precondition",
                              &ore_parameter_setting_value_set_removal::precondition},
    };
}

/**
 * @brief The members of ore_parameter_setting_value_set_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_parameter_setting_value_set_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &ore_parameter_setting_value_set_lookup::key},
        utility::codec::field{
            "ore_parameter_setting_value_set",
            &ore_parameter_setting_value_set_lookup::ore_parameter_setting_value_set},
    };
}

/**
 * @brief The members of ore_parameter_setting_value_sets_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_parameter_setting_value_sets_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &ore_parameter_setting_value_sets_filter::code_one_of},
    };
}

/**
 * @brief The members of ore_parameter_setting_value_set_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_parameter_setting_value_set_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &ore_parameter_setting_value_set_event::event_id},
        utility::codec::field{"key", &ore_parameter_setting_value_set_event::key},
        utility::codec::field{"action", &ore_parameter_setting_value_set_event::action},
        utility::codec::field{"version", &ore_parameter_setting_value_set_event::version},
        utility::codec::field{"occurred_at", &ore_parameter_setting_value_set_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &ore_parameter_setting_value_set_event::correlation_id},
    };
}

/**
 * @brief The members of ore_parameter_setting_value_set_version_key, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_parameter_setting_value_set_version_key&) {
    return std::tuple{
        utility::codec::field{
            "ore_parameter_setting_value_set",
            &ore_parameter_setting_value_set_version_key::ore_parameter_setting_value_set},
        utility::codec::field{"version", &ore_parameter_setting_value_set_version_key::version},
    };
}

/**
 * @brief The members of ore_parameter_setting_value_set_versions_filter, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_parameter_setting_value_set_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &ore_parameter_setting_value_set_versions_filter::version},
        utility::codec::field{"from_version",
                              &ore_parameter_setting_value_set_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &ore_parameter_setting_value_set_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_ore_parameter_setting_value_sets_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_ore_parameter_setting_value_sets_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_ore_parameter_setting_value_sets_request::offset},
        utility::codec::field{"limit", &list_ore_parameter_setting_value_sets_request::limit},
        utility::codec::field{"order", &list_ore_parameter_setting_value_sets_request::order},
        utility::codec::field{"filter", &list_ore_parameter_setting_value_sets_request::filter},
        utility::codec::field{"as_of", &list_ore_parameter_setting_value_sets_request::as_of},
    };
}

/**
 * @brief The members of list_ore_parameter_setting_value_sets_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_ore_parameter_setting_value_sets_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_ore_parameter_setting_value_sets_response::result},
        utility::codec::field{"setting_value_sets",
                              &list_ore_parameter_setting_value_sets_response::setting_value_sets},
        utility::codec::field{"total", &list_ore_parameter_setting_value_sets_response::total},
    };
}

/**
 * @brief The members of get_ore_parameter_setting_value_set_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_ore_parameter_setting_value_set_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_ore_parameter_setting_value_set_request::key},
    };
}

/**
 * @brief The members of get_ore_parameter_setting_value_set_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_ore_parameter_setting_value_set_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_ore_parameter_setting_value_set_response::result},
        utility::codec::field{
            "ore_parameter_setting_value_set",
            &get_ore_parameter_setting_value_set_response::ore_parameter_setting_value_set},
    };
}

/**
 * @brief The members of get_many_ore_parameter_setting_value_sets_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_ore_parameter_setting_value_sets_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_ore_parameter_setting_value_sets_request::keys},
    };
}

/**
 * @brief The members of get_many_ore_parameter_setting_value_sets_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_many_ore_parameter_setting_value_sets_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_many_ore_parameter_setting_value_sets_response::result},
        utility::codec::field{"entries",
                              &get_many_ore_parameter_setting_value_sets_response::entries},
    };
}

/**
 * @brief The members of put_ore_parameter_setting_value_set_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_ore_parameter_setting_value_set_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_ore_parameter_setting_value_set_request::change},
        utility::codec::field{"intent", &put_ore_parameter_setting_value_set_request::intent},
    };
}

/**
 * @brief The members of put_ore_parameter_setting_value_set_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_ore_parameter_setting_value_set_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_ore_parameter_setting_value_set_response::result},
        utility::codec::field{
            "ore_parameter_setting_value_set",
            &put_ore_parameter_setting_value_set_response::ore_parameter_setting_value_set},
    };
}

/**
 * @brief The members of put_many_ore_parameter_setting_value_sets_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_ore_parameter_setting_value_sets_request&) {
    return std::tuple{
        utility::codec::field{"changes",
                              &put_many_ore_parameter_setting_value_sets_request::changes},
        utility::codec::field{"intent", &put_many_ore_parameter_setting_value_sets_request::intent},
    };
}

/**
 * @brief The members of put_many_ore_parameter_setting_value_sets_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const put_many_ore_parameter_setting_value_sets_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &put_many_ore_parameter_setting_value_sets_response::result},
        utility::codec::field{
            "setting_value_sets",
            &put_many_ore_parameter_setting_value_sets_response::setting_value_sets},
    };
}

/**
 * @brief The members of delete_ore_parameter_setting_value_set_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_ore_parameter_setting_value_set_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_ore_parameter_setting_value_set_request::removal},
        utility::codec::field{"intent", &delete_ore_parameter_setting_value_set_request::intent},
    };
}

/**
 * @brief The members of delete_ore_parameter_setting_value_set_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_ore_parameter_setting_value_set_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_ore_parameter_setting_value_set_response::result},
    };
}

/**
 * @brief The members of delete_many_ore_parameter_setting_value_sets_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_ore_parameter_setting_value_sets_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_ore_parameter_setting_value_sets_request::removals},
        utility::codec::field{"intent",
                              &delete_many_ore_parameter_setting_value_sets_request::intent},
    };
}

/**
 * @brief The members of delete_many_ore_parameter_setting_value_sets_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_ore_parameter_setting_value_sets_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_ore_parameter_setting_value_sets_response::result},
    };
}

/**
 * @brief The members of list_ore_parameter_setting_value_set_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_ore_parameter_setting_value_set_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_ore_parameter_setting_value_set_versions_request::key},
        utility::codec::field{"offset",
                              &list_ore_parameter_setting_value_set_versions_request::offset},
        utility::codec::field{"limit",
                              &list_ore_parameter_setting_value_set_versions_request::limit},
        utility::codec::field{"order",
                              &list_ore_parameter_setting_value_set_versions_request::order},
        utility::codec::field{"filter",
                              &list_ore_parameter_setting_value_set_versions_request::filter},
    };
}

/**
 * @brief The members of list_ore_parameter_setting_value_set_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_ore_parameter_setting_value_set_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_ore_parameter_setting_value_set_versions_response::result},
        utility::codec::field{"versions",
                              &list_ore_parameter_setting_value_set_versions_response::versions},
        utility::codec::field{"total",
                              &list_ore_parameter_setting_value_set_versions_response::total},
    };
}

/**
 * @brief The members of get_ore_parameter_setting_value_set_version_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_ore_parameter_setting_value_set_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_ore_parameter_setting_value_set_version_request::key},
    };
}

/**
 * @brief The members of get_ore_parameter_setting_value_set_version_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_ore_parameter_setting_value_set_version_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_ore_parameter_setting_value_set_version_response::result},
        utility::codec::field{"version",
                              &get_ore_parameter_setting_value_set_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::ore_parameter_setting_value_set_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::ore_parameter_setting_value_set_key& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::ore_parameter_setting_value_set_key& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::ore_parameter_setting_value_set_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::ore_parameter_setting_value_set_write& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::ore_parameter_setting_value_set_write& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::ore_parameter_setting_value_set_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::ore_parameter_setting_value_set_change& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::ore_parameter_setting_value_set_change& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::ore_parameter_setting_value_set_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::ore_parameter_setting_value_set_removal& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::ore_parameter_setting_value_set_removal& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::ore_parameter_setting_value_set_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::ore_parameter_setting_value_set_lookup& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::ore_parameter_setting_value_set_lookup& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::ore_parameter_setting_value_sets_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::ore_parameter_setting_value_sets_filter& v);
    static ores::reporting::messaging::ore_parameter_setting_value_sets_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::ore_parameter_setting_value_sets_filter& v);
    static ores::reporting::messaging::ore_parameter_setting_value_sets_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::ore_parameter_setting_value_set_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::ore_parameter_setting_value_set_event& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::ore_parameter_setting_value_set_event& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::ore_parameter_setting_value_set_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::ore_parameter_setting_value_set_version_key& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::ore_parameter_setting_value_set_version_key& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::ore_parameter_setting_value_set_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::ore_parameter_setting_value_set_versions_filter& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::ore_parameter_setting_value_set_versions_filter& v);
    static ores::reporting::messaging::ore_parameter_setting_value_set_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_ore_parameter_setting_value_sets_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_ore_parameter_setting_value_sets_request& v);
    static ores::reporting::messaging::list_ore_parameter_setting_value_sets_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_ore_parameter_setting_value_sets_request& v);
    static ores::reporting::messaging::list_ore_parameter_setting_value_sets_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_ore_parameter_setting_value_sets_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_ore_parameter_setting_value_sets_response& v);
    static ores::reporting::messaging::list_ore_parameter_setting_value_sets_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_ore_parameter_setting_value_sets_response& v);
    static ores::reporting::messaging::list_ore_parameter_setting_value_sets_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_ore_parameter_setting_value_set_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_ore_parameter_setting_value_set_request& v);
    static ores::reporting::messaging::get_ore_parameter_setting_value_set_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_ore_parameter_setting_value_set_request& v);
    static ores::reporting::messaging::get_ore_parameter_setting_value_set_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_ore_parameter_setting_value_set_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_ore_parameter_setting_value_set_response& v);
    static ores::reporting::messaging::get_ore_parameter_setting_value_set_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_ore_parameter_setting_value_set_response& v);
    static ores::reporting::messaging::get_ore_parameter_setting_value_set_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_ore_parameter_setting_value_sets_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::get_many_ore_parameter_setting_value_sets_request& v);
    static ores::reporting::messaging::get_many_ore_parameter_setting_value_sets_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_many_ore_parameter_setting_value_sets_request& v);
    static ores::reporting::messaging::get_many_ore_parameter_setting_value_sets_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_ore_parameter_setting_value_sets_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::get_many_ore_parameter_setting_value_sets_response& v);
    static ores::reporting::messaging::get_many_ore_parameter_setting_value_sets_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_many_ore_parameter_setting_value_sets_response& v);
    static ores::reporting::messaging::get_many_ore_parameter_setting_value_sets_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_ore_parameter_setting_value_set_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_ore_parameter_setting_value_set_request& v);
    static ores::reporting::messaging::put_ore_parameter_setting_value_set_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_ore_parameter_setting_value_set_request& v);
    static ores::reporting::messaging::put_ore_parameter_setting_value_set_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_ore_parameter_setting_value_set_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_ore_parameter_setting_value_set_response& v);
    static ores::reporting::messaging::put_ore_parameter_setting_value_set_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::put_ore_parameter_setting_value_set_response& v);
    static ores::reporting::messaging::put_ore_parameter_setting_value_set_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_ore_parameter_setting_value_sets_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::put_many_ore_parameter_setting_value_sets_request& v);
    static ores::reporting::messaging::put_many_ore_parameter_setting_value_sets_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::put_many_ore_parameter_setting_value_sets_request& v);
    static ores::reporting::messaging::put_many_ore_parameter_setting_value_sets_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_ore_parameter_setting_value_sets_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::put_many_ore_parameter_setting_value_sets_response& v);
    static ores::reporting::messaging::put_many_ore_parameter_setting_value_sets_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::put_many_ore_parameter_setting_value_sets_response& v);
    static ores::reporting::messaging::put_many_ore_parameter_setting_value_sets_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_ore_parameter_setting_value_set_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_ore_parameter_setting_value_set_request& v);
    static ores::reporting::messaging::delete_ore_parameter_setting_value_set_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_ore_parameter_setting_value_set_request& v);
    static ores::reporting::messaging::delete_ore_parameter_setting_value_set_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_ore_parameter_setting_value_set_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::delete_ore_parameter_setting_value_set_response& v);
    static ores::reporting::messaging::delete_ore_parameter_setting_value_set_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_ore_parameter_setting_value_set_response& v);
    static ores::reporting::messaging::delete_ore_parameter_setting_value_set_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_ore_parameter_setting_value_sets_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::delete_many_ore_parameter_setting_value_sets_request& v);
    static ores::reporting::messaging::delete_many_ore_parameter_setting_value_sets_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_many_ore_parameter_setting_value_sets_request& v);
    static ores::reporting::messaging::delete_many_ore_parameter_setting_value_sets_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_ore_parameter_setting_value_sets_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::delete_many_ore_parameter_setting_value_sets_response& v);
    static ores::reporting::messaging::delete_many_ore_parameter_setting_value_sets_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_many_ore_parameter_setting_value_sets_response& v);
    static ores::reporting::messaging::delete_many_ore_parameter_setting_value_sets_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_ore_parameter_setting_value_set_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_ore_parameter_setting_value_set_versions_request& v);
    static ores::reporting::messaging::list_ore_parameter_setting_value_set_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_ore_parameter_setting_value_set_versions_request& v);
    static ores::reporting::messaging::list_ore_parameter_setting_value_set_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_ore_parameter_setting_value_set_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_ore_parameter_setting_value_set_versions_response&
            v);
    static ores::reporting::messaging::list_ore_parameter_setting_value_set_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_ore_parameter_setting_value_set_versions_response&
            v);
    static ores::reporting::messaging::list_ore_parameter_setting_value_set_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_ore_parameter_setting_value_set_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::get_ore_parameter_setting_value_set_version_request& v);
    static ores::reporting::messaging::get_ore_parameter_setting_value_set_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_ore_parameter_setting_value_set_version_request& v);
    static ores::reporting::messaging::get_ore_parameter_setting_value_set_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_ore_parameter_setting_value_set_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::get_ore_parameter_setting_value_set_version_response& v);
    static ores::reporting::messaging::get_ore_parameter_setting_value_set_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_ore_parameter_setting_value_set_version_response& v);
    static ores::reporting::messaging::get_ore_parameter_setting_value_set_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
