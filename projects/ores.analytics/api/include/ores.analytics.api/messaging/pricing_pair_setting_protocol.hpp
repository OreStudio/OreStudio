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
#ifndef ORES_ANALYTICS_API_MESSAGING_PRICING_PAIR_SETTING_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_PRICING_PAIR_SETTING_PROTOCOL_HPP

#include "ores.analytics.api/domain/pricing_pair_setting_codec.hpp"
#include "ores.analytics.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::analytics::messaging {

struct pricing_pair_setting_key {
    std::string code;

    friend bool operator==(const pricing_pair_setting_key&,
                           const pricing_pair_setting_key&) = default;
};

struct pricing_pair_setting_write {
    std::string code;
    std::string model;
    std::string engine;
    std::string pricing_engine_type_code;
    std::string scope;
    std::string name;
    std::string usual_value;
    bool is_required;
    std::string note;

    friend bool operator==(const pricing_pair_setting_write&,
                           const pricing_pair_setting_write&) = default;
};

struct pricing_pair_setting_change {
    pricing_pair_setting_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const pricing_pair_setting_change&,
                           const pricing_pair_setting_change&) = default;
};

struct pricing_pair_setting_removal {
    pricing_pair_setting_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const pricing_pair_setting_removal&,
                           const pricing_pair_setting_removal&) = default;
};

struct pricing_pair_setting_lookup {
    pricing_pair_setting_key key;
    std::optional<ores::analytics::domain::pricing_pair_setting> pricing_pair_setting;

    friend bool operator==(const pricing_pair_setting_lookup&,
                           const pricing_pair_setting_lookup&) = default;
};

struct pricing_pair_settings_filter {
    std::optional<std::vector<std::string>> code_one_of;

    friend bool operator==(const pricing_pair_settings_filter&,
                           const pricing_pair_settings_filter&) = default;
};

struct pricing_pair_setting_event {
    boost::uuids::uuid event_id;
    pricing_pair_setting_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const pricing_pair_setting_event&,
                           const pricing_pair_setting_event&) = default;
};

struct pricing_pair_setting_version_key {
    pricing_pair_setting_key pricing_pair_setting;
    std::uint32_t version;

    friend bool operator==(const pricing_pair_setting_version_key&,
                           const pricing_pair_setting_version_key&) = default;
};

struct pricing_pair_setting_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const pricing_pair_setting_versions_filter&,
                           const pricing_pair_setting_versions_filter&) = default;
};

struct list_pricing_pair_settings_request {
    using response_type = struct list_pricing_pair_settings_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_pair_settings.list";
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
    std::optional<pricing_pair_settings_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_pricing_pair_settings_request&,
                           const list_pricing_pair_settings_request&) = default;
};

struct list_pricing_pair_settings_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::pricing_pair_setting> settings;
    std::uint64_t total;

    friend bool operator==(const list_pricing_pair_settings_response&,
                           const list_pricing_pair_settings_response&) = default;
};

struct get_pricing_pair_setting_request {
    using response_type = struct get_pricing_pair_setting_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_pair_settings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_pair_setting_key key;

    friend bool operator==(const get_pricing_pair_setting_request&,
                           const get_pricing_pair_setting_request&) = default;
};

struct get_pricing_pair_setting_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::pricing_pair_setting> pricing_pair_setting;

    friend bool operator==(const get_pricing_pair_setting_response&,
                           const get_pricing_pair_setting_response&) = default;
};

struct get_many_pricing_pair_settings_request {
    using response_type = struct get_many_pricing_pair_settings_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_pair_settings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<pricing_pair_setting_key> keys;

    friend bool operator==(const get_many_pricing_pair_settings_request&,
                           const get_many_pricing_pair_settings_request&) = default;
};

struct get_many_pricing_pair_settings_response {
    ores::utility::domain::result result;
    std::vector<pricing_pair_setting_lookup> entries;

    friend bool operator==(const get_many_pricing_pair_settings_response&,
                           const get_many_pricing_pair_settings_response&) = default;
};

struct put_pricing_pair_setting_request {
    using response_type = struct put_pricing_pair_setting_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_pair_settings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_pair_setting_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_pricing_pair_setting_request&,
                           const put_pricing_pair_setting_request&) = default;
};

struct put_pricing_pair_setting_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::pricing_pair_setting> pricing_pair_setting;

    friend bool operator==(const put_pricing_pair_setting_response&,
                           const put_pricing_pair_setting_response&) = default;
};

struct put_many_pricing_pair_settings_request {
    using response_type = struct put_many_pricing_pair_settings_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_pair_settings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<pricing_pair_setting_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_pricing_pair_settings_request&,
                           const put_many_pricing_pair_settings_request&) = default;
};

struct put_many_pricing_pair_settings_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::pricing_pair_setting> settings;

    friend bool operator==(const put_many_pricing_pair_settings_response&,
                           const put_many_pricing_pair_settings_response&) = default;
};

struct delete_pricing_pair_setting_request {
    using response_type = struct delete_pricing_pair_setting_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_pair_settings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_pair_setting_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_pricing_pair_setting_request&,
                           const delete_pricing_pair_setting_request&) = default;
};

struct delete_pricing_pair_setting_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_pricing_pair_setting_response&,
                           const delete_pricing_pair_setting_response&) = default;
};

struct delete_many_pricing_pair_settings_request {
    using response_type = struct delete_many_pricing_pair_settings_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_pair_settings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<pricing_pair_setting_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_pricing_pair_settings_request&,
                           const delete_many_pricing_pair_settings_request&) = default;
};

struct delete_many_pricing_pair_settings_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_pricing_pair_settings_response&,
                           const delete_many_pricing_pair_settings_response&) = default;
};

struct list_pricing_pair_setting_versions_request {
    using response_type = struct list_pricing_pair_setting_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_pair_settings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_pair_setting_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<pricing_pair_setting_versions_filter> filter;

    friend bool operator==(const list_pricing_pair_setting_versions_request&,
                           const list_pricing_pair_setting_versions_request&) = default;
};

struct list_pricing_pair_setting_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::pricing_pair_setting> versions;
    std::uint64_t total;

    friend bool operator==(const list_pricing_pair_setting_versions_response&,
                           const list_pricing_pair_setting_versions_response&) = default;
};

struct get_pricing_pair_setting_version_request {
    using response_type = struct get_pricing_pair_setting_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_pair_settings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_pair_setting_version_key key;

    friend bool operator==(const get_pricing_pair_setting_version_request&,
                           const get_pricing_pair_setting_version_request&) = default;
};

struct get_pricing_pair_setting_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::pricing_pair_setting> version;

    friend bool operator==(const get_pricing_pair_setting_version_response&,
                           const get_pricing_pair_setting_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace pricing_pair_setting_event_subjects {
inline constexpr std::string_view created = "analytics.v1.events.pricing_pair_settings.created";
inline constexpr std::string_view updated = "analytics.v1.events.pricing_pair_settings.updated";
inline constexpr std::string_view deleted = "analytics.v1.events.pricing_pair_settings.deleted";
}
/**
 * @brief The members of pricing_pair_setting_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_pair_setting_key>) {
    return std::tuple{
        utility::codec::field{"code", &pricing_pair_setting_key::code},
    };
}

/**
 * @brief The members of pricing_pair_setting_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_pair_setting_write>) {
    return std::tuple{
        utility::codec::field{"code", &pricing_pair_setting_write::code},
        utility::codec::field{"model", &pricing_pair_setting_write::model},
        utility::codec::field{"engine", &pricing_pair_setting_write::engine},
        utility::codec::field{"pricing_engine_type_code",
                              &pricing_pair_setting_write::pricing_engine_type_code},
        utility::codec::field{"scope", &pricing_pair_setting_write::scope},
        utility::codec::field{"name", &pricing_pair_setting_write::name},
        utility::codec::field{"usual_value", &pricing_pair_setting_write::usual_value},
        utility::codec::field{"is_required", &pricing_pair_setting_write::is_required},
        utility::codec::field{"note", &pricing_pair_setting_write::note},
    };
}

/**
 * @brief The members of pricing_pair_setting_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_pair_setting_change>) {
    return std::tuple{
        utility::codec::field{"write", &pricing_pair_setting_change::write},
        utility::codec::field{"precondition", &pricing_pair_setting_change::precondition},
    };
}

/**
 * @brief The members of pricing_pair_setting_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_pair_setting_removal>) {
    return std::tuple{
        utility::codec::field{"key", &pricing_pair_setting_removal::key},
        utility::codec::field{"precondition", &pricing_pair_setting_removal::precondition},
    };
}

/**
 * @brief The members of pricing_pair_setting_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_pair_setting_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &pricing_pair_setting_lookup::key},
        utility::codec::field{"pricing_pair_setting",
                              &pricing_pair_setting_lookup::pricing_pair_setting},
    };
}

/**
 * @brief The members of pricing_pair_settings_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_pair_settings_filter>) {
    return std::tuple{
        utility::codec::field{"code_one_of", &pricing_pair_settings_filter::code_one_of},
    };
}

/**
 * @brief The members of pricing_pair_setting_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_pair_setting_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &pricing_pair_setting_event::event_id},
        utility::codec::field{"key", &pricing_pair_setting_event::key},
        utility::codec::field{"action", &pricing_pair_setting_event::action},
        utility::codec::field{"version", &pricing_pair_setting_event::version},
        utility::codec::field{"occurred_at", &pricing_pair_setting_event::occurred_at},
        utility::codec::field{"correlation_id", &pricing_pair_setting_event::correlation_id},
    };
}

/**
 * @brief The members of pricing_pair_setting_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_pair_setting_version_key>) {
    return std::tuple{
        utility::codec::field{"pricing_pair_setting",
                              &pricing_pair_setting_version_key::pricing_pair_setting},
        utility::codec::field{"version", &pricing_pair_setting_version_key::version},
    };
}

/**
 * @brief The members of pricing_pair_setting_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<pricing_pair_setting_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &pricing_pair_setting_versions_filter::version},
        utility::codec::field{"from_version", &pricing_pair_setting_versions_filter::from_version},
        utility::codec::field{"to_version", &pricing_pair_setting_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_pricing_pair_settings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_pricing_pair_settings_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_pricing_pair_settings_request::offset},
        utility::codec::field{"limit", &list_pricing_pair_settings_request::limit},
        utility::codec::field{"order", &list_pricing_pair_settings_request::order},
        utility::codec::field{"filter", &list_pricing_pair_settings_request::filter},
        utility::codec::field{"as_of", &list_pricing_pair_settings_request::as_of},
    };
}

/**
 * @brief The members of list_pricing_pair_settings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_pricing_pair_settings_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_pricing_pair_settings_response::result},
        utility::codec::field{"settings", &list_pricing_pair_settings_response::settings},
        utility::codec::field{"total", &list_pricing_pair_settings_response::total},
    };
}

/**
 * @brief The members of get_pricing_pair_setting_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_pricing_pair_setting_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_pricing_pair_setting_request::key},
    };
}

/**
 * @brief The members of get_pricing_pair_setting_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_pricing_pair_setting_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_pricing_pair_setting_response::result},
        utility::codec::field{"pricing_pair_setting",
                              &get_pricing_pair_setting_response::pricing_pair_setting},
    };
}

/**
 * @brief The members of get_many_pricing_pair_settings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_pricing_pair_settings_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_pricing_pair_settings_request::keys},
    };
}

/**
 * @brief The members of get_many_pricing_pair_settings_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_pricing_pair_settings_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_pricing_pair_settings_response::result},
        utility::codec::field{"entries", &get_many_pricing_pair_settings_response::entries},
    };
}

/**
 * @brief The members of put_pricing_pair_setting_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_pricing_pair_setting_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_pricing_pair_setting_request::change},
        utility::codec::field{"intent", &put_pricing_pair_setting_request::intent},
    };
}

/**
 * @brief The members of put_pricing_pair_setting_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_pricing_pair_setting_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_pricing_pair_setting_response::result},
        utility::codec::field{"pricing_pair_setting",
                              &put_pricing_pair_setting_response::pricing_pair_setting},
    };
}

/**
 * @brief The members of put_many_pricing_pair_settings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_pricing_pair_settings_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_pricing_pair_settings_request::changes},
        utility::codec::field{"intent", &put_many_pricing_pair_settings_request::intent},
    };
}

/**
 * @brief The members of put_many_pricing_pair_settings_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_pricing_pair_settings_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_pricing_pair_settings_response::result},
        utility::codec::field{"settings", &put_many_pricing_pair_settings_response::settings},
    };
}

/**
 * @brief The members of delete_pricing_pair_setting_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_pricing_pair_setting_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_pricing_pair_setting_request::removal},
        utility::codec::field{"intent", &delete_pricing_pair_setting_request::intent},
    };
}

/**
 * @brief The members of delete_pricing_pair_setting_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_pricing_pair_setting_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_pricing_pair_setting_response::result},
    };
}

/**
 * @brief The members of delete_many_pricing_pair_settings_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_pricing_pair_settings_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_pricing_pair_settings_request::removals},
        utility::codec::field{"intent", &delete_many_pricing_pair_settings_request::intent},
    };
}

/**
 * @brief The members of delete_many_pricing_pair_settings_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_pricing_pair_settings_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_pricing_pair_settings_response::result},
    };
}

/**
 * @brief The members of list_pricing_pair_setting_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_pricing_pair_setting_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_pricing_pair_setting_versions_request::key},
        utility::codec::field{"offset", &list_pricing_pair_setting_versions_request::offset},
        utility::codec::field{"limit", &list_pricing_pair_setting_versions_request::limit},
        utility::codec::field{"order", &list_pricing_pair_setting_versions_request::order},
        utility::codec::field{"filter", &list_pricing_pair_setting_versions_request::filter},
    };
}

/**
 * @brief The members of list_pricing_pair_setting_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_pricing_pair_setting_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_pricing_pair_setting_versions_response::result},
        utility::codec::field{"versions", &list_pricing_pair_setting_versions_response::versions},
        utility::codec::field{"total", &list_pricing_pair_setting_versions_response::total},
    };
}

/**
 * @brief The members of get_pricing_pair_setting_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_pricing_pair_setting_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_pricing_pair_setting_version_request::key},
    };
}

/**
 * @brief The members of get_pricing_pair_setting_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_pricing_pair_setting_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_pricing_pair_setting_version_response::result},
        utility::codec::field{"version", &get_pricing_pair_setting_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::pricing_pair_setting_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::pricing_pair_setting_key& v);
    static ores::analytics::messaging::pricing_pair_setting_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::pricing_pair_setting_key& v);
    static ores::analytics::messaging::pricing_pair_setting_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::pricing_pair_setting_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::pricing_pair_setting_write& v);
    static ores::analytics::messaging::pricing_pair_setting_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::pricing_pair_setting_write& v);
    static ores::analytics::messaging::pricing_pair_setting_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::pricing_pair_setting_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::pricing_pair_setting_change& v);
    static ores::analytics::messaging::pricing_pair_setting_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::pricing_pair_setting_change& v);
    static ores::analytics::messaging::pricing_pair_setting_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::pricing_pair_setting_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::pricing_pair_setting_removal& v);
    static ores::analytics::messaging::pricing_pair_setting_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::pricing_pair_setting_removal& v);
    static ores::analytics::messaging::pricing_pair_setting_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::pricing_pair_setting_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::pricing_pair_setting_lookup& v);
    static ores::analytics::messaging::pricing_pair_setting_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::pricing_pair_setting_lookup& v);
    static ores::analytics::messaging::pricing_pair_setting_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::pricing_pair_settings_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::pricing_pair_settings_filter& v);
    static ores::analytics::messaging::pricing_pair_settings_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::pricing_pair_settings_filter& v);
    static ores::analytics::messaging::pricing_pair_settings_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::pricing_pair_setting_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::pricing_pair_setting_event& v);
    static ores::analytics::messaging::pricing_pair_setting_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::pricing_pair_setting_event& v);
    static ores::analytics::messaging::pricing_pair_setting_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::pricing_pair_setting_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::pricing_pair_setting_version_key& v);
    static ores::analytics::messaging::pricing_pair_setting_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::pricing_pair_setting_version_key& v);
    static ores::analytics::messaging::pricing_pair_setting_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::pricing_pair_setting_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::pricing_pair_setting_versions_filter& v);
    static ores::analytics::messaging::pricing_pair_setting_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::pricing_pair_setting_versions_filter& v);
    static ores::analytics::messaging::pricing_pair_setting_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_pricing_pair_settings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_pricing_pair_settings_request& v);
    static ores::analytics::messaging::list_pricing_pair_settings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_pricing_pair_settings_request& v);
    static ores::analytics::messaging::list_pricing_pair_settings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_pricing_pair_settings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_pricing_pair_settings_response& v);
    static ores::analytics::messaging::list_pricing_pair_settings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_pricing_pair_settings_response& v);
    static ores::analytics::messaging::list_pricing_pair_settings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_pricing_pair_setting_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_pricing_pair_setting_request& v);
    static ores::analytics::messaging::get_pricing_pair_setting_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_pricing_pair_setting_request& v);
    static ores::analytics::messaging::get_pricing_pair_setting_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_pricing_pair_setting_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_pricing_pair_setting_response& v);
    static ores::analytics::messaging::get_pricing_pair_setting_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_pricing_pair_setting_response& v);
    static ores::analytics::messaging::get_pricing_pair_setting_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_pricing_pair_settings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_pricing_pair_settings_request& v);
    static ores::analytics::messaging::get_many_pricing_pair_settings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_pricing_pair_settings_request& v);
    static ores::analytics::messaging::get_many_pricing_pair_settings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_pricing_pair_settings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_pricing_pair_settings_response& v);
    static ores::analytics::messaging::get_many_pricing_pair_settings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_pricing_pair_settings_response& v);
    static ores::analytics::messaging::get_many_pricing_pair_settings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_pricing_pair_setting_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_pricing_pair_setting_request& v);
    static ores::analytics::messaging::put_pricing_pair_setting_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_pricing_pair_setting_request& v);
    static ores::analytics::messaging::put_pricing_pair_setting_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_pricing_pair_setting_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_pricing_pair_setting_response& v);
    static ores::analytics::messaging::put_pricing_pair_setting_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_pricing_pair_setting_response& v);
    static ores::analytics::messaging::put_pricing_pair_setting_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_pricing_pair_settings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_pricing_pair_settings_request& v);
    static ores::analytics::messaging::put_many_pricing_pair_settings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_pricing_pair_settings_request& v);
    static ores::analytics::messaging::put_many_pricing_pair_settings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_pricing_pair_settings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_pricing_pair_settings_response& v);
    static ores::analytics::messaging::put_many_pricing_pair_settings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_pricing_pair_settings_response& v);
    static ores::analytics::messaging::put_many_pricing_pair_settings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_pricing_pair_setting_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_pricing_pair_setting_request& v);
    static ores::analytics::messaging::delete_pricing_pair_setting_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_pricing_pair_setting_request& v);
    static ores::analytics::messaging::delete_pricing_pair_setting_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_pricing_pair_setting_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_pricing_pair_setting_response& v);
    static ores::analytics::messaging::delete_pricing_pair_setting_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_pricing_pair_setting_response& v);
    static ores::analytics::messaging::delete_pricing_pair_setting_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_pricing_pair_settings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_pricing_pair_settings_request& v);
    static ores::analytics::messaging::delete_many_pricing_pair_settings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_many_pricing_pair_settings_request& v);
    static ores::analytics::messaging::delete_many_pricing_pair_settings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_pricing_pair_settings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_pricing_pair_settings_response& v);
    static ores::analytics::messaging::delete_many_pricing_pair_settings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_many_pricing_pair_settings_response& v);
    static ores::analytics::messaging::delete_many_pricing_pair_settings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_pricing_pair_setting_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_pricing_pair_setting_versions_request& v);
    static ores::analytics::messaging::list_pricing_pair_setting_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_pricing_pair_setting_versions_request& v);
    static ores::analytics::messaging::list_pricing_pair_setting_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_pricing_pair_setting_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_pricing_pair_setting_versions_response& v);
    static ores::analytics::messaging::list_pricing_pair_setting_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_pricing_pair_setting_versions_response& v);
    static ores::analytics::messaging::list_pricing_pair_setting_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_pricing_pair_setting_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_pricing_pair_setting_version_request& v);
    static ores::analytics::messaging::get_pricing_pair_setting_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_pricing_pair_setting_version_request& v);
    static ores::analytics::messaging::get_pricing_pair_setting_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_pricing_pair_setting_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_pricing_pair_setting_version_response& v);
    static ores::analytics::messaging::get_pricing_pair_setting_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_pricing_pair_setting_version_response& v);
    static ores::analytics::messaging::get_pricing_pair_setting_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
