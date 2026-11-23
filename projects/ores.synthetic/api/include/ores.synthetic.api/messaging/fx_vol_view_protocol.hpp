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
#ifndef ORES_SYNTHETIC_API_MESSAGING_FX_VOL_VIEW_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FX_VOL_VIEW_PROTOCOL_HPP

#include "ores.synthetic.api/domain/fx_vol_view_codec.hpp"
#include "ores.synthetic.api/export.hpp"
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

namespace ores::synthetic::messaging {

struct fx_vol_view_key {
    boost::uuids::uuid id;

    friend bool operator==(const fx_vol_view_key&, const fx_vol_view_key&) = default;
};

struct fx_vol_view_write {
    boost::uuids::uuid id;
    boost::uuids::uuid feed_id;
    boost::uuids::uuid contributor_id;
    boost::uuids::uuid config_id;
    std::optional<boost::uuids::uuid> folder_id;
    std::string source_name;
    bool enabled{};
    bool auto_start{};

    friend bool operator==(const fx_vol_view_write&, const fx_vol_view_write&) = default;
};

struct fx_vol_view_change {
    fx_vol_view_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const fx_vol_view_change&, const fx_vol_view_change&) = default;
};

struct fx_vol_view_removal {
    fx_vol_view_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const fx_vol_view_removal&, const fx_vol_view_removal&) = default;
};

struct fx_vol_view_lookup {
    fx_vol_view_key key;
    std::optional<ores::synthetic::domain::fx_vol_view> fx_vol_view;

    friend bool operator==(const fx_vol_view_lookup&, const fx_vol_view_lookup&) = default;
};

struct fx_vol_views_filter {
    std::optional<boost::uuids::uuid> feed_id;
    std::optional<boost::uuids::uuid> contributor_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> feed_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> contributor_id_one_of;

    friend bool operator==(const fx_vol_views_filter&, const fx_vol_views_filter&) = default;
};

struct fx_vol_view_event {
    boost::uuids::uuid event_id;
    fx_vol_view_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const fx_vol_view_event&, const fx_vol_view_event&) = default;
};

struct fx_vol_view_version_key {
    fx_vol_view_key fx_vol_view;
    std::uint32_t version{};

    friend bool operator==(const fx_vol_view_version_key&,
                           const fx_vol_view_version_key&) = default;
};

struct fx_vol_view_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const fx_vol_view_versions_filter&,
                           const fx_vol_view_versions_filter&) = default;
};

struct list_fx_vol_views_request {
    using response_type = struct list_fx_vol_views_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_vol_views.list";
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
    std::optional<fx_vol_views_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_fx_vol_views_request&,
                           const list_fx_vol_views_request&) = default;
};

struct list_fx_vol_views_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_vol_view> fx_vol_views;
    std::uint64_t total{};

    friend bool operator==(const list_fx_vol_views_response&,
                           const list_fx_vol_views_response&) = default;
};

struct get_fx_vol_view_request {
    using response_type = struct get_fx_vol_view_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_vol_views.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_vol_view_key key;

    friend bool operator==(const get_fx_vol_view_request&,
                           const get_fx_vol_view_request&) = default;
};

struct get_fx_vol_view_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_vol_view> fx_vol_view;

    friend bool operator==(const get_fx_vol_view_response&,
                           const get_fx_vol_view_response&) = default;
};

struct get_many_fx_vol_views_request {
    using response_type = struct get_many_fx_vol_views_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_vol_views.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_vol_view_key> keys;

    friend bool operator==(const get_many_fx_vol_views_request&,
                           const get_many_fx_vol_views_request&) = default;
};

struct get_many_fx_vol_views_response {
    ores::utility::domain::result result;
    std::vector<fx_vol_view_lookup> entries;

    friend bool operator==(const get_many_fx_vol_views_response&,
                           const get_many_fx_vol_views_response&) = default;
};

struct put_fx_vol_view_request {
    using response_type = struct put_fx_vol_view_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_vol_views.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_vol_view_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_fx_vol_view_request&,
                           const put_fx_vol_view_request&) = default;
};

struct put_fx_vol_view_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_vol_view> fx_vol_view;

    friend bool operator==(const put_fx_vol_view_response&,
                           const put_fx_vol_view_response&) = default;
};

struct put_many_fx_vol_views_request {
    using response_type = struct put_many_fx_vol_views_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_vol_views.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_vol_view_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_fx_vol_views_request&,
                           const put_many_fx_vol_views_request&) = default;
};

struct put_many_fx_vol_views_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_vol_view> fx_vol_views;

    friend bool operator==(const put_many_fx_vol_views_response&,
                           const put_many_fx_vol_views_response&) = default;
};

struct delete_fx_vol_view_request {
    using response_type = struct delete_fx_vol_view_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_vol_views.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_vol_view_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_fx_vol_view_request&,
                           const delete_fx_vol_view_request&) = default;
};

struct delete_fx_vol_view_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_fx_vol_view_response&,
                           const delete_fx_vol_view_response&) = default;
};

struct delete_many_fx_vol_views_request {
    using response_type = struct delete_many_fx_vol_views_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_vol_views.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_vol_view_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_fx_vol_views_request&,
                           const delete_many_fx_vol_views_request&) = default;
};

struct delete_many_fx_vol_views_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_fx_vol_views_response&,
                           const delete_many_fx_vol_views_response&) = default;
};

struct list_fx_vol_view_versions_request {
    using response_type = struct list_fx_vol_view_versions_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_vol_views_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_vol_view_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fx_vol_view_versions_filter> filter;

    friend bool operator==(const list_fx_vol_view_versions_request&,
                           const list_fx_vol_view_versions_request&) = default;
};

struct list_fx_vol_view_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_vol_view> versions;
    std::uint64_t total{};

    friend bool operator==(const list_fx_vol_view_versions_response&,
                           const list_fx_vol_view_versions_response&) = default;
};

struct get_fx_vol_view_version_request {
    using response_type = struct get_fx_vol_view_version_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_vol_views_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_vol_view_version_key key;

    friend bool operator==(const get_fx_vol_view_version_request&,
                           const get_fx_vol_view_version_request&) = default;
};

struct get_fx_vol_view_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_vol_view> version;

    friend bool operator==(const get_fx_vol_view_version_response&,
                           const get_fx_vol_view_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fx_vol_view_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.events.fx_vol_views.created";
inline constexpr std::string_view updated = "synthetic.v1.events.fx_vol_views.updated";
inline constexpr std::string_view deleted = "synthetic.v1.events.fx_vol_views.deleted";
}
/**
 * @brief The members of fx_vol_view_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_view_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &fx_vol_view_key::id},
    };
}

/**
 * @brief The members of fx_vol_view_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_view_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &fx_vol_view_write::id},
        utility::codec::field{"feed_id", &fx_vol_view_write::feed_id},
        utility::codec::field{"contributor_id", &fx_vol_view_write::contributor_id},
        utility::codec::field{"config_id", &fx_vol_view_write::config_id},
        utility::codec::field{"folder_id", &fx_vol_view_write::folder_id},
        utility::codec::field{"source_name", &fx_vol_view_write::source_name},
        utility::codec::field{"enabled", &fx_vol_view_write::enabled},
        utility::codec::field{"auto_start", &fx_vol_view_write::auto_start},
    };
}

/**
 * @brief The members of fx_vol_view_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_view_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &fx_vol_view_change::write},
        utility::codec::field{"precondition", &fx_vol_view_change::precondition},
    };
}

/**
 * @brief The members of fx_vol_view_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_view_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &fx_vol_view_removal::key},
        utility::codec::field{"precondition", &fx_vol_view_removal::precondition},
    };
}

/**
 * @brief The members of fx_vol_view_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_view_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &fx_vol_view_lookup::key},
        utility::codec::field{"fx_vol_view", &fx_vol_view_lookup::fx_vol_view},
    };
}

/**
 * @brief The members of fx_vol_views_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_views_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"feed_id", &fx_vol_views_filter::feed_id},
        utility::codec::field{"contributor_id", &fx_vol_views_filter::contributor_id},
        utility::codec::field{"id_one_of", &fx_vol_views_filter::id_one_of},
        utility::codec::field{"feed_id_one_of", &fx_vol_views_filter::feed_id_one_of},
        utility::codec::field{"contributor_id_one_of", &fx_vol_views_filter::contributor_id_one_of},
    };
}

/**
 * @brief The members of fx_vol_view_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_view_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &fx_vol_view_event::event_id},
        utility::codec::field{"key", &fx_vol_view_event::key},
        utility::codec::field{"action", &fx_vol_view_event::action},
        utility::codec::field{"version", &fx_vol_view_event::version},
        utility::codec::field{"occurred_at", &fx_vol_view_event::occurred_at},
        utility::codec::field{"correlation_id", &fx_vol_view_event::correlation_id},
    };
}

/**
 * @brief The members of fx_vol_view_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_view_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"fx_vol_view", &fx_vol_view_version_key::fx_vol_view},
        utility::codec::field{"version", &fx_vol_view_version_key::version},
    };
}

/**
 * @brief The members of fx_vol_view_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_view_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &fx_vol_view_versions_filter::version},
        utility::codec::field{"from_version", &fx_vol_view_versions_filter::from_version},
        utility::codec::field{"to_version", &fx_vol_view_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_fx_vol_views_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_fx_vol_views_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_fx_vol_views_request::offset},
        utility::codec::field{"limit", &list_fx_vol_views_request::limit},
        utility::codec::field{"order", &list_fx_vol_views_request::order},
        utility::codec::field{"filter", &list_fx_vol_views_request::filter},
        utility::codec::field{"as_of", &list_fx_vol_views_request::as_of},
    };
}

/**
 * @brief The members of list_fx_vol_views_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_fx_vol_views_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_fx_vol_views_response::result},
        utility::codec::field{"fx_vol_views", &list_fx_vol_views_response::fx_vol_views},
        utility::codec::field{"total", &list_fx_vol_views_response::total},
    };
}

/**
 * @brief The members of get_fx_vol_view_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fx_vol_view_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_fx_vol_view_request::key},
    };
}

/**
 * @brief The members of get_fx_vol_view_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fx_vol_view_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_fx_vol_view_response::result},
        utility::codec::field{"fx_vol_view", &get_fx_vol_view_response::fx_vol_view},
    };
}

/**
 * @brief The members of get_many_fx_vol_views_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_fx_vol_views_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_fx_vol_views_request::keys},
    };
}

/**
 * @brief The members of get_many_fx_vol_views_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_fx_vol_views_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_fx_vol_views_response::result},
        utility::codec::field{"entries", &get_many_fx_vol_views_response::entries},
    };
}

/**
 * @brief The members of put_fx_vol_view_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_fx_vol_view_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_fx_vol_view_request::change},
        utility::codec::field{"intent", &put_fx_vol_view_request::intent},
    };
}

/**
 * @brief The members of put_fx_vol_view_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_fx_vol_view_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_fx_vol_view_response::result},
        utility::codec::field{"fx_vol_view", &put_fx_vol_view_response::fx_vol_view},
    };
}

/**
 * @brief The members of put_many_fx_vol_views_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_fx_vol_views_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_fx_vol_views_request::changes},
        utility::codec::field{"intent", &put_many_fx_vol_views_request::intent},
    };
}

/**
 * @brief The members of put_many_fx_vol_views_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_fx_vol_views_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_fx_vol_views_response::result},
        utility::codec::field{"fx_vol_views", &put_many_fx_vol_views_response::fx_vol_views},
    };
}

/**
 * @brief The members of delete_fx_vol_view_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_fx_vol_view_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_fx_vol_view_request::removal},
        utility::codec::field{"intent", &delete_fx_vol_view_request::intent},
    };
}

/**
 * @brief The members of delete_fx_vol_view_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_fx_vol_view_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_fx_vol_view_response::result},
    };
}

/**
 * @brief The members of delete_many_fx_vol_views_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_fx_vol_views_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_fx_vol_views_request::removals},
        utility::codec::field{"intent", &delete_many_fx_vol_views_request::intent},
    };
}

/**
 * @brief The members of delete_many_fx_vol_views_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_fx_vol_views_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_fx_vol_views_response::result},
    };
}

/**
 * @brief The members of list_fx_vol_view_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_fx_vol_view_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_fx_vol_view_versions_request::key},
        utility::codec::field{"offset", &list_fx_vol_view_versions_request::offset},
        utility::codec::field{"limit", &list_fx_vol_view_versions_request::limit},
        utility::codec::field{"order", &list_fx_vol_view_versions_request::order},
        utility::codec::field{"filter", &list_fx_vol_view_versions_request::filter},
    };
}

/**
 * @brief The members of list_fx_vol_view_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_fx_vol_view_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_fx_vol_view_versions_response::result},
        utility::codec::field{"versions", &list_fx_vol_view_versions_response::versions},
        utility::codec::field{"total", &list_fx_vol_view_versions_response::total},
    };
}

/**
 * @brief The members of get_fx_vol_view_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fx_vol_view_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_fx_vol_view_version_request::key},
    };
}

/**
 * @brief The members of get_fx_vol_view_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fx_vol_view_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_fx_vol_view_version_response::result},
        utility::codec::field{"version", &get_fx_vol_view_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::fx_vol_view_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_vol_view_key& v);
    static ores::synthetic::messaging::fx_vol_view_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::fx_vol_view_key& v);
    static ores::synthetic::messaging::fx_vol_view_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::fx_vol_view_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_vol_view_write& v);
    static ores::synthetic::messaging::fx_vol_view_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::fx_vol_view_write& v);
    static ores::synthetic::messaging::fx_vol_view_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::fx_vol_view_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_vol_view_change& v);
    static ores::synthetic::messaging::fx_vol_view_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::fx_vol_view_change& v);
    static ores::synthetic::messaging::fx_vol_view_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::fx_vol_view_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_vol_view_removal& v);
    static ores::synthetic::messaging::fx_vol_view_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::fx_vol_view_removal& v);
    static ores::synthetic::messaging::fx_vol_view_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::fx_vol_view_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_vol_view_lookup& v);
    static ores::synthetic::messaging::fx_vol_view_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::fx_vol_view_lookup& v);
    static ores::synthetic::messaging::fx_vol_view_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::fx_vol_views_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_vol_views_filter& v);
    static ores::synthetic::messaging::fx_vol_views_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::fx_vol_views_filter& v);
    static ores::synthetic::messaging::fx_vol_views_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::fx_vol_view_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_vol_view_event& v);
    static ores::synthetic::messaging::fx_vol_view_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::fx_vol_view_event& v);
    static ores::synthetic::messaging::fx_vol_view_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::fx_vol_view_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_vol_view_version_key& v);
    static ores::synthetic::messaging::fx_vol_view_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::fx_vol_view_version_key& v);
    static ores::synthetic::messaging::fx_vol_view_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_vol_view_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_vol_view_versions_filter& v);
    static ores::synthetic::messaging::fx_vol_view_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_vol_view_versions_filter& v);
    static ores::synthetic::messaging::fx_vol_view_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_vol_views_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::list_fx_vol_views_request& v);
    static ores::synthetic::messaging::list_fx_vol_views_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_fx_vol_views_request& v);
    static ores::synthetic::messaging::list_fx_vol_views_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_vol_views_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::list_fx_vol_views_response& v);
    static ores::synthetic::messaging::list_fx_vol_views_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_fx_vol_views_response& v);
    static ores::synthetic::messaging::list_fx_vol_views_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::get_fx_vol_view_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::get_fx_vol_view_request& v);
    static ores::synthetic::messaging::get_fx_vol_view_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::get_fx_vol_view_request& v);
    static ores::synthetic::messaging::get_fx_vol_view_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::get_fx_vol_view_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::get_fx_vol_view_response& v);
    static ores::synthetic::messaging::get_fx_vol_view_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::get_fx_vol_view_response& v);
    static ores::synthetic::messaging::get_fx_vol_view_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_fx_vol_views_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_fx_vol_views_request& v);
    static ores::synthetic::messaging::get_many_fx_vol_views_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_fx_vol_views_request& v);
    static ores::synthetic::messaging::get_many_fx_vol_views_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_fx_vol_views_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_fx_vol_views_response& v);
    static ores::synthetic::messaging::get_many_fx_vol_views_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_fx_vol_views_response& v);
    static ores::synthetic::messaging::get_many_fx_vol_views_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::put_fx_vol_view_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::put_fx_vol_view_request& v);
    static ores::synthetic::messaging::put_fx_vol_view_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::put_fx_vol_view_request& v);
    static ores::synthetic::messaging::put_fx_vol_view_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::put_fx_vol_view_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::put_fx_vol_view_response& v);
    static ores::synthetic::messaging::put_fx_vol_view_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::put_fx_vol_view_response& v);
    static ores::synthetic::messaging::put_fx_vol_view_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_fx_vol_views_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_fx_vol_views_request& v);
    static ores::synthetic::messaging::put_many_fx_vol_views_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_fx_vol_views_request& v);
    static ores::synthetic::messaging::put_many_fx_vol_views_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_fx_vol_views_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_fx_vol_views_response& v);
    static ores::synthetic::messaging::put_many_fx_vol_views_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_fx_vol_views_response& v);
    static ores::synthetic::messaging::put_many_fx_vol_views_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_fx_vol_view_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::delete_fx_vol_view_request& v);
    static ores::synthetic::messaging::delete_fx_vol_view_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_fx_vol_view_request& v);
    static ores::synthetic::messaging::delete_fx_vol_view_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_fx_vol_view_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::delete_fx_vol_view_response& v);
    static ores::synthetic::messaging::delete_fx_vol_view_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_fx_vol_view_response& v);
    static ores::synthetic::messaging::delete_fx_vol_view_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_fx_vol_views_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_fx_vol_views_request& v);
    static ores::synthetic::messaging::delete_many_fx_vol_views_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_many_fx_vol_views_request& v);
    static ores::synthetic::messaging::delete_many_fx_vol_views_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_fx_vol_views_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_fx_vol_views_response& v);
    static ores::synthetic::messaging::delete_many_fx_vol_views_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_many_fx_vol_views_response& v);
    static ores::synthetic::messaging::delete_many_fx_vol_views_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_vol_view_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_fx_vol_view_versions_request& v);
    static ores::synthetic::messaging::list_fx_vol_view_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_fx_vol_view_versions_request& v);
    static ores::synthetic::messaging::list_fx_vol_view_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_vol_view_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_fx_vol_view_versions_response& v);
    static ores::synthetic::messaging::list_fx_vol_view_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_fx_vol_view_versions_response& v);
    static ores::synthetic::messaging::list_fx_vol_view_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_fx_vol_view_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_fx_vol_view_version_request& v);
    static ores::synthetic::messaging::get_fx_vol_view_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_fx_vol_view_version_request& v);
    static ores::synthetic::messaging::get_fx_vol_view_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_fx_vol_view_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_fx_vol_view_version_response& v);
    static ores::synthetic::messaging::get_fx_vol_view_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_fx_vol_view_version_response& v);
    static ores::synthetic::messaging::get_fx_vol_view_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
