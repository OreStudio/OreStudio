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
#ifndef ORES_MARKETDATA_API_MESSAGING_PROVIDER_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_PROVIDER_PROTOCOL_HPP

#include "ores.marketdata.api/domain/provider.hpp"
#include "ores.marketdata.api/export.hpp"
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

namespace ores::marketdata::messaging {

struct provider_key {
    std::string code;
};

struct provider_write {
    std::string code;
    std::string name;
    std::string kind;
    std::string description;
    int display_order;
    std::string terms_url;
    std::string terms_credit;
    bool allows_storage;
    std::string credential_ref;
};

struct provider_change {
    provider_write write;
    ores::utility::domain::precondition precondition;
};

struct provider_removal {
    provider_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct provider_lookup {
    provider_key key;
    std::optional<ores::marketdata::domain::provider> provider;
};

struct providers_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct provider_event {
    boost::uuids::uuid event_id;
    provider_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct provider_version_key {
    provider_key provider;
    std::uint32_t version;
};

struct provider_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_providers_request {
    using response_type = struct list_providers_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.providers.list";
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
    std::optional<providers_filter> filter;
    std::optional<std::string> as_of;
};

struct list_providers_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::provider> providers;
    std::uint64_t total;
};

struct get_provider_request {
    using response_type = struct get_provider_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.providers.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    provider_key key;
};

struct get_provider_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::provider> provider;
};

struct get_many_providers_request {
    using response_type = struct get_many_providers_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.providers.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<provider_key> keys;
};

struct get_many_providers_response {
    ores::utility::domain::result result;
    std::vector<provider_lookup> entries;
};

struct put_provider_request {
    using response_type = struct put_provider_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.providers.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    provider_change change;
    ores::utility::domain::change_intent intent;
};

struct put_provider_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::provider> provider;
};

struct put_many_providers_request {
    using response_type = struct put_many_providers_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.providers.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<provider_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_providers_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::provider> providers;
};

struct delete_provider_request {
    using response_type = struct delete_provider_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.providers.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    provider_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_provider_response {
    ores::utility::domain::result result;
};

struct delete_many_providers_request {
    using response_type = struct delete_many_providers_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.providers.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<provider_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_providers_response {
    ores::utility::domain::result result;
};

struct list_provider_versions_request {
    using response_type = struct list_provider_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.providers_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    provider_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<provider_versions_filter> filter;
};

struct list_provider_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::provider> versions;
    std::uint64_t total;
};

struct get_provider_version_request {
    using response_type = struct get_provider_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.providers_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    provider_version_key key;
};

struct get_provider_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::provider> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace provider_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.providers_events.created";
inline constexpr std::string_view updated = "marketdata.v1.providers_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.providers_events.deleted";
}
/**
 * @brief The members of provider_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_key&) {
    return std::tuple{
        utility::codec::field{"code", &provider_key::code},
    };
}

/**
 * @brief The members of provider_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_write&) {
    return std::tuple{
        utility::codec::field{"code", &provider_write::code},
        utility::codec::field{"name", &provider_write::name},
        utility::codec::field{"kind", &provider_write::kind},
        utility::codec::field{"description", &provider_write::description},
        utility::codec::field{"display_order", &provider_write::display_order},
        utility::codec::field{"terms_url", &provider_write::terms_url},
        utility::codec::field{"terms_credit", &provider_write::terms_credit},
        utility::codec::field{"allows_storage", &provider_write::allows_storage},
        utility::codec::field{"credential_ref", &provider_write::credential_ref},
    };
}

/**
 * @brief The members of provider_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_change&) {
    return std::tuple{
        utility::codec::field{"write", &provider_change::write},
        utility::codec::field{"precondition", &provider_change::precondition},
    };
}

/**
 * @brief The members of provider_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_removal&) {
    return std::tuple{
        utility::codec::field{"key", &provider_removal::key},
        utility::codec::field{"precondition", &provider_removal::precondition},
    };
}

/**
 * @brief The members of provider_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &provider_lookup::key},
        utility::codec::field{"provider", &provider_lookup::provider},
    };
}

/**
 * @brief The members of providers_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const providers_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &providers_filter::code_one_of},
    };
}

/**
 * @brief The members of provider_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &provider_event::event_id},
        utility::codec::field{"key", &provider_event::key},
        utility::codec::field{"action", &provider_event::action},
        utility::codec::field{"version", &provider_event::version},
        utility::codec::field{"occurred_at", &provider_event::occurred_at},
        utility::codec::field{"correlation_id", &provider_event::correlation_id},
    };
}

/**
 * @brief The members of provider_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_version_key&) {
    return std::tuple{
        utility::codec::field{"provider", &provider_version_key::provider},
        utility::codec::field{"version", &provider_version_key::version},
    };
}

/**
 * @brief The members of provider_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &provider_versions_filter::version},
        utility::codec::field{"from_version", &provider_versions_filter::from_version},
        utility::codec::field{"to_version", &provider_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_providers_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_providers_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_providers_request::offset},
        utility::codec::field{"limit", &list_providers_request::limit},
        utility::codec::field{"order", &list_providers_request::order},
        utility::codec::field{"filter", &list_providers_request::filter},
        utility::codec::field{"as_of", &list_providers_request::as_of},
    };
}

/**
 * @brief The members of list_providers_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_providers_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_providers_response::result},
        utility::codec::field{"providers", &list_providers_response::providers},
        utility::codec::field{"total", &list_providers_response::total},
    };
}

/**
 * @brief The members of get_provider_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_provider_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_provider_request::key},
    };
}

/**
 * @brief The members of get_provider_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_provider_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_provider_response::result},
        utility::codec::field{"provider", &get_provider_response::provider},
    };
}

/**
 * @brief The members of get_many_providers_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_providers_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_providers_request::keys},
    };
}

/**
 * @brief The members of get_many_providers_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_providers_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_providers_response::result},
        utility::codec::field{"entries", &get_many_providers_response::entries},
    };
}

/**
 * @brief The members of put_provider_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_provider_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_provider_request::change},
        utility::codec::field{"intent", &put_provider_request::intent},
    };
}

/**
 * @brief The members of put_provider_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_provider_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_provider_response::result},
        utility::codec::field{"provider", &put_provider_response::provider},
    };
}

/**
 * @brief The members of put_many_providers_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_providers_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_providers_request::changes},
        utility::codec::field{"intent", &put_many_providers_request::intent},
    };
}

/**
 * @brief The members of put_many_providers_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_providers_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_providers_response::result},
        utility::codec::field{"providers", &put_many_providers_response::providers},
    };
}

/**
 * @brief The members of delete_provider_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_provider_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_provider_request::removal},
        utility::codec::field{"intent", &delete_provider_request::intent},
    };
}

/**
 * @brief The members of delete_provider_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_provider_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_provider_response::result},
    };
}

/**
 * @brief The members of delete_many_providers_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_providers_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_providers_request::removals},
        utility::codec::field{"intent", &delete_many_providers_request::intent},
    };
}

/**
 * @brief The members of delete_many_providers_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_providers_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_providers_response::result},
    };
}

/**
 * @brief The members of list_provider_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_provider_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_provider_versions_request::key},
        utility::codec::field{"offset", &list_provider_versions_request::offset},
        utility::codec::field{"limit", &list_provider_versions_request::limit},
        utility::codec::field{"order", &list_provider_versions_request::order},
        utility::codec::field{"filter", &list_provider_versions_request::filter},
    };
}

/**
 * @brief The members of list_provider_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_provider_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_provider_versions_response::result},
        utility::codec::field{"versions", &list_provider_versions_response::versions},
        utility::codec::field{"total", &list_provider_versions_response::total},
    };
}

/**
 * @brief The members of get_provider_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_provider_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_provider_version_request::key},
    };
}

/**
 * @brief The members of get_provider_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_provider_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_provider_version_response::result},
        utility::codec::field{"version", &get_provider_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::provider_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::provider_key& v);
    static ores::marketdata::messaging::provider_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::provider_key& v);
    static ores::marketdata::messaging::provider_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::provider_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::provider_write& v);
    static ores::marketdata::messaging::provider_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::provider_write& v);
    static ores::marketdata::messaging::provider_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::provider_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::provider_change& v);
    static ores::marketdata::messaging::provider_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::provider_change& v);
    static ores::marketdata::messaging::provider_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::provider_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::provider_removal& v);
    static ores::marketdata::messaging::provider_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::provider_removal& v);
    static ores::marketdata::messaging::provider_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::provider_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::provider_lookup& v);
    static ores::marketdata::messaging::provider_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::provider_lookup& v);
    static ores::marketdata::messaging::provider_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::providers_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::providers_filter& v);
    static ores::marketdata::messaging::providers_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::providers_filter& v);
    static ores::marketdata::messaging::providers_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::provider_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::provider_event& v);
    static ores::marketdata::messaging::provider_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::provider_event& v);
    static ores::marketdata::messaging::provider_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::provider_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::provider_version_key& v);
    static ores::marketdata::messaging::provider_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::provider_version_key& v);
    static ores::marketdata::messaging::provider_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::provider_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::provider_versions_filter& v);
    static ores::marketdata::messaging::provider_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::provider_versions_filter& v);
    static ores::marketdata::messaging::provider_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::list_providers_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_providers_request& v);
    static ores::marketdata::messaging::list_providers_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::list_providers_request& v);
    static ores::marketdata::messaging::list_providers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_providers_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_providers_response& v);
    static ores::marketdata::messaging::list_providers_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::list_providers_response& v);
    static ores::marketdata::messaging::list_providers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_provider_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_provider_request& v);
    static ores::marketdata::messaging::get_provider_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_provider_request& v);
    static ores::marketdata::messaging::get_provider_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_provider_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_provider_response& v);
    static ores::marketdata::messaging::get_provider_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_provider_response& v);
    static ores::marketdata::messaging::get_provider_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_providers_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_many_providers_request& v);
    static ores::marketdata::messaging::get_many_providers_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_providers_request& v);
    static ores::marketdata::messaging::get_many_providers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_providers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_providers_response& v);
    static ores::marketdata::messaging::get_many_providers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_providers_response& v);
    static ores::marketdata::messaging::get_many_providers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_provider_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_provider_request& v);
    static ores::marketdata::messaging::put_provider_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_provider_request& v);
    static ores::marketdata::messaging::put_provider_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_provider_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_provider_response& v);
    static ores::marketdata::messaging::put_provider_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_provider_response& v);
    static ores::marketdata::messaging::put_provider_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_providers_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_many_providers_request& v);
    static ores::marketdata::messaging::put_many_providers_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_providers_request& v);
    static ores::marketdata::messaging::put_many_providers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_providers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_providers_response& v);
    static ores::marketdata::messaging::put_many_providers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_providers_response& v);
    static ores::marketdata::messaging::put_many_providers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_provider_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_provider_request& v);
    static ores::marketdata::messaging::delete_provider_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delete_provider_request& v);
    static ores::marketdata::messaging::delete_provider_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_provider_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_provider_response& v);
    static ores::marketdata::messaging::delete_provider_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_provider_response& v);
    static ores::marketdata::messaging::delete_provider_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_providers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_providers_request& v);
    static ores::marketdata::messaging::delete_many_providers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_providers_request& v);
    static ores::marketdata::messaging::delete_many_providers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_providers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_providers_response& v);
    static ores::marketdata::messaging::delete_many_providers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_providers_response& v);
    static ores::marketdata::messaging::delete_many_providers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_provider_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_provider_versions_request& v);
    static ores::marketdata::messaging::list_provider_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_provider_versions_request& v);
    static ores::marketdata::messaging::list_provider_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_provider_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_provider_versions_response& v);
    static ores::marketdata::messaging::list_provider_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_provider_versions_response& v);
    static ores::marketdata::messaging::list_provider_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_provider_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_provider_version_request& v);
    static ores::marketdata::messaging::get_provider_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_provider_version_request& v);
    static ores::marketdata::messaging::get_provider_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_provider_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_provider_version_response& v);
    static ores::marketdata::messaging::get_provider_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_provider_version_response& v);
    static ores::marketdata::messaging::get_provider_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
