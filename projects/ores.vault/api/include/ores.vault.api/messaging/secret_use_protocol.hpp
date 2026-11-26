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
#ifndef ORES_VAULT_API_MESSAGING_SECRET_USE_PROTOCOL_HPP
#define ORES_VAULT_API_MESSAGING_SECRET_USE_PROTOCOL_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include "ores.vault.api/domain/secret_use_codec.hpp"
#include "ores.vault.api/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::vault::messaging {

struct secret_use_key {
    boost::uuids::uuid id;

    friend bool operator==(const secret_use_key&, const secret_use_key&) = default;
};

struct secret_use_write {
    boost::uuids::uuid id;
    std::string name;
    std::string subject;
    std::string outcome;
    int key_version{};
    std::chrono::system_clock::time_point used_at;

    friend bool operator==(const secret_use_write&, const secret_use_write&) = default;
};

struct secret_use_change {
    secret_use_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const secret_use_change&, const secret_use_change&) = default;
};

struct secret_use_removal {
    secret_use_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const secret_use_removal&, const secret_use_removal&) = default;
};

struct secret_use_lookup {
    secret_use_key key;
    std::optional<ores::vault::domain::secret_use> secret_use;

    friend bool operator==(const secret_use_lookup&, const secret_use_lookup&) = default;
};

struct secret_uses_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const secret_uses_filter&, const secret_uses_filter&) = default;
};

struct secret_use_event {
    boost::uuids::uuid event_id;
    secret_use_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const secret_use_event&, const secret_use_event&) = default;
};

struct list_secret_uses_request {
    using response_type = struct list_secret_uses_response;
    static constexpr std::string_view nats_subject = "vault.v1.secret_uses.list";
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
    std::optional<secret_uses_filter> filter;

    friend bool operator==(const list_secret_uses_request&,
                           const list_secret_uses_request&) = default;
};

struct list_secret_uses_response {
    ores::utility::domain::result result;
    std::vector<ores::vault::domain::secret_use> secret_uses;
    std::uint64_t total{};

    friend bool operator==(const list_secret_uses_response&,
                           const list_secret_uses_response&) = default;
};

struct get_secret_use_request {
    using response_type = struct get_secret_use_response;
    static constexpr std::string_view nats_subject = "vault.v1.secret_uses.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    secret_use_key key;

    friend bool operator==(const get_secret_use_request&, const get_secret_use_request&) = default;
};

struct get_secret_use_response {
    ores::utility::domain::result result;
    std::optional<ores::vault::domain::secret_use> secret_use;

    friend bool operator==(const get_secret_use_response&,
                           const get_secret_use_response&) = default;
};

struct get_many_secret_uses_request {
    using response_type = struct get_many_secret_uses_response;
    static constexpr std::string_view nats_subject = "vault.v1.secret_uses.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<secret_use_key> keys;

    friend bool operator==(const get_many_secret_uses_request&,
                           const get_many_secret_uses_request&) = default;
};

struct get_many_secret_uses_response {
    ores::utility::domain::result result;
    std::vector<secret_use_lookup> entries;

    friend bool operator==(const get_many_secret_uses_response&,
                           const get_many_secret_uses_response&) = default;
};

struct put_secret_use_request {
    using response_type = struct put_secret_use_response;
    static constexpr std::string_view nats_subject = "vault.v1.secret_uses.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    secret_use_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_secret_use_request&, const put_secret_use_request&) = default;
};

struct put_secret_use_response {
    ores::utility::domain::result result;
    std::optional<ores::vault::domain::secret_use> secret_use;

    friend bool operator==(const put_secret_use_response&,
                           const put_secret_use_response&) = default;
};

struct put_many_secret_uses_request {
    using response_type = struct put_many_secret_uses_response;
    static constexpr std::string_view nats_subject = "vault.v1.secret_uses.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<secret_use_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_secret_uses_request&,
                           const put_many_secret_uses_request&) = default;
};

struct put_many_secret_uses_response {
    ores::utility::domain::result result;
    std::vector<ores::vault::domain::secret_use> secret_uses;

    friend bool operator==(const put_many_secret_uses_response&,
                           const put_many_secret_uses_response&) = default;
};

struct delete_secret_use_request {
    using response_type = struct delete_secret_use_response;
    static constexpr std::string_view nats_subject = "vault.v1.secret_uses.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    secret_use_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_secret_use_request&,
                           const delete_secret_use_request&) = default;
};

struct delete_secret_use_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_secret_use_response&,
                           const delete_secret_use_response&) = default;
};

struct delete_many_secret_uses_request {
    using response_type = struct delete_many_secret_uses_response;
    static constexpr std::string_view nats_subject = "vault.v1.secret_uses.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<secret_use_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_secret_uses_request&,
                           const delete_many_secret_uses_request&) = default;
};

struct delete_many_secret_uses_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_secret_uses_response&,
                           const delete_many_secret_uses_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace secret_use_event_subjects {
inline constexpr std::string_view created = "vault.v1.events.secret_uses.created";
inline constexpr std::string_view updated = "vault.v1.events.secret_uses.updated";
inline constexpr std::string_view deleted = "vault.v1.events.secret_uses.deleted";
}
/**
 * @brief The members of secret_use_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<secret_use_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &secret_use_key::id},
    };
}

/**
 * @brief The members of secret_use_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<secret_use_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &secret_use_write::id},
        utility::codec::field{"name", &secret_use_write::name},
        utility::codec::field{"subject", &secret_use_write::subject},
        utility::codec::field{"outcome", &secret_use_write::outcome},
        utility::codec::field{"key_version", &secret_use_write::key_version},
        utility::codec::field{"used_at", &secret_use_write::used_at},
    };
}

/**
 * @brief The members of secret_use_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<secret_use_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &secret_use_change::write},
        utility::codec::field{"precondition", &secret_use_change::precondition},
    };
}

/**
 * @brief The members of secret_use_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<secret_use_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &secret_use_removal::key},
        utility::codec::field{"precondition", &secret_use_removal::precondition},
    };
}

/**
 * @brief The members of secret_use_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<secret_use_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &secret_use_lookup::key},
        utility::codec::field{"secret_use", &secret_use_lookup::secret_use},
    };
}

/**
 * @brief The members of secret_uses_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<secret_uses_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &secret_uses_filter::id_one_of},
    };
}

/**
 * @brief The members of secret_use_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<secret_use_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &secret_use_event::event_id},
        utility::codec::field{"key", &secret_use_event::key},
        utility::codec::field{"action", &secret_use_event::action},
        utility::codec::field{"version", &secret_use_event::version},
        utility::codec::field{"occurred_at", &secret_use_event::occurred_at},
        utility::codec::field{"correlation_id", &secret_use_event::correlation_id},
    };
}

/**
 * @brief The members of list_secret_uses_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_secret_uses_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_secret_uses_request::offset},
        utility::codec::field{"limit", &list_secret_uses_request::limit},
        utility::codec::field{"order", &list_secret_uses_request::order},
        utility::codec::field{"filter", &list_secret_uses_request::filter},
    };
}

/**
 * @brief The members of list_secret_uses_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_secret_uses_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_secret_uses_response::result},
        utility::codec::field{"secret_uses", &list_secret_uses_response::secret_uses},
        utility::codec::field{"total", &list_secret_uses_response::total},
    };
}

/**
 * @brief The members of get_secret_use_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_secret_use_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_secret_use_request::key},
    };
}

/**
 * @brief The members of get_secret_use_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_secret_use_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_secret_use_response::result},
        utility::codec::field{"secret_use", &get_secret_use_response::secret_use},
    };
}

/**
 * @brief The members of get_many_secret_uses_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_secret_uses_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_secret_uses_request::keys},
    };
}

/**
 * @brief The members of get_many_secret_uses_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_secret_uses_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_secret_uses_response::result},
        utility::codec::field{"entries", &get_many_secret_uses_response::entries},
    };
}

/**
 * @brief The members of put_secret_use_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_secret_use_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_secret_use_request::change},
        utility::codec::field{"intent", &put_secret_use_request::intent},
    };
}

/**
 * @brief The members of put_secret_use_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_secret_use_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_secret_use_response::result},
        utility::codec::field{"secret_use", &put_secret_use_response::secret_use},
    };
}

/**
 * @brief The members of put_many_secret_uses_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_secret_uses_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_secret_uses_request::changes},
        utility::codec::field{"intent", &put_many_secret_uses_request::intent},
    };
}

/**
 * @brief The members of put_many_secret_uses_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_secret_uses_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_secret_uses_response::result},
        utility::codec::field{"secret_uses", &put_many_secret_uses_response::secret_uses},
    };
}

/**
 * @brief The members of delete_secret_use_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_secret_use_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_secret_use_request::removal},
        utility::codec::field{"intent", &delete_secret_use_request::intent},
    };
}

/**
 * @brief The members of delete_secret_use_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_secret_use_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_secret_use_response::result},
    };
}

/**
 * @brief The members of delete_many_secret_uses_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_secret_uses_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_secret_uses_request::removals},
        utility::codec::field{"intent", &delete_many_secret_uses_request::intent},
    };
}

/**
 * @brief The members of delete_many_secret_uses_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_secret_uses_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_secret_uses_response::result},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::secret_use_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::secret_use_key& v);
    static ores::vault::messaging::secret_use_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::secret_use_key& v);
    static ores::vault::messaging::secret_use_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::secret_use_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::secret_use_write& v);
    static ores::vault::messaging::secret_use_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::secret_use_write& v);
    static ores::vault::messaging::secret_use_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::secret_use_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::secret_use_change& v);
    static ores::vault::messaging::secret_use_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::secret_use_change& v);
    static ores::vault::messaging::secret_use_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::secret_use_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::secret_use_removal& v);
    static ores::vault::messaging::secret_use_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::secret_use_removal& v);
    static ores::vault::messaging::secret_use_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::secret_use_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::secret_use_lookup& v);
    static ores::vault::messaging::secret_use_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::secret_use_lookup& v);
    static ores::vault::messaging::secret_use_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::secret_uses_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::secret_uses_filter& v);
    static ores::vault::messaging::secret_uses_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::secret_uses_filter& v);
    static ores::vault::messaging::secret_uses_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::secret_use_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::secret_use_event& v);
    static ores::vault::messaging::secret_use_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::secret_use_event& v);
    static ores::vault::messaging::secret_use_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::list_secret_uses_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::list_secret_uses_request& v);
    static ores::vault::messaging::list_secret_uses_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::list_secret_uses_request& v);
    static ores::vault::messaging::list_secret_uses_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::list_secret_uses_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::list_secret_uses_response& v);
    static ores::vault::messaging::list_secret_uses_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::list_secret_uses_response& v);
    static ores::vault::messaging::list_secret_uses_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::get_secret_use_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::get_secret_use_request& v);
    static ores::vault::messaging::get_secret_use_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::get_secret_use_request& v);
    static ores::vault::messaging::get_secret_use_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::get_secret_use_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::get_secret_use_response& v);
    static ores::vault::messaging::get_secret_use_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::get_secret_use_response& v);
    static ores::vault::messaging::get_secret_use_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::get_many_secret_uses_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::get_many_secret_uses_request& v);
    static ores::vault::messaging::get_many_secret_uses_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::get_many_secret_uses_request& v);
    static ores::vault::messaging::get_many_secret_uses_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::get_many_secret_uses_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::get_many_secret_uses_response& v);
    static ores::vault::messaging::get_many_secret_uses_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::vault::messaging::get_many_secret_uses_response& v);
    static ores::vault::messaging::get_many_secret_uses_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::put_secret_use_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::put_secret_use_request& v);
    static ores::vault::messaging::put_secret_use_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::put_secret_use_request& v);
    static ores::vault::messaging::put_secret_use_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::put_secret_use_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::put_secret_use_response& v);
    static ores::vault::messaging::put_secret_use_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::put_secret_use_response& v);
    static ores::vault::messaging::put_secret_use_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::put_many_secret_uses_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::put_many_secret_uses_request& v);
    static ores::vault::messaging::put_many_secret_uses_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::put_many_secret_uses_request& v);
    static ores::vault::messaging::put_many_secret_uses_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::put_many_secret_uses_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::put_many_secret_uses_response& v);
    static ores::vault::messaging::put_many_secret_uses_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::vault::messaging::put_many_secret_uses_response& v);
    static ores::vault::messaging::put_many_secret_uses_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::delete_secret_use_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::delete_secret_use_request& v);
    static ores::vault::messaging::delete_secret_use_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::delete_secret_use_request& v);
    static ores::vault::messaging::delete_secret_use_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::delete_secret_use_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::delete_secret_use_response& v);
    static ores::vault::messaging::delete_secret_use_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::delete_secret_use_response& v);
    static ores::vault::messaging::delete_secret_use_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::delete_many_secret_uses_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::delete_many_secret_uses_request& v);
    static ores::vault::messaging::delete_many_secret_uses_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::vault::messaging::delete_many_secret_uses_request& v);
    static ores::vault::messaging::delete_many_secret_uses_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::delete_many_secret_uses_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::vault::messaging::delete_many_secret_uses_response& v);
    static ores::vault::messaging::delete_many_secret_uses_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::vault::messaging::delete_many_secret_uses_response& v);
    static ores::vault::messaging::delete_many_secret_uses_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
