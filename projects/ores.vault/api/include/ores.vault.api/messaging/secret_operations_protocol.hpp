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
#ifndef ORES_VAULT_API_MESSAGING_SECRET_OPERATIONS_PROTOCOL_HPP
#define ORES_VAULT_API_MESSAGING_SECRET_OPERATIONS_PROTOCOL_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.vault.api/export.hpp"
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::vault::messaging {

/**
 * @brief Stores a new version of one secret, sealed to the vault.
 *
 * The value is sealed to the vault's transport public key, which the caller
 * reads from =vault.v1.ops.public_key=. The vault opens it with its own private
 * key, seals it again under the key encryption key and stores one row.
 */
struct write_secret_request {
    using response_type = struct write_secret_response;
    static constexpr std::string_view nats_subject = "vault.v1.ops.write";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string name;
    std::string owner;
    std::string sealed_value;

    friend bool operator==(const write_secret_request&, const write_secret_request&) = default;
};

struct write_secret_response {
    bool success = false;
    std::string message;
    std::string name;
    std::int32_t key_version = 0;

    friend bool operator==(const write_secret_response&, const write_secret_response&) = default;
};

/**
 * @brief Answers the metadata of one secret, and never its value.
 *
 * The read a screen and an operator use: it says that a value exists, who may
 * resolve it and under which key version, and it answers no ciphertext.
 */
struct describe_secret_request {
    using response_type = struct describe_secret_response;
    static constexpr std::string_view nats_subject = "vault.v1.ops.describe";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string name;

    friend bool operator==(const describe_secret_request&,
                           const describe_secret_request&) = default;
};

struct describe_secret_response {
    bool success = false;
    std::string message;
    std::string name;
    std::string owner;
    std::int32_t key_version = 0;
    std::chrono::system_clock::time_point written_at;

    friend bool operator==(const describe_secret_response&,
                           const describe_secret_response&) = default;
};

/**
 * @brief Answers one secret, sealed to the caller's registered key.
 *
 * The vault chooses the key from the subject in the verified token and never
 * from the request, so a caller cannot ask for a value sealed to somebody
 * else's key. Only the owner of the secret, named by the actor the verified
 * token carries, may resolve it.
 */
struct resolve_secret_request {
    using response_type = struct resolve_secret_response;
    static constexpr std::string_view nats_subject = "vault.v1.ops.resolve";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string name;

    friend bool operator==(const resolve_secret_request&, const resolve_secret_request&) = default;
};

struct resolve_secret_response {
    bool success = false;
    std::string message;
    std::string name;
    std::int32_t key_version = 0;
    std::string sealed_value;
    std::string subject;

    friend bool operator==(const resolve_secret_response&,
                           const resolve_secret_response&) = default;
};

/**
 * @brief Removes one secret.
 *
 * The use record outlives the row it describes, so the delete leaves its own row
 * in the record and nothing else behind.
 */
struct remove_secret_request {
    using response_type = struct remove_secret_response;
    static constexpr std::string_view nats_subject = "vault.v1.ops.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string name;

    friend bool operator==(const remove_secret_request&, const remove_secret_request&) = default;
};

struct remove_secret_response {
    bool success = false;
    std::string message;

    friend bool operator==(const remove_secret_response&, const remove_secret_response&) = default;
};

/**
 * @brief Re-wraps every secret of the tenant under a new key version.
 *
 * One audited act: every stored value is opened under the version that sealed it
 * and sealed again under the new one, and no plaintext leaves the process. A run
 * that fails part way leaves the rows it has not reached under the old version,
 * which is why every row records its own version.
 */
struct rotate_secrets_request {
    using response_type = struct rotate_secrets_response;
    static constexpr std::string_view nats_subject = "vault.v1.ops.rotate";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const rotate_secrets_request&, const rotate_secrets_request&) = default;
};

struct rotate_secrets_response {
    bool success = false;
    std::string message;
    std::int32_t rotated = 0;
    std::int32_t key_version = 0;

    friend bool operator==(const rotate_secrets_response&,
                           const rotate_secrets_response&) = default;
};

/**
 * @brief Answers the transport public key a caller seals a write to.
 *
 * A public key is not a secret and this read needs no permission beyond a
 * session: a caller cannot seal a value to the vault without it.
 */
struct vault_public_key_request {
    using response_type = struct vault_public_key_response;
    static constexpr std::string_view nats_subject = "vault.v1.ops.public_key";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const vault_public_key_request&,
                           const vault_public_key_request&) = default;
};

struct vault_public_key_response {
    bool success = false;
    std::string message;
    std::string public_key;

    friend bool operator==(const vault_public_key_response&,
                           const vault_public_key_response&) = default;
};

/**
 * @brief Registers the caller's own public key, so that a resolve can seal to it.
 *
 * The row is about the caller and nobody else: the name is the subject the
 * verified token carries, never a field of this message, so a caller cannot
 * register a key for somebody else. It needs no permission beyond a session for
 * the same reason: the only consumer it can touch is itself.
 */
struct register_consumer_request {
    using response_type = struct register_consumer_response;
    static constexpr std::string_view nats_subject = "vault.v1.ops.register_consumer";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string public_key;

    friend bool operator==(const register_consumer_request&,
                           const register_consumer_request&) = default;
};

struct register_consumer_response {
    bool success = false;
    std::string message;

    friend bool operator==(const register_consumer_response&,
                           const register_consumer_response&) = default;
};

/**
 * @brief The members of write_secret_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<write_secret_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &write_secret_request::name},
        ::ores::utility::codec::field{"owner", &write_secret_request::owner},
        ::ores::utility::codec::field{"sealed_value", &write_secret_request::sealed_value},
    };
}

/**
 * @brief The members of write_secret_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<write_secret_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &write_secret_response::success},
        ::ores::utility::codec::field{"message", &write_secret_response::message},
        ::ores::utility::codec::field{"name", &write_secret_response::name},
        ::ores::utility::codec::field{"key_version", &write_secret_response::key_version},
    };
}

/**
 * @brief The members of describe_secret_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<describe_secret_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &describe_secret_request::name},
    };
}

/**
 * @brief The members of describe_secret_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<describe_secret_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &describe_secret_response::success},
        ::ores::utility::codec::field{"message", &describe_secret_response::message},
        ::ores::utility::codec::field{"name", &describe_secret_response::name},
        ::ores::utility::codec::field{"owner", &describe_secret_response::owner},
        ::ores::utility::codec::field{"key_version", &describe_secret_response::key_version},
        ::ores::utility::codec::field{"written_at", &describe_secret_response::written_at},
    };
}

/**
 * @brief The members of resolve_secret_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<resolve_secret_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &resolve_secret_request::name},
    };
}

/**
 * @brief The members of resolve_secret_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<resolve_secret_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &resolve_secret_response::success},
        ::ores::utility::codec::field{"message", &resolve_secret_response::message},
        ::ores::utility::codec::field{"name", &resolve_secret_response::name},
        ::ores::utility::codec::field{"key_version", &resolve_secret_response::key_version},
        ::ores::utility::codec::field{"sealed_value", &resolve_secret_response::sealed_value},
        ::ores::utility::codec::field{"subject", &resolve_secret_response::subject},
    };
}

/**
 * @brief The members of remove_secret_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<remove_secret_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &remove_secret_request::name},
    };
}

/**
 * @brief The members of remove_secret_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<remove_secret_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &remove_secret_response::success},
        ::ores::utility::codec::field{"message", &remove_secret_response::message},
    };
}

/**
 * @brief The members of rotate_secrets_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rotate_secrets_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of rotate_secrets_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rotate_secrets_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &rotate_secrets_response::success},
        ::ores::utility::codec::field{"message", &rotate_secrets_response::message},
        ::ores::utility::codec::field{"rotated", &rotate_secrets_response::rotated},
        ::ores::utility::codec::field{"key_version", &rotate_secrets_response::key_version},
    };
}

/**
 * @brief The members of vault_public_key_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<vault_public_key_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of vault_public_key_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<vault_public_key_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &vault_public_key_response::success},
        ::ores::utility::codec::field{"message", &vault_public_key_response::message},
        ::ores::utility::codec::field{"public_key", &vault_public_key_response::public_key},
    };
}

/**
 * @brief The members of register_consumer_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<register_consumer_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"public_key", &register_consumer_request::public_key},
    };
}

/**
 * @brief The members of register_consumer_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<register_consumer_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &register_consumer_response::success},
        ::ores::utility::codec::field{"message", &register_consumer_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::write_secret_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::write_secret_request& v);
    static ores::vault::messaging::write_secret_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::write_secret_request& v);
    static ores::vault::messaging::write_secret_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::write_secret_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::write_secret_response& v);
    static ores::vault::messaging::write_secret_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::write_secret_response& v);
    static ores::vault::messaging::write_secret_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::describe_secret_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::describe_secret_request& v);
    static ores::vault::messaging::describe_secret_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::describe_secret_request& v);
    static ores::vault::messaging::describe_secret_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::describe_secret_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::describe_secret_response& v);
    static ores::vault::messaging::describe_secret_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::describe_secret_response& v);
    static ores::vault::messaging::describe_secret_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::resolve_secret_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::resolve_secret_request& v);
    static ores::vault::messaging::resolve_secret_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::resolve_secret_request& v);
    static ores::vault::messaging::resolve_secret_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::resolve_secret_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::resolve_secret_response& v);
    static ores::vault::messaging::resolve_secret_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::resolve_secret_response& v);
    static ores::vault::messaging::resolve_secret_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::remove_secret_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::remove_secret_request& v);
    static ores::vault::messaging::remove_secret_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::remove_secret_request& v);
    static ores::vault::messaging::remove_secret_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::remove_secret_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::remove_secret_response& v);
    static ores::vault::messaging::remove_secret_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::remove_secret_response& v);
    static ores::vault::messaging::remove_secret_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::rotate_secrets_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::rotate_secrets_request& v);
    static ores::vault::messaging::rotate_secrets_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::rotate_secrets_request& v);
    static ores::vault::messaging::rotate_secrets_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::rotate_secrets_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::rotate_secrets_response& v);
    static ores::vault::messaging::rotate_secrets_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::rotate_secrets_response& v);
    static ores::vault::messaging::rotate_secrets_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::vault_public_key_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::vault_public_key_request& v);
    static ores::vault::messaging::vault_public_key_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::vault_public_key_request& v);
    static ores::vault::messaging::vault_public_key_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::vault_public_key_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::vault_public_key_response& v);
    static ores::vault::messaging::vault_public_key_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::vault_public_key_response& v);
    static ores::vault::messaging::vault_public_key_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::register_consumer_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::register_consumer_request& v);
    static ores::vault::messaging::register_consumer_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::register_consumer_request& v);
    static ores::vault::messaging::register_consumer_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::register_consumer_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::register_consumer_response& v);
    static ores::vault::messaging::register_consumer_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::register_consumer_response& v);
    static ores::vault::messaging::register_consumer_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
