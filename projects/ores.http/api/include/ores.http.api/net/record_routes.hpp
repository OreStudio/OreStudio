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
#ifndef ORES_HTTP_API_NET_RECORD_ROUTES_HPP
#define ORES_HTTP_API_NET_RECORD_ROUTES_HPP

#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/export.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/lexical_cast.hpp>
#include <cstddef>
#include <cstdint>
#include <exception>
#include <initializer_list>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * @brief What every record route a screen reads shares.
 *
 * The screens read each resource through the same addresses, and each
 * generated unit refuses, reads and answers in the same way. The parts that
 * need neither the unit's types nor a transport live here once.
 */
namespace ores::http::net::record_routes {

/**
 * @brief A refusal a route states, with the status and the code the browser reads.
 *
 * It is not a standard exception, so a handler's catch of a standard exception
 * cannot take it for a failure of its own.
 */
struct ORES_HTTP_API_EXPORT refusal final {
    domain::http_status status;
    std::string code;
    std::string message;
};

/**
 * @brief The sentence the browser sees when a route cannot answer.
 *
 * The detail goes to the log, so the browser sees no library's own words.
 */
inline constexpr std::string_view unanswered = "The service could not answer this request.";

/**
 * @brief The system tenant's id, the one tenant that writes a catalogue every tenant shares.
 */
inline constexpr std::string_view system_tenant_id = "ffffffff-ffff-ffff-ffff-ffffffffffff";

/**
 * @brief One write member's default, as a JSON literal.
 */
struct write_default final {
    std::string_view name;
    std::string_view json;
};

/**
 * @brief Refuse with this status, code and message.
 */
[[noreturn]] ORES_HTTP_API_EXPORT void
refuse(domain::http_status status, std::string code, std::string message);

/**
 * @brief Refuse as a request the browser stated wrongly.
 */
[[noreturn]] ORES_HTTP_API_EXPORT void invalid(std::string message);

/**
 * @brief The response a refusal states.
 */
ORES_HTTP_API_EXPORT domain::http_response answer(const refusal& r);

/**
 * @brief A write's answer: no content.
 */
ORES_HTTP_API_EXPORT domain::http_response no_content();

/**
 * @brief The text without its leading and trailing spaces, tabs and line ends.
 */
ORES_HTTP_API_EXPORT std::string trimmed(const std::string& raw);

/**
 * @brief How many characters a text holds, as the BFF counted them: a byte of
 * UTF-8 that continues a character is not counted.
 */
ORES_HTTP_API_EXPORT std::size_t characters_of(std::string_view text);

/**
 * @brief The response a refusal with this status, code and message states.
 */
ORES_HTTP_API_EXPORT domain::http_response
refused(domain::http_status status, std::string code, std::string message);

/**
 * @brief A code named in the address: 1 to 100 characters once trimmed.
 *
 * @throws refusal naming the sentence when the code is empty or too long.
 */
ORES_HTTP_API_EXPORT std::string code(const std::string& raw, const std::string& sentence);

/**
 * @brief Whether a text is an identifier in its canonical form: five groups of
 * hexadecimal digits joined by hyphens.
 */
ORES_HTTP_API_EXPORT bool is_uuid(std::string_view text);

/**
 * @brief Whether a reason is 1 to 200 characters once trimmed.
 */
ORES_HTTP_API_EXPORT bool has_reason(const std::string& reason);

/**
 * @brief Why a screen's write is made, as the screen states it: the commentary
 * may be left out.
 */
struct stated_intent final {
    std::string reason_code;
    std::optional<std::string> commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<stated_intent>) {
    using ores::utility::codec::field;
    return std::tuple{field{"reason_code", &stated_intent::reason_code},
                      field{"commentary", &stated_intent::commentary}};
}

/**
 * @brief The intent a service reads, with the reason trimmed, or nothing when
 * the reason is not 1 to 200 characters or the commentary is over 2000.
 */
ORES_HTTP_API_EXPORT std::optional<ores::utility::domain::change_intent>
intent_of(const stated_intent& stated);

/**
 * @brief Refuse with the status a refused outcome names.
 *
 * A conflict the service states no words for says what it means.
 */
ORES_HTTP_API_EXPORT void require_ok(const ores::utility::domain::result& result);

/**
 * @brief Refuse a failed outcome as the BFF refused a failed operation: a
 * conflict whose code is invalid-request and whose words are the service's.
 *
 * A route that keeps the BFF's answer uses this, whatever the outcome names.
 */
ORES_HTTP_API_EXPORT void require_done(const ores::utility::domain::result& result);

/**
 * @brief The body with each default added where a write leaves its member out.
 *
 * A write is the body's =write= object, or the =write= of each item of its
 * =changes= array. A member the write states is kept, even when it is null.
 * A body that is not JSON comes back as it was, so the read that follows
 * refuses it. The body is written again through the JSON library, so a number
 * keeps only what 64 bits hold; no write member is wider.
 */
ORES_HTTP_API_EXPORT std::string with_write_defaults(std::string_view body,
                                                     std::initializer_list<write_default> defaults);

/**
 * @brief What a one-row write states: the row, the version it was read at, and why.
 *
 * Every resource's write has this shape and differs only in its row, so one
 * template serves every record unit.
 */
template <typename Write>
struct write_body final {
    Write write;
    std::optional<std::uint32_t> version;
    std::string reasonCode;
    std::string commentary;
};

template <typename Write>
[[nodiscard]] constexpr auto wire_fields(std::type_identity<write_body<Write>>) {
    using ores::utility::codec::field;
    return std::tuple{field{"write", &write_body<Write>::write},
                      field{"version", &write_body<Write>::version},
                      field{"reasonCode", &write_body<Write>::reasonCode},
                      field{"commentary", &write_body<Write>::commentary}};
}

/**
 * @brief What a one-row removal states: the row's key, the version it was read at, and why.
 */
template <typename Key>
struct removal_body final {
    Key key;
    std::optional<std::uint32_t> version;
    std::string reasonCode;
    std::string commentary;
};

template <typename Key>
[[nodiscard]] constexpr auto wire_fields(std::type_identity<removal_body<Key>>) {
    using ores::utility::codec::field;
    return std::tuple{field{"key", &removal_body<Key>::key},
                      field{"version", &removal_body<Key>::version},
                      field{"reasonCode", &removal_body<Key>::reasonCode},
                      field{"commentary", &removal_body<Key>::commentary}};
}

/**
 * @brief One row of a batch write: the row and the version it was read at.
 */
template <typename Write>
struct change_body final {
    Write write;
    std::optional<std::uint32_t> version;
};

template <typename Write>
[[nodiscard]] constexpr auto wire_fields(std::type_identity<change_body<Write>>) {
    using ores::utility::codec::field;
    return std::tuple{field{"write", &change_body<Write>::write},
                      field{"version", &change_body<Write>::version}};
}

/**
 * @brief What a batch write states: why, and each row with its version.
 *
 * Every write states its reason as the same change intent, so only the row varies.
 */
template <typename Write>
struct save_body final {
    ores::utility::domain::change_intent intent;
    std::vector<change_body<Write>> changes;
};

template <typename Write>
[[nodiscard]] constexpr auto wire_fields(std::type_identity<save_body<Write>>) {
    using ores::utility::codec::field;
    return std::tuple{field{"intent", &save_body<Write>::intent},
                      field{"changes", &save_body<Write>::changes}};
}

/**
 * @brief One row of a batch removal: its key as the address names it, and its version.
 */
struct removal_item final {
    std::string key;
    std::optional<std::uint32_t> version;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<removal_item>) {
    using ores::utility::codec::field;
    return std::tuple{field{"key", &removal_item::key}, field{"version", &removal_item::version}};
}

/**
 * @brief What a batch removal states: why, and each row by its key.
 *
 * The keys arrive as the address names them, so one shape serves every resource.
 */
struct remove_body final {
    ores::utility::domain::change_intent intent;
    std::vector<removal_item> removals;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<remove_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"intent", &remove_body::intent},
                      field{"removals", &remove_body::removals}};
}

/**
 * @brief A body read as its own shape, or nothing when it states another.
 *
 * An optional member may be left out.
 */
template <typename T>
std::optional<T> read_body(std::string_view text) {
    auto value = ores::utility::codec::json::read_expected<T>(text);
    if (!value)
        return std::nullopt;
    return std::move(*value);
}

/**
 * @brief One parameter read as the member's own type, refused by name when malformed.
 */
template <typename T>
T read(const std::string& raw, const std::string& name) {
    try {
        if constexpr (std::is_same_v<T, std::string>) {
            return raw;
        } else if constexpr (std::is_same_v<T, bool>) {
            if (raw == "true")
                return true;
            if (raw == "false")
                return false;
            throw std::invalid_argument(raw);
        } else {
            return boost::lexical_cast<T>(raw);
        }
    } catch (const std::exception&) {
        invalid("Invalid value for " + name + ": " + raw);
    }
}

}

#endif
