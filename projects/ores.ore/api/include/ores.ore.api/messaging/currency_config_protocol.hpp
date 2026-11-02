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
#ifndef ORES_ORE_API_MESSAGING_CURRENCY_CONFIG_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_CURRENCY_CONFIG_PROTOCOL_HPP

#include "ores.ore.api/export.hpp"
#include "ores.ore.api/messaging/ore_document_protocol.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::ore::messaging {

/**
 * @brief Writes the tenant's currencies as an ORE currency configuration.
 *
 * An empty list of codes exports every currency; a list exports those alone.
 */
struct export_currency_config_request {
    using response_type = struct export_currency_config_response;
    static constexpr std::string_view nats_subject = "ore.v1.ops.export_currency_config";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> iso_codes;
};

/**
 * @brief The document, one file, and how many currencies it holds.
 */
struct export_currency_config_response {
    bool success = false;
    std::string message;
    std::uint32_t count = 0;
    std::vector<ores::ore::messaging::ore_document> files;
};

/**
 * @brief One currency the import would touch: added, changed or removed, and
 * in words what differs.
 */
struct currency_import_row {
    std::string iso_code;
    std::string name;
    /** One of added, changed, removed. */
    std::string change;
    std::string what;
};

/**
 * @brief Something in the file the system does not know: a value with no row,
 * an element with no column, or a currency the file names twice.
 */
struct currency_import_issue {
    /** One of value, dropped, duplicate. */
    std::string kind;
    std::string subject;
    std::string detail;
};

/**
 * @brief Parses a currency configuration and says what importing it would do.
 *
 * Nothing is written. A file of another kind is refused, and the refusal
 * names the root element found.
 */
struct preview_currency_config_import_request {
    using response_type = struct preview_currency_config_import_response;
    static constexpr std::string_view nats_subject = "ore.v1.ops.preview_currency_config_import";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string content;
};

/**
 * @brief The counts, the difference against what the tenant holds, and what
 * the system does not know.
 */
struct preview_currency_config_import_response {
    bool success = false;
    std::string message;
    std::string root_element;
    std::uint32_t in_file = 0;
    /** Currencies the tenant holds with the same values. */
    std::uint32_t held = 0;
    std::uint32_t changed = 0;
    std::uint32_t added = 0;
    /** Currencies the tenant holds and the file omits. */
    std::uint32_t removed = 0;
    std::vector<currency_import_row> rows;
    std::vector<currency_import_issue> issues;
};

/**
 * @brief What the person chose for one currency of the difference.
 */
struct currency_import_choice {
    std::string iso_code;
    /** One of take, keep, delete, skip. Take writes the file's row; keep leaves ours; delete
     * removes a row the file omits; skip leaves a new row out. */
    std::string action;
};

/**
 * @brief Writes a currency configuration with the person's choices.
 *
 * A currency the choices do not name takes the file's row when it is new or
 * changed, and is kept when the file omits it.
 */
struct import_currency_config_request {
    using response_type = struct import_currency_config_response;
    static constexpr std::string_view nats_subject = "ore.v1.ops.import_currency_config";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string content;
    std::vector<currency_import_choice> choices;
    std::string reason_code;
    std::string commentary;
};

/**
 * @brief A currency the import could not write, and the owner's reason.
 */
struct currency_import_error {
    std::string iso_code;
    std::string message;
};

/**
 * @brief What the import wrote, kept and could not write.
 */
struct import_currency_config_response {
    bool success = false;
    std::string message;
    std::uint32_t added = 0;
    std::uint32_t changed = 0;
    std::uint32_t deleted = 0;
    std::uint32_t kept = 0;
    std::vector<currency_import_error> errors;
};

/**
 * @brief The members of export_currency_config_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const export_currency_config_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"iso_codes", &export_currency_config_request::iso_codes},
    };
}

/**
 * @brief The members of export_currency_config_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const export_currency_config_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &export_currency_config_response::success},
        ::ores::utility::codec::field{"message", &export_currency_config_response::message},
        ::ores::utility::codec::field{"count", &export_currency_config_response::count},
        ::ores::utility::codec::field{"files", &export_currency_config_response::files},
    };
}

/**
 * @brief The members of currency_import_row, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_import_row&) {
    return std::tuple{
        ::ores::utility::codec::field{"iso_code", &currency_import_row::iso_code},
        ::ores::utility::codec::field{"name", &currency_import_row::name},
        ::ores::utility::codec::field{"change", &currency_import_row::change},
        ::ores::utility::codec::field{"what", &currency_import_row::what},
    };
}

/**
 * @brief The members of currency_import_issue, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_import_issue&) {
    return std::tuple{
        ::ores::utility::codec::field{"kind", &currency_import_issue::kind},
        ::ores::utility::codec::field{"subject", &currency_import_issue::subject},
        ::ores::utility::codec::field{"detail", &currency_import_issue::detail},
    };
}

/**
 * @brief The members of preview_currency_config_import_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const preview_currency_config_import_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"content", &preview_currency_config_import_request::content},
    };
}

/**
 * @brief The members of preview_currency_config_import_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const preview_currency_config_import_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &preview_currency_config_import_response::success},
        ::ores::utility::codec::field{"message", &preview_currency_config_import_response::message},
        ::ores::utility::codec::field{"root_element",
                                      &preview_currency_config_import_response::root_element},
        ::ores::utility::codec::field{"in_file", &preview_currency_config_import_response::in_file},
        ::ores::utility::codec::field{"held", &preview_currency_config_import_response::held},
        ::ores::utility::codec::field{"changed", &preview_currency_config_import_response::changed},
        ::ores::utility::codec::field{"added", &preview_currency_config_import_response::added},
        ::ores::utility::codec::field{"removed", &preview_currency_config_import_response::removed},
        ::ores::utility::codec::field{"rows", &preview_currency_config_import_response::rows},
        ::ores::utility::codec::field{"issues", &preview_currency_config_import_response::issues},
    };
}

/**
 * @brief The members of currency_import_choice, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_import_choice&) {
    return std::tuple{
        ::ores::utility::codec::field{"iso_code", &currency_import_choice::iso_code},
        ::ores::utility::codec::field{"action", &currency_import_choice::action},
    };
}

/**
 * @brief The members of import_currency_config_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const import_currency_config_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"content", &import_currency_config_request::content},
        ::ores::utility::codec::field{"choices", &import_currency_config_request::choices},
        ::ores::utility::codec::field{"reason_code", &import_currency_config_request::reason_code},
        ::ores::utility::codec::field{"commentary", &import_currency_config_request::commentary},
    };
}

/**
 * @brief The members of currency_import_error, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_import_error&) {
    return std::tuple{
        ::ores::utility::codec::field{"iso_code", &currency_import_error::iso_code},
        ::ores::utility::codec::field{"message", &currency_import_error::message},
    };
}

/**
 * @brief The members of import_currency_config_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const import_currency_config_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &import_currency_config_response::success},
        ::ores::utility::codec::field{"message", &import_currency_config_response::message},
        ::ores::utility::codec::field{"added", &import_currency_config_response::added},
        ::ores::utility::codec::field{"changed", &import_currency_config_response::changed},
        ::ores::utility::codec::field{"deleted", &import_currency_config_response::deleted},
        ::ores::utility::codec::field{"kept", &import_currency_config_response::kept},
        ::ores::utility::codec::field{"errors", &import_currency_config_response::errors},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::export_currency_config_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::export_currency_config_request& v);
    static ores::ore::messaging::export_currency_config_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::export_currency_config_request& v);
    static ores::ore::messaging::export_currency_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::export_currency_config_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::export_currency_config_response& v);
    static ores::ore::messaging::export_currency_config_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::export_currency_config_response& v);
    static ores::ore::messaging::export_currency_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::currency_import_row> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::currency_import_row& v);
    static ores::ore::messaging::currency_import_row read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::currency_import_row& v);
    static ores::ore::messaging::currency_import_row read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::currency_import_issue> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::currency_import_issue& v);
    static ores::ore::messaging::currency_import_issue read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::currency_import_issue& v);
    static ores::ore::messaging::currency_import_issue
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT
    wire_traits<ores::ore::messaging::preview_currency_config_import_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::ore::messaging::preview_currency_config_import_request& v);
    static ores::ore::messaging::preview_currency_config_import_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::preview_currency_config_import_request& v);
    static ores::ore::messaging::preview_currency_config_import_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT
    wire_traits<ores::ore::messaging::preview_currency_config_import_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::ore::messaging::preview_currency_config_import_response& v);
    static ores::ore::messaging::preview_currency_config_import_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::preview_currency_config_import_response& v);
    static ores::ore::messaging::preview_currency_config_import_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::currency_import_choice> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::currency_import_choice& v);
    static ores::ore::messaging::currency_import_choice read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::currency_import_choice& v);
    static ores::ore::messaging::currency_import_choice
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::import_currency_config_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::import_currency_config_request& v);
    static ores::ore::messaging::import_currency_config_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::import_currency_config_request& v);
    static ores::ore::messaging::import_currency_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::currency_import_error> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::currency_import_error& v);
    static ores::ore::messaging::currency_import_error read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::currency_import_error& v);
    static ores::ore::messaging::currency_import_error
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::import_currency_config_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::import_currency_config_response& v);
    static ores::ore::messaging::import_currency_config_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::import_currency_config_response& v);
    static ores::ore::messaging::import_currency_config_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
