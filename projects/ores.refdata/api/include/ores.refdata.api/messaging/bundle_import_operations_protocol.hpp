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
#ifndef ORES_REFDATA_API_MESSAGING_BUNDLE_IMPORT_OPERATIONS_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BUNDLE_IMPORT_OPERATIONS_PROTOCOL_HPP

#include "ores.refdata.api/domain/bundle_import.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief What a commit did with one staged family.
 */
struct bundle_import_family_outcome {
    std::string family;
    int sequence = 0;
    bool applied = false;
    std::string message;

    friend bool operator==(const bundle_import_family_outcome&,
                           const bundle_import_family_outcome&) = default;
};

/**
 * @brief Opens a staging session for a bundle.
 *
 * The caller supplies the id, so a retried begin names the same import. The
 * service stamps the session's party on the row.
 */
struct begin_bundle_import_request {
    using response_type = struct begin_bundle_import_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.begin_bundle_import";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid id;
    std::optional<std::string> source;
    std::optional<int> families_expected;
    std::optional<std::chrono::system_clock::time_point> expires_at;
    /**
     * Refused when set: the service states the state, never the caller.
     */
    std::optional<std::string> state;

    friend bool operator==(const begin_bundle_import_request&,
                           const begin_bundle_import_request&) = default;
};

/**
 * @brief The import that is open for staging.
 */
struct begin_bundle_import_response {
    bool success = false;
    std::string message;
    ores::refdata::domain::bundle_import import_row;

    friend bool operator==(const begin_bundle_import_response&,
                           const begin_bundle_import_response&) = default;
};

/**
 * @brief Stages one family's payload under an open import.
 *
 * The payload is the family's own generated value type, serialised as JSON.
 */
struct stage_bundle_family_request {
    using response_type = struct stage_bundle_family_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.stage_bundle_family";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid import_id;
    std::string family;
    int sequence = 0;
    std::string payload;
    std::optional<std::string> checksum;

    friend bool operator==(const stage_bundle_family_request&,
                           const stage_bundle_family_request&) = default;
};

/**
 * @brief Whether the stage wrote its row.
 */
struct stage_bundle_family_response {
    bool success = false;
    std::string message;

    friend bool operator==(const stage_bundle_family_response&,
                           const stage_bundle_family_response&) = default;
};

/**
 * @brief Applies every staged family of one import in one transaction.
 */
struct commit_bundle_import_request {
    using response_type = struct commit_bundle_import_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.commit_bundle_import";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid import_id;

    friend bool operator==(const commit_bundle_import_request&,
                           const commit_bundle_import_request&) = default;
};

/**
 * @brief What the commit stored, and what it did not.
 *
 * The world conventions the tenant already held are kept, as a bundle apply
 * keeps them. The family outcomes list one entry per applied staged family.
 */
struct commit_bundle_import_response {
    bool success = false;
    std::string message;
    std::vector<std::string> world_kept;
    std::vector<std::string> fx_skipped;
    std::vector<bundle_import_family_outcome> families;

    friend bool operator==(const commit_bundle_import_response&,
                           const commit_bundle_import_response&) = default;
};

/**
 * @brief The members of bundle_import_family_outcome, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_import_family_outcome>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"family", &bundle_import_family_outcome::family},
        ::ores::utility::codec::field{"sequence", &bundle_import_family_outcome::sequence},
        ::ores::utility::codec::field{"applied", &bundle_import_family_outcome::applied},
        ::ores::utility::codec::field{"message", &bundle_import_family_outcome::message},
    };
}

/**
 * @brief The members of begin_bundle_import_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<begin_bundle_import_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"id", &begin_bundle_import_request::id},
        ::ores::utility::codec::field{"source", &begin_bundle_import_request::source},
        ::ores::utility::codec::field{"families_expected",
                                      &begin_bundle_import_request::families_expected},
        ::ores::utility::codec::field{"expires_at", &begin_bundle_import_request::expires_at},
        ::ores::utility::codec::field{"state", &begin_bundle_import_request::state},
    };
}

/**
 * @brief The members of begin_bundle_import_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<begin_bundle_import_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &begin_bundle_import_response::success},
        ::ores::utility::codec::field{"message", &begin_bundle_import_response::message},
        ::ores::utility::codec::field{"import_row", &begin_bundle_import_response::import_row},
    };
}

/**
 * @brief The members of stage_bundle_family_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stage_bundle_family_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"import_id", &stage_bundle_family_request::import_id},
        ::ores::utility::codec::field{"family", &stage_bundle_family_request::family},
        ::ores::utility::codec::field{"sequence", &stage_bundle_family_request::sequence},
        ::ores::utility::codec::field{"payload", &stage_bundle_family_request::payload},
        ::ores::utility::codec::field{"checksum", &stage_bundle_family_request::checksum},
    };
}

/**
 * @brief The members of stage_bundle_family_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stage_bundle_family_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &stage_bundle_family_response::success},
        ::ores::utility::codec::field{"message", &stage_bundle_family_response::message},
    };
}

/**
 * @brief The members of commit_bundle_import_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commit_bundle_import_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"import_id", &commit_bundle_import_request::import_id},
    };
}

/**
 * @brief The members of commit_bundle_import_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commit_bundle_import_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &commit_bundle_import_response::success},
        ::ores::utility::codec::field{"message", &commit_bundle_import_response::message},
        ::ores::utility::codec::field{"world_kept", &commit_bundle_import_response::world_kept},
        ::ores::utility::codec::field{"fx_skipped", &commit_bundle_import_response::fx_skipped},
        ::ores::utility::codec::field{"families", &commit_bundle_import_response::families},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_family_outcome> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_family_outcome& v);
    static ores::refdata::messaging::bundle_import_family_outcome read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bundle_import_family_outcome& v);
    static ores::refdata::messaging::bundle_import_family_outcome
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::begin_bundle_import_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::begin_bundle_import_request& v);
    static ores::refdata::messaging::begin_bundle_import_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::begin_bundle_import_request& v);
    static ores::refdata::messaging::begin_bundle_import_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::begin_bundle_import_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::begin_bundle_import_response& v);
    static ores::refdata::messaging::begin_bundle_import_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::begin_bundle_import_response& v);
    static ores::refdata::messaging::begin_bundle_import_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::stage_bundle_family_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::stage_bundle_family_request& v);
    static ores::refdata::messaging::stage_bundle_family_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::stage_bundle_family_request& v);
    static ores::refdata::messaging::stage_bundle_family_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::stage_bundle_family_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::stage_bundle_family_response& v);
    static ores::refdata::messaging::stage_bundle_family_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::stage_bundle_family_response& v);
    static ores::refdata::messaging::stage_bundle_family_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::commit_bundle_import_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::commit_bundle_import_request& v);
    static ores::refdata::messaging::commit_bundle_import_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commit_bundle_import_request& v);
    static ores::refdata::messaging::commit_bundle_import_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commit_bundle_import_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::commit_bundle_import_response& v);
    static ores::refdata::messaging::commit_bundle_import_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commit_bundle_import_response& v);
    static ores::refdata::messaging::commit_bundle_import_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
