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
#ifndef ORES_DQ_API_MESSAGING_BUNDLE_ROSTER_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_BUNDLE_ROSTER_PROTOCOL_HPP

#include "ores.dq.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief Asks for the roster, or for one bundle of it with its members.
 */
struct bundle_roster_request {
    using response_type = struct bundle_roster_response;
    static constexpr std::string_view nats_subject = "dq.v1.ops.bundle_roster";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The bundle whose members to read, or empty for the roster alone.
     */
    std::string bundle_code;

    friend bool operator==(const bundle_roster_request&, const bundle_roster_request&) = default;
};

/**
 * @brief One dataset a bundle carries, and what its artefact table holds.
 */
struct bundle_roster_member {
    /**
     * @brief Where the member sits in the bundle's own order.
     */
    int display_order = 0;
    /**
     * @brief The dataset the member names.
     */
    std::string dataset_code;
    /**
     * @brief The dataset's name, or a note that no dataset row was found.
     */
    std::string dataset_name;
    /**
     * @brief The artefact type whose table the dataset stages into.
     */
    std::string artefact_type;
    /**
     * @brief The table, or family of tables, the dataset stages into.
     */
    std::string artefact_table;
    /**
     * @brief The subject a publish writes to, or a note that it names none.
     */
    std::string target_subject;
    /**
     * @brief How many rows the dataset has staged, ready to publish.
     */
    std::int64_t staged_count = 0;
    /**
     * @brief Whether the member has both a dataset row and a target subject.
     */
    bool is_publishable = false;
    /**
     * @brief Whether the member publishes only when a caller opts in.
     */
    bool optional = false;

    friend bool operator==(const bundle_roster_member&, const bundle_roster_member&) = default;
};

/**
 * @brief One bundle, the level it publishes at, and its members.
 */
struct bundle_roster_bundle {
    /**
     * @brief The bundle's code.
     */
    std::string code;
    /**
     * @brief The bundle's name.
     */
    std::string name;
    /**
     * @brief What the bundle carries.
     */
    std::string description;
    /**
     * @brief The level the bundle publishes at: platform, world or tenant.
     */
    std::string level;
    /**
     * @brief How many datasets the bundle carries.
     */
    std::int64_t dataset_count = 0;
    /**
     * @brief The members, in the bundle's own order, when the request named it.
     */
    std::vector<bundle_roster_member> members;

    friend bool operator==(const bundle_roster_bundle&, const bundle_roster_bundle&) = default;
};

/**
 * @brief Reports the roster, or why it could not be read.
 */
struct bundle_roster_response {
    /**
     * @brief Whether the roster was read.
     */
    bool success = false;
    /**
     * @brief Why it could not be read, when it could not.
     */
    std::string error_message;
    /**
     * @brief Every bundle, ordered by code.
     */
    std::vector<bundle_roster_bundle> bundles;

    friend bool operator==(const bundle_roster_response&, const bundle_roster_response&) = default;
};

/**
 * @brief The members of bundle_roster_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_roster_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"bundle_code", &bundle_roster_request::bundle_code},
    };
}

/**
 * @brief The members of bundle_roster_member, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_roster_member>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"display_order", &bundle_roster_member::display_order},
        ::ores::utility::codec::field{"dataset_code", &bundle_roster_member::dataset_code},
        ::ores::utility::codec::field{"dataset_name", &bundle_roster_member::dataset_name},
        ::ores::utility::codec::field{"artefact_type", &bundle_roster_member::artefact_type},
        ::ores::utility::codec::field{"artefact_table", &bundle_roster_member::artefact_table},
        ::ores::utility::codec::field{"target_subject", &bundle_roster_member::target_subject},
        ::ores::utility::codec::field{"staged_count", &bundle_roster_member::staged_count},
        ::ores::utility::codec::field{"is_publishable", &bundle_roster_member::is_publishable},
        ::ores::utility::codec::field{"optional", &bundle_roster_member::optional},
    };
}

/**
 * @brief The members of bundle_roster_bundle, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_roster_bundle>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"code", &bundle_roster_bundle::code},
        ::ores::utility::codec::field{"name", &bundle_roster_bundle::name},
        ::ores::utility::codec::field{"description", &bundle_roster_bundle::description},
        ::ores::utility::codec::field{"level", &bundle_roster_bundle::level},
        ::ores::utility::codec::field{"dataset_count", &bundle_roster_bundle::dataset_count},
        ::ores::utility::codec::field{"members", &bundle_roster_bundle::members},
    };
}

/**
 * @brief The members of bundle_roster_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_roster_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &bundle_roster_response::success},
        ::ores::utility::codec::field{"error_message", &bundle_roster_response::error_message},
        ::ores::utility::codec::field{"bundles", &bundle_roster_response::bundles},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::bundle_roster_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::bundle_roster_request& v);
    static ores::dq::messaging::bundle_roster_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::bundle_roster_request& v);
    static ores::dq::messaging::bundle_roster_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::bundle_roster_member> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::bundle_roster_member& v);
    static ores::dq::messaging::bundle_roster_member read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::bundle_roster_member& v);
    static ores::dq::messaging::bundle_roster_member read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::bundle_roster_bundle> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::bundle_roster_bundle& v);
    static ores::dq::messaging::bundle_roster_bundle read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::bundle_roster_bundle& v);
    static ores::dq::messaging::bundle_roster_bundle read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::bundle_roster_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::bundle_roster_response& v);
    static ores::dq::messaging::bundle_roster_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::bundle_roster_response& v);
    static ores::dq::messaging::bundle_roster_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
