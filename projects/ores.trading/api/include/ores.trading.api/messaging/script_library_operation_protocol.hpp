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
#ifndef ORES_TRADING_API_MESSAGING_SCRIPT_LIBRARY_OPERATION_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_SCRIPT_LIBRARY_OPERATION_PROTOCOL_HPP

#include "ores.trading.api/domain/script.hpp"
#include "ores.trading.api/domain/script_calibration.hpp"
#include "ores.trading.api/domain/script_calibration_strike.hpp"
#include "ores.trading.api/domain/script_coarsening.hpp"
#include "ores.trading.api/domain/script_result.hpp"
#include "ores.trading.api/domain/script_variant.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::trading::messaging {

/**
 * @brief One version of a library script, as a scripted trade records it.
 */
struct script_pin {
    /**
     * @brief The tenant that holds the script: the trade's own, or the system tenant for the world
     * library.
     */
    boost::uuids::uuid tenant_id;
    /**
     * @brief The script's name.
     */
    std::string name;
    /**
     * @brief The version of the script.
     */
    int version = 0;

    friend bool operator==(const script_pin&, const script_pin&) = default;
};

/**
 * @brief Reads the rows of stated versions of library scripts.
 */
struct export_script_library_request {
    using response_type = struct export_script_library_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.export_script_library";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The versions to read.
     */
    std::vector<script_pin> pins;

    friend bool operator==(const export_script_library_request&,
                           const export_script_library_request&) = default;
};

/**
 * @brief The rows of the stated versions, and the versions the store does not hold.
 */
struct export_script_library_response {
    /**
     * @brief Whether the read ran.
     */
    bool success = false;
    /**
     * @brief Why the read failed, when it did.
     */
    std::string message;
    /**
     * @brief The script row of each version held.
     */
    std::vector<ores::trading::domain::script> scripts;
    /**
     * @brief The variants of each version held.
     */
    std::vector<ores::trading::domain::script_variant> variants;
    /**
     * @brief The results of each version held.
     */
    std::vector<ores::trading::domain::script_result> results;
    /**
     * @brief The calibrations of each version held.
     */
    std::vector<ores::trading::domain::script_calibration> calibrations;
    /**
     * @brief The calibration strikes of each version held.
     */
    std::vector<ores::trading::domain::script_calibration_strike> calibration_strikes;
    /**
     * @brief The schedule coarsenings of each version held.
     */
    std::vector<ores::trading::domain::script_coarsening> coarsenings;
    /**
     * @brief The stated versions the store does not hold, in the order stated.
     */
    std::vector<script_pin> missing;

    friend bool operator==(const export_script_library_response&,
                           const export_script_library_response&) = default;
};

/**
 * @brief The members of script_pin, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_pin>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"tenant_id", &script_pin::tenant_id},
        ::ores::utility::codec::field{"name", &script_pin::name},
        ::ores::utility::codec::field{"version", &script_pin::version},
    };
}

/**
 * @brief The members of export_script_library_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<export_script_library_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"pins", &export_script_library_request::pins},
    };
}

/**
 * @brief The members of export_script_library_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<export_script_library_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &export_script_library_response::success},
        ::ores::utility::codec::field{"message", &export_script_library_response::message},
        ::ores::utility::codec::field{"scripts", &export_script_library_response::scripts},
        ::ores::utility::codec::field{"variants", &export_script_library_response::variants},
        ::ores::utility::codec::field{"results", &export_script_library_response::results},
        ::ores::utility::codec::field{"calibrations",
                                      &export_script_library_response::calibrations},
        ::ores::utility::codec::field{"calibration_strikes",
                                      &export_script_library_response::calibration_strikes},
        ::ores::utility::codec::field{"coarsenings", &export_script_library_response::coarsenings},
        ::ores::utility::codec::field{"missing", &export_script_library_response::missing},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_pin> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_pin& v);
    static ores::trading::messaging::script_pin read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_pin& v);
    static ores::trading::messaging::script_pin read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::export_script_library_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::export_script_library_request& v);
    static ores::trading::messaging::export_script_library_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::export_script_library_request& v);
    static ores::trading::messaging::export_script_library_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::export_script_library_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::export_script_library_response& v);
    static ores::trading::messaging::export_script_library_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::export_script_library_response& v);
    static ores::trading::messaging::export_script_library_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
