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
 * @brief One result a variant of a published script reports, in the order the script states it.
 */
struct published_result {
    /**
     * @brief The variable whose value the engine reports.
     */
    std::string variable;
    /**
     * @brief The name the engine reports it under, when it differs.
     */
    std::optional<std::string> rename;

    friend bool operator==(const published_result&, const published_result&) = default;
};

/**
 * @brief One calibration of a variant of a published script.
 */
struct published_calibration {
    /**
     * @brief The variable naming the index the engine calibrates.
     */
    std::string index_variable;
    /**
     * @brief The variables naming the strikes the engine calibrates at, in order.
     */
    std::vector<std::string> strikes;

    friend bool operator==(const published_calibration&, const published_calibration&) = default;
};

/**
 * @brief One variant of a published script: its code and what the engine reads beside it.
 */
struct published_variant {
    /**
     * @brief What the variant is for: default for the variant a trade prices with, or the purpose
     * ORE names, such as AMC.
     */
    std::string purpose;
    /**
     * @brief The variant's text in ORE's payoff language.
     */
    std::string code;
    /**
     * @brief The variable whose value is the trade's NPV.
     */
    std::string npv_variable;
    /**
     * @brief The results the variant reports.
     */
    std::vector<published_result> results;
    /**
     * @brief The calibrations the variant asks for.
     */
    std::vector<published_calibration> calibrations;
    /**
     * @brief The schedules the engine may coarsen, in order.
     */
    std::vector<std::string> coarsenings;

    friend bool operator==(const published_variant&, const published_variant&) = default;
};

/**
 * @brief One finding of the parse of a published script, located in the variant it concerns.
 */
struct script_diagnostic {
    /**
     * @brief The variant the finding is in.
     */
    std::string purpose;
    /**
     * @brief error or warning; an error refuses the publish.
     */
    std::string severity;
    /**
     * @brief The diagnostic's code, such as SCR1001.
     */
    std::string code;
    /**
     * @brief The one-based line of the finding's start, or 0 where its position is unknown.
     */
    int line = 0;
    /**
     * @brief The one-based column of the finding's start, where known.
     */
    std::optional<int> column;
    /**
     * @brief What the finding says.
     */
    std::string message;
    /**
     * @brief The variable a finding about the trade's variables is about; empty for a finding
     * located in the text.
     */
    std::string subject;

    friend bool operator==(const script_diagnostic&, const script_diagnostic&) = default;
};

/**
 * @brief One variable a trade states for its script.
 */
struct script_variable_value {
    /**
     * @brief The variable's name, as the script reads it.
     */
    std::string name;
    /**
     * @brief The type ORE reads it as: Number, Event, Index, Currency or Daycounter.
     */
    std::string type;
    /**
     * @brief Whether the variable is a list.
     */
    bool is_array = false;
    /**
     * @brief Its value, or its values when it is a list, as text.
     */
    std::vector<std::string> values;

    friend bool operator==(const script_variable_value&, const script_variable_value&) = default;
};

/**
 * @brief Validates a script against the variables a trade states, booking nothing.
 */
struct validate_script_request {
    using response_type = struct validate_script_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.validate_script";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The library script to validate, as the caller's tenant resolves the name; empty to
     * validate code instead.
     */
    std::string script_name;
    /**
     * @brief The version of the library script; 0 for its current version. A negative version names
     * no script.
     */
    int script_version = 0;
    /**
     * @brief An inline script's code, read when script_name is empty.
     */
    std::string code;
    /**
     * @brief The variables the trade states.
     */
    std::vector<script_variable_value> variables;

    friend bool operator==(const validate_script_request&,
                           const validate_script_request&) = default;
};

/**
 * @brief The findings of a script's validation.
 */
struct validate_script_response {
    /**
     * @brief Whether the validation ran. It does not run for a script that is not held, or that has
     * no variant to parse; the message then says which, and is_valid is false.
     */
    bool success = false;
    /**
     * @brief Why it did not, when it did not, or a summary of the findings.
     */
    std::string message;
    /**
     * @brief Whether no finding is an error, so a booking on these variables would pass.
     */
    bool is_valid = false;
    /**
     * @brief Every finding: those in the text by line and column, then those about a variable by
     * its name.
     */
    std::vector<script_diagnostic> diagnostics;

    friend bool operator==(const validate_script_response&,
                           const validate_script_response&) = default;
};

/**
 * @brief Publishes a script of the caller's tenant as its next version.
 *
 * The trading service parses each variant with ORE's grammar and derives the
 * script's inputs from the variants, so the inputs are never stated by the
 * caller. A script whose parse reports an error is refused with the
 * diagnostics, and nothing is written.
 */
struct publish_script_request {
    using response_type = struct publish_script_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.publish_script";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The script's name, which a scripted trade of the tenant names.
     */
    std::string name;
    /**
     * @brief The product tag the engine prices the script with.
     */
    std::optional<std::string> product_tag;
    /**
     * @brief What the script pays.
     */
    std::optional<std::string> description;
    /**
     * @brief The script's variants; one must be the default.
     */
    std::vector<published_variant> variants;

    friend bool operator==(const publish_script_request&, const publish_script_request&) = default;
};

/**
 * @brief The version a publish wrote, or why it wrote none.
 */
struct publish_script_response {
    /**
     * @brief Whether the script is published.
     */
    bool success = false;
    /**
     * @brief What the publish did, or why it refused.
     */
    std::string message;
    /**
     * @brief The script's version in the tenant after the publish; unchanged when the script equals
     * its current version.
     */
    int version = 0;
    /**
     * @brief The parse's findings, errors and warnings, by variant.
     */
    std::vector<script_diagnostic> diagnostics;
    /**
     * @brief What the caller should know that does not refuse the publish, such as a name the world
     * library also holds.
     */
    std::vector<std::string> warnings;

    friend bool operator==(const publish_script_response&,
                           const publish_script_response&) = default;
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

/**
 * @brief The members of published_result, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<published_result>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"variable", &published_result::variable},
        ::ores::utility::codec::field{"rename", &published_result::rename},
    };
}

/**
 * @brief The members of published_calibration, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<published_calibration>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"index_variable", &published_calibration::index_variable},
        ::ores::utility::codec::field{"strikes", &published_calibration::strikes},
    };
}

/**
 * @brief The members of published_variant, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<published_variant>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"purpose", &published_variant::purpose},
        ::ores::utility::codec::field{"code", &published_variant::code},
        ::ores::utility::codec::field{"npv_variable", &published_variant::npv_variable},
        ::ores::utility::codec::field{"results", &published_variant::results},
        ::ores::utility::codec::field{"calibrations", &published_variant::calibrations},
        ::ores::utility::codec::field{"coarsenings", &published_variant::coarsenings},
    };
}

/**
 * @brief The members of script_diagnostic, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_diagnostic>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"purpose", &script_diagnostic::purpose},
        ::ores::utility::codec::field{"severity", &script_diagnostic::severity},
        ::ores::utility::codec::field{"code", &script_diagnostic::code},
        ::ores::utility::codec::field{"line", &script_diagnostic::line},
        ::ores::utility::codec::field{"column", &script_diagnostic::column},
        ::ores::utility::codec::field{"message", &script_diagnostic::message},
        ::ores::utility::codec::field{"subject", &script_diagnostic::subject},
    };
}

/**
 * @brief The members of script_variable_value, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_variable_value>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &script_variable_value::name},
        ::ores::utility::codec::field{"type", &script_variable_value::type},
        ::ores::utility::codec::field{"is_array", &script_variable_value::is_array},
        ::ores::utility::codec::field{"values", &script_variable_value::values},
    };
}

/**
 * @brief The members of validate_script_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<validate_script_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"script_name", &validate_script_request::script_name},
        ::ores::utility::codec::field{"script_version", &validate_script_request::script_version},
        ::ores::utility::codec::field{"code", &validate_script_request::code},
        ::ores::utility::codec::field{"variables", &validate_script_request::variables},
    };
}

/**
 * @brief The members of validate_script_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<validate_script_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &validate_script_response::success},
        ::ores::utility::codec::field{"message", &validate_script_response::message},
        ::ores::utility::codec::field{"is_valid", &validate_script_response::is_valid},
        ::ores::utility::codec::field{"diagnostics", &validate_script_response::diagnostics},
    };
}

/**
 * @brief The members of publish_script_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_script_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &publish_script_request::name},
        ::ores::utility::codec::field{"product_tag", &publish_script_request::product_tag},
        ::ores::utility::codec::field{"description", &publish_script_request::description},
        ::ores::utility::codec::field{"variants", &publish_script_request::variants},
    };
}

/**
 * @brief The members of publish_script_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_script_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &publish_script_response::success},
        ::ores::utility::codec::field{"message", &publish_script_response::message},
        ::ores::utility::codec::field{"version", &publish_script_response::version},
        ::ores::utility::codec::field{"diagnostics", &publish_script_response::diagnostics},
        ::ores::utility::codec::field{"warnings", &publish_script_response::warnings},
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
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::published_result> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::published_result& v);
    static ores::trading::messaging::published_result read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::published_result& v);
    static ores::trading::messaging::published_result read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::published_calibration> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::published_calibration& v);
    static ores::trading::messaging::published_calibration read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::published_calibration& v);
    static ores::trading::messaging::published_calibration
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::published_variant> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::published_variant& v);
    static ores::trading::messaging::published_variant read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::published_variant& v);
    static ores::trading::messaging::published_variant
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_diagnostic> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_diagnostic& v);
    static ores::trading::messaging::script_diagnostic read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_diagnostic& v);
    static ores::trading::messaging::script_diagnostic
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_variable_value> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_variable_value& v);
    static ores::trading::messaging::script_variable_value read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_variable_value& v);
    static ores::trading::messaging::script_variable_value
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::validate_script_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::validate_script_request& v);
    static ores::trading::messaging::validate_script_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::validate_script_request& v);
    static ores::trading::messaging::validate_script_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::validate_script_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::validate_script_response& v);
    static ores::trading::messaging::validate_script_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::validate_script_response& v);
    static ores::trading::messaging::validate_script_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::publish_script_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::publish_script_request& v);
    static ores::trading::messaging::publish_script_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::publish_script_request& v);
    static ores::trading::messaging::publish_script_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::publish_script_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::publish_script_response& v);
    static ores::trading::messaging::publish_script_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::publish_script_response& v);
    static ores::trading::messaging::publish_script_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
