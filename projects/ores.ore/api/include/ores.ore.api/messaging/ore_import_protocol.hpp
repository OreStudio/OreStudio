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
#ifndef ORES_ORE_API_MESSAGING_ORE_IMPORT_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_ORE_IMPORT_PROTOCOL_HPP

#include "ores.ore.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::ore::messaging {

/**
 * @brief Error for a single item within an ORE import.
 *
 * Carries the exact source file and item identifier so the caller can present
 * a precise failure location, such as "trades.xml / trade-0042".
 */
struct ore_import_item_error {
    std::string source_file;
    std::string item_id;
    std::string message;

    friend bool operator==(const ore_import_item_error&, const ore_import_item_error&) = default;
};

/**
 * @brief Request to import an ORE directory already uploaded to storage.
 *
 * The caller uploads the packed directory to the platform bucket ores at
 * ore/imports/{request_id}.tar.gz before sending this message.
 */
struct ore_import_request {
    using response_type = struct ore_import_response;
    static constexpr std::string_view nats_subject = "workflow.v1.ops.ore_import";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** UUID; also the storage key root. */
    std::string request_id;
    std::string import_choices_json;
    std::string correlation_id;

    friend bool operator==(const ore_import_request&, const ore_import_request&) = default;
};

/**
 * @brief Response for an ore_import_request.
 *
 * On partial failure success is still true and item_errors is non-empty; a
 * false success means the saga itself failed and compensation ran.
 */
struct ore_import_response {
    bool success = false;
    std::string message;
    std::vector<ore_import_item_error> item_errors;
    /**
     * Set when the import was dispatched asynchronously through the workflow
     * engine. The import then runs in the background, item_errors is empty in
     * this response, and this id queries the workflow's status.
     */
    std::string correlation_id;
    std::string workflow_instance_id;

    friend bool operator==(const ore_import_response&, const ore_import_response&) = default;
};

/**
 * @brief Workflow step command: execute the full ORE import.
 *
 * Published by the workflow engine when the ore_import_workflow starts. The
 * handler fetches the packed tarball, scans, plans and saves every item, then
 * calls publish_step_completion.
 */
struct ore_import_execute_request {
    static constexpr std::string_view nats_subject = "ore.v1.ops.ore_import_execute";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;
    std::string import_choices_json;
    std::string correlation_id;
    /** The caller's JWT, so the handler can delegate the caller's identity downstream. */
    std::string bearer_token;

    friend bool operator==(const ore_import_execute_request&,
                           const ore_import_execute_request&) = default;
};

/**
 * @brief The step's stored response.
 *
 * Carries everything compensation needs -- the identifiers of every entity
 * the step saved -- alongside the caller-facing item_errors list.
 */
struct ore_import_execute_result {
    bool success = false;
    std::string message;
    std::string correlation_id;
    std::vector<ore_import_item_error> item_errors;
    std::vector<std::string> saved_currency_iso_codes;
    std::vector<std::string> saved_portfolio_names;
    std::vector<std::string> saved_book_names;
    /**
     * The id of each booked trade. The rollback closes these trades' bookings,
     * because a trade's anchor is immutable.
     */
    std::vector<std::string> saved_trade_ids;

    friend bool operator==(const ore_import_execute_result&,
                           const ore_import_execute_result&) = default;
};

/**
 * @brief Workflow compensation command: roll back a completed ORE import.
 *
 * Published by the workflow engine when step 0's compensation is triggered.
 * The handler deletes every saved entity in reverse order and calls
 * publish_step_completion.
 */
struct ore_import_rollback_request {
    static constexpr std::string_view nats_subject = "ore.v1.ops.ore_import_rollback";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string correlation_id;
    /** The caller's JWT, so the handler can delegate the caller's identity downstream. */
    std::string bearer_token;
    std::vector<std::string> saved_currency_iso_codes;
    std::vector<std::string> saved_portfolio_names;
    std::vector<std::string> saved_book_names;
    /** The id of each booked trade, whose booking the rollback closes. */
    std::vector<std::string> saved_trade_ids;

    friend bool operator==(const ore_import_rollback_request&,
                           const ore_import_rollback_request&) = default;
};

/**
 * @brief The members of ore_import_item_error, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_import_item_error&) {
    return std::tuple{
        ::ores::utility::codec::field{"source_file", &ore_import_item_error::source_file},
        ::ores::utility::codec::field{"item_id", &ore_import_item_error::item_id},
        ::ores::utility::codec::field{"message", &ore_import_item_error::message},
    };
}

/**
 * @brief The members of ore_import_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_import_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"request_id", &ore_import_request::request_id},
        ::ores::utility::codec::field{"import_choices_json",
                                      &ore_import_request::import_choices_json},
        ::ores::utility::codec::field{"correlation_id", &ore_import_request::correlation_id},
    };
}

/**
 * @brief The members of ore_import_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_import_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &ore_import_response::success},
        ::ores::utility::codec::field{"message", &ore_import_response::message},
        ::ores::utility::codec::field{"item_errors", &ore_import_response::item_errors},
        ::ores::utility::codec::field{"correlation_id", &ore_import_response::correlation_id},
        ::ores::utility::codec::field{"workflow_instance_id",
                                      &ore_import_response::workflow_instance_id},
    };
}

/**
 * @brief The members of ore_import_execute_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_import_execute_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"request_id", &ore_import_execute_request::request_id},
        ::ores::utility::codec::field{"import_choices_json",
                                      &ore_import_execute_request::import_choices_json},
        ::ores::utility::codec::field{"correlation_id",
                                      &ore_import_execute_request::correlation_id},
        ::ores::utility::codec::field{"bearer_token", &ore_import_execute_request::bearer_token},
    };
}

/**
 * @brief The members of ore_import_execute_result, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_import_execute_result&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &ore_import_execute_result::success},
        ::ores::utility::codec::field{"message", &ore_import_execute_result::message},
        ::ores::utility::codec::field{"correlation_id", &ore_import_execute_result::correlation_id},
        ::ores::utility::codec::field{"item_errors", &ore_import_execute_result::item_errors},
        ::ores::utility::codec::field{"saved_currency_iso_codes",
                                      &ore_import_execute_result::saved_currency_iso_codes},
        ::ores::utility::codec::field{"saved_portfolio_names",
                                      &ore_import_execute_result::saved_portfolio_names},
        ::ores::utility::codec::field{"saved_book_names",
                                      &ore_import_execute_result::saved_book_names},
        ::ores::utility::codec::field{"saved_trade_ids",
                                      &ore_import_execute_result::saved_trade_ids},
    };
}

/**
 * @brief The members of ore_import_rollback_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ore_import_rollback_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"correlation_id",
                                      &ore_import_rollback_request::correlation_id},
        ::ores::utility::codec::field{"bearer_token", &ore_import_rollback_request::bearer_token},
        ::ores::utility::codec::field{"saved_currency_iso_codes",
                                      &ore_import_rollback_request::saved_currency_iso_codes},
        ::ores::utility::codec::field{"saved_portfolio_names",
                                      &ore_import_rollback_request::saved_portfolio_names},
        ::ores::utility::codec::field{"saved_book_names",
                                      &ore_import_rollback_request::saved_book_names},
        ::ores::utility::codec::field{"saved_trade_ids",
                                      &ore_import_rollback_request::saved_trade_ids},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::ore_import_item_error> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::ore_import_item_error& v);
    static ores::ore::messaging::ore_import_item_error read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::ore_import_item_error& v);
    static ores::ore::messaging::ore_import_item_error
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::ore_import_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::ore_import_request& v);
    static ores::ore::messaging::ore_import_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::ore_import_request& v);
    static ores::ore::messaging::ore_import_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::ore_import_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::ore_import_response& v);
    static ores::ore::messaging::ore_import_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::ore_import_response& v);
    static ores::ore::messaging::ore_import_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::ore_import_execute_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::ore_import_execute_request& v);
    static ores::ore::messaging::ore_import_execute_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::ore_import_execute_request& v);
    static ores::ore::messaging::ore_import_execute_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::ore_import_execute_result> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::ore_import_execute_result& v);
    static ores::ore::messaging::ore_import_execute_result read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::ore_import_execute_result& v);
    static ores::ore::messaging::ore_import_execute_result
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::ore_import_rollback_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::ore_import_rollback_request& v);
    static ores::ore::messaging::ore_import_rollback_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::ore_import_rollback_request& v);
    static ores::ore::messaging::ore_import_rollback_request
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
