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
#ifndef ORES_ANALYTICS_API_MESSAGING_CONFIGURATION_DOCUMENT_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_CONFIGURATION_DOCUMENT_PROTOCOL_HPP

#include "ores.analytics.api/domain/pricing_model_config.hpp"
#include "ores.analytics.api/domain/pricing_model_product.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"
#include "ores.analytics.api/domain/todays_market_collection.hpp"
#include "ores.analytics.api/domain/todays_market_config.hpp"
#include "ores.analytics.api/domain/todays_market_configuration.hpp"
#include "ores.analytics.api/domain/todays_market_configuration_binding.hpp"
#include "ores.analytics.api/domain/todays_market_entry.hpp"
#include "ores.analytics.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::analytics::messaging {

/**
 * @brief One ORE pricing engines document as the rows analytics stores.
 */
struct pricing_engines_document {
    ores::analytics::domain::pricing_model_config config;
    std::vector<ores::analytics::domain::pricing_model_product> products;
    std::vector<ores::analytics::domain::pricing_model_product_parameter> parameters;

    friend bool operator==(const pricing_engines_document&,
                           const pricing_engines_document&) = default;
};

/**
 * @brief One ORE today's market document as the rows analytics stores.
 */
struct todays_market_document {
    ores::analytics::domain::todays_market_config config;
    std::vector<ores::analytics::domain::todays_market_collection> collections;
    std::vector<ores::analytics::domain::todays_market_entry> entries;
    std::vector<ores::analytics::domain::todays_market_configuration> configurations;
    std::vector<ores::analytics::domain::todays_market_configuration_binding> bindings;

    friend bool operator==(const todays_market_document&, const todays_market_document&) = default;
};

/**
 * @brief One reason a save is refused, named so a client can act on it.
 *
 * A refused save reports every problem it found, not the first, so a person
 * fixes them in one pass. Every field but code and message is empty when the
 * problem is not about a product or a parameter.
 */
struct pricing_engines_document_problem {
    /**
     * @brief Which rule the document broke: name_missing, no_products, model_missing,
     * engine_missing, parameter_repeated or parameter_value_missing.
     */
    std::string code;
    /**
     * @brief The product the problem is about, when it is about one.
     */
    std::string product_id;
    /**
     * @brief The scope of the setting the problem is about, when it is about one.
     */
    std::string parameter_scope;
    /**
     * @brief The name of the setting the problem is about, when it is about one.
     */
    std::string parameter_name;
    /**
     * @brief The problem in a sentence a person reads.
     */
    std::string message;

    friend bool operator==(const pricing_engines_document_problem&,
                           const pricing_engines_document_problem&) = default;
};

/**
 * @brief Stores a pricing engines document.
 *
 * The session must act for a party, which owns every row. The header carries the reporting
 * configuration it fills, which is what a get finds it by.
 */
struct save_pricing_engines_document_request {
    using response_type = struct save_pricing_engines_document_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_engines_documents.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_engines_document document;

    friend bool operator==(const save_pricing_engines_document_request&,
                           const save_pricing_engines_document_request&) = default;
};

/**
 * @brief The id of the header the save stored.
 */
struct save_pricing_engines_document_response {
    bool success = false;
    std::string message;
    std::string id;
    /**
     * @brief Every reason the save was refused, in the order a person reads them. Empty when it
     * saved.
     */
    std::vector<pricing_engines_document_problem> problems;

    friend bool operator==(const save_pricing_engines_document_response&,
                           const save_pricing_engines_document_response&) = default;
};

/**
 * @brief Reads a pricing engines document by the reporting configuration it fills.
 */
struct get_pricing_engines_document_request {
    using response_type = struct get_pricing_engines_document_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_engines_documents.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read.
     */
    std::string party_id;

    friend bool operator==(const get_pricing_engines_document_request&,
                           const get_pricing_engines_document_request&) = default;
};

/**
 * @brief A pricing engines document, when one fills the configuration.
 */
struct get_pricing_engines_document_response {
    bool success = false;
    std::string message;
    pricing_engines_document document;

    friend bool operator==(const get_pricing_engines_document_response&,
                           const get_pricing_engines_document_response&) = default;
};

/**
 * @brief Deletes a pricing engines document that fills a reporting configuration.
 *
 * A run import's compensation sends it, to undo a save. Deleting a
 * configuration no document fills succeeds, so a compensation can run twice.
 */
struct delete_pricing_engines_document_request {
    using response_type = struct delete_pricing_engines_document_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_engines_documents.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;

    friend bool operator==(const delete_pricing_engines_document_request&,
                           const delete_pricing_engines_document_request&) = default;
};

/**
 * @brief Whether the delete succeeded.
 */
struct delete_pricing_engines_document_response {
    bool success = false;
    std::string message;

    friend bool operator==(const delete_pricing_engines_document_response&,
                           const delete_pricing_engines_document_response&) = default;
};

/**
 * @brief Stores a today's market document.
 *
 * The session must act for a party, which owns every row. The header carries the reporting
 * configuration it fills, which is what a get finds it by.
 */
struct save_todays_market_document_request {
    using response_type = struct save_todays_market_document_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_documents.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_document document;

    friend bool operator==(const save_todays_market_document_request&,
                           const save_todays_market_document_request&) = default;
};

/**
 * @brief The id of the header the save stored.
 */
struct save_todays_market_document_response {
    bool success = false;
    std::string message;
    std::string id;

    friend bool operator==(const save_todays_market_document_response&,
                           const save_todays_market_document_response&) = default;
};

/**
 * @brief Reads a today's market document by the reporting configuration it fills.
 */
struct get_todays_market_document_request {
    using response_type = struct get_todays_market_document_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_documents.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read.
     */
    std::string party_id;

    friend bool operator==(const get_todays_market_document_request&,
                           const get_todays_market_document_request&) = default;
};

/**
 * @brief A today's market document, when one fills the configuration.
 */
struct get_todays_market_document_response {
    bool success = false;
    std::string message;
    todays_market_document document;

    friend bool operator==(const get_todays_market_document_response&,
                           const get_todays_market_document_response&) = default;
};

/**
 * @brief Deletes a today's market document that fills a reporting configuration.
 *
 * A run import's compensation sends it, to undo a save. Deleting a
 * configuration no document fills succeeds, so a compensation can run twice.
 */
struct delete_todays_market_document_request {
    using response_type = struct delete_todays_market_document_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_documents.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;

    friend bool operator==(const delete_todays_market_document_request&,
                           const delete_todays_market_document_request&) = default;
};

/**
 * @brief Whether the delete succeeded.
 */
struct delete_todays_market_document_response {
    bool success = false;
    std::string message;

    friend bool operator==(const delete_todays_market_document_response&,
                           const delete_todays_market_document_response&) = default;
};

/**
 * @brief The members of pricing_engines_document, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const pricing_engines_document&) {
    return std::tuple{
        ::ores::utility::codec::field{"config", &pricing_engines_document::config},
        ::ores::utility::codec::field{"products", &pricing_engines_document::products},
        ::ores::utility::codec::field{"parameters", &pricing_engines_document::parameters},
    };
}

/**
 * @brief The members of todays_market_document, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const todays_market_document&) {
    return std::tuple{
        ::ores::utility::codec::field{"config", &todays_market_document::config},
        ::ores::utility::codec::field{"collections", &todays_market_document::collections},
        ::ores::utility::codec::field{"entries", &todays_market_document::entries},
        ::ores::utility::codec::field{"configurations", &todays_market_document::configurations},
        ::ores::utility::codec::field{"bindings", &todays_market_document::bindings},
    };
}

/**
 * @brief The members of pricing_engines_document_problem, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const pricing_engines_document_problem&) {
    return std::tuple{
        ::ores::utility::codec::field{"code", &pricing_engines_document_problem::code},
        ::ores::utility::codec::field{"product_id", &pricing_engines_document_problem::product_id},
        ::ores::utility::codec::field{"parameter_scope",
                                      &pricing_engines_document_problem::parameter_scope},
        ::ores::utility::codec::field{"parameter_name",
                                      &pricing_engines_document_problem::parameter_name},
        ::ores::utility::codec::field{"message", &pricing_engines_document_problem::message},
    };
}

/**
 * @brief The members of save_pricing_engines_document_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const save_pricing_engines_document_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"document", &save_pricing_engines_document_request::document},
    };
}

/**
 * @brief The members of save_pricing_engines_document_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const save_pricing_engines_document_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &save_pricing_engines_document_response::success},
        ::ores::utility::codec::field{"message", &save_pricing_engines_document_response::message},
        ::ores::utility::codec::field{"id", &save_pricing_engines_document_response::id},
        ::ores::utility::codec::field{"problems",
                                      &save_pricing_engines_document_response::problems},
    };
}

/**
 * @brief The members of get_pricing_engines_document_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_pricing_engines_document_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"configuration_id",
                                      &get_pricing_engines_document_request::configuration_id},
        ::ores::utility::codec::field{"party_id", &get_pricing_engines_document_request::party_id},
    };
}

/**
 * @brief The members of get_pricing_engines_document_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_pricing_engines_document_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_pricing_engines_document_response::success},
        ::ores::utility::codec::field{"message", &get_pricing_engines_document_response::message},
        ::ores::utility::codec::field{"document", &get_pricing_engines_document_response::document},
    };
}

/**
 * @brief The members of delete_pricing_engines_document_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_pricing_engines_document_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"configuration_id",
                                      &delete_pricing_engines_document_request::configuration_id},
    };
}

/**
 * @brief The members of delete_pricing_engines_document_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_pricing_engines_document_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success",
                                      &delete_pricing_engines_document_response::success},
        ::ores::utility::codec::field{"message",
                                      &delete_pricing_engines_document_response::message},
    };
}

/**
 * @brief The members of save_todays_market_document_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const save_todays_market_document_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"document", &save_todays_market_document_request::document},
    };
}

/**
 * @brief The members of save_todays_market_document_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const save_todays_market_document_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &save_todays_market_document_response::success},
        ::ores::utility::codec::field{"message", &save_todays_market_document_response::message},
        ::ores::utility::codec::field{"id", &save_todays_market_document_response::id},
    };
}

/**
 * @brief The members of get_todays_market_document_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_todays_market_document_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"configuration_id",
                                      &get_todays_market_document_request::configuration_id},
        ::ores::utility::codec::field{"party_id", &get_todays_market_document_request::party_id},
    };
}

/**
 * @brief The members of get_todays_market_document_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_todays_market_document_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_todays_market_document_response::success},
        ::ores::utility::codec::field{"message", &get_todays_market_document_response::message},
        ::ores::utility::codec::field{"document", &get_todays_market_document_response::document},
    };
}

/**
 * @brief The members of delete_todays_market_document_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_todays_market_document_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"configuration_id",
                                      &delete_todays_market_document_request::configuration_id},
    };
}

/**
 * @brief The members of delete_todays_market_document_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_todays_market_document_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &delete_todays_market_document_response::success},
        ::ores::utility::codec::field{"message", &delete_todays_market_document_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::pricing_engines_document> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::pricing_engines_document& v);
    static ores::analytics::messaging::pricing_engines_document read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::pricing_engines_document& v);
    static ores::analytics::messaging::pricing_engines_document
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::todays_market_document> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::todays_market_document& v);
    static ores::analytics::messaging::todays_market_document read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::todays_market_document& v);
    static ores::analytics::messaging::todays_market_document
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::pricing_engines_document_problem> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::pricing_engines_document_problem& v);
    static ores::analytics::messaging::pricing_engines_document_problem
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::pricing_engines_document_problem& v);
    static ores::analytics::messaging::pricing_engines_document_problem
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::save_pricing_engines_document_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::save_pricing_engines_document_request& v);
    static ores::analytics::messaging::save_pricing_engines_document_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::save_pricing_engines_document_request& v);
    static ores::analytics::messaging::save_pricing_engines_document_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::save_pricing_engines_document_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::save_pricing_engines_document_response& v);
    static ores::analytics::messaging::save_pricing_engines_document_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::save_pricing_engines_document_response& v);
    static ores::analytics::messaging::save_pricing_engines_document_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_pricing_engines_document_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_pricing_engines_document_request& v);
    static ores::analytics::messaging::get_pricing_engines_document_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_pricing_engines_document_request& v);
    static ores::analytics::messaging::get_pricing_engines_document_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_pricing_engines_document_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_pricing_engines_document_response& v);
    static ores::analytics::messaging::get_pricing_engines_document_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_pricing_engines_document_response& v);
    static ores::analytics::messaging::get_pricing_engines_document_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_pricing_engines_document_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_pricing_engines_document_request& v);
    static ores::analytics::messaging::delete_pricing_engines_document_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_pricing_engines_document_request& v);
    static ores::analytics::messaging::delete_pricing_engines_document_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_pricing_engines_document_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_pricing_engines_document_response& v);
    static ores::analytics::messaging::delete_pricing_engines_document_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_pricing_engines_document_response& v);
    static ores::analytics::messaging::delete_pricing_engines_document_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::save_todays_market_document_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::save_todays_market_document_request& v);
    static ores::analytics::messaging::save_todays_market_document_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::save_todays_market_document_request& v);
    static ores::analytics::messaging::save_todays_market_document_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::save_todays_market_document_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::save_todays_market_document_response& v);
    static ores::analytics::messaging::save_todays_market_document_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::save_todays_market_document_response& v);
    static ores::analytics::messaging::save_todays_market_document_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_todays_market_document_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_todays_market_document_request& v);
    static ores::analytics::messaging::get_todays_market_document_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_todays_market_document_request& v);
    static ores::analytics::messaging::get_todays_market_document_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_todays_market_document_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_todays_market_document_response& v);
    static ores::analytics::messaging::get_todays_market_document_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_todays_market_document_response& v);
    static ores::analytics::messaging::get_todays_market_document_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_todays_market_document_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_todays_market_document_request& v);
    static ores::analytics::messaging::delete_todays_market_document_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_todays_market_document_request& v);
    static ores::analytics::messaging::delete_todays_market_document_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_todays_market_document_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_todays_market_document_response& v);
    static ores::analytics::messaging::delete_todays_market_document_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_todays_market_document_response& v);
    static ores::analytics::messaging::delete_todays_market_document_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
