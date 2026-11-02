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
#ifndef ORES_REPORTING_API_MESSAGING_RUN_DOCUMENT_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_RUN_DOCUMENT_PROTOCOL_HPP

#include "ores.reporting.api/domain/configuration_analytic.hpp"
#include "ores.reporting.api/domain/configuration_market_binding.hpp"
#include "ores.reporting.api/domain/engine_settings.hpp"
#include "ores.reporting.api/domain/engine_settings_building.hpp"
#include "ores.reporting.api/domain/engine_settings_execution.hpp"
#include "ores.reporting.api/domain/engine_settings_fixings.hpp"
#include "ores.reporting.api/domain/report_configuration.hpp"
#include "ores.reporting.api/domain/risk_report_config.hpp"
#include "ores.reporting.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::reporting::messaging {

/**
 * @brief One parameter of an analytic, by the name the run document uses.
 *
 * The stored parameter holds a definition id and a value, and the id is a
 * database concern. The document is one step earlier, where the parameter is
 * still the name ORE wrote, so the name rides beside the value and the store
 * resolves it.
 */
struct run_parameter {
    std::string name;
    std::string value;
    int position = 0;
};

/**
 * @brief An analytic of the run and the parameters it sets.
 *
 * The active flag is a parameter in ORE's schema and a column on the analytic,
 * so it lives on the analytic; the remaining parameters stay beside it, in the
 * order the document wrote them.
 */
struct run_analytic {
    ores::reporting::domain::configuration_analytic analytic;
    std::vector<run_parameter> parameters;
};

/**
 * @brief The name a run document gives to one file it reads.
 *
 * The files are data and documents that live outside the configuration: the
 * configuration documents, the portfolio, the market data and the fixings. A
 * configuration does not store the names. They come from the table of well known
 * file names when a run is built, and from the Setup of an ORE sample when one
 * is imported, where the import uses them to find the files.
 */
struct run_file {
    std::string parameter;
    std::string name;
};

/**
 * @brief The engine settings of a run: the central row and the parts it sets.
 *
 * A part that is absent means the engine default for every setting in it. The
 * Setup block of a run document holds these as parameters, and the projection
 * reads them into the parts and writes them back.
 */
struct run_engine_settings {
    ores::reporting::domain::engine_settings settings;
    std::optional<ores::reporting::domain::engine_settings_fixings> fixings;
    std::optional<ores::reporting::domain::engine_settings_building> building;
    std::optional<ores::reporting::domain::engine_settings_execution> execution;
};

/**
 * @brief One ORE run document, built from the configuration a report definition
 * binds: the choices of the run, the analytics with their parameters, the market
 * bindings and the names of the files the run reads.
 *
 * The choices, the engine settings, the analytics and the market bindings belong
 * to shared configurations. Only the file names are not stored. A document with
 * no engine settings runs with the engine defaults.
 */
struct run_document {
    ores::reporting::domain::risk_report_config settings;
    std::optional<run_engine_settings> engine_settings;
    std::vector<run_file> files;
    std::vector<run_analytic> analytics;
    std::vector<ores::reporting::domain::configuration_market_binding> market_bindings;
};

/**
 * @brief Stores a run document against a report definition.
 *
 * Refused when the definition already holds one, or when an analytic sets a
 * parameter no definition describes.
 */
struct save_run_document_request {
    using response_type = struct save_run_document_response;
    static constexpr std::string_view nats_subject = "reporting.v1.run_documents.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_definition_id;
    run_document document;
};

/**
 * @brief Whether the save succeeded.
 */
struct save_run_document_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Creates a configuration of a type and binds it to the definition in
 * that type's slot.
 */
struct bind_configuration_request {
    using response_type = struct bind_configuration_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.bind_configuration";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_definition_id;
    std::string configuration_type_code;
    std::string name;
};

/**
 * @brief The configuration the binding created.
 */
struct bind_configuration_response {
    bool success = false;
    std::string message;
    std::string configuration_id;
};

/**
 * @brief Reads a definition's run document and its bindings.
 */
struct get_run_document_request {
    using response_type = struct get_run_document_response;
    static constexpr std::string_view nats_subject = "reporting.v1.run_documents.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_definition_id;
};

/**
 * @brief The run document, the slots it fills, and the party that owns it.
 */
struct get_run_document_response {
    bool success = false;
    std::string message;
    run_document document;
    std::vector<domain::report_configuration> bindings;
    /** The party that owns the definition, whose configuration documents the run reads. */
    std::string party_id;
};

/**
 * @brief Deletes a definition's run document, its bindings and the
 * configuration rows they name.
 *
 * A run import's compensation sends it, to undo a save.
 */
struct delete_run_document_request {
    using response_type = struct delete_run_document_response;
    static constexpr std::string_view nats_subject = "reporting.v1.run_documents.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_definition_id;
};

/**
 * @brief Whether the delete succeeded.
 */
struct delete_run_document_response {
    bool success = false;
    std::string message;
};

/**
 * @brief The members of run_parameter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const run_parameter&) {
    return std::tuple{
        ::ores::utility::codec::field{"name", &run_parameter::name},
        ::ores::utility::codec::field{"value", &run_parameter::value},
        ::ores::utility::codec::field{"position", &run_parameter::position},
    };
}

/**
 * @brief The members of run_analytic, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const run_analytic&) {
    return std::tuple{
        ::ores::utility::codec::field{"analytic", &run_analytic::analytic},
        ::ores::utility::codec::field{"parameters", &run_analytic::parameters},
    };
}

/**
 * @brief The members of run_file, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const run_file&) {
    return std::tuple{
        ::ores::utility::codec::field{"parameter", &run_file::parameter},
        ::ores::utility::codec::field{"name", &run_file::name},
    };
}

/**
 * @brief The members of run_engine_settings, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const run_engine_settings&) {
    return std::tuple{
        ::ores::utility::codec::field{"settings", &run_engine_settings::settings},
        ::ores::utility::codec::field{"fixings", &run_engine_settings::fixings},
        ::ores::utility::codec::field{"building", &run_engine_settings::building},
        ::ores::utility::codec::field{"execution", &run_engine_settings::execution},
    };
}

/**
 * @brief The members of run_document, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const run_document&) {
    return std::tuple{
        ::ores::utility::codec::field{"settings", &run_document::settings},
        ::ores::utility::codec::field{"engine_settings", &run_document::engine_settings},
        ::ores::utility::codec::field{"files", &run_document::files},
        ::ores::utility::codec::field{"analytics", &run_document::analytics},
        ::ores::utility::codec::field{"market_bindings", &run_document::market_bindings},
    };
}

/**
 * @brief The members of save_run_document_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const save_run_document_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"report_definition_id",
                                      &save_run_document_request::report_definition_id},
        ::ores::utility::codec::field{"document", &save_run_document_request::document},
    };
}

/**
 * @brief The members of save_run_document_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const save_run_document_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &save_run_document_response::success},
        ::ores::utility::codec::field{"message", &save_run_document_response::message},
    };
}

/**
 * @brief The members of bind_configuration_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bind_configuration_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"report_definition_id",
                                      &bind_configuration_request::report_definition_id},
        ::ores::utility::codec::field{"configuration_type_code",
                                      &bind_configuration_request::configuration_type_code},
        ::ores::utility::codec::field{"name", &bind_configuration_request::name},
    };
}

/**
 * @brief The members of bind_configuration_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bind_configuration_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &bind_configuration_response::success},
        ::ores::utility::codec::field{"message", &bind_configuration_response::message},
        ::ores::utility::codec::field{"configuration_id",
                                      &bind_configuration_response::configuration_id},
    };
}

/**
 * @brief The members of get_run_document_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_run_document_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"report_definition_id",
                                      &get_run_document_request::report_definition_id},
    };
}

/**
 * @brief The members of get_run_document_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_run_document_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_run_document_response::success},
        ::ores::utility::codec::field{"message", &get_run_document_response::message},
        ::ores::utility::codec::field{"document", &get_run_document_response::document},
        ::ores::utility::codec::field{"bindings", &get_run_document_response::bindings},
        ::ores::utility::codec::field{"party_id", &get_run_document_response::party_id},
    };
}

/**
 * @brief The members of delete_run_document_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_run_document_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"report_definition_id",
                                      &delete_run_document_request::report_definition_id},
    };
}

/**
 * @brief The members of delete_run_document_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_run_document_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &delete_run_document_response::success},
        ::ores::utility::codec::field{"message", &delete_run_document_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::run_parameter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::run_parameter& v);
    static ores::reporting::messaging::run_parameter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::run_parameter& v);
    static ores::reporting::messaging::run_parameter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::run_analytic> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::run_analytic& v);
    static ores::reporting::messaging::run_analytic read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::run_analytic& v);
    static ores::reporting::messaging::run_analytic read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::run_file> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::run_file& v);
    static ores::reporting::messaging::run_file read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::run_file& v);
    static ores::reporting::messaging::run_file read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::run_engine_settings> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::run_engine_settings& v);
    static ores::reporting::messaging::run_engine_settings read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::run_engine_settings& v);
    static ores::reporting::messaging::run_engine_settings
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::run_document> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::run_document& v);
    static ores::reporting::messaging::run_document read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::run_document& v);
    static ores::reporting::messaging::run_document read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::save_run_document_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::save_run_document_request& v);
    static ores::reporting::messaging::save_run_document_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::save_run_document_request& v);
    static ores::reporting::messaging::save_run_document_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::save_run_document_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::save_run_document_response& v);
    static ores::reporting::messaging::save_run_document_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::save_run_document_response& v);
    static ores::reporting::messaging::save_run_document_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::bind_configuration_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::bind_configuration_request& v);
    static ores::reporting::messaging::bind_configuration_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::bind_configuration_request& v);
    static ores::reporting::messaging::bind_configuration_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::bind_configuration_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::bind_configuration_response& v);
    static ores::reporting::messaging::bind_configuration_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::bind_configuration_response& v);
    static ores::reporting::messaging::bind_configuration_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_run_document_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_run_document_request& v);
    static ores::reporting::messaging::get_run_document_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_run_document_request& v);
    static ores::reporting::messaging::get_run_document_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_run_document_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_run_document_response& v);
    static ores::reporting::messaging::get_run_document_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_run_document_response& v);
    static ores::reporting::messaging::get_run_document_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_run_document_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::delete_run_document_request& v);
    static ores::reporting::messaging::delete_run_document_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_run_document_request& v);
    static ores::reporting::messaging::delete_run_document_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_run_document_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_run_document_response& v);
    static ores::reporting::messaging::delete_run_document_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_run_document_response& v);
    static ores::reporting::messaging::delete_run_document_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
