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
#ifndef ORES_ORE_API_MESSAGING_RUN_CONFIGURATION_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_RUN_CONFIGURATION_PROTOCOL_HPP

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
 * @brief One file of an ORE input directory.
 *
 * The name is the one the run document gives the file; the run document
 * itself is ore.xml.
 */
struct run_input_file {
    std::string name;
    std::string content;
};

/**
 * @brief A configuration document an owner stored, by the configuration it
 * fills, so an undo can delete it.
 */
struct saved_document {
    std::string configuration_type_code;
    std::string configuration_id;
};

/**
 * @brief Starts importing an ORE input directory into a report definition.
 *
 * The definition must hold no run document yet, and the session must act for
 * the party that owns it.
 */
struct import_run_configuration_request {
    using response_type = struct import_run_configuration_response;
    static constexpr std::string_view nats_subject = "ore.v1.ops.import_run_configuration";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_definition_id;
    /** Prefixes each configuration the import creates, as name/file. */
    std::string name;
    std::vector<run_input_file> files;
};

/**
 * @brief The workflow running the import, whose status the caller follows.
 */
struct import_run_configuration_response {
    bool success = false;
    std::string message;
    std::string correlation_id;
    std::string workflow_instance_id;
};

/**
 * @brief The import's execute step, which the workflow engine sends.
 *
 * Carries the caller's token so the step stores each document as the caller.
 */
struct run_configuration_import_execute_request {
    static constexpr std::string_view nats_subject = "ore.v1.ops.run_configuration_import_execute";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_definition_id;
    std::string name;
    std::vector<run_input_file> files;
    std::string correlation_id;
    /** The caller's JWT, which the step delegates to the owners. */
    std::string bearer_token;
};

/**
 * @brief What the execute step stored, travelling as the step's result.
 *
 * It names what was stored, so a compensation can delete it.
 */
struct run_configuration_import_execute_result {
    bool success = false;
    std::string message;
    /** The files stored, run document first. */
    std::vector<std::string> stored;
    /** Input files that hold no configuration the owners keep. */
    std::vector<std::string> not_stored;
    /** World conventions the tenant already held, left unchanged. */
    std::vector<std::string> world_conventions_kept;
    /** FX conventions, which have no store yet. */
    std::vector<std::string> fx_conventions_skipped;
    /** Setup parameters the model does not hold and the import dropped, one warning each. */
    std::vector<std::string> warnings;
    std::string report_definition_id;
    /** Whether reporting stored the run document, which the undo deletes. */
    bool run_document_saved = false;
    /** The documents the owners stored, which the undo deletes. */
    std::vector<saved_document> saved_documents;
};

/**
 * @brief Deletes what an import stored.
 *
 * The workflow engine sends it as the import's compensation. Deleting what is
 * already gone succeeds, so it can run twice.
 */
struct run_configuration_import_rollback_request {
    static constexpr std::string_view nats_subject = "ore.v1.ops.run_configuration_import_rollback";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string correlation_id;
    /** The caller's JWT, which the rollback delegates to the owners. */
    std::string bearer_token;
    std::string report_definition_id;
    /** Whether reporting stored the run document, which the undo deletes. */
    bool run_document_saved = false;
    /** The documents the owners stored, which the undo deletes. */
    std::vector<saved_document> saved_documents;
};

/**
 * @brief Rebuilds a report definition's ORE input directory from the owners.
 */
struct export_run_configuration_request {
    using response_type = struct export_run_configuration_response;
    static constexpr std::string_view nats_subject = "ore.v1.ops.export_run_configuration";
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
 * @brief The run document and every configuration document the definition binds.
 */
struct export_run_configuration_response {
    bool success = false;
    std::string message;
    std::vector<run_input_file> files;
};

/**
 * @brief The members of run_input_file, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const run_input_file&) {
    return std::tuple{
        ::ores::utility::codec::field{"name", &run_input_file::name},
        ::ores::utility::codec::field{"content", &run_input_file::content},
    };
}

/**
 * @brief The members of saved_document, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const saved_document&) {
    return std::tuple{
        ::ores::utility::codec::field{"configuration_type_code",
                                      &saved_document::configuration_type_code},
        ::ores::utility::codec::field{"configuration_id", &saved_document::configuration_id},
    };
}

/**
 * @brief The members of import_run_configuration_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const import_run_configuration_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"report_definition_id",
                                      &import_run_configuration_request::report_definition_id},
        ::ores::utility::codec::field{"name", &import_run_configuration_request::name},
        ::ores::utility::codec::field{"files", &import_run_configuration_request::files},
    };
}

/**
 * @brief The members of import_run_configuration_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const import_run_configuration_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &import_run_configuration_response::success},
        ::ores::utility::codec::field{"message", &import_run_configuration_response::message},
        ::ores::utility::codec::field{"correlation_id",
                                      &import_run_configuration_response::correlation_id},
        ::ores::utility::codec::field{"workflow_instance_id",
                                      &import_run_configuration_response::workflow_instance_id},
    };
}

/**
 * @brief The members of run_configuration_import_execute_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const run_configuration_import_execute_request&) {
    return std::tuple{
        ::ores::utility::codec::field{
            "report_definition_id",
            &run_configuration_import_execute_request::report_definition_id},
        ::ores::utility::codec::field{"name", &run_configuration_import_execute_request::name},
        ::ores::utility::codec::field{"files", &run_configuration_import_execute_request::files},
        ::ores::utility::codec::field{"correlation_id",
                                      &run_configuration_import_execute_request::correlation_id},
        ::ores::utility::codec::field{"bearer_token",
                                      &run_configuration_import_execute_request::bearer_token},
    };
}

/**
 * @brief The members of run_configuration_import_execute_result, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const run_configuration_import_execute_result&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &run_configuration_import_execute_result::success},
        ::ores::utility::codec::field{"message", &run_configuration_import_execute_result::message},
        ::ores::utility::codec::field{"stored", &run_configuration_import_execute_result::stored},
        ::ores::utility::codec::field{"not_stored",
                                      &run_configuration_import_execute_result::not_stored},
        ::ores::utility::codec::field{
            "world_conventions_kept",
            &run_configuration_import_execute_result::world_conventions_kept},
        ::ores::utility::codec::field{
            "fx_conventions_skipped",
            &run_configuration_import_execute_result::fx_conventions_skipped},
        ::ores::utility::codec::field{"warnings",
                                      &run_configuration_import_execute_result::warnings},
        ::ores::utility::codec::field{
            "report_definition_id", &run_configuration_import_execute_result::report_definition_id},
        ::ores::utility::codec::field{"run_document_saved",
                                      &run_configuration_import_execute_result::run_document_saved},
        ::ores::utility::codec::field{"saved_documents",
                                      &run_configuration_import_execute_result::saved_documents},
    };
}

/**
 * @brief The members of run_configuration_import_rollback_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const run_configuration_import_rollback_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"correlation_id",
                                      &run_configuration_import_rollback_request::correlation_id},
        ::ores::utility::codec::field{"bearer_token",
                                      &run_configuration_import_rollback_request::bearer_token},
        ::ores::utility::codec::field{
            "report_definition_id",
            &run_configuration_import_rollback_request::report_definition_id},
        ::ores::utility::codec::field{
            "run_document_saved", &run_configuration_import_rollback_request::run_document_saved},
        ::ores::utility::codec::field{"saved_documents",
                                      &run_configuration_import_rollback_request::saved_documents},
    };
}

/**
 * @brief The members of export_run_configuration_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const export_run_configuration_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"report_definition_id",
                                      &export_run_configuration_request::report_definition_id},
    };
}

/**
 * @brief The members of export_run_configuration_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const export_run_configuration_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &export_run_configuration_response::success},
        ::ores::utility::codec::field{"message", &export_run_configuration_response::message},
        ::ores::utility::codec::field{"files", &export_run_configuration_response::files},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::run_input_file> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::run_input_file& v);
    static ores::ore::messaging::run_input_file read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::run_input_file& v);
    static ores::ore::messaging::run_input_file read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::saved_document> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::saved_document& v);
    static ores::ore::messaging::saved_document read_json(std::string_view text);
    static std::string write_msgpack(const ores::ore::messaging::saved_document& v);
    static ores::ore::messaging::saved_document read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::import_run_configuration_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::import_run_configuration_request& v);
    static ores::ore::messaging::import_run_configuration_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::import_run_configuration_request& v);
    static ores::ore::messaging::import_run_configuration_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::import_run_configuration_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::import_run_configuration_response& v);
    static ores::ore::messaging::import_run_configuration_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::import_run_configuration_response& v);
    static ores::ore::messaging::import_run_configuration_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT
    wire_traits<ores::ore::messaging::run_configuration_import_execute_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::ore::messaging::run_configuration_import_execute_request& v);
    static ores::ore::messaging::run_configuration_import_execute_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::run_configuration_import_execute_request& v);
    static ores::ore::messaging::run_configuration_import_execute_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT
    wire_traits<ores::ore::messaging::run_configuration_import_execute_result> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::ore::messaging::run_configuration_import_execute_result& v);
    static ores::ore::messaging::run_configuration_import_execute_result
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::run_configuration_import_execute_result& v);
    static ores::ore::messaging::run_configuration_import_execute_result
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT
    wire_traits<ores::ore::messaging::run_configuration_import_rollback_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::ore::messaging::run_configuration_import_rollback_request& v);
    static ores::ore::messaging::run_configuration_import_rollback_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::run_configuration_import_rollback_request& v);
    static ores::ore::messaging::run_configuration_import_rollback_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::export_run_configuration_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::export_run_configuration_request& v);
    static ores::ore::messaging::export_run_configuration_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::export_run_configuration_request& v);
    static ores::ore::messaging::export_run_configuration_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORE_API_EXPORT wire_traits<ores::ore::messaging::export_run_configuration_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::ore::messaging::export_run_configuration_response& v);
    static ores::ore::messaging::export_run_configuration_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::ore::messaging::export_run_configuration_response& v);
    static ores::ore::messaging::export_run_configuration_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
