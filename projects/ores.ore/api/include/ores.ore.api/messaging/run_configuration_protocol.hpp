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

#include <string>
#include <string_view>
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

}

#endif
