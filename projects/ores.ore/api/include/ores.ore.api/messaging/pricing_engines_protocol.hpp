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
#ifndef ORES_ORE_API_MESSAGING_PRICING_ENGINES_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_PRICING_ENGINES_PROTOCOL_HPP

#include "ores.ore.api/messaging/ore_document_protocol.hpp"
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::messaging {

/**
 * @brief One reason the export is refused, named so a client can act on it.
 *
 * A refusal reports every reason it found, not the first, so a person fixes
 * them in one pass.
 */
struct pricing_engines_export_problem {
    /**
     * @brief Which rule the export could not follow: document_unreadable,
     * file_name_missing or write_failed.
     */
    std::string code;
    std::string message;
};

/**
 * @brief Writes one pricing engine configuration as an ORE pricing engines
 * document.
 *
 * The configuration is named by the reporting configuration it fills, which is
 * what analytics' own read finds a document by.
 */
struct export_pricing_engines_request {
    using response_type = struct export_pricing_engines_response;
    static constexpr std::string_view nats_subject = "ore.v1.ops.export_pricing_engines";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;
};

/**
 * @brief The document, one file, and how many products it holds.
 */
struct export_pricing_engines_response {
    bool success = false;
    std::string message;
    std::uint32_t count = 0;
    std::vector<ores::ore::messaging::ore_document> files;
    std::vector<pricing_engines_export_problem> problems;
};

/**
 * @brief One product the file holds, as the preview states it.
 *
 * A person compares the file against the configurations they hold by the type,
 * the model and the engine, so the preview names them rather than counting
 * them alone.
 */
struct pricing_engines_import_product {
    std::string type_code;
    std::string model;
    std::string engine;
    /** How many settings this product carries, of both scopes. */
    std::uint32_t parameters = 0;
};

/**
 * @brief One row of the difference between the file and the target: added,
 * changed or removed, and in words what differs.
 *
 * =key= is the row's identity within its configuration, and it is what the
 * import's choices name, so a person acts on a row rather than on a position in
 * a list they cannot see twice.
 */
struct pricing_engines_import_row {
    /** The row's identity, such as =Swap#0= or =Swap#0/engine/DiscountFactor=. */
    std::string key;
    /** One of product, parameter. */
    std::string entity;
    /** One of added, changed, removed. */
    std::string change;
    /** The product the row belongs to, or the configuration for a global setting. */
    std::string owner;
    /** The setting's name, empty for a product. */
    std::string name;
    std::string what;
};

/**
 * @brief Something in the file the system does not know: a product type with no
 * vocabulary row, an element the tables cannot hold, or a row the file states
 * twice.
 */
struct pricing_engines_import_issue {
    /** One of unknown_type, dropped, duplicate. */
    std::string kind;
    std::string subject;
    std::string detail;
};

/**
 * @brief Parses a pricing engines document and says what importing it would do.
 *
 * Nothing is written. A file of another kind is refused, and the refusal names
 * the root element found. The target is optional: a file read against no
 * target is a file for a configuration that does not exist yet, so every row it
 * holds is an addition.
 */
struct preview_pricing_engines_import_request {
    using response_type = struct preview_pricing_engines_import_response;
    static constexpr std::string_view nats_subject = "ore.v1.ops.preview_pricing_engines_import";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string content;
    /** The reporting configuration to compare against, or empty for none. */
    std::string target_configuration_id;
};

/**
 * @brief What the file holds, what the system does not know, and how it differs
 * from the target.
 */
struct preview_pricing_engines_import_response {
    bool success = false;
    std::string message;
    std::string root_element;
    std::uint32_t in_file_products = 0;
    std::uint32_t in_file_parameters = 0;
    std::vector<pricing_engines_import_product> products;
    /** The names of the root-level settings the file states. */
    std::vector<std::string> global_parameters;
    /** The configuration the file was compared against, or empty for none. */
    std::string target_name;
    /** Rows the target holds with the file's values. */
    std::uint32_t held = 0;
    std::uint32_t added = 0;
    std::uint32_t changed = 0;
    /** Rows the target holds and the file omits. */
    std::uint32_t removed = 0;
    std::vector<pricing_engines_import_row> rows;
    std::vector<pricing_engines_import_issue> issues;
};

/**
 * @brief Writes a pricing engines document with the person's choices.
 *
 * The target is an existing configuration, named by the reporting
 * configuration it fills, or a new one, named here. A new configuration is
 * registered in reporting first and removed again when the document is
 * refused, so a refused import leaves nothing behind.
 *
 * A removed row the choices do not name is dropped, which is the file's
 * reading; the screen names every removed row it keeps, because a row a file
 * omits is usually omitted by accident.
 */
struct import_pricing_engines_request {
    using response_type = struct import_pricing_engines_response;
    static constexpr std::string_view nats_subject = "ore.v1.ops.import_pricing_engines";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string content;
    /** The reporting configuration to write into, or empty to make a new one. */
    std::string target_configuration_id;
    std::string new_configuration_name;
    std::string new_configuration_variant;
    /** The keys of the removed rows the person chose to keep. */
    std::vector<std::string> removals_kept;
    std::string reason_code;
    std::string commentary;
};

/**
 * @brief A row the import could not write, and the owner's reason.
 */
struct pricing_engines_import_error {
    std::string key;
    std::string message;
};

/**
 * @brief What the import wrote, kept and could not write.
 */
struct import_pricing_engines_response {
    bool success = false;
    std::string message;
    /** The pricing engine configuration the import wrote, as the screen names it. */
    std::string configuration_id;
    std::string configuration_name;
    std::uint32_t added = 0;
    std::uint32_t changed = 0;
    std::uint32_t deleted = 0;
    std::uint32_t kept = 0;
    std::vector<pricing_engines_import_error> errors;
};

}

#endif
